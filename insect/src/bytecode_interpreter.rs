use std::collections::HashMap;

use crate::ffi;
use crate::interpreter::{Interpreter, InterpreterResult, Result, RuntimeError};
use crate::prefix::Prefix;
use crate::typed_ast::{BinaryOperator, Expression, Statement};
use crate::unit::Unit;
use crate::unit_registry::UnitRegistry;
use crate::vm::{Constant, Op, Vm};

pub struct BytecodeInterpreter {
    vm: Vm,
    unit_registry: UnitRegistry, // TODO(minor): do we even need the unit registry here?
    /// List of local variables currently in scope
    local_variables: Vec<String>,
    // Maps names of units to indices of the respective constants in the VM
    unit_name_to_constant_index: HashMap<(Prefix, String), u8>,
}

impl BytecodeInterpreter {
    fn compile_expression(&mut self, expr: &Expression) -> Result<()> {
        match expr {
            Expression::Scalar(n) => {
                let index = self.vm.add_constant(Constant::Scalar(n.to_f64()));
                self.vm.add_op1(Op::LoadConstant, index);
            }
            Expression::Identifier(identifier, _type) => {
                if let Some(position) = self.local_variables.iter().position(|n| n == identifier) {
                    self.vm.add_op1(Op::GetLocal, position as u8); // TODO: check overflow
                } else {
                    let identifier_idx = self.vm.add_global_identifier(identifier);
                    self.vm.add_op1(Op::GetVariable, identifier_idx);
                }
            }
            Expression::UnitIdentifier(prefix, unit_name, _type) => {
                if let Some(index) = self
                    .unit_name_to_constant_index
                    .get(&(*prefix, unit_name.clone()))
                // TODO: (1) resolve the unwrap (2) can we get rid of the clone?
                {
                    self.vm.add_op1(Op::LoadConstant, *index);
                } else {
                    let index_prefixless = self
                        .unit_name_to_constant_index
                        .get(&(Prefix::none(), unit_name.clone()))
                        .expect("Unit has been defined");
                    if let Constant::Unit(ref unit) = self.vm.constants[*index_prefixless as usize]
                    {
                        let index = self
                            .vm
                            .add_constant(Constant::Unit(unit.clone().with_prefix(*prefix)));
                        self.vm.add_op1(Op::LoadConstant, index);
                    } else {
                        unreachable!() // TODO(minor): this is a bit ugly. maybe store the units here instead of extracting them again from the VM constants?
                    }
                }
            }
            Expression::Negate(rhs, _type) => {
                self.compile_expression(rhs)?;
                self.vm.add_op(Op::Negate);
            }
            Expression::BinaryOperator(operator, lhs, rhs, _type) => {
                self.compile_expression(lhs)?;
                self.compile_expression(rhs)?;

                let op = match operator {
                    BinaryOperator::Add => Op::Add,
                    BinaryOperator::Sub => Op::Subtract,
                    BinaryOperator::Mul => Op::Multiply,
                    BinaryOperator::Div => Op::Divide,
                    BinaryOperator::Power => Op::Power,
                    BinaryOperator::ConvertTo => Op::ConvertTo,
                };
                self.vm.add_op(op);
            }
            Expression::FunctionCall(name, args, _type) => {
                // Put all arguments on top of the stack
                for arg in args {
                    self.compile_expression(arg)?;
                }

                if let Some(idx) = self.vm.get_ffi_callable_idx(name) {
                    // TODO: check overflow:
                    self.vm.add_op2(Op::FFICallFunction, idx, args.len() as u8);
                } else {
                    let idx = self.vm.get_function_idx(name);

                    self.vm.add_op2(Op::Call, idx, args.len() as u8); // TODO: check overflow
                }
            }
        };

        Ok(())
    }

    fn compile_statement(&mut self, stmt: &Statement) -> Result<()> {
        match stmt {
            Statement::Expression(expr) => {
                self.compile_expression(expr)?;
                self.vm.add_op(Op::Return);
            }
            Statement::DeclareVariable(identifier, expr, _dexpr) => {
                self.compile_expression(expr)?;
                let identifier_idx = self.vm.add_global_identifier(identifier);
                self.vm.add_op1(Op::SetVariable, identifier_idx);
            }
            Statement::DeclareFunction(name, parameters, Some(expr), _return_type) => {
                self.vm.begin_function(name);
                for parameter in parameters.iter() {
                    self.local_variables.push(parameter.0.clone());
                }
                self.compile_expression(expr)?;
                self.vm.add_op(Op::Return);
                for _ in parameters {
                    self.local_variables.pop();
                }
                self.vm.end_function();
            }
            Statement::DeclareFunction(name, parameters, None, _return_type) => {
                // Declaring a foreign function does not generate any bytecode. But we register
                // its name and arity here to be able to distinguish it from normal functions.

                self.vm
                    .add_foreign_function(name, parameters.len()..=parameters.len());
            }
            Statement::DeclareDimension(_name) => {
                // Declaring a dimension is like introducing a new type. The information
                // is only relevant for the type checker. Nothing happens at run time.
            }
            Statement::DeclareBaseUnit(unit_name, _decorators, dexpr) => {
                self.unit_registry
                    .add_base_unit(unit_name, dexpr.clone())
                    .map_err(RuntimeError::UnitRegistryError)?;

                let constant_idx = self
                    .vm
                    .add_constant(Constant::Unit(Unit::new_standard(unit_name)));
                self.unit_name_to_constant_index
                    .insert((Prefix::none(), unit_name.into()), constant_idx);
            }
            Statement::DeclareDerivedUnit(unit_name, expr, _decorators) => {
                self.unit_registry
                    .add_derived_unit(unit_name, expr)
                    .map_err(RuntimeError::UnitRegistryError)?;

                let constant_idx = self
                    .vm
                    .add_constant(Constant::Unit(Unit::new_standard("<dummy>"))); // TODO: dummy is just a temp. value until the SetUnitConstant op runs
                let identifier_idx = self.vm.add_global_identifier(unit_name); // TODO: there is some asymmetry here because we do not introduce identifiers for base units

                self.compile_expression(expr)?;
                self.vm
                    .add_op2(Op::SetUnitConstant, identifier_idx, constant_idx);
                self.unit_name_to_constant_index
                    .insert((Prefix::none(), unit_name.into()), constant_idx);
            }
            Statement::ProcedureCall(kind, args) => {
                // Put all arguments on top of the stack
                for arg in args {
                    self.compile_expression(arg)?;
                }

                let name = &ffi::procedures().get(kind).unwrap().name;

                let idx = self.vm.get_ffi_callable_idx(name).unwrap();
                self.vm.add_op2(Op::FFICallProcedure, idx, args.len() as u8); // TODO: check overflow
            }
        }

        Ok(())
    }

    fn run(&mut self) -> Result<InterpreterResult> {
        self.vm.disassemble();

        let result = self.vm.run();

        self.vm.debug();

        result
    }
}

impl Interpreter for BytecodeInterpreter {
    fn new(debug: bool) -> Self {
        Self {
            vm: Vm::new(debug),
            unit_registry: UnitRegistry::new(),
            local_variables: vec![],
            unit_name_to_constant_index: HashMap::new(),
        }
    }

    fn interpret_statement(&mut self, statement: &Statement) -> Result<InterpreterResult> {
        self.compile_statement(statement)?;
        self.run()
    }
}
