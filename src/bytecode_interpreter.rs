use crate::interpreter::{Interpreter, InterpreterError, InterpreterResult, Result};
use crate::typed_ast::{BinaryOperator, Command, Expression, Statement};
use crate::unit::Unit;
use crate::unit_registry::UnitRegistry;
use crate::vm::{Constant, Op, Vm};

pub struct BytecodeInterpreter {
    vm: Vm,
    unit_registry: UnitRegistry,
    /// List of local variables currently in scope
    local_variables: Vec<(String, usize)>,
}

impl BytecodeInterpreter {
    fn compile_expression(&mut self, expr: &Expression) -> Result<()> {
        match expr {
            Expression::Scalar(n) => {
                let index = self.vm.add_constant(Constant::Scalar(n.to_f64()));
                self.vm.add_op1(Op::LoadConstant, index);
            }
            Expression::Identifier(identifier, _type) => {
                if let Some(position) = self
                    .local_variables
                    .iter()
                    .position(|(n, _)| n == identifier)
                {
                    let slot_idx = self.local_variables[position].1;
                    self.vm.add_op1(Op::GetLocal, slot_idx as u8); // TODO: check overflow
                } else {
                    let identifier_idx = self.vm.add_global_identifier(identifier);
                    self.vm.add_op1(Op::GetVariable, identifier_idx);
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
                let idx = self.vm.get_function_idx(name);
                // Put all arguments on top of the stacak
                for arg in args {
                    self.compile_expression(arg)?;
                }
                self.vm.add_op1(Op::Call, idx);
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
            Statement::Command(Command::List) => {
                self.vm.add_op(Op::List);
            }
            Statement::Command(Command::Exit) => {
                self.vm.add_op(Op::Exit);
            }
            Statement::DeclareVariable(identifier, expr, _dexpr) => {
                self.compile_expression(expr)?;
                let identifier_idx = self.vm.add_global_identifier(identifier);
                self.vm.add_op1(Op::SetVariable, identifier_idx);
            }
            Statement::DeclareFunction(name, parameters, expr, _return_type) => {
                let arity = parameters.len();
                self.vm.begin_function(&name);
                for (i, parameter) in parameters.iter().enumerate() {
                    let slot_idx = arity - i - 1;
                    self.local_variables.push((parameter.0.clone(), slot_idx));
                }
                self.compile_expression(expr)?;
                self.vm.add_op(Op::Return);
                for _ in parameters {
                    self.local_variables.pop();
                }
                self.vm.end_function();
            }
            Statement::DeclareDimension(_name) => {
                // Declaring a dimension is like introducing a new type. The information
                // is only relevant for the type checker. Nothing happens at run time.
            }
            Statement::DeclareBaseUnit(name, dexpr) => {
                self.unit_registry
                    .add_base_unit(name, dexpr.clone())
                    .map_err(InterpreterError::UnitRegistryError)?;

                let constant_idx = self
                    .vm
                    .add_constant(Constant::Unit(Unit::new_standard(name)));
                self.vm.add_op1(Op::LoadConstant, constant_idx);
                let identifier_idx = self.vm.add_global_identifier(name);
                self.vm.add_op1(Op::SetVariable, identifier_idx);
            }
            Statement::DeclareDerivedUnit(name, expr, _dexpr) => {
                self.unit_registry
                    .add_derived_unit(name, expr)
                    .map_err(InterpreterError::UnitRegistryError)?;

                let constant_idx = self
                    .vm
                    .add_constant(Constant::Unit(Unit::new_standard(name)));
                self.vm.add_op1(Op::LoadConstant, constant_idx);
                let identifier_idx = self.vm.add_global_identifier(name);
                self.vm.add_op1(Op::SetVariable, identifier_idx);
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
        }
    }

    fn interpret_statement(&mut self, statement: &Statement) -> Result<InterpreterResult> {
        self.compile_statement(statement)?;
        self.run()
    }
}
