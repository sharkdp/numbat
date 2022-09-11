use crate::ast::{BinaryOperator, Command, Expression, Statement};
use crate::dimension::DimensionRegistry;
use crate::interpreter::{Interpreter, InterpreterError, InterpreterResult, Result};
use crate::unit::Unit;
use crate::unit_registry::UnitRegistry;
use crate::vm::{Constant, Op, Vm};

pub struct BytecodeInterpreter {
    vm: Vm,
    dimension_registry: DimensionRegistry,
    unit_registry: UnitRegistry,
}

impl BytecodeInterpreter {
    fn compile_expression(&mut self, expr: &Expression) -> Result<()> {
        match expr {
            Expression::Scalar(n) => {
                let index = self.vm.add_constant(Constant::Scalar(n.to_f64()));
                self.vm.add_op1(Op::Constant, index);
            }
            Expression::Identifier(identifier) => {
                let identifier_idx = self.vm.add_identifier(identifier);
                self.vm.add_op1(Op::GetVariable, identifier_idx);
            }
            Expression::Negate(rhs) => {
                self.compile_expression(rhs)?;
                self.vm.add_op(Op::Negate);
            }
            Expression::BinaryOperator(operator, lhs, rhs) => {
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
            Statement::DeclareVariable(identifier, expr) => {
                self.compile_expression(expr)?;
                let identifier_idx = self.vm.add_identifier(identifier);
                self.vm.add_op1(Op::SetVariable, identifier_idx);
            }
            Statement::DeclareDimension(name, exprs) => {
                if exprs.is_empty() {
                    self.dimension_registry
                        .add_base_dimension(name)
                        .map_err(InterpreterError::RegistryError)?;
                } else {
                    self.dimension_registry
                        .add_derived_dimension(name, &exprs[0])
                        .map_err(InterpreterError::RegistryError)?;

                    let base_representation = self
                        .dimension_registry
                        .get_base_representation_for_name(name)
                        .expect("we just inserted it");

                    for alternative_expr in &exprs[1..] {
                        let alternative_base_representation = self
                            .dimension_registry
                            .get_base_representation(alternative_expr)
                            .map_err(InterpreterError::RegistryError)?;
                        if alternative_base_representation != base_representation {
                            return Err(
                                InterpreterError::IncompatibleAlternativeDimensionExpression(
                                    name.clone(),
                                ),
                            );
                        }
                    }
                }

                self.vm.add_op(Op::List); // TODO
            }
            Statement::DeclareBaseUnit(name, dexpr) => {
                self.unit_registry
                    .add_base_unit(name, dexpr.clone())
                    .map_err(InterpreterError::UnitRegistryError)?;

                let constant_idx = self
                    .vm
                    .add_constant(Constant::Unit(Unit::new_standard(name)));
                self.vm.add_op1(Op::Constant, constant_idx);
                let identifier_idx = self.vm.add_identifier(name);
                self.vm.add_op1(Op::SetVariable, identifier_idx);
            }
            Statement::DeclareDerivedUnit(name, expr, dexpr) => {
                self.unit_registry
                    .add_derived_unit(name, expr, &self.dimension_registry, dexpr.as_ref())
                    .map_err(InterpreterError::UnitRegistryError)?;

                let constant_idx = self
                    .vm
                    .add_constant(Constant::Unit(Unit::new_standard(name)));
                self.vm.add_op1(Op::Constant, constant_idx);
                let identifier_idx = self.vm.add_identifier(name);
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
            dimension_registry: DimensionRegistry::new(),
            unit_registry: UnitRegistry::new(),
        }
    }

    fn interpret_statement(&mut self, statement: &Statement) -> Result<InterpreterResult> {
        self.compile_statement(statement)?;
        self.run()
    }
}
