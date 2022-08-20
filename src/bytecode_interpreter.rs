use crate::ast::{BinaryOperator, Command, Expression, Statement};
use crate::interpreter::{Interpreter, InterpreterResult, Result};
use crate::vm::{Op, Vm};

pub struct BytecodeInterpreter {
    vm: Vm,
}

impl BytecodeInterpreter {
    pub fn new() -> Self {
        Self { vm: Vm::new() }
    }

    fn compile_expression(&mut self, expr: &Expression) -> Result<()> {
        match expr {
            Expression::Scalar(n) => {
                let index = self.vm.add_constant(n.to_f64());
                self.vm.add_op1(Op::Constant, index);
            }
            Expression::Identifier(_) => todo!(),
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
                    BinaryOperator::ConvertTo => todo!(),
                };
                self.vm.add_op(op);
            }
        };

        Ok(())
    }

    fn compile_statement(&mut self, stmt: &Statement) -> Result<()> {
        match stmt {
            Statement::Expression(expr) => {
                self.compile_expression(&expr)?;
                self.vm.add_op(Op::Return);
            }
            Statement::Command(Command::List) => {
                self.vm.add_op(Op::List);
            }
            Statement::Command(Command::Exit) => {
                self.vm.add_op(Op::Exit);
            }
            Statement::Assignment(_, _) => {
                todo!();
            }
        }

        Ok(())
    }

    fn run(&mut self) -> Result<InterpreterResult> {
        self.vm.disassemble();
        self.vm.run()
    }
}

impl Interpreter for BytecodeInterpreter {
    fn interpret(&mut self, stmt: &Statement) -> Result<InterpreterResult> {
        self.compile_statement(stmt)?;
        self.run()
    }
}
