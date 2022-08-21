use std::collections::HashMap;

use crate::interpreter::{InterpreterError, InterpreterResult, Result};

#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(u8)]
pub enum Op {
    Constant,

    SetVariable,
    GetVariable,

    Negate,

    Add,
    Subtract,
    Multiply,
    Divide,

    Return,

    List,

    Exit,
}

impl Op {
    fn num_operands(self) -> usize {
        match self {
            Op::Constant | Op::SetVariable | Op::GetVariable => 1,
            Op::Add
            | Op::Subtract
            | Op::Multiply
            | Op::Divide
            | Op::Negate
            | Op::Return
            | Op::List
            | Op::Exit => 0,
        }
    }

    fn to_string(self) -> &'static str {
        match self {
            Op::Constant => "Constant",
            Op::SetVariable => "SetVariable",
            Op::GetVariable => "GetVariable",
            Op::Negate => "Negate",
            Op::Add => "Add",
            Op::Subtract => "Subtract",
            Op::Multiply => "Multiply",
            Op::Divide => "Divide",
            Op::Return => "Return",
            Op::List => "List",
            Op::Exit => "Exit",
        }
    }
}

pub struct Vm {
    constants: Vec<f64>,
    identifiers: Vec<String>,
    bytecode: Vec<u8>,
    stack: Vec<f64>,
    globals: HashMap<String, f64>,
    ip: usize,
}

impl Vm {
    pub fn new() -> Self {
        Self {
            constants: vec![],
            identifiers: vec![],
            bytecode: vec![],
            stack: vec![],
            globals: HashMap::new(),
            ip: 0,
        }
    }

    pub fn disassemble(&self) {
        println!();
        println!(".CONSTANTS");
        for (idx, constant) in self.constants.iter().enumerate() {
            println!("  {:04} {}", idx, constant);
        }
        println!(".IDENTIFIERS");
        for (idx, identifier) in self.identifiers.iter().enumerate() {
            println!("  {:04} {}", idx, identifier);
        }
        println!(".CODE");
        let mut offset = 0;
        while offset < self.bytecode.len() {
            let op = self.bytecode[offset];
            offset += 1;
            let op = unsafe { std::mem::transmute::<u8, Op>(op) };

            let operands = &self.bytecode[offset..(offset + op.num_operands())];
            offset += op.num_operands();

            let operands_str = operands
                .iter()
                .map(u8::to_string)
                .collect::<Vec<String>>()
                .join(" ");

            print!("  {:04} {:<10} {}", offset, op.to_string(), operands_str);

            if op == Op::Constant {
                print!("     (value: {})", self.constants[operands[0] as usize]);
            }
            println!();
        }
        println!();
    }

    pub fn run(&mut self) -> Result<InterpreterResult> {
        loop {
            let op = unsafe { std::mem::transmute::<u8, Op>(self.read_byte()) };

            match op {
                Op::Constant => {
                    let constant_idx = self.read_byte();
                    self.stack.push(self.constants[constant_idx as usize]);
                }
                Op::SetVariable => {
                    let identifier_idx = self.read_byte();
                    let value = self.pop();
                    let identifier: String = self.identifiers[identifier_idx as usize].clone();

                    self.globals.insert(identifier, value);

                    return Ok(InterpreterResult::Continue);
                }
                Op::GetVariable => {
                    let identifier_idx = self.read_byte();
                    let identifier = &self.identifiers[identifier_idx as usize];

                    let value = self
                        .globals
                        .get(identifier)
                        .ok_or_else(|| InterpreterError::UnknownVariable(identifier.clone()))?;

                    // TODO:
                    // execute the program
                    //   foo
                    //   bar
                    // => stack is not cleaned up after first error

                    self.push(*value);
                }
                op @ (Op::Add | Op::Subtract | Op::Multiply | Op::Divide) => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    let result = match op {
                        Op::Add => lhs + rhs,
                        Op::Subtract => lhs - rhs,
                        Op::Multiply => lhs * rhs,
                        Op::Divide => {
                            if rhs == 0.0 {
                                return Err(InterpreterError::DivisionByZero);
                            } else {
                                lhs / rhs
                            }
                        }
                        _ => unreachable!(),
                    };
                    self.push(result);
                }
                Op::Negate => {
                    let rhs = self.pop();
                    self.push(-rhs);
                }
                Op::Return => return Ok(InterpreterResult::Value(self.pop())),
                Op::List => {
                    println!("List of variables:");
                    return Ok(InterpreterResult::Continue);
                }
                Op::Exit => {
                    return Ok(InterpreterResult::Exit);
                }
            }
        }
    }

    fn push(&mut self, value: f64) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> f64 {
        self.stack.pop().expect("stack not empty")
    }

    fn read_byte(&mut self) -> u8 {
        let byte = self.bytecode[self.ip];
        self.ip += 1;
        byte
    }

    pub fn add_constant(&mut self, x: f64) -> u8 {
        self.constants.push(x);
        (self.constants.len() - 1) as u8 // TODO: this can overflow
    }

    pub fn add_identifier(&mut self, identifier: &str) -> u8 {
        // TODO: do not push identifiers multiple times

        self.identifiers.push(identifier.to_owned());
        (self.identifiers.len() - 1) as u8 // TODO: this can overflow
    }

    pub fn add_op(&mut self, op: Op) {
        self.bytecode.push(op as u8);
    }

    pub fn add_op1(&mut self, op: Op, arg: u8) {
        self.bytecode.push(op as u8);
        self.bytecode.push(arg);
    }

    pub fn debug(&self) {
        println!("IP = {}", self.ip);
        println!(
            "Stack: [{}]",
            self.stack
                .iter()
                .map(|x| x.to_string())
                .collect::<Vec<_>>()
                .join(" ")
        );
    }
}

#[test]
fn vm_basic() {
    let mut vm = Vm::new();
    vm.add_constant(42.0);
    vm.add_constant(1.0);

    vm.add_op1(Op::Constant, 0);
    vm.add_op1(Op::Constant, 1);
    vm.add_op(Op::Add);
    vm.add_op(Op::Return);

    assert_eq!(vm.run().unwrap(), InterpreterResult::Value(42.0 + 1.0));
}
