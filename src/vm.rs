use std::collections::HashMap;

use crate::{
    interpreter::{InterpreterError, InterpreterResult, Result},
    quantity::Quantity,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
    stack: Vec<Quantity>,
    globals: HashMap<String, Quantity>,
    ip: usize,
    debug: bool,
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
            debug: false,
        }
    }

    pub fn disassemble(&self) {
        if !self.debug {
            return;
        }

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
            let this_offset = offset;
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

            print!(
                "  {:04} {:<10} {}",
                this_offset,
                op.to_string(),
                operands_str
            );

            if op == Op::Constant {
                print!("     (value: {})", self.constants[operands[0] as usize]);
            }
            println!();
        }
        println!();
    }

    pub fn run(&mut self) -> Result<InterpreterResult> {
        let result = self.run_without_cleanup();
        if result.is_err() {
            // Perform cleanup: clear the stack and move IP to the end
            self.stack.clear();
            self.ip = self.bytecode.len();
        }
        result
    }

    fn run_without_cleanup(&mut self) -> Result<InterpreterResult> {
        loop {
            self.debug();
            let op = unsafe { std::mem::transmute::<u8, Op>(self.read_byte()) };

            match op {
                Op::Constant => {
                    let constant_idx = self.read_byte();
                    self.stack
                        .push(Quantity::scalar(self.constants[constant_idx as usize]));
                }
                Op::SetVariable => {
                    let identifier_idx = self.read_byte();
                    let quantity = self.pop();
                    let identifier: String = self.identifiers[identifier_idx as usize].clone();

                    self.globals.insert(identifier, quantity);

                    return Ok(InterpreterResult::Continue);
                }
                Op::GetVariable => {
                    let identifier_idx = self.read_byte();
                    let identifier = &self.identifiers[identifier_idx as usize];

                    let quantity = self
                        .globals
                        .get(identifier)
                        .ok_or_else(|| InterpreterError::UnknownVariable(identifier.clone()))?;

                    self.push(quantity.clone());
                }
                op @ (Op::Add | Op::Subtract | Op::Multiply | Op::Divide) => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    let result = match op {
                        Op::Add => lhs + rhs,
                        Op::Subtract => lhs - rhs,
                        Op::Multiply => lhs * rhs,
                        Op::Divide => {
                            // TODO: should this be implemented in Quantity::div?
                            if rhs.is_zero() {
                                return Err(InterpreterError::DivisionByZero);
                            } else {
                                lhs / rhs
                            }
                        }
                        _ => unreachable!(),
                    };
                    self.push(result.map_err(InterpreterError::UnitError)?);
                }
                Op::Negate => {
                    let rhs = self.pop();
                    self.push(-rhs);
                }
                Op::Return => return Ok(InterpreterResult::Quantity(self.pop())),
                Op::List => {
                    return Ok(InterpreterResult::Continue);
                }
                Op::Exit => {
                    return Ok(InterpreterResult::Exit);
                }
            }
        }
    }

    fn push(&mut self, quantity: Quantity) {
        self.stack.push(quantity);
    }

    fn pop(&mut self) -> Quantity {
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
        if let Some(idx) = self.identifiers.iter().position(|i| i == identifier) {
            return idx as u8;
        }

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
        if !self.debug {
            return;
        }

        print!("IP = {}, ", self.ip);
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

    assert_eq!(
        vm.run().unwrap(),
        InterpreterResult::Quantity(Quantity::scalar(42.0 + 1.0))
    );
}
