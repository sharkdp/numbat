use std::{collections::HashMap, fmt::Display};

use crate::{
    interpreter::{InterpreterError, InterpreterResult, Result},
    quantity::Quantity,
    unit::Unit,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum Op {
    /// Push the value of the specified constant onto the stack
    LoadConstant,

    /// Set the specified variable to the value on top of the stack
    SetVariable,
    /// Push the value of the specified variable onto the stack
    GetVariable,

    /// Negate the top of the stack
    Negate,

    /// Pop two values off the stack, add them, push the result onto
    /// the stack.
    Add,
    /// Similar to Add.
    Subtract,
    /// Similar to Add.
    Multiply,
    /// Similar to Add.
    Divide,
    /// Similar to Add.
    Power,
    /// Similar to Add.
    ConvertTo,

    Return,
    List,
    Exit,
}

impl Op {
    fn num_operands(self) -> usize {
        match self {
            Op::LoadConstant | Op::SetVariable | Op::GetVariable => 1,
            Op::Add
            | Op::Subtract
            | Op::Multiply
            | Op::Divide
            | Op::Power
            | Op::ConvertTo
            | Op::Negate
            | Op::Return
            | Op::List
            | Op::Exit => 0,
        }
    }

    fn to_string(self) -> &'static str {
        match self {
            Op::LoadConstant => "LoadConstant",
            Op::SetVariable => "SetVariable",
            Op::GetVariable => "GetVariable",
            Op::Negate => "Negate",
            Op::Add => "Add",
            Op::Subtract => "Subtract",
            Op::Multiply => "Multiply",
            Op::Divide => "Divide",
            Op::Power => "Power",
            Op::ConvertTo => "ConvertTo",
            Op::Return => "Return",
            Op::List => "List",
            Op::Exit => "Exit",
        }
    }
}

pub enum Constant {
    Scalar(f64),
    Unit(Unit),
}

impl Constant {
    fn to_quantity(&self) -> Quantity {
        match self {
            Constant::Scalar(n) => Quantity::from_scalar(*n),
            Constant::Unit(u) => Quantity::from_unit(u.clone()),
        }
    }
}

impl Display for Constant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Constant::Scalar(n) => write!(f, "{}", n),
            Constant::Unit(unit) => write!(f, "{}", unit),
        }
    }
}

pub struct Vm {
    /// The actual code of the program, structured by function name. The code
    /// for the global scope is at index 0 under the function name `<main>`.
    bytecode: Vec<(String, Vec<u8>)>,

    /// An index into the `bytecode` vector referring to the function which is
    /// currently being compiled.
    current_chunk_index: usize,

    /// Constants are numbers like '1.4' or a [Unit] like 'meter'.
    constants: Vec<Constant>,

    /// Identifiers are the names of variables or the names of [Unit]s.
    identifiers: Vec<String>,

    /// A dictionary of global variables and their respective values.
    globals: HashMap<String, Quantity>,

    /// The stack of the VM. Each entry is a [Quantity], i.e. something like `3.4 m/s²`.
    stack: Vec<Quantity>,

    /// Instruction "pointer". An index into the bytecode of the currently executed
    /// function.
    ip: usize,

    /// Whether or not to run in debug mode.
    debug: bool,
}

impl Vm {
    pub fn new(debug: bool) -> Self {
        Self {
            bytecode: vec![("<main>".into(), vec![])],
            current_chunk_index: 0,
            constants: vec![],
            identifiers: vec![],
            globals: HashMap::new(),
            stack: vec![],
            ip: 0,
            debug,
        }
    }

    // The following functions are helpers for the compilation process

    fn current_chunk_mut(&mut self) -> &mut Vec<u8> {
        &mut self.bytecode[self.current_chunk_index].1
    }

    pub fn add_op(&mut self, op: Op) {
        self.current_chunk_mut().push(op as u8);
    }

    pub fn add_op1(&mut self, op: Op, arg: u8) {
        let current_chunk = self.current_chunk_mut();
        current_chunk.push(op as u8);
        current_chunk.push(arg);
    }

    pub fn add_constant(&mut self, constant: Constant) -> u8 {
        self.constants.push(constant);
        assert!(self.constants.len() <= u8::MAX as usize);
        (self.constants.len() - 1) as u8 // TODO: this can overflow, see above
    }

    pub fn add_identifier(&mut self, identifier: &str) -> u8 {
        if let Some(idx) = self.identifiers.iter().position(|i| i == identifier) {
            return idx as u8;
        }

        self.identifiers.push(identifier.to_owned());
        assert!(self.identifiers.len() <= u8::MAX as usize);
        (self.identifiers.len() - 1) as u8 // TODO: this can overflow, see above
    }

    pub(crate) fn begin_function(&mut self, name: &str) {
        self.bytecode.push((name.into(), vec![]));
        self.current_chunk_index = self.bytecode.len() - 1
    }

    pub(crate) fn end_function(&mut self) {
        // Continue compilation of "main"/global code
        self.current_chunk_index = 0;
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
        for (function_name, bytecode) in &self.bytecode {
            println!(".CODE ({})", function_name);
            let mut offset = 0;
            while offset < bytecode.len() {
                let this_offset = offset;
                let op = bytecode[offset];
                offset += 1;
                let op = unsafe { std::mem::transmute::<u8, Op>(op) };

                let operands = &bytecode[offset..(offset + op.num_operands())];
                offset += op.num_operands();

                let operands_str = operands
                    .iter()
                    .map(u8::to_string)
                    .collect::<Vec<String>>()
                    .join(" ");

                print!(
                    "  {:04} {:<13} {}",
                    this_offset,
                    op.to_string(),
                    operands_str
                );

                if op == Op::LoadConstant {
                    print!("     (value: {})", self.constants[operands[0] as usize]);
                }
                println!();
            }
        }
        println!();
    }

    // The following functions are helpers for the actual execution of the code

    fn read_byte(&mut self) -> u8 {
        let byte = self.bytecode[0].1[self.ip]; // TODO
        self.ip += 1;
        byte
    }

    fn push(&mut self, quantity: Quantity) {
        self.stack.push(quantity);
    }

    fn pop(&mut self) -> Quantity {
        self.stack.pop().expect("stack not empty")
    }

    pub fn run(&mut self) -> Result<InterpreterResult> {
        let result = self.run_without_cleanup();
        if result.is_err() {
            // Perform cleanup: clear the stack and move IP to the end
            self.stack.clear();
            self.ip = self.bytecode[0].1.len();
        }
        result
    }

    fn run_without_cleanup(&mut self) -> Result<InterpreterResult> {
        if self.ip >= self.bytecode[0].1.len() {
            return Ok(InterpreterResult::Continue);
        }

        loop {
            self.debug();

            let op = unsafe { std::mem::transmute::<u8, Op>(self.read_byte()) };

            match op {
                Op::LoadConstant => {
                    let constant_idx = self.read_byte();
                    self.stack
                        .push(self.constants[constant_idx as usize].to_quantity());
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
                op @ (Op::Add
                | Op::Subtract
                | Op::Multiply
                | Op::Divide
                | Op::Power
                | Op::ConvertTo) => {
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
                        Op::Power => lhs.power(rhs),
                        Op::ConvertTo => lhs.convert_to(rhs.unit()),
                        _ => unreachable!(),
                    };
                    self.push(result.map_err(InterpreterError::ConversionError)?);
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
                .join("] [")
        );
    }
}

#[test]
fn vm_basic() {
    let mut vm = Vm::new(false);
    vm.add_constant(Constant::Scalar(42.0));
    vm.add_constant(Constant::Scalar(1.0));

    vm.add_op1(Op::LoadConstant, 0);
    vm.add_op1(Op::LoadConstant, 1);
    vm.add_op(Op::Add);
    vm.add_op(Op::Return);

    assert_eq!(
        vm.run().unwrap(),
        InterpreterResult::Quantity(Quantity::from_scalar(42.0 + 1.0))
    );
}
