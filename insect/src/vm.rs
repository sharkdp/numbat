use std::{collections::HashMap, fmt::Display};

use crate::{
    ffi::{Callable, ForeignFunction},
    interpreter::{InterpreterResult, Result, RuntimeError},
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

    /// Push the value of the specified local variable onto the stack (even
    /// though it is already on the stack, somewhere lower down).
    GetLocal,

    /// Negate the top of the stack
    Negate,

    /// Pop two values off the stack, add them, push the result onto the stack.
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

    /// Call the specified function with the specified number of arguments
    Call,
    /// Same as above, but call a foreign/native function
    FFICallFunction,
    /// Same as above, but call a macro which does not return anything (does not push a value onto the stack)
    FFICallMacro,

    /// Return from the current function
    Return,
}

impl Op {
    fn num_operands(self) -> usize {
        match self {
            Op::Call => 2,
            Op::LoadConstant
            | Op::SetVariable
            | Op::GetVariable
            | Op::GetLocal
            | Op::FFICallFunction
            | Op::FFICallMacro => 1,
            Op::Negate
            | Op::Add
            | Op::Subtract
            | Op::Multiply
            | Op::Divide
            | Op::Power
            | Op::ConvertTo
            | Op::Return => 0,
        }
    }

    fn to_string(self) -> &'static str {
        match self {
            Op::LoadConstant => "LoadConstant",
            Op::SetVariable => "SetVariable",
            Op::GetVariable => "GetVariable",
            Op::GetLocal => "GetLocal",
            Op::Negate => "Negate",
            Op::Add => "Add",
            Op::Subtract => "Subtract",
            Op::Multiply => "Multiply",
            Op::Divide => "Divide",
            Op::Power => "Power",
            Op::ConvertTo => "ConvertTo",
            Op::Call => "Call",
            Op::FFICallFunction => "FFICallFunction",
            Op::FFICallMacro => "FFICallMacro",
            Op::Return => "Return",
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

struct CallFrame {
    /// The function being executed, index into [Vm]s `bytecode` vector.
    function_idx: usize,

    /// Instruction "pointer". An index into the bytecode of the currently
    /// executed function.
    ip: usize,

    /// Frame "pointer". Where on the stack do arguments and local variables
    /// start?
    fp: usize,
}

impl CallFrame {
    fn root() -> Self {
        CallFrame {
            function_idx: 0,
            ip: 0,
            fp: 0,
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

    /// The names of global variables or [Unit]s.
    global_identifiers: Vec<String>,

    /// A dictionary of global variables and their respective values.
    globals: HashMap<String, Quantity>,

    /// List of registered native/foreign functions
    foreign_functions: Vec<ForeignFunction>,

    /// The call stack
    frames: Vec<CallFrame>,

    /// The stack of the VM. Each entry is a [Quantity], i.e. something like
    /// `3.4 m/s²`.
    stack: Vec<Quantity>,

    /// Whether or not to run in debug mode.
    debug: bool,
}

impl Vm {
    pub fn new(debug: bool) -> Self {
        Self {
            bytecode: vec![("<main>".into(), vec![])],
            current_chunk_index: 0,
            constants: vec![],
            global_identifiers: vec![],
            globals: HashMap::new(),
            foreign_functions: vec![ForeignFunction {
                name: "print!".into(),
                arity: 1,
                callable: Callable::Macro(crate::ffi::print),
            }],
            frames: vec![CallFrame::root()],
            stack: vec![],
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

    pub(crate) fn add_op2(&mut self, op: Op, arg1: u8, arg2: u8) {
        let current_chunk = self.current_chunk_mut();
        current_chunk.push(op as u8);
        current_chunk.push(arg1);
        current_chunk.push(arg2);
    }

    pub fn add_constant(&mut self, constant: Constant) -> u8 {
        self.constants.push(constant);
        assert!(self.constants.len() <= u8::MAX as usize);
        (self.constants.len() - 1) as u8 // TODO: this can overflow, see above
    }

    pub fn add_global_identifier(&mut self, identifier: &str) -> u8 {
        if let Some(idx) = self.global_identifiers.iter().position(|i| i == identifier) {
            return idx as u8;
        }

        self.global_identifiers.push(identifier.to_owned());
        assert!(self.global_identifiers.len() <= u8::MAX as usize);
        (self.global_identifiers.len() - 1) as u8 // TODO: this can overflow, see above
    }

    pub(crate) fn begin_function(&mut self, name: &str) {
        self.bytecode.push((name.into(), vec![]));
        self.current_chunk_index = self.bytecode.len() - 1
    }

    pub(crate) fn end_function(&mut self) {
        // Continue compilation of "main"/global code
        self.current_chunk_index = 0;
    }

    pub(crate) fn get_function_idx(&self, name: &str) -> u8 {
        let position = self.bytecode.iter().position(|(n, _)| n == name).unwrap();
        assert!(position <= u8::MAX as usize);
        position as u8
    }

    pub(crate) fn add_foreign_function(&mut self, name: &str, arity: usize) {
        self.foreign_functions.push(ForeignFunction {
            name: name.into(),
            arity,
            callable: match name {
                "abs" => Callable::Function(crate::ffi::abs),
                "sin" => Callable::Function(crate::ffi::sin),
                "atan2" => Callable::Function(crate::ffi::atan2),
                _ => unimplemented!(), // TODO
            },
        });
    }

    pub(crate) fn get_foreign_function_idx(&self, name: &str) -> Option<u8> {
        // TODO: this is a linear search that can certainly be optimized
        let position = self
            .foreign_functions
            .iter()
            .position(|ff| ff.name == name)?;
        assert!(position <= u8::MAX as usize);
        Some(position as u8)
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
        for (idx, identifier) in self.global_identifiers.iter().enumerate() {
            println!("  {:04} {}", idx, identifier);
        }
        for (idx, (function_name, bytecode)) in self.bytecode.iter().enumerate() {
            println!(".CODE {idx} ({name})", idx = idx, name = function_name);
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
                } else if op == Op::Call {
                    print!(
                        "   ({}, num_args={})",
                        self.bytecode[operands[0] as usize].0, operands[1] as usize
                    );
                }
                println!();
            }
        }
        println!();
    }

    // The following functions are helpers for the actual execution of the code

    fn current_frame(&self) -> &CallFrame {
        self.frames.last().expect("Call stack is not empty")
    }

    fn current_frame_mut(&mut self) -> &mut CallFrame {
        self.frames.last_mut().expect("Call stack is not empty")
    }

    fn read_byte(&mut self) -> u8 {
        let frame = self.current_frame();
        let byte = self.bytecode[frame.function_idx].1[frame.ip];
        self.current_frame_mut().ip += 1;
        byte
    }

    fn push(&mut self, quantity: Quantity) {
        self.stack.push(quantity);
    }

    fn pop(&mut self) -> Quantity {
        self.stack.pop().expect("stack should not be empty")
    }

    pub fn run(&mut self) -> Result<InterpreterResult> {
        let result = self.run_without_cleanup();
        if result.is_err() {
            // Perform cleanup: clear the stack and move IP to the end.
            // This is useful for the REPL.
            //
            // TODO(minor): is this really enough? Shouln't we also remove
            // the bytecode?
            self.stack.clear();

            // Reset the call stack
            // TODO: move the following to a function?
            self.frames.clear();
            self.frames.push(CallFrame::root());
            self.frames[0].ip = self.bytecode[0].1.len();
        }
        result
    }

    fn run_without_cleanup(&mut self) -> Result<InterpreterResult> {
        if self.current_frame().ip >= self.bytecode[self.current_frame().function_idx].1.len() {
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
                    let identifier: String =
                        self.global_identifiers[identifier_idx as usize].clone();

                    self.globals.insert(identifier, quantity);

                    return Ok(InterpreterResult::Continue);
                }
                Op::GetVariable => {
                    let identifier_idx = self.read_byte();
                    let identifier = &self.global_identifiers[identifier_idx as usize];

                    let quantity = self
                        .globals
                        .get(identifier)
                        .ok_or_else(|| RuntimeError::UnknownVariable(identifier.clone()))?; // TODO: can this even be triggered? Shouldn't that be covered in the type checker?

                    self.push(quantity.clone());
                }
                Op::GetLocal => {
                    let slot_idx = self.read_byte() as usize;
                    let stack_idx = self.current_frame().fp + slot_idx;
                    self.push(self.stack[stack_idx].clone());
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
                                return Err(RuntimeError::DivisionByZero);
                            } else {
                                lhs / rhs
                            }
                        }
                        Op::Power => lhs.power(rhs),
                        Op::ConvertTo => lhs.convert_to(rhs.unit()),
                        _ => unreachable!(),
                    };
                    self.push(result.map_err(RuntimeError::ConversionError)?);
                }
                Op::Negate => {
                    let rhs = self.pop();
                    self.push(-rhs);
                }
                Op::Call => {
                    let function_idx = self.read_byte() as usize;
                    let num_args = self.read_byte() as usize;
                    self.frames.push(CallFrame {
                        function_idx,
                        ip: 0,
                        fp: self.stack.len() - num_args,
                    })
                }
                Op::FFICallFunction | Op::FFICallMacro => {
                    let function_idx = self.read_byte() as usize;
                    let foreign_function = &self.foreign_functions[function_idx];

                    let mut args = vec![];
                    for _ in 0..foreign_function.arity {
                        args.push(self.pop());
                    }
                    args.reverse(); // TODO: use a deque?

                    match self.foreign_functions[function_idx].callable {
                        Callable::Function(function) => {
                            let result = (function)(&args[..]);
                            self.push(result);
                        }
                        Callable::Macro(macro_) => {
                            (macro_)(&args[..]);
                            return Ok(InterpreterResult::Continue);
                        }
                    }
                }
                Op::Return => {
                    if self.frames.len() == 1 {
                        return Ok(InterpreterResult::Quantity(self.pop()));
                    } else {
                        let discarded_frame = self.frames.pop().unwrap();

                        // Remember the return value which is currently on top of the stack
                        let return_value = self.stack.pop().unwrap();

                        // Pop off arguments from previous call
                        while self.stack.len() > discarded_frame.fp {
                            self.stack.pop();
                        }

                        // Push the return value back on top of the stack
                        self.stack.push(return_value);
                    }
                }
            }
        }
    }

    pub fn debug(&self) {
        if !self.debug {
            return;
        }

        let frame = self.current_frame();
        print!(
            "FRAME = {}, IP = {}, ",
            self.bytecode[frame.function_idx].0, frame.ip
        );
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
