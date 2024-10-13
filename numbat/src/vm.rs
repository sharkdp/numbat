use std::collections::{HashMap, VecDeque};
use std::sync::Arc;
use std::{cmp::Ordering, fmt::Display};

use compact_str::{CompactString, ToCompactString};
use indexmap::IndexMap;
use num_traits::ToPrimitive;

use crate::list::NumbatList;
use crate::span::Span;
use crate::typed_ast::StructInfo;
use crate::{
    ffi::{self, ArityRange, Callable, ForeignFunction},
    interpreter::{InterpreterResult, PrintFunction, Result, RuntimeError},
    markup::Markup,
    math,
    number::Number,
    prefix::Prefix,
    quantity::{Quantity, QuantityError},
    unit::Unit,
    unit_registry::{UnitMetadata, UnitRegistry},
    value::{FunctionReference, Value},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum Op {
    /// Push the value of the specified constant onto the stack
    LoadConstant,

    /// Add a prefix to the unit on the stack
    ApplyPrefix,

    /// This is a special operation for declaring derived units.
    /// It takes two operands: a global identifier index and a
    /// constant index.
    /// It pops the current quantity from the stack and creates
    /// a new derived unit whose name is specified by the global
    /// identifier index. It then proceeds to assign a value of
    /// `1 <new_unit>` to the constant with the given index.
    SetUnitConstant,

    /// Push the value of the specified local variable onto the stack (even
    /// though it is already on the stack, somewhere lower down).
    GetLocal,

    /// Similar to GetLocal, but get variable from surrounding scope
    GetUpvalue,

    /// Get the last stored result (_ and ans)
    GetLastResult,

    /// Negate the top of the stack
    Negate,

    /// Evaluate the factorial of the top of the stack
    Factorial,

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
    /// Similar to Add:
    LessThan,
    GreaterThan,
    LessOrEqual,
    GreatorOrEqual,
    Equal,
    NotEqual,
    LogicalAnd,
    LogicalOr,
    LogicalNeg,

    /// Similar to Add, but has DateTime on the LHS and a quantity on the RHS
    AddToDateTime,
    /// Similar to Sub, but has DateTime on the LHS and a quantity on the RHS
    SubFromDateTime,
    /// Computes the difference between two DateTimes
    DiffDateTime,

    /// Move IP forward by the given offset argument if the popped-of value on
    /// top of the stack is false.
    JumpIfFalse,
    /// Unconditionally move IP forward by the given offset argument
    Jump,

    /// Call the specified function with the specified number of arguments
    Call,
    /// Same as above, but call a foreign/native function
    FFICallFunction,
    /// Same as above, but call a procedure which does not return anything (does not push a value onto the stack)
    /// It has a third argument which is an index to retrieve the source-span of the arguments
    FFICallProcedure,

    /// Call a callable object
    CallCallable,

    /// Print a compile-time string
    PrintString,

    /// Combine N strings on the stack into a single part, used by string interpolation
    JoinString,

    /// Build a struct from the field values on the stack
    BuildStructInstance,
    /// Access a single field of a struct
    AccessStructField,

    /// Build a list from the elements on the stack
    BuildList,

    /// Return from the current function
    Return,
}

impl Op {
    fn num_operands(self) -> usize {
        match self {
            Op::FFICallProcedure => 3,
            Op::SetUnitConstant | Op::Call | Op::FFICallFunction | Op::BuildStructInstance => 2,
            Op::LoadConstant
            | Op::ApplyPrefix
            | Op::GetLocal
            | Op::GetUpvalue
            | Op::PrintString
            | Op::JoinString
            | Op::JumpIfFalse
            | Op::Jump
            | Op::CallCallable
            | Op::AccessStructField
            | Op::BuildList => 1,
            Op::Negate
            | Op::Factorial
            | Op::Add
            | Op::AddToDateTime
            | Op::Subtract
            | Op::SubFromDateTime
            | Op::DiffDateTime
            | Op::Multiply
            | Op::Divide
            | Op::Power
            | Op::ConvertTo
            | Op::LessThan
            | Op::GreaterThan
            | Op::LessOrEqual
            | Op::GreatorOrEqual
            | Op::Equal
            | Op::NotEqual
            | Op::LogicalAnd
            | Op::LogicalOr
            | Op::LogicalNeg
            | Op::Return
            | Op::GetLastResult => 0,
        }
    }

    fn to_string(self) -> &'static str {
        match self {
            Op::LoadConstant => "LoadConstant",
            Op::ApplyPrefix => "ApplyPrefix",
            Op::SetUnitConstant => "SetUnitConstant",
            Op::GetLocal => "GetLocal",
            Op::GetUpvalue => "GetUpvalue",
            Op::GetLastResult => "GetLastResult",
            Op::Negate => "Negate",
            Op::Factorial => "Factorial",
            Op::Add => "Add",
            Op::AddToDateTime => "AddDateTime",
            Op::Subtract => "Subtract",
            Op::SubFromDateTime => "SubDateTime",
            Op::DiffDateTime => "DiffDateTime",
            Op::Multiply => "Multiply",
            Op::Divide => "Divide",
            Op::Power => "Power",
            Op::ConvertTo => "ConvertTo",
            Op::LessThan => "LessThan",
            Op::GreaterThan => "GreaterThan",
            Op::LessOrEqual => "LessOrEqual",
            Op::GreatorOrEqual => "GreatorOrEqual",
            Op::Equal => "Equal",
            Op::NotEqual => "NotEqual",
            Op::LogicalAnd => "LogicalAnd",
            Op::LogicalOr => "LogicalOr",
            Op::LogicalNeg => "LogicalNeg",
            Op::JumpIfFalse => "JumpIfFalse",
            Op::Jump => "Jump",
            Op::Call => "Call",
            Op::FFICallFunction => "FFICallFunction",
            Op::FFICallProcedure => "FFICallProcedure",
            Op::CallCallable => "CallCallable",
            Op::PrintString => "PrintString",
            Op::JoinString => "JoinString",
            Op::Return => "Return",
            Op::BuildStructInstance => "BuildStructInstance",
            Op::AccessStructField => "AccessStructField",
            Op::BuildList => "BuildList",
        }
    }
}

#[derive(Clone, Debug)]
pub enum Constant {
    Scalar(f64),
    Unit(Unit),
    Boolean(bool),
    String(CompactString),
    FunctionReference(FunctionReference),
    FormatSpecifiers(Option<CompactString>),
}

impl Constant {
    fn to_value(&self) -> Value {
        match self {
            Constant::Scalar(n) => Value::Quantity(Quantity::from_scalar(*n)),
            Constant::Unit(u) => Value::Quantity(Quantity::from_unit(u.clone())),
            Constant::Boolean(b) => Value::Boolean(*b),
            Constant::String(s) => Value::String(s.clone()),
            Constant::FunctionReference(inner) => Value::FunctionReference(inner.clone()),
            Constant::FormatSpecifiers(s) => Value::FormatSpecifiers(s.clone()),
        }
    }
}

impl Display for Constant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Constant::Scalar(n) => write!(f, "{n}"),
            Constant::Unit(unit) => write!(f, "{unit}"),
            Constant::Boolean(val) => write!(f, "{val}"),
            Constant::String(val) => write!(f, "\"{val}\""),
            Constant::FunctionReference(inner) => write!(f, "{inner}"),
            Constant::FormatSpecifiers(_) => write!(f, "<format specfiers>"),
        }
    }
}

#[derive(Clone)]
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

pub struct ExecutionContext<'a> {
    pub print_fn: &'a mut PrintFunction,
}

#[derive(Clone)]
pub struct Vm {
    /// The actual code of the program, structured by function name. The code
    /// for the global scope is at index 0 under the function name `<main>`.
    bytecode: Vec<(CompactString, Vec<u8>)>,

    /// An index into the `bytecode` vector referring to the function which is
    /// currently being compiled.
    current_chunk_index: usize,

    /// Constants are numbers like '1.4' or a [Unit] like 'meter'.
    pub constants: Vec<Constant>,

    /// struct metadata, used so we can display struct fields at runtime
    struct_infos: IndexMap<CompactString, Arc<StructInfo>>,

    /// Unit prefixes in use
    prefixes: Vec<Prefix>,

    /// Strings/text that is already available at compile time
    strings: Vec<Markup>,

    /// Meta information about derived units:
    /// - Unit name
    /// - Canonical name
    /// - Metadata
    unit_information: Vec<(CompactString, Option<CompactString>, UnitMetadata)>,

    /// Result of the last expression
    last_result: Option<Value>,

    /// List of registered native/foreign functions
    ffi_callables: Vec<&'static ForeignFunction>,

    /// Spans for arguments of procedure calls. This is used for
    /// assertion error messages, for example.
    procedure_arg_spans: Vec<Vec<Span>>,

    /// The call stack
    frames: Vec<CallFrame>,

    /// The stack of the VM.
    stack: Vec<Value>,

    /// Whether or not to run in debug mode.
    debug: bool,

    pub unit_registry: UnitRegistry,
}

impl Vm {
    pub fn new() -> Self {
        Self {
            bytecode: vec![("<main>".into(), vec![])],
            current_chunk_index: 0,
            constants: vec![],
            struct_infos: IndexMap::new(),
            prefixes: vec![],
            strings: vec![],
            unit_information: vec![],
            last_result: None,
            ffi_callables: ffi::procedures().iter().map(|(_, ff)| ff).collect(),
            procedure_arg_spans: vec![],
            frames: vec![CallFrame::root()],
            stack: vec![],
            debug: false,
            unit_registry: UnitRegistry::new(),
        }
    }
    pub fn set_debug(&mut self, activate: bool) {
        self.debug = activate;
    }

    // The following functions are helpers for the compilation process

    fn current_chunk_mut(&mut self) -> &mut Vec<u8> {
        &mut self.bytecode[self.current_chunk_index].1
    }

    fn push_u16(chunk: &mut Vec<u8>, data: u16) {
        let arg_bytes = data.to_le_bytes();
        chunk.push(arg_bytes[0]);
        chunk.push(arg_bytes[1]);
    }

    pub fn add_op(&mut self, op: Op) {
        self.current_chunk_mut().push(op as u8);
    }

    pub fn add_op1(&mut self, op: Op, arg: u16) {
        let current_chunk = self.current_chunk_mut();
        current_chunk.push(op as u8);
        Self::push_u16(current_chunk, arg)
    }

    pub(crate) fn add_op2(&mut self, op: Op, arg1: u16, arg2: u16) {
        let current_chunk = self.current_chunk_mut();
        current_chunk.push(op as u8);
        Self::push_u16(current_chunk, arg1);
        Self::push_u16(current_chunk, arg2);
    }

    pub(crate) fn add_op3(&mut self, op: Op, arg1: u16, arg2: u16, arg3: u16) {
        let current_chunk = self.current_chunk_mut();
        current_chunk.push(op as u8);
        Self::push_u16(current_chunk, arg1);
        Self::push_u16(current_chunk, arg2);
        Self::push_u16(current_chunk, arg3);
    }

    pub fn current_offset(&self) -> u16 {
        self.bytecode[self.current_chunk_index].1.len() as u16
    }

    pub fn patch_u16_value_at(&mut self, offset: u16, arg: u16) {
        let offset = offset as usize;
        let chunk = self.current_chunk_mut();
        chunk[offset] = (arg & 0xff) as u8;
        chunk[offset + 1] = ((arg >> 8) & 0xff) as u8;
    }

    pub fn add_constant(&mut self, constant: Constant) -> u16 {
        self.constants.push(constant);
        assert!(self.constants.len() <= u16::MAX as usize);
        (self.constants.len() - 1) as u16 // TODO: this can overflow, see above
    }

    pub fn add_struct_info(&mut self, struct_info: &StructInfo) -> usize {
        let e = self.struct_infos.entry(struct_info.name.clone());
        let idx = e.index();
        e.or_insert_with(|| Arc::new(struct_info.clone()));

        idx
    }

    pub fn get_structinfo_idx(&self, name: &str) -> Option<usize> {
        self.struct_infos.get_index_of(name)
    }

    pub fn add_prefix(&mut self, prefix: Prefix) -> u16 {
        if let Some(idx) = self.prefixes.iter().position(|p| p == &prefix) {
            idx as u16
        } else {
            self.prefixes.push(prefix);
            assert!(self.constants.len() <= u16::MAX as usize);
            (self.prefixes.len() - 1) as u16 // TODO: this can overflow, see above
        }
    }

    pub fn add_unit_information(
        &mut self,
        unit_name: &str,
        canonical_unit_name: Option<&str>,
        metadata: UnitMetadata,
    ) -> u16 {
        if let Some(idx) = self.unit_information.iter().position(|i| i.0 == unit_name) {
            return idx as u16;
        }

        self.unit_information.push((
            unit_name.to_compact_string(),
            canonical_unit_name.map(|s| s.to_compact_string()),
            metadata,
        ));
        assert!(self.unit_information.len() <= u16::MAX as usize);
        (self.unit_information.len() - 1) as u16 // TODO: this can overflow, see above
    }

    pub(crate) fn begin_function(&mut self, name: &str) {
        self.bytecode.push((name.into(), vec![]));
        self.current_chunk_index = self.bytecode.len() - 1
    }

    pub(crate) fn end_function(&mut self) {
        // Continue compilation of "main"/global code
        self.current_chunk_index = 0;
    }

    pub(crate) fn get_function_idx(&self, name: &str) -> u16 {
        // We search backwards to allow for functions
        // to be overwritten.
        let rev_position = self
            .bytecode
            .iter()
            .rev()
            .position(|(n, _)| n == name)
            .unwrap();
        let position = self.bytecode.len() - 1 - rev_position;
        assert!(position <= u16::MAX as usize);
        position as u16
    }

    pub(crate) fn add_foreign_function(&mut self, name: &str, arity: ArityRange) {
        let ff = ffi::functions().get(name).unwrap();
        assert!(ff.arity == arity);
        self.ffi_callables.push(ff);
    }

    pub(crate) fn get_ffi_callable_idx(&self, name: &str) -> Option<u16> {
        // TODO: this is a linear search that can certainly be optimized
        let position = self.ffi_callables.iter().position(|ff| ff.name == name)?;
        assert!(position <= u16::MAX as usize);
        Some(position as u16)
    }

    pub(crate) fn add_procedure_arg_span(&mut self, spans: Vec<Span>) -> u16 {
        self.procedure_arg_spans.push(spans);
        assert!(self.procedure_arg_spans.len() <= u16::MAX as usize);
        (self.procedure_arg_spans.len() - 1) as u16
    }

    pub fn disassemble(&self) {
        if !self.debug {
            return;
        }

        eprintln!();
        eprintln!(".CONSTANTS");
        for (idx, constant) in self.constants.iter().enumerate() {
            eprintln!("  {idx:04} {constant}");
        }
        eprintln!(".IDENTIFIERS");
        for (idx, identifier) in self.unit_information.iter().enumerate() {
            eprintln!("  {:04} {}", idx, identifier.0);
        }
        for (idx, (function_name, bytecode)) in self.bytecode.iter().enumerate() {
            eprintln!(".CODE {idx} ({function_name})");
            let mut offset = 0;
            while offset < bytecode.len() {
                let this_offset = offset;
                let op = bytecode[offset];
                offset += 1;
                let op = unsafe { std::mem::transmute::<u8, Op>(op) };

                let mut operands: Vec<u16> = vec![];
                for _ in 0..op.num_operands() {
                    let operand =
                        u16::from_le_bytes(bytecode[offset..(offset + 2)].try_into().unwrap());
                    operands.push(operand);
                    offset += 2;
                }

                let operands_str = operands
                    .iter()
                    .map(u16::to_compact_string)
                    .collect::<Vec<_>>()
                    .join(" ");

                eprint!(
                    "  {:04} {:<13} {}",
                    this_offset,
                    op.to_string(),
                    operands_str,
                );

                if op == Op::LoadConstant {
                    eprint!("     (value: {})", self.constants[operands[0] as usize]);
                } else if op == Op::Call {
                    eprint!(
                        "   ({}, num_args={})",
                        self.bytecode[operands[0] as usize].0, operands[1] as usize
                    );
                }
                eprintln!();
            }
        }
        eprintln!();
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

    fn read_u16(&mut self) -> u16 {
        let bytes = [self.read_byte(), self.read_byte()];
        u16::from_le_bytes(bytes)
    }

    fn push_quantity(&mut self, quantity: Quantity) {
        self.stack.push(Value::Quantity(quantity));
    }

    fn push_bool(&mut self, boolean: bool) {
        self.stack.push(Value::Boolean(boolean));
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    #[track_caller]
    fn pop_quantity(&mut self) -> Quantity {
        match self.pop() {
            Value::Quantity(q) => q,
            _ => panic!("Expected quantity to be on the top of the stack"),
        }
    }

    #[track_caller]
    fn pop_bool(&mut self) -> bool {
        self.pop().unsafe_as_bool()
    }

    #[track_caller]
    fn pop_datetime(&mut self) -> jiff::Zoned {
        match self.pop() {
            Value::DateTime(q) => q,
            _ => panic!("Expected datetime to be on the top of the stack"),
        }
    }

    #[track_caller]
    fn pop(&mut self) -> Value {
        self.stack.pop().expect("stack should not be empty")
    }

    pub fn run(&mut self, ctx: &mut ExecutionContext) -> Result<InterpreterResult> {
        let old_stack = self.stack.clone();
        let result = self.run_without_cleanup(ctx);
        if result.is_err() {
            // Perform cleanup: clear the stack and move IP to the end.
            // This is useful for the REPL.
            //
            // TODO(minor): is this really enough? Shouldn't we also remove
            // the bytecode?
            self.stack = old_stack;

            // Reset the call stack
            // TODO: move the following to a function?
            self.frames.clear();
            self.frames.push(CallFrame::root());
            self.frames[0].ip = self.bytecode[0].1.len();
        }
        result
    }

    fn is_at_the_end(&self) -> bool {
        self.current_frame().ip >= self.bytecode[self.current_frame().function_idx].1.len()
    }

    fn run_without_cleanup(&mut self, ctx: &mut ExecutionContext) -> Result<InterpreterResult> {
        let mut result_last_statement = None;
        while !self.is_at_the_end() {
            self.debug();

            let op = unsafe { std::mem::transmute::<u8, Op>(self.read_byte()) };

            match op {
                Op::LoadConstant => {
                    let constant_idx = self.read_u16();
                    self.stack
                        .push(self.constants[constant_idx as usize].to_value());
                }
                Op::ApplyPrefix => {
                    let quantity = self.pop_quantity();
                    let prefix_idx = self.read_u16();
                    let prefix = self.prefixes[prefix_idx as usize];
                    self.push_quantity(Quantity::new(
                        *quantity.unsafe_value(),
                        quantity.unit().clone().with_prefix(prefix),
                    ));
                }
                Op::SetUnitConstant => {
                    let unit_information_idx = self.read_u16();
                    let constant_idx = self.read_u16();

                    let conversion_value = self.pop_quantity();

                    let unit_information = &self.unit_information[unit_information_idx as usize];
                    let defining_unit = conversion_value.unit();

                    let (base_unit_representation, _) = defining_unit.to_base_unit_representation();

                    self.unit_registry
                        .add_derived_unit(
                            &unit_information.0,
                            &base_unit_representation,
                            unit_information.2.clone(),
                        )
                        .map_err(RuntimeError::UnitRegistryError)?;

                    self.constants[constant_idx as usize] = Constant::Unit(Unit::new_derived(
                        unit_information.0.to_compact_string(),
                        unit_information.2.canonical_name.clone(),
                        *conversion_value.unsafe_value(),
                        defining_unit.clone(),
                    ));
                }
                Op::GetLocal => {
                    let slot_idx = self.read_u16() as usize;
                    let stack_idx = self.current_frame().fp + slot_idx;
                    self.push(self.stack[stack_idx].clone());
                }
                Op::GetUpvalue => {
                    let stack_idx = self.read_u16() as usize;
                    self.push(self.stack[stack_idx].clone());
                }
                Op::GetLastResult => {
                    self.push(self.last_result.as_ref().unwrap().clone());
                }
                op @ (Op::Add
                | Op::Subtract
                | Op::Multiply
                | Op::Divide
                | Op::Power
                | Op::ConvertTo) => {
                    let rhs = self.pop_quantity();
                    let lhs = self.pop_quantity();
                    let result = match op {
                        Op::Add => &lhs + &rhs,
                        Op::Subtract => &lhs - &rhs,
                        Op::Multiply => Ok(lhs * rhs),
                        Op::Divide => {
                            Ok(lhs.checked_div(rhs).ok_or(RuntimeError::DivisionByZero)?)
                        }
                        Op::Power => lhs.power(rhs),
                        // If the user specifically converted the type of a unit, we should NOT simplify this value
                        // before any operations are applied to it
                        Op::ConvertTo => lhs.convert_to(rhs.unit()).map(Quantity::no_simplify),
                        _ => unreachable!(),
                    };
                    self.push_quantity(result.map_err(RuntimeError::QuantityError)?);
                }
                op @ (Op::AddToDateTime | Op::SubFromDateTime) => {
                    let rhs = self.pop_quantity();
                    let lhs = self.pop_datetime();

                    // for time, the base unit is in seconds
                    let base = rhs.to_base_unit_representation();
                    let seconds_f64 = base.unsafe_value().to_f64();

                    let seconds_i64 = seconds_f64
                        .to_i64()
                        .ok_or(RuntimeError::DurationOutOfRange)?;

                    let span = jiff::Span::new()
                        .try_seconds(seconds_i64)
                        .map_err(|_| RuntimeError::DurationOutOfRange)?
                        .nanoseconds((seconds_f64.fract() * 1_000_000_000f64).round() as i64);

                    self.push(Value::DateTime(match op {
                        Op::AddToDateTime => lhs
                            .checked_add(span)
                            .map_err(|_| RuntimeError::DateTimeOutOfRange)?,
                        Op::SubFromDateTime => lhs
                            .checked_sub(span)
                            .map_err(|_| RuntimeError::DateTimeOutOfRange)?,
                        _ => unreachable!(),
                    }));
                }
                Op::DiffDateTime => {
                    let unit = self.pop_quantity();
                    let rhs = self.pop_datetime();
                    let lhs = self.pop_datetime();

                    let duration = lhs
                        .since(&rhs)
                        .map_err(|_| RuntimeError::DateTimeOutOfRange)?;
                    let duration = duration
                        .total(jiff::Unit::Second)
                        .map_err(|_| RuntimeError::DurationOutOfRange)?;

                    let ret = Value::Quantity(Quantity::new(
                        Number::from_f64(duration),
                        unit.unit().clone(),
                    ));

                    self.push(ret);
                }
                op @ (Op::LessThan | Op::GreaterThan | Op::LessOrEqual | Op::GreatorOrEqual) => {
                    let rhs = self.pop_quantity();
                    let lhs = self.pop_quantity();

                    let result = lhs.partial_cmp(&rhs).ok_or_else(|| {
                        RuntimeError::QuantityError(QuantityError::IncompatibleUnits(
                            lhs.unit().clone(),
                            rhs.unit().clone(),
                        ))
                    })?;

                    let result = match op {
                        Op::LessThan => result == Ordering::Less,
                        Op::GreaterThan => result == Ordering::Greater,
                        Op::LessOrEqual => result != Ordering::Greater,
                        Op::GreatorOrEqual => result != Ordering::Less,
                        _ => unreachable!(),
                    };

                    self.push(Value::Boolean(result));
                }
                op @ (Op::Equal | Op::NotEqual) => {
                    let rhs = self.pop();
                    let lhs = self.pop();

                    let result = match op {
                        Op::Equal => lhs == rhs,
                        Op::NotEqual => lhs != rhs,
                        _ => unreachable!(),
                    };
                    self.push(Value::Boolean(result));
                }
                op @ (Op::LogicalAnd | Op::LogicalOr) => {
                    let rhs = self.pop_bool();
                    let lhs = self.pop_bool();

                    let result = match op {
                        Op::LogicalAnd => lhs && rhs,
                        Op::LogicalOr => lhs || rhs,
                        _ => unreachable!(),
                    };
                    self.push_bool(result);
                }
                Op::LogicalNeg => {
                    let rhs = self.pop_bool();
                    self.push_bool(!rhs);
                }
                Op::Negate => {
                    let rhs = self.pop_quantity();
                    self.push_quantity(-rhs);
                }
                Op::Factorial => {
                    let lhs = self
                        .pop_quantity()
                        .as_scalar()
                        .expect("Expected factorial operand to be scalar")
                        .to_f64();

                    if lhs < 0. {
                        return Err(Box::new(RuntimeError::FactorialOfNegativeNumber));
                    } else if lhs.fract() != 0. {
                        return Err(Box::new(RuntimeError::FactorialOfNonInteger));
                    }

                    self.push_quantity(Quantity::from_scalar(math::factorial(lhs)));
                }
                Op::JumpIfFalse => {
                    let offset = self.read_u16() as usize;
                    if !self.pop_bool() {
                        self.current_frame_mut().ip += offset;
                    }
                }
                Op::Jump => {
                    let offset = self.read_u16() as usize;
                    self.current_frame_mut().ip += offset;
                }
                Op::Call => {
                    let function_idx = self.read_u16() as usize;
                    let num_args = self.read_u16() as usize;
                    self.frames.push(CallFrame {
                        function_idx,
                        ip: 0,
                        fp: self.stack.len() - num_args,
                    })
                }
                Op::FFICallFunction | Op::FFICallProcedure => {
                    let function_idx = self.read_u16() as usize;
                    let num_args = self.read_u16() as usize;
                    let foreign_function = &self.ffi_callables[function_idx];

                    debug_assert!(foreign_function.arity.contains(&num_args));

                    let mut args = VecDeque::new();
                    for _ in 0..num_args {
                        args.push_front(self.pop());
                    }

                    match &self.ffi_callables[function_idx].callable {
                        Callable::Function(function) => {
                            let result = (function)(args);
                            self.push(result?);
                        }
                        Callable::Procedure(procedure) => {
                            let span_idx = self.read_u16() as usize;
                            let spans = &self.procedure_arg_spans[span_idx];

                            let result = (procedure)(ctx, args, spans.clone());

                            match result {
                                std::ops::ControlFlow::Continue(()) => {}
                                std::ops::ControlFlow::Break(runtime_error) => {
                                    return Err(Box::new(runtime_error));
                                }
                            }
                        }
                    }
                }
                Op::CallCallable => {
                    let num_args = self.read_u16() as usize;

                    let callable = self.pop();
                    match callable.unsafe_as_function_reference() {
                        FunctionReference::Normal(ref name) => {
                            let function_idx = self.get_function_idx(name) as usize;

                            // TODO: unify code with 'Op::Call'?
                            self.frames.push(CallFrame {
                                function_idx,
                                ip: 0,
                                fp: self.stack.len() - num_args,
                            })
                        }
                        FunctionReference::Foreign(ref name) => {
                            let function_idx = self
                                .get_ffi_callable_idx(name)
                                .expect("Foreign function exists")
                                as usize;

                            let mut args = VecDeque::new();
                            for _ in 0..num_args {
                                args.push_front(self.pop());
                            }

                            match &self.ffi_callables[function_idx].callable {
                                Callable::Function(function) => {
                                    let result = (function)(args);
                                    self.push(result?);
                                }
                                Callable::Procedure(..) => unreachable!("Foreign procedures can not be targeted by a function reference"),
                            }
                        }
                        FunctionReference::TzConversion(tz_name) => {
                            // TODO: implement this using a closure, once we have that in the language

                            let dt = self.pop_datetime();

                            let tz = jiff::tz::TimeZone::get(&tz_name)
                                .map_err(|_| RuntimeError::UnknownTimezone(tz_name.to_string()))?;

                            let dt = dt.with_time_zone(tz);

                            self.push(Value::DateTime(dt));
                        }
                    }
                }
                Op::PrintString => {
                    let s_idx = self.read_u16() as usize;
                    let s = &self.strings[s_idx];
                    self.print(ctx, s);
                }
                Op::JoinString => {
                    let num_parts = self.read_u16() as usize;
                    let mut joined = CompactString::with_capacity(num_parts);
                    let to_str = |value| match value {
                        Value::Quantity(q) => q.full_simplify().to_compact_string(),
                        Value::Boolean(b) => b.to_compact_string(),
                        Value::String(s) => s.to_compact_string(),
                        Value::DateTime(dt) => crate::datetime::to_string(&dt),
                        Value::FunctionReference(r) => r.to_compact_string(),
                        s @ Value::StructInstance(..) => s.to_compact_string(),
                        l @ Value::List(_) => l.to_compact_string(),
                        Value::FormatSpecifiers(_) => unreachable!(),
                    };

                    let map_strfmt_error_to_runtime_error = |err| match err {
                        strfmt::FmtError::Invalid(s) => RuntimeError::InvalidFormatSpecifiers(s),
                        strfmt::FmtError::TypeError(s) => {
                            RuntimeError::InvalidTypeForFormatSpecifiers(s)
                        }
                        strfmt::FmtError::KeyError(_) => unreachable!(),
                    };

                    for _ in 0..num_parts {
                        let part = match self.pop() {
                            Value::FormatSpecifiers(Some(specifiers)) => match self.pop() {
                                Value::Quantity(q) => {
                                    let q = q.full_simplify();

                                    let mut vars = HashMap::new();
                                    vars.insert(
                                        CompactString::const_new("value"),
                                        q.unsafe_value().to_f64(),
                                    );

                                    let mut str =
                                        strfmt::strfmt(&format!("{{value{specifiers}}}"), &vars)
                                            .map(CompactString::from)
                                            .map_err(map_strfmt_error_to_runtime_error)?;

                                    let unit_str = q.unit().to_compact_string();

                                    if !unit_str.is_empty() {
                                        str += " ";
                                        str += &unit_str;
                                    }

                                    str
                                }
                                value => {
                                    let mut vars = HashMap::new();
                                    vars.insert("value".to_owned(), to_str(value).to_string());

                                    strfmt::strfmt(&format!("{{value{specifiers}}}"), &vars)
                                        .map(CompactString::from)
                                        .map_err(map_strfmt_error_to_runtime_error)?
                                }
                            },
                            Value::FormatSpecifiers(None) => to_str(self.pop()),
                            v => to_str(v),
                        };
                        joined = part + &joined; // reverse order
                    }
                    self.push(Value::String(joined))
                }
                Op::Return => {
                    if self.frames.len() == 1 {
                        let return_value = self.pop();

                        self.last_result = Some(return_value.clone());

                        result_last_statement = Some(return_value);
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
                Op::BuildStructInstance => {
                    let info_idx = self.read_u16();
                    let (_, struct_info) = self
                        .struct_infos
                        .get_index(info_idx as usize)
                        .expect("Missing struct metadata");
                    let struct_info = Arc::clone(struct_info);
                    let num_args = self.read_u16();

                    let mut content = Vec::with_capacity(num_args as usize);

                    for _ in 0..num_args {
                        content.push(self.pop());
                    }

                    self.stack.push(Value::StructInstance(struct_info, content));
                }
                Op::AccessStructField => {
                    let field_idx = self.read_u16();

                    let mut fields = self.pop().unsafe_as_struct_fields();

                    let value = fields.swap_remove(field_idx as usize);
                    self.stack.push(value);
                }
                Op::BuildList => {
                    let length = self.read_u16();
                    let mut list = NumbatList::with_capacity(length as usize);

                    for _ in 0..length {
                        list.push_front(self.pop());
                    }

                    self.stack.push(list.into());
                }
            }
        }

        if let Some(value) = result_last_statement {
            Ok(InterpreterResult::Value(value))
        } else {
            Ok(InterpreterResult::Continue)
        }
    }

    pub fn debug(&self) {
        if !self.debug {
            return;
        }

        let frame = self.current_frame();
        eprint!(
            "FRAME = {}, IP = {}, ",
            self.bytecode[frame.function_idx].0, frame.ip
        );
        eprintln!(
            "Stack: [{}]",
            self.stack
                .iter()
                .map(|x| x.to_string())
                .collect::<Vec<_>>()
                .join("] [")
        );
    }

    pub fn add_string(&mut self, m: Markup) -> u16 {
        self.strings.push(m);
        assert!(self.strings.len() <= u16::MAX as usize);
        (self.strings.len() - 1) as u16 // TODO: this can overflow, see above
    }

    fn print(&self, ctx: &mut ExecutionContext, m: &Markup) {
        (ctx.print_fn)(m);
    }
}

#[test]
fn vm_basic() {
    let mut vm = Vm::new();
    vm.add_constant(Constant::Scalar(42.0));
    vm.add_constant(Constant::Scalar(1.0));

    vm.add_op1(Op::LoadConstant, 0);
    vm.add_op1(Op::LoadConstant, 1);
    vm.add_op(Op::Add);
    vm.add_op(Op::Return);

    let mut print_fn = |_: &Markup| {};
    let mut ctx = ExecutionContext {
        print_fn: &mut print_fn,
    };

    assert_eq!(
        vm.run(&mut ctx).unwrap(),
        InterpreterResult::Value(Value::Quantity(Quantity::from_scalar(42.0 + 1.0)))
    );
}
