mod utils;

use insect::bytecode_interpreter::BytecodeInterpreter;
use insect::interpreter::{Interpreter, InterpreterResult};
use insect::parser::parse;
use insect::typechecker::TypeChecker;

use wasm_bindgen::prelude::*;

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen]
pub fn interpret(code: &str) -> String {
    utils::set_panic_hook();

    let code = format!("{}\n{}", include_str!("../../prelude.ins"), code); // TODO

    let statements = parse(&code).unwrap();
    let mut typechecker = TypeChecker::default();
    let statements = typechecker.check_statements(statements).unwrap();
    let mut interpreter = BytecodeInterpreter::new(true);
    let result = interpreter.interpret_statements(&statements).unwrap();

    match result {
        InterpreterResult::Quantity(q) => format!("{}", q),
        InterpreterResult::Continue => todo!(),
        InterpreterResult::Exit => todo!(),
    }
}
