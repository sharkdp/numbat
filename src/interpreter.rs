use crate::ast::Statement;

use thiserror::Error;

#[derive(Debug, Error, PartialEq)]
pub enum InterpreterError {
    #[error("Division by zero")]
    DivisionByZero,
    #[error("Unknown variable '{0}'")]
    UnknownVariable(String),
}

#[derive(Debug, PartialEq)]
pub enum InterpreterResult {
    Value(f64),
    Continue,
    Exit,
}

pub type Result<T> = std::result::Result<T, InterpreterError>;

pub trait Interpreter {
    fn interpret(&mut self, statement: &Statement) -> Result<InterpreterResult>;
}

#[cfg(test)]
fn assert_evaluates_to(interpreter: &mut dyn Interpreter, input: &str, expected: f64) {
    let statement = crate::parser::parse(input).unwrap();

    if let InterpreterResult::Value(actual) = interpreter.interpret(&statement).unwrap() {
        assert_eq!(actual, expected);
    } else {
        assert!(false);
    }
}

#[cfg(test)]
fn test_interpreter(interpreter: &mut dyn Interpreter) {
    //  TODO: do not reuse the same interpreter context!

    assert_evaluates_to(interpreter, "0", 0.0);
    assert_evaluates_to(interpreter, "1", 1.0);
    assert_evaluates_to(interpreter, "1+2", 1.0 + 2.0);
    assert_evaluates_to(interpreter, "-1", -1.0);

    assert_evaluates_to(interpreter, "2+3*4", 2.0 + 3.0 * 4.0);
    assert_evaluates_to(interpreter, "2*3+4", 2.0 * 3.0 + 4.0);
    assert_evaluates_to(interpreter, "(2+3)*4", (2.0 + 3.0) * 4.0);

    assert_evaluates_to(interpreter, "(2/3)*4", (2.0 / 3.0) * 4.0);
    assert_evaluates_to(interpreter, "-2 * 3", -2.0 * 3.0);
    assert_evaluates_to(interpreter, "2 * -3", 2.0 * -3.0);
    assert_evaluates_to(interpreter, "2 - 3 - 4", 2.0 - 3.0 - 4.0);
    assert_evaluates_to(interpreter, "2 - -3", 2.0 - -3.0);

    // assert_evaluates_to(interpreter, "let x = 2\nlet y = 3\nx + y", 2.0 + 3.0);
}

#[test]
fn test_bytecode_interpreter() {
    let mut interpreter: Box<dyn Interpreter> =
        Box::new(crate::bytecode_interpreter::BytecodeInterpreter::new());

    test_interpreter(interpreter.as_mut());
}

#[test]
fn test_treewalk_interpreter() {
    let mut interpreter: Box<dyn Interpreter> =
        Box::new(crate::treewalk_interpreter::TreewalkInterpreter::new());

    test_interpreter(interpreter.as_mut());
}
