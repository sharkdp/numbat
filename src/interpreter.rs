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
    fn new() -> Self;
    fn interpret(&mut self, statement: &Statement) -> Result<InterpreterResult>;
}

#[cfg(test)]
fn get_interpreter_result<I: Interpreter>(input: &str) -> Result<InterpreterResult> {
    let mut interpreter = I::new();
    let statement =
        crate::parser::parse(input).expect("No parse errors for inputs in this test suite");
    interpreter.interpret(&statement)
}

#[cfg(test)]
fn assert_evaluates_to<I: Interpreter>(input: &str, expected: f64) {
    if let InterpreterResult::Value(actual) = get_interpreter_result::<I>(input).unwrap() {
        assert_eq!(actual, expected);
    } else {
        assert!(false);
    }
}

#[cfg(test)]
fn assert_interpreter_error<I: Interpreter>(input: &str, err_expected: InterpreterError) {
    if let Err(err_actual) = get_interpreter_result::<I>(input) {
        assert_eq!(err_actual, err_expected);
    } else {
        assert!(false);
    }
}

#[cfg(test)]
fn test_interpreter<I: Interpreter>() {
    //  TODO: do not reuse the same interpreter context!

    assert_evaluates_to::<I>("0", 0.0);
    assert_evaluates_to::<I>("1", 1.0);
    assert_evaluates_to::<I>("1+2", 1.0 + 2.0);
    assert_evaluates_to::<I>("-1", -1.0);

    assert_evaluates_to::<I>("2+3*4", 2.0 + 3.0 * 4.0);
    assert_evaluates_to::<I>("2*3+4", 2.0 * 3.0 + 4.0);
    assert_evaluates_to::<I>("(2+3)*4", (2.0 + 3.0) * 4.0);

    assert_evaluates_to::<I>("(2/3)*4", (2.0 / 3.0) * 4.0);
    assert_evaluates_to::<I>("-2 * 3", -2.0 * 3.0);
    assert_evaluates_to::<I>("2 * -3", 2.0 * -3.0);
    assert_evaluates_to::<I>("2 - 3 - 4", 2.0 - 3.0 - 4.0);
    assert_evaluates_to::<I>("2 - -3", 2.0 - -3.0);

    // assert_evaluates_to(interpreter, "let x = 2\nlet y = 3\nx + y", 2.0 + 3.0);

    assert_interpreter_error::<I>("1/0", InterpreterError::DivisionByZero);
    assert_interpreter_error::<I>("foo", InterpreterError::UnknownVariable("foo".into()));
}

#[test]
fn test_bytecode_interpreter() {
    use crate::bytecode_interpreter::BytecodeInterpreter;
    test_interpreter::<BytecodeInterpreter>();
}

#[test]
fn test_treewalk_interpreter() {
    use crate::treewalk_interpreter::TreewalkInterpreter;
    test_interpreter::<TreewalkInterpreter>();
}
