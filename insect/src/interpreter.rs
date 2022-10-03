use crate::{
    quantity::{ConversionError, Quantity},
    typed_ast::Statement,
    unit_registry::UnitRegistryError,
};

use thiserror::Error;

#[derive(Debug, Error, PartialEq, Eq)]
pub enum InterpreterError {
    #[error("Division by zero")]
    DivisionByZero,
    #[error("Unknown variable '{0}'")]
    UnknownVariable(String),
    #[error("No statements in program")]
    NoStatements,
    #[error("{0}")]
    UnitRegistryError(UnitRegistryError),
    #[error("{0}")]
    ConversionError(ConversionError),
}

#[derive(Debug, PartialEq, Eq)]
pub enum InterpreterResult {
    Quantity(Quantity),
    Continue,
    Exit,
}

pub type Result<T> = std::result::Result<T, InterpreterError>;

pub trait Interpreter {
    fn new(debug: bool) -> Self;

    fn interpret_statement(&mut self, statements: &Statement) -> Result<InterpreterResult>;

    fn interpret_statements(&mut self, statements: &[Statement]) -> Result<InterpreterResult> {
        let mut result = Err(InterpreterError::NoStatements);
        if statements.is_empty() {
            return result;
        }

        for statement in statements {
            result = Ok(self.interpret_statement(statement)?);
        }
        result
    }
}

#[cfg(test)]
fn get_interpreter_result<I: Interpreter>(input: &str) -> Result<InterpreterResult> {
    use crate::typechecker::typecheck;

    let mut interpreter = I::new(false);
    let statements =
        crate::parser::parse(input).expect("No parse errors for inputs in this test suite");
    let statements_typechecked = typecheck(statements).expect("No type check errors");
    interpreter.interpret_statements(&statements_typechecked)
}

#[cfg(test)]
fn assert_evaluates_to<I: Interpreter>(input: &str, expected: Quantity) {
    if let InterpreterResult::Quantity(actual) = get_interpreter_result::<I>(input).unwrap() {
        assert_eq!(actual, expected);
    } else {
        assert!(false);
    }
}

#[cfg(test)]
fn assert_evaluates_to_scalar<I: Interpreter>(input: &str, expected: f64) {
    assert_evaluates_to::<I>(input, Quantity::from_scalar(expected))
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
    assert_evaluates_to_scalar::<I>("0", 0.0);
    assert_evaluates_to_scalar::<I>("1", 1.0);
    assert_evaluates_to_scalar::<I>("1+2", 1.0 + 2.0);
    assert_evaluates_to_scalar::<I>("-1", -1.0);

    assert_evaluates_to_scalar::<I>("2+3*4", 2.0 + 3.0 * 4.0);
    assert_evaluates_to_scalar::<I>("2*3+4", 2.0 * 3.0 + 4.0);
    assert_evaluates_to_scalar::<I>("(2+3)*4", (2.0 + 3.0) * 4.0);

    assert_evaluates_to_scalar::<I>("(2/3)*4", (2.0 / 3.0) * 4.0);
    assert_evaluates_to_scalar::<I>("-2 * 3", -2.0 * 3.0);
    assert_evaluates_to_scalar::<I>("2 * -3", 2.0 * -3.0);
    assert_evaluates_to_scalar::<I>("2 - 3 - 4", 2.0 - 3.0 - 4.0);
    assert_evaluates_to_scalar::<I>("2 - -3", 2.0 - -3.0);

    assert_evaluates_to_scalar::<I>("2^3", 2.0f64.powf(3.0));

    assert_evaluates_to_scalar::<I>("2\n3", 3.0);
    assert_evaluates_to_scalar::<I>("let x = 2\nlet y = 3\nx + y", 2.0 + 3.0);

    assert_interpreter_error::<I>("", InterpreterError::NoStatements);
    assert_interpreter_error::<I>("1/0", InterpreterError::DivisionByZero);
    //assert_interpreter_error::<I>("foo", InterpreterError::UnknownVariable("foo".into()));
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

// TODO(minor): generalize these tests to both interpreters
#[test]
fn test_advanced_bytecode_interpreter() {
    use crate::bytecode_interpreter::BytecodeInterpreter;
    use crate::unit::Unit;

    let mini_prelude = "
        dimension Length
        dimension Time
        dimension Mass

        dimension Speed = Length / Time
        dimension Momentum = Mass * Speed
        
        unit meter : Length
        unit second : Time";

    assert_evaluates_to::<BytecodeInterpreter>(
        "dimension Length
         unit meter : Length
         2 * meter",
        (Quantity::from_scalar(2.0) * Quantity::from_unit(Unit::new_standard("meter"))).unwrap(),
    );

    assert_evaluates_to::<BytecodeInterpreter>(
        &format!(
            "{mini_prelude}
             dimension Energy = Mass * Speed^2 = Momentum^2 / Mass
             1",
            mini_prelude = mini_prelude
        ),
        Quantity::from_scalar(1.0),
    );

    assert_evaluates_to::<BytecodeInterpreter>(
        &format!(
            "{mini_prelude}
             fn speed(distance: Length, time: Time) -> Speed = distance / time
             speed(10 * meter, 2 * second)",
            mini_prelude = mini_prelude
        ),
        (Quantity::from_scalar(5.0)
            * (Quantity::from_unit(Unit::new_standard("meter"))
                / Quantity::from_unit(Unit::new_standard("second")))
            .unwrap())
        .unwrap(),
    );
}
