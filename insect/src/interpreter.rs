use crate::{
    quantity::{ConversionError, Quantity},
    typed_ast::Statement,
    unit_registry::UnitRegistryError,
};

use thiserror::Error;

#[derive(Debug, Error, PartialEq, Eq)]
pub enum RuntimeError {
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

pub type Result<T> = std::result::Result<T, RuntimeError>;

pub trait Interpreter {
    fn new(debug: bool) -> Self;

    fn interpret_statement(&mut self, statements: &Statement) -> Result<InterpreterResult>;

    fn interpret_statements(&mut self, statements: &[Statement]) -> Result<InterpreterResult> {
        let mut result = Err(RuntimeError::NoStatements);
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
mod tests {
    use crate::bytecode_interpreter::BytecodeInterpreter;

    use super::*;

    static MINI_PRELUDE: &'static str = "
        dimension Length
        dimension Time
        dimension Mass

        dimension Speed = Length / Time
        dimension Momentum = Mass * Speed

        unit meter : Length
        unit second : Time";

    fn get_interpreter_result(input: &str) -> Result<InterpreterResult> {
        use crate::typechecker::typecheck;

        let mut interpreter = BytecodeInterpreter::new(false);
        let statements =
            crate::parser::parse(input).expect("No parse errors for inputs in this test suite");
        let statements_typechecked =
            typecheck(statements).expect("No type check errors for inputs in this test suite");
        interpreter.interpret_statements(&statements_typechecked)
    }

    fn assert_evaluates_to(input: &str, expected: Quantity) {
        if let InterpreterResult::Quantity(actual) = get_interpreter_result(input).unwrap() {
            assert_eq!(actual, expected);
        } else {
            assert!(false);
        }
    }

    fn assert_evaluates_to_scalar(input: &str, expected: f64) {
        assert_evaluates_to(input, Quantity::from_scalar(expected))
    }

    fn assert_runtime_error(input: &str, err_expected: RuntimeError) {
        if let Err(err_actual) = get_interpreter_result(input) {
            assert_eq!(err_actual, err_expected);
        } else {
            assert!(false);
        }
    }

    #[test]
    fn test_simple_arithmetic() {
        assert_evaluates_to_scalar("0", 0.0);
        assert_evaluates_to_scalar("1", 1.0);
        assert_evaluates_to_scalar("1+2", 1.0 + 2.0);
        assert_evaluates_to_scalar("-1", -1.0);

        assert_evaluates_to_scalar("2+3*4", 2.0 + 3.0 * 4.0);
        assert_evaluates_to_scalar("2*3+4", 2.0 * 3.0 + 4.0);
        assert_evaluates_to_scalar("(2+3)*4", (2.0 + 3.0) * 4.0);

        assert_evaluates_to_scalar("(2/3)*4", (2.0 / 3.0) * 4.0);
        assert_evaluates_to_scalar("-2 * 3", -2.0 * 3.0);
        assert_evaluates_to_scalar("2 * -3", 2.0 * -3.0);
        assert_evaluates_to_scalar("2 - 3 - 4", 2.0 - 3.0 - 4.0);
        assert_evaluates_to_scalar("2 - -3", 2.0 - -3.0);
    }

    #[test]
    fn test_arithmetic_with_units() {
        use crate::unit::Unit;

        assert_evaluates_to(
            "dimension Length
             unit meter : Length
             2 * meter",
            (Quantity::from_scalar(2.0) * Quantity::from_unit(Unit::new_standard("meter")))
                .unwrap(),
        );

        assert_evaluates_to(
            &format!(
                "{mini_prelude}
                 fn speed(distance: Length, time: Time) -> Speed = distance / time
                 speed(10 * meter, 2 * second)",
                mini_prelude = MINI_PRELUDE
            ),
            (Quantity::from_scalar(5.0)
                * (Quantity::from_unit(Unit::new_standard("meter"))
                    / Quantity::from_unit(Unit::new_standard("second")))
                .unwrap())
            .unwrap(),
        );
    }

    #[test]
    fn test_power_operator() {
        assert_evaluates_to_scalar("2^3", 2.0f64.powf(3.0));
        assert_evaluates_to_scalar("-2^4", -(2.0f64.powf(4.0)));
        assert_evaluates_to_scalar("2^(-3)", 2.0f64.powf(-3.0));
    }

    #[test]
    fn test_multiline_code() {
        assert_evaluates_to_scalar("2\n3", 3.0);
    }

    #[test]
    fn test_variable_declarations() {
        assert_evaluates_to_scalar("let x = 2\nlet y = 3\nx + y", 2.0 + 3.0);
    }

    #[test]
    fn test_division_by_zero_raises_runtime_error() {
        assert_runtime_error("1/0", RuntimeError::DivisionByZero);
    }

    #[test]
    fn test_no_statements_raise_runtime_error() {
        assert_runtime_error("", RuntimeError::NoStatements);
    }
}
