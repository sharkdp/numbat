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
    #[error("No statements in program")]
    NoStatements, // TODO: move this error to an earlier stage?
    #[error("{0}")]
    UnitRegistryError(UnitRegistryError), // TODO: can this even be triggered?
    #[error("{0}")]
    ConversionError(ConversionError), // TODO: this can currently be triggered if there are multiple base units for the same dimension (no way to convert between them)
    #[error(
        "Assertion failed because the following two quantities are not the same:\n  {0}\n  {1}"
    )]
    AssertEq2Failed(Quantity, Quantity),
    #[error("Assertion failed because the following two quantities differ by more than {2}:\n  {0}\n  {1}")]
    AssertEq3Failed(Quantity, Quantity, Quantity),
}

#[derive(Debug, PartialEq, Eq)]
pub enum ExitStatus {
    Success,
    Error,
}

#[derive(Debug, PartialEq, Eq)]
#[must_use]
pub enum InterpreterResult {
    Quantity(Quantity),
    Continue,
    Exit(ExitStatus),
}

impl InterpreterResult {
    pub(crate) fn is_success(&self) -> bool {
        match self {
            Self::Quantity(_) => true,
            Self::Continue => true,
            Self::Exit(_) => false,
        }
    }
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
    use crate::{bytecode_interpreter::BytecodeInterpreter, prefix_transformer::Transformer};

    use super::*;

    static TEST_PRELUDE: &str = "
        dimension Scalar = 1

        dimension Length
        dimension Time
        dimension Mass

        dimension Speed = Length / Time
        dimension Momentum = Mass * Speed
        dimension Frequency = 1 / Time

        unit meter : Length
        unit second : Time
        
        unit hertz: Frequency = 1 / second
        
        fn sin(x: Scalar) -> Scalar
        fn atan2<T>(y: T, x: T) -> Scalar";

    fn get_interpreter_result(input: &str) -> Result<InterpreterResult> {
        let full_code = format!("{prelude}\n{input}", prelude = TEST_PRELUDE, input = input);
        let statements = crate::parser::parse(&full_code)
            .expect("No parse errors for inputs in this test suite");
        let statements_transformed = Transformer::new()
            .transform(statements)
            .expect("No name resolution errors for inputs in this test suite");
        let statements_typechecked = crate::typechecker::TypeChecker::default()
            .check_statements(statements_transformed)
            .expect("No type check errors for inputs in this test suite");
        BytecodeInterpreter::new(false).interpret_statements(&statements_typechecked)
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
    fn simple_arithmetic() {
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
    fn arithmetic_with_units() {
        use crate::unit::Unit;

        assert_evaluates_to(
            "2 meter + 3 meter",
            (Quantity::from_scalar(2.0 + 3.0) * Quantity::from_unit(Unit::meter())).unwrap(),
        );

        assert_evaluates_to(
            "dimension Pixel
             unit pixel : Pixel
             2 * pixel",
            (Quantity::from_scalar(2.0) * Quantity::from_unit(Unit::new_base("pixel"))).unwrap(),
        );

        assert_evaluates_to(
            "fn speed(distance: Length, time: Time) -> Speed = distance / time
             speed(10 * meter, 2 * second)",
            (Quantity::from_scalar(5.0)
                * (Quantity::from_unit(Unit::meter()) / Quantity::from_unit(Unit::second()))
                    .unwrap())
            .unwrap(),
        );
    }

    #[test]
    fn power_operator() {
        assert_evaluates_to_scalar("2^3", 2.0f64.powf(3.0));
        assert_evaluates_to_scalar("-2^4", -(2.0f64.powf(4.0)));
        assert_evaluates_to_scalar("2^(-3)", 2.0f64.powf(-3.0));
    }

    #[test]
    fn multiline_input_yields_result_of_last_line() {
        assert_evaluates_to_scalar("2\n3", 3.0);
    }

    #[test]
    fn variable_declarations() {
        assert_evaluates_to_scalar("let x = 2\nlet y = 3\nx + y", 2.0 + 3.0);
    }

    #[test]
    fn function_declarations() {
        assert_evaluates_to_scalar("fn f(x: Scalar) = 2 * x + 3\nf(5)", 2.0 * 5.0 + 3.0);
    }

    #[test]
    fn foreign_functions() {
        assert_evaluates_to_scalar("sin(1)", 1.0f64.sin());
        assert_evaluates_to_scalar("atan2(2 meter, 1 meter)", 2.0f64.atan2(1.0f64));
    }

    #[test]
    fn division_by_zero_raises_runtime_error() {
        assert_runtime_error("1/0", RuntimeError::DivisionByZero);
    }
}
