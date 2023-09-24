use crate::{
    markup::Markup,
    quantity::{Quantity, QuantityError},
    typed_ast::Statement,
    unit_registry::{UnitRegistry, UnitRegistryError},
};

use thiserror::Error;

pub use crate::value::Value;

#[derive(Debug, Error, PartialEq, Eq)]
pub enum RuntimeError {
    #[error("Division by zero")]
    DivisionByZero,
    #[error("Expected factorial argument to be a non-negative integer")]
    FactorialOfNegativeNumber,
    #[error("Expected factorial argument to be a finite integer number")]
    FactorialOfNonInteger,
    #[error("{0}")]
    UnitRegistryError(UnitRegistryError), // TODO: can this even be triggered?
    #[error("{0}")]
    QuantityError(QuantityError),
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
    Value(Value),
    Continue,
    Exit(ExitStatus),
}

impl InterpreterResult {
    pub fn is_success(&self) -> bool {
        match self {
            Self::Value(_) => true,
            Self::Continue => true,
            Self::Exit(_) => false,
        }
    }
}

pub type Result<T> = std::result::Result<T, RuntimeError>;

pub type PrintFunction = dyn FnMut(&Markup) -> () + Send;

pub struct InterpreterSettings {
    pub print_fn: Box<PrintFunction>,
}

impl Default for InterpreterSettings {
    fn default() -> Self {
        Self {
            print_fn: Box::new(move |s: &Markup| {
                print!("{}", s);
            }),
        }
    }
}

pub trait Interpreter {
    fn new() -> Self;

    fn interpret_statements(
        &mut self,
        settings: &mut InterpreterSettings,
        statements: &[Statement],
    ) -> Result<InterpreterResult>;
    fn get_unit_registry(&self) -> &UnitRegistry;
}

#[cfg(test)]
mod tests {
    use crate::unit::Unit;
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

        @metric_prefixes
        @aliases(m: short)
        unit meter : Length

        @aliases(s: short)
        unit second : Time

        @aliases(Hz: short)
        unit hertz: Frequency = 1 / second
        
        fn sin(x: Scalar) -> Scalar
        fn atan2<D>(y: D, x: D) -> Scalar
        fn mean<D>(xs: D…) -> D
        fn maximum<D>(xs: D…) -> D
        fn minimum<D>(xs: D…) -> D";

    fn get_interpreter_result(input: &str) -> Result<InterpreterResult> {
        let full_code = format!("{prelude}\n{input}", prelude = TEST_PRELUDE, input = input);
        let statements = crate::parser::parse(&full_code, 0)
            .expect("No parse errors for inputs in this test suite");
        let statements_transformed = Transformer::new()
            .transform(statements)
            .expect("No name resolution errors for inputs in this test suite");
        let statements_typechecked = crate::typechecker::TypeChecker::default()
            .check_statements(statements_transformed)
            .expect("No type check errors for inputs in this test suite");
        BytecodeInterpreter::new()
            .interpret_statements(&mut InterpreterSettings::default(), &statements_typechecked)
    }

    fn assert_evaluates_to(input: &str, expected: Quantity) {
        if let InterpreterResult::Value(actual) = get_interpreter_result(input).unwrap() {
            let actual = actual.unsafe_as_quantity();
            assert_eq!(actual, &expected);
        } else {
            panic!();
        }
    }

    fn assert_evaluates_to_scalar(input: &str, expected: f64) {
        assert_evaluates_to(input, Quantity::from_scalar(expected))
    }

    fn assert_runtime_error(input: &str, err_expected: RuntimeError) {
        if let Err(err_actual) = get_interpreter_result(input) {
            assert_eq!(err_actual, err_expected);
        } else {
            panic!();
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
            Quantity::from_scalar(2.0 + 3.0) * Quantity::from_unit(Unit::meter()),
        );

        assert_evaluates_to(
            "dimension Pixel
             @aliases(px: short)
             unit pixel : Pixel
             2 * pixel",
            Quantity::from_scalar(2.0) * Quantity::from_unit(Unit::new_base("pixel", "px")),
        );

        assert_evaluates_to(
            "fn speed(distance: Length, time: Time) -> Speed = distance / time
             speed(10 * meter, 2 * second)",
            Quantity::from_scalar(5.0)
                * (Quantity::from_unit(Unit::meter()) / Quantity::from_unit(Unit::second())),
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
    fn variable_definitions() {
        assert_evaluates_to_scalar("let x = 2\nlet y = 3\nx + y", 2.0 + 3.0);
    }

    #[test]
    fn function_definitions() {
        assert_evaluates_to_scalar("fn f(x: Scalar) = 2 * x + 3\nf(5)", 2.0 * 5.0 + 3.0);
    }

    #[test]
    fn foreign_functions() {
        assert_evaluates_to_scalar("sin(1)", 1.0f64.sin());
        assert_evaluates_to_scalar("atan2(2 meter, 1 meter)", 2.0f64.atan2(1.0f64));
    }

    #[test]
    fn statistics_functions() {
        assert_evaluates_to_scalar("mean(1, 1, 1, 0)", 0.75);
        assert_evaluates_to(
            "mean(1 m, 1 m, 1 m, 0 m)",
            Quantity::new_f64(0.75, Unit::meter()),
        );
        assert_evaluates_to("mean(2 m, 100 cm)", Quantity::new_f64(1.5, Unit::meter()));

        assert_evaluates_to_scalar("maximum(1, 2, 0, -3)", 2.0);
        assert_evaluates_to(
            "maximum(2 m, 0.1 km)",
            Quantity::new_f64(100.0, Unit::meter()),
        );

        assert_evaluates_to_scalar("minimum(1, 2, 0, -3)", -3.0);
        assert_evaluates_to(
            "minimum(2 m, 150 cm)",
            Quantity::new_f64(1.5, Unit::meter()),
        );
    }

    #[test]
    fn division_by_zero_raises_runtime_error() {
        assert_runtime_error("1/0", RuntimeError::DivisionByZero);
    }

    #[test]
    fn non_rational_exponent() {
        // Regression test, found using fuzzing
        assert_runtime_error(
            "0**0⁻⁸",
            RuntimeError::QuantityError(QuantityError::NonRationalExponent),
        );
    }
}
