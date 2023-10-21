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
    #[error("Assertion failed")]
    AssertFailed,
    #[error(
        "Assertion failed because the following two quantities are not the same:\n  {0}\n  {1}"
    )]
    AssertEq2Failed(Quantity, Quantity),
    #[error("Assertion failed because the following two quantities differ by more than {2}:\n  {0}\n  {1}")]
    AssertEq3Failed(Quantity, Quantity, Quantity),
    #[error("Could not load exchange rates from European Central Bank.")]
    CouldNotLoadExchangeRates,
    #[error("User error: {0}")]
    UserError(String),
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
    use super::*;
    use crate::{bytecode_interpreter::BytecodeInterpreter, prefix_transformer::Transformer};
    use insta::assert_snapshot;

    const TEST_PRELUDE: &str = "
        dimension Scalar = 1

        dimension Length
        dimension Time
        dimension Mass

        dimension Velocity = Length / Time
        dimension Momentum = Mass * Velocity
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

    #[track_caller]
    fn get_interpreter_result(input: impl AsRef<str>) -> Result<InterpreterResult> {
        let full_code = format!(
            "{prelude}\n{input}",
            prelude = TEST_PRELUDE,
            input = input.as_ref()
        );
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

    #[track_caller]
    fn evaluates(input: impl AsRef<str>) -> String {
        match get_interpreter_result(input.as_ref()) {
            Ok(InterpreterResult::Value(value)) => format!("{}", value),
            Ok(_) => panic!("Received non value result"),
            Err(e) => format!("{e:?}: `{e}`"),
        }
    }

    #[test]
    fn simple_arithmetic() {
        assert_snapshot!(evaluates("0"), @"0");
        assert_snapshot!(evaluates("1"), @"1");
        assert_snapshot!(evaluates("1+2"), @"3");
        assert_snapshot!(evaluates("-1"), @"-1");

        assert_snapshot!(evaluates("2+3*4"), @"14");
        assert_snapshot!(evaluates("2*3+4"), @"10");
        assert_snapshot!(evaluates("(2+3)*4"), @"20");

        assert_snapshot!(evaluates("(2/3)*4"), @"2.66667");
        assert_snapshot!(evaluates("-2 * 3"), @"-6");
        assert_snapshot!(evaluates("2 * -3"), @"-6");
        assert_snapshot!(evaluates("2 - 3 - 4"), @"-5");
        assert_snapshot!(evaluates("2 - -3"), @"5");

        assert_snapshot!(evaluates("+2 * 3"), @"6");
        assert_snapshot!(evaluates("2 * +3"), @"6");
        assert_snapshot!(evaluates("+2 - +3"), @"-1");
    }

    #[test]
    fn arithmetic_with_units() {
        assert_snapshot!(evaluates(
            "2 meter + 3 meter"), @"5 m");

        assert_snapshot!(evaluates(
            "dimension Pixel
             @aliases(px: short)
             unit pixel : Pixel
             2 * pixel"), @"2 px");

        assert_snapshot!(evaluates(
            "fn speed(distance: Length, time: Time) -> Velocity = distance / time
            speed(10 * meter, 2 * second)"), @"5 m/s");
    }

    #[test]
    fn power_operator() {
        assert_snapshot!(evaluates("2^3"), @"8");
        assert_snapshot!(evaluates("-2^4"), @"-16");
        assert_snapshot!(evaluates("2^(-3)"), @"0.125");
    }

    #[test]
    fn multiline_input_yields_result_of_last_line() {
        assert_snapshot!(evaluates("2\n3"), @"3");
    }

    #[test]
    fn variable_definitions() {
        assert_snapshot!(evaluates("let x = 2\nlet y = 3\nx + y"), @"5");
    }

    #[test]
    fn function_definitions() {
        assert_snapshot!(evaluates(
            "fn f(x: Scalar) = 2 * x + 3\nf(5)"),
            @"13"
        );
    }

    #[test]
    fn foreign_functions() {
        assert_snapshot!(evaluates("sin(1)"), @"0.841471");
        assert_snapshot!(evaluates("atan2(2 meter, 1 meter)"), @"1.10715");
    }

    #[test]
    fn statistics_functions() {
        assert_snapshot!(evaluates("mean(1, 1, 1, 0)"), @"0.75");
        assert_snapshot!(evaluates(
            "mean(1 m, 1 m, 1 m, 0 m)"),
            @"0.75 m"
        );
        assert_snapshot!(evaluates("mean(2 m, 100 cm)"), @"1.5 m");

        assert_snapshot!(evaluates("maximum(1, 2, 0, -3)"), @"2");
        assert_snapshot!(evaluates(
            "maximum(2 m, 0.1 km)"), @"100 m");

        assert_snapshot!(evaluates("minimum(1, 2, 0, -3)"), @"-3");
        assert_snapshot!(evaluates(
            "minimum(2 m, 150 cm)"), @"1.5 m");
    }

    #[test]
    fn division_by_zero_raises_runtime_error() {
        assert_snapshot!(evaluates("1/0"), @"DivisionByZero: `Division by zero`");
    }

    #[test]
    fn non_rational_exponent() {
        // Regression test, found using fuzzing
        assert_snapshot!(evaluates(
            "0**0⁻⁸"),
            @"QuantityError(NonRationalExponent): `Non-rational exponent`"
        );
    }
}
