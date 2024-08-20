pub(crate) mod assert_eq_3;

use crate::{
    dimension::DimensionRegistry,
    markup::Markup,
    pretty_print::PrettyPrint,
    quantity::QuantityError,
    span::Span,
    typed_ast::Statement,
    unit_registry::{UnitRegistry, UnitRegistryError},
};

pub use crate::markup as m;

use assert_eq_3::AssertEq3Error;
use thiserror::Error;

pub use crate::value::Value;

#[derive(Debug, Clone, Error, PartialEq, Eq)]
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
    AssertFailed(Span),
    #[error("Assertion failed because the following two values are not the same:\n  {1}\n  {3}")]
    AssertEq2Failed(Span, Value, Span, Value),
    #[error("{0}")]
    AssertEq3Failed(AssertEq3Error),
    #[error("Could not load exchange rates from European Central Bank.")]
    CouldNotLoadExchangeRates,
    #[error("User error: {0}")]
    UserError(String),
    #[error("Unrecognized datetime format: {0}")]
    DateParsingError(String),
    #[error("Unknown timezone: {0}")]
    UnknownTimezone(String),
    #[error("Exceeded maximum size for time durations")]
    DurationOutOfRange,
    #[error("DateTime out of range")]
    DateTimeOutOfRange,
    #[error("Error in datetime format. See https://docs.rs/jiff/latest/jiff/fmt/strtime/index.html#conversion-specifications for possible format specifiers.")]
    DateFormattingError,

    #[error("Invalid format specifiers: {0}")]
    InvalidFormatSpecifiers(String),
    #[error("Incorrect type for format specifiers: {0}")]
    InvalidTypeForFormatSpecifiers(String),

    #[error("Chemical element not found: {0}")]
    ChemicalElementNotFound(String),

    #[error("Empty list")]
    EmptyList,
}

#[derive(Debug, PartialEq, Eq)]
#[must_use]
pub enum InterpreterResult {
    Value(Value),
    Continue,
}

impl InterpreterResult {
    pub fn to_markup(
        &self,
        evaluated_statement: Option<&Statement>,
        registry: &DimensionRegistry,
        with_type_info: bool,
        with_equal_sign: bool,
    ) -> Markup {
        match self {
            Self::Value(value) => {
                let leader = if with_equal_sign {
                    m::whitespace("    ") + m::operator("=") + m::space()
                } else {
                    m::empty()
                };

                let type_markup = if with_type_info {
                    evaluated_statement
                        .and_then(Statement::as_expression)
                        .and_then(|e| {
                            let type_ = e.get_type_scheme();
                            if type_.is_scalar() {
                                None
                            } else {
                                let ty = type_.to_readable_type(registry, true);
                                Some(m::dimmed("    [") + ty + m::dimmed("]"))
                            }
                        })
                        .unwrap_or_else(m::empty)
                } else {
                    m::empty()
                };

                leader + value.pretty_print() + type_markup + m::nl()
            }
            Self::Continue => m::empty(),
        }
    }

    /// Returns `true` if the interpreter result is [`Value`].
    ///
    /// [`Value`]: InterpreterResult::Value
    #[must_use]
    pub fn is_value(&self) -> bool {
        matches!(self, Self::Value(..))
    }

    /// Returns `true` if the interpreter result is [`Continue`].
    ///
    /// [`Continue`]: InterpreterResult::Continue
    #[must_use]
    pub fn is_continue(&self) -> bool {
        matches!(self, Self::Continue)
    }

    pub fn value_as_string(&self) -> Option<String> {
        match self {
            Self::Continue => None,
            Self::Value(value) => Some(value.to_string()),
        }
    }
}

pub type Result<T> = std::result::Result<T, RuntimeError>;

pub type PrintFunction = dyn FnMut(&Markup) + Send;

pub struct InterpreterSettings {
    pub print_fn: Box<PrintFunction>,
}

impl Default for InterpreterSettings {
    fn default() -> Self {
        Self {
            print_fn: Box::new(move |s: &Markup| {
                print!("{s}");
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
        dimension_registry: &DimensionRegistry,
    ) -> Result<InterpreterResult>;
    fn get_unit_registry(&self) -> &UnitRegistry;
}

#[cfg(test)]
mod tests {
    use crate::prefix_parser::AcceptsPrefix;
    use crate::quantity::Quantity;
    use crate::unit::{CanonicalName, Unit};
    use crate::{bytecode_interpreter::BytecodeInterpreter, prefix_transformer::Transformer};

    use super::*;

    static TEST_PRELUDE: &str = "
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

        unit alternative_length_base_unit: Length # maybe this should be disallowed

        @aliases(s: short)
        unit second : Time

        @aliases(Hz: short)
        unit hertz: Frequency = 1 / second

        fn sin(x: Scalar) -> Scalar
        fn atan2<D>(y: D, x: D) -> Scalar";

    #[track_caller]
    fn get_interpreter_result(input: &str) -> Result<InterpreterResult> {
        let full_code = format!("{TEST_PRELUDE}\n{input}");
        let statements = crate::parser::parse(&full_code, 0)
            .expect("No parse errors for inputs in this test suite");
        let statements_transformed = Transformer::new()
            .transform(statements)
            .expect("No name resolution errors for inputs in this test suite");
        let mut typechecker = crate::typechecker::TypeChecker::default();
        let statements_typechecked = typechecker
            .check(statements_transformed)
            .expect("No type check errors for inputs in this test suite");
        BytecodeInterpreter::new().interpret_statements(
            &mut InterpreterSettings::default(),
            &statements_typechecked,
            typechecker.registry(),
        )
    }

    #[track_caller]
    fn assert_evaluates_to(input: &str, expected: Quantity) {
        if let InterpreterResult::Value(actual) = get_interpreter_result(input).unwrap() {
            let actual = actual.unsafe_as_quantity();
            assert_eq!(actual, expected);
        } else {
            panic!();
        }
    }

    #[track_caller]
    fn assert_evaluates_to_scalar(input: &str, expected: f64) {
        assert_evaluates_to(input, Quantity::from_scalar(expected))
    }

    #[track_caller]
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

        assert_evaluates_to_scalar("+2 * 3", 2.0 * 3.0);
        assert_evaluates_to_scalar("2 * +3", 2.0 * 3.0);
        assert_evaluates_to_scalar("+2 - +3", 2.0 - 3.0);
    }

    #[test]
    fn comparisons() {
        assert_evaluates_to_scalar("if 2 meter > 150 cm then 1 else 0", 1.0);

        assert_runtime_error(
            "1 meter > alternative_length_base_unit",
            RuntimeError::QuantityError(QuantityError::IncompatibleUnits(
                Unit::new_base(
                    "meter",
                    CanonicalName::new("m", AcceptsPrefix::only_short()),
                ),
                Unit::new_base(
                    "alternative_length_base_unit",
                    CanonicalName::new("alternative_length_base_unit", AcceptsPrefix::only_long()),
                ),
            )),
        );
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
            Quantity::from_scalar(2.0)
                * Quantity::from_unit(Unit::new_base(
                    "pixel",
                    CanonicalName::new("px", AcceptsPrefix::only_short()),
                )),
        );

        assert_evaluates_to(
            "fn speed(distance: Length, time: Time) -> Velocity = distance / time
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
    fn division_by_zero_raises_runtime_error() {
        assert_runtime_error("1/0", RuntimeError::DivisionByZero);
    }
}
