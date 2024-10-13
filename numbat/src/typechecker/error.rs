use crate::ffi::ArityRange;
use crate::registry::RegistryError;
use crate::span::Span;
use crate::typed_ast::BinaryOperator;
use crate::{BaseRepresentation, NameResolutionError, Type};

use compact_str::CompactString;
use thiserror::Error;

use super::substitutions::SubstitutionError;
use super::IncompatibleDimensionsError;

#[derive(Debug, Clone, Error, PartialEq, Eq)]
pub enum TypeCheckError {
    #[error("Unknown identifier '{1}'.")]
    UnknownIdentifier(Span, String, Option<String>),

    #[error(transparent)]
    IncompatibleDimensions(IncompatibleDimensionsError),

    #[error("Exponents need to be dimensionless (got {1}).")]
    NonScalarExponent(Span, Type),

    #[error("Argument of factorial needs to be dimensionless (got {1}).")]
    NonScalarFactorialArgument(Span, Type),

    #[error("Unsupported expression in const-evaluation of exponent: {1}.")]
    UnsupportedConstEvalExpression(Span, &'static str),

    #[error("Division by zero in const. eval. expression")]
    DivisionByZeroInConstEvalExpression(Span),

    #[error("{0}")]
    RegistryError(RegistryError),

    #[error("Incompatible alternative expressions have been provided for dimension '{0}'")]
    IncompatibleAlternativeDimensionExpression(
        String,
        Span,
        BaseRepresentation,
        Span,
        BaseRepresentation,
    ),

    #[error("Function or procedure '{callable_name}' called with {num_args} arguments(s), but needs {}..{}", arity.start(), arity.end())]
    WrongArity {
        callable_span: Span,
        callable_name: String,
        callable_definition_span: Option<Span>,
        arity: ArityRange,
        num_args: usize,
    },

    #[error("'{1}' can not be used as a type parameter because it is also an existing dimension identifier.")]
    TypeParameterNameClash(Span, String),

    #[error("Foreign function definition (without body) '{1}' needs parameter and return type annotations.")]
    ForeignFunctionNeedsTypeAnnotations(Span, String),

    #[error("Unknown foreign function (without body) '{1}'")]
    UnknownForeignFunction(Span, String),

    #[error("Out-of bounds or non-rational exponent value")]
    NonRationalExponent(Span),

    #[error("Numerical overflow in const-eval expression")]
    OverflowInConstExpr(Span),

    #[error("Expected dimension type, got {1} instead")]
    ExpectedDimensionType(Span, Type),

    #[error("Expected boolean value")]
    ExpectedBool(Span),

    #[error("Incompatible types in condition")]
    IncompatibleTypesInCondition(Span, Type, Span, Type, Span),

    #[error("Argument types in assert call must be boolean")]
    IncompatibleTypeInAssert(Span, Type, Span),

    #[error("Argument types in assert_eq calls must match")]
    IncompatibleTypesInAssertEq(Span, Type, Span, Type, Span),

    #[error("Incompatible types in {0}")]
    IncompatibleTypesInAnnotation(String, Span, Type, Span, Type, Span),

    #[error("Incompatible types in comparison operator")]
    IncompatibleTypesInComparison(Span, Type, Span, Type, Span),

    #[error("Incompatible types in operator")]
    IncompatibleTypesInOperator(Span, BinaryOperator, Type, Span, Type, Span),

    #[error("Incompatible types in function call: expected '{1}', got '{3}' instead")]
    IncompatibleTypesInFunctionCall(Option<Span>, Type, Span, Type),

    #[error("Incompatible types for struct field: expected '{1}', got '{3}' instead")]
    IncompatibleTypesForStructField(Span, Type, Span, Type),

    #[error("Missing a definition for dimension {1}")]
    MissingDimension(Span, String),

    #[error("Function references can not point to generic functions")]
    NoFunctionReferenceToGenericFunction(Span),

    #[error("Only functions and function references can be called")]
    OnlyFunctionsAndReferencesCanBeCalled(Span),

    #[error("Base units can not be dimensionless.")]
    NoDimensionlessBaseUnit(Span, String),

    #[error("Unknown struct '{1}")]
    UnknownStruct(Span, String),

    #[error("Field '{2}' does not exist in struct '{3}'")]
    UnknownFieldInStructInstantiation(Span, Span, String, String),

    #[error("Duplicate field '{2}' in struct definition")]
    DuplicateFieldInStructDefinition(Span, Span, String),

    #[error("Duplicate field '{2}' in struct instantiation")]
    DuplicateFieldInStructInstantiation(Span, Span, String),

    #[error("Can not access field '{2}' of non struct type '{3}'")]
    FieldAccessOfNonStructType(Span, Span, String, Type),

    #[error("Field '{2}' does not exist in struct '{3}'")]
    UnknownFieldAccess(Span, Span, String, Type),

    #[error("Missing fields in struct instantiation")]
    MissingFieldsInStructInstantiation(Span, Span, Vec<(CompactString, Type)>),

    #[error("Incompatible types in list: expected '{1}', got '{3}' instead")]
    IncompatibleTypesInList(Span, Type, Span, Type),

    #[error(transparent)]
    NameResolutionError(#[from] NameResolutionError),

    #[error("Could not solve the following constraints:\n{0}\n.. while trying to infer types in the (elaborated) statement:\n  {1}\n")]
    ConstraintSolverError(String, String),

    #[error("{1}\nThis error occured while trying to infer types in the (elaborated) statement:\n  {0}\n")]
    SubstitutionError(String, SubstitutionError),

    #[error("Missing dimension bound for type parameter")]
    MissingDimBound(Span),

    #[error("Type for exponentiation operation can not be inferred for this case, consider adding a type annotation for the base")]
    ExponentiationNeedsTypeAnnotation(Span),

    #[error("Derived unit definitions may not contain generic types. Use a variable instead")]
    DerivedUnitDefinitionMustNotBeGeneric(Span),

    #[error("Typed hole")]
    TypedHoleInStatement(Span, String, String, Vec<String>),

    #[error("Multiple typed holes in statement")]
    MultipleTypedHoles(Span),
}

pub type Result<T> = std::result::Result<T, Box<TypeCheckError>>;
