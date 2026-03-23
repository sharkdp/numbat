use crate::dimension::DimensionRegistryError;
use crate::ffi::ArityRange;
use crate::span::Span;
use crate::typed_ast::BinaryOperator;
use crate::{BaseRepresentation, NameResolutionError, Type};

use compact_str::CompactString;
use thiserror::Error;

use super::IncompatibleDimensionsError;
use super::substitutions::SubstitutionError;

fn format_constraint_error(constraints: &[String]) -> String {
    if constraints.len() == 1 {
        format!(
            "Could not solve the following constraint: {}",
            constraints[0]
        )
    } else {
        let indented: Vec<_> = constraints.iter().map(|c| format!("  {c}")).collect();
        format!(
            "Could not solve the following constraints:\n{}\n",
            indented.join("\n")
        )
    }
}

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
    DimensionRegistryError(#[from] DimensionRegistryError),

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

    #[error(
        "'{1}' can not be used as a type parameter because it is also an existing dimension identifier."
    )]
    TypeParameterNameClash(Span, String),

    #[error(
        "Foreign function definition (without body) '{1}' needs parameter and return type annotations."
    )]
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

    #[error("Incompatible types in function call: expected function type, got '{1}'")]
    OnlyFunctionsAndReferencesCanBeCalled(Span, Type),

    #[error("Base units can not be dimensionless.")]
    NoDimensionlessBaseUnit(Span, String),

    #[error("Unknown struct '{1}")]
    UnknownStruct(Span, String),

    #[error("Wrong number of type arguments for '{type_name}': expected {expected}, got {actual}")]
    WrongNumberOfTypeArguments {
        span: Span,
        type_name: String,
        expected: usize,
        actual: usize,
    },

    #[error("Field '{2}' does not exist in struct '{3}'")]
    UnknownFieldInStructInstantiation(Span, Span, String, String),

    #[error("Duplicate field '{2}' in struct definition")]
    DuplicateFieldInStructDefinition(Span, Span, String),

    #[error("Duplicate member '{2}' in struct definition")]
    DuplicateMemberInStructDefinition(Span, Span, String),

    #[error("Duplicate field '{2}' in struct instantiation")]
    DuplicateFieldInStructInstantiation(Span, Span, String),

    #[error("Can not access field '{2}' of non struct type '{3}'")]
    FieldAccessOfNonStructType(Span, Span, String, Type),

    #[error("Field '{2}' does not exist in struct '{3}'")]
    UnknownFieldAccess(Span, Span, String, Type),

    #[error("Can not call method '{1}' on non struct type '{2}'")]
    MethodCallOnNonStructType(Span, String, Type),

    #[error("Method '{1}' does not exist on struct '{2}'")]
    MethodNotFound(Span, String, String),

    #[error("Constructor '{1}' of struct '{2}' can not be called as an instance method")]
    ConstructorCalledAsMethod(Span, String, String),

    #[error("Instance method '{1}' of struct '{2}' can not be called as a constructor")]
    InstanceMethodCalledAsConstructor(Span, String, String),

    #[error("Type of 'self' parameter in method '{1}' must be '{2}'")]
    InvalidSelfParameterType(Span, String, String),

    #[error(
        "Operator decorator on method '{1}' requires an instance method with exactly one non-self parameter"
    )]
    InvalidOperatorMethodSignature(Span, String),

    #[error(
        "Index decorator on method '{1}' requires an instance method with at least one non-self parameter"
    )]
    InvalidIndexMethodSignature(Span, String),

    #[error("Can not index value of type '{1}'")]
    IndexCallOnNonStructType(Span, Type),

    #[error("List indexing expects exactly one argument, got {1}")]
    InvalidListIndexArity(Span, usize),

    #[error("List indices must be scalars, got '{1}'")]
    InvalidListIndexType(Span, Type),

    #[error("No matching index overload with {2} argument(s) exists on struct '{1}'")]
    IndexMethodNotFound(Span, String, usize),

    #[error("Multiple index overloads matched on struct '{1}' with {2} argument(s)")]
    AmbiguousIndexOverload(Span, String, usize),

    #[error("`Self` can only be used inside struct method definitions")]
    SelfTypeOutsideStructMethod(Span),

    #[error("Only function definitions are allowed in struct method section")]
    InvalidStructMember(Span),

    #[error("Missing fields in struct instantiation")]
    MissingFieldsInStructInstantiation(Span, Span, Vec<(CompactString, Type)>),

    #[error("Incompatible types in list: expected '{1}', got '{3}' instead")]
    IncompatibleTypesInList(Span, Type, Span, Type),

    #[error("Multiple operator overloads matched for '{1:?}' with operands '{2}' and '{3}'")]
    AmbiguousOperatorOverload(Span, BinaryOperator, Type, Type),

    #[error(transparent)]
    NameResolutionError(#[from] NameResolutionError),

    #[error("{}", format_constraint_error(_1))]
    ConstraintSolverError(Span, Vec<String>),

    #[error(
        "{1}\nThis error occured while trying to infer types in the (elaborated) statement:\n  {0}\n"
    )]
    SubstitutionError(String, SubstitutionError),

    #[error("Missing dimension bound for type parameter")]
    MissingDimBound(Span),

    #[error(
        "Type for exponentiation operation can not be inferred for this case, consider adding a type annotation for the base"
    )]
    ExponentiationNeedsTypeAnnotation(Span),

    #[error("Derived unit definitions may not contain generic types. Use a variable instead")]
    DerivedUnitDefinitionMustNotBeGeneric(Span),

    #[error("Typed hole")]
    TypedHoleInStatement(Span, String, String, Vec<String>),

    #[error("Multiple typed holes in statement")]
    MultipleTypedHoles(Span),
}

pub type Result<T> = std::result::Result<T, Box<TypeCheckError>>;
