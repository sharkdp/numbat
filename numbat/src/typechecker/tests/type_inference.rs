use qualified_type::Bounds;
use qualified_type::QualifiedType;
use tests::get_typecheck_error;
use tests::{type_a, type_b, type_c};

use super::super::*;

use super::get_inferred_fn_type;

fn scalar() -> Type {
    Type::Dimension(DType::scalar())
}

// Concrete types A, B and C = A×B (see TEST_PRELUDE)
fn a() -> Type {
    Type::Dimension(type_a())
}

fn a_squared() -> Type {
    Type::Dimension(type_a().power(2.into()))
}

fn b() -> Type {
    Type::Dimension(type_b())
}

fn c() -> Type {
    Type::Dimension(type_c())
}

// List<A>
fn list_a() -> Type {
    Type::List(Box::new(a()))
}

// Type variables S and T
fn s() -> Type {
    Type::TVar(TypeVariable::new("S"))
}

fn t() -> Type {
    Type::TVar(TypeVariable::new("T"))
}

// List<T>
fn list_t() -> Type {
    Type::List(Box::new(t()))
}

// Dimension type variable D and E
fn d_type() -> Type {
    Type::TVar(TypeVariable::new("D"))
}

fn d_dtype() -> Type {
    Type::Dimension(DType::from_type_variable(TypeVariable::new("D")))
}

fn d_squared() -> Type {
    Type::Dimension(DType::from_type_variable(TypeVariable::new("D")).power(2.into()))
}

fn d_cubed() -> Type {
    Type::Dimension(DType::from_type_variable(TypeVariable::new("D")).power(3.into()))
}

fn d_power6() -> Type {
    Type::Dimension(DType::from_type_variable(TypeVariable::new("D")).power(6.into()))
}

fn e_type() -> Type {
    Type::TVar(TypeVariable::new("E"))
}

fn d_times_e() -> Type {
    let d = DType::from_type_variable(TypeVariable::new("D"));
    let e = DType::from_type_variable(TypeVariable::new("E"));
    Type::Dimension(d.multiply(&e))
}

// List<D>
fn list_d() -> Type {
    Type::List(Box::new(d_dtype()))
}

fn generalize(t: Type, variables: &[Type], dvariables: &[Type]) -> TypeScheme {
    let variables = variables
        .iter()
        .flat_map(|t| t.type_variables(false))
        .collect::<Vec<_>>();

    let bounds: Bounds = dvariables.iter().map(|v| Bound::IsDim(v.clone())).collect();

    let qt = QualifiedType::new(t, bounds);

    qt.quantify(&variables)
}

macro_rules! fn_type {
    (forall $($type_params:expr),* ; dim $($dtypes:expr),* ; $($param_types:expr),* => $return_type:expr) => {
        generalize(Type::Fn(vec![$($param_types),*], Box::new($return_type)), &[$($type_params),*], &[$($dtypes),*])
    };
    (forall $($type_params:expr),* ; $($param_types:expr),* => $return_type:expr) => {
        generalize(Type::Fn(vec![$($param_types),*], Box::new($return_type)), &[$($type_params),*], &[])
    };
    ($($param_types:expr),* => $return_type:expr) => {
        TypeScheme::make_quantified(Type::Fn(vec![$($param_types),*], Box::new($return_type)))
    };
}

macro_rules! concrete_fn_type {
    ($($param_types:expr),* => $return_type:expr) => {
        Type::Fn(vec![$($param_types),*], Box::new($return_type))
    };

}

#[test]
fn if_then_else() {
    assert_eq!(
        get_inferred_fn_type("fn f(x) = if true then x else a"),
        fn_type!(a() => a())
    );
    assert_eq!(
        get_inferred_fn_type("fn f(x) = if true then a else x"),
        fn_type!(a() => a())
    );
    assert_eq!(
        get_inferred_fn_type("fn f(x) = if x then a else 0"),
        fn_type!(Type::Boolean => a())
    );
    assert_eq!(
        get_inferred_fn_type("fn f(x) = if x then 0 else a"),
        fn_type!(Type::Boolean => a())
    );
}

#[test]
fn equality() {
    assert_eq!(
        get_inferred_fn_type("fn f(x) = x == a"),
        fn_type!(a() => Type::Boolean)
    );
    assert_eq!(
        get_inferred_fn_type("fn f(x) = x == \"foo\""),
        fn_type!(Type::String => Type::Boolean)
    );
    // Ideally, this should be restricted by an `Eq` bound:
    assert_eq!(
        get_inferred_fn_type("fn f(x, y) = x == y"),
        fn_type!(forall t(); t(), t() => Type::Boolean)
    );
}

#[test]
fn comparisons() {
    assert_eq!(
        get_inferred_fn_type("fn f(x) = x > a"),
        fn_type!(a() => Type::Boolean)
    );
    assert_eq!(
        get_inferred_fn_type("fn f(x, y) = x > y"),
        fn_type!(forall t(); dim t(); t(), t() => Type::Boolean)
    );
}

#[test]
fn unary_minus() {
    assert_eq!(
        get_inferred_fn_type("fn f(x) = -x"),
        fn_type!(forall t(); dim t(); t() => t())
    );
}

#[test]
fn logical_operators() {
    assert_eq!(
        get_inferred_fn_type("fn f(x) = x && true"),
        fn_type!(Type::Boolean => Type::Boolean)
    );
}

#[test]
fn structs() {
    assert_eq!(
        get_inferred_fn_type("fn f(x) = (SomeStruct { a: x, b: b }).a"),
        fn_type!(a() => a())
    );
    assert_eq!(
        get_inferred_fn_type("fn f(x) = (SomeStruct { a: a, b: x }).a"),
        fn_type!(b() => a())
    );
}

#[test]
fn factorial() {
    assert_eq!(
        get_inferred_fn_type("fn f(x) = x!"),
        fn_type!(scalar() => scalar())
    );
}

#[test]
fn lists() {
    assert_eq!(
        get_inferred_fn_type("fn f(x) = [x]"),
        fn_type!(forall t(); t() => list_t())
    );
    assert_eq!(
        get_inferred_fn_type("fn f(x) = [a, x]"),
        fn_type!(a() => list_a())
    );
    assert_eq!(
        get_inferred_fn_type("fn f(x) = [x, a]"),
        fn_type!(a() => list_a())
    );
    assert_eq!(
        get_inferred_fn_type("fn f(xs) = [a] == xs"),
        fn_type!(list_a() => Type::Boolean)
    );
    assert_eq!(
        get_inferred_fn_type("fn f(xs) = len(xs)"),
        fn_type!(forall d_type(); list_d() => scalar())
    );
}

#[test]
fn basic_polymorphic() {
    // identity function
    assert_eq!(
        get_inferred_fn_type("fn f(x) = x"),
        fn_type!(forall t() ; t() => t())
    );

    // constant function
    assert_eq!(
        get_inferred_fn_type("fn f(x, y) = x"),
        fn_type!(forall t(), s() ; s(), t() => s())
    );
    assert_eq!(
        get_inferred_fn_type("fn f(x, y) = y"),
        fn_type!(forall t(), s() ; t(), s() => s())
    );
}

#[test]
fn dimension_types_addition_subtraction() {
    assert_eq!(
        get_inferred_fn_type("fn f(x) = x + a"),
        fn_type!(a() => a())
    );
    assert_eq!(
        get_inferred_fn_type("fn f(x) = a + x"),
        fn_type!(a() => a())
    );
    assert_eq!(
        get_inferred_fn_type("fn f(x) = x - a"),
        fn_type!(a() => a())
    );
    assert_eq!(
        get_inferred_fn_type("fn f(x) = a - x"),
        fn_type!(a() => a())
    );

    assert_eq!(
        get_inferred_fn_type("fn f(x) = x + x"),
        fn_type!(forall t(); dim t(); t() => t())
    );
    assert_eq!(
        get_inferred_fn_type("fn f(x) = x - x"),
        fn_type!(forall t(); dim t(); t() => t())
    );

    assert_eq!(
        get_inferred_fn_type("fn f(x, y) = x + y"),
        fn_type!(forall d_type(); dim d_type(); d_type(), d_type() => d_type())
    );

    assert!(matches!(
        get_typecheck_error("fn f(x) = x + true"),
        TypeCheckError::ExpectedDimensionType(..)
    ));
}

#[test]
fn dimension_types_multiplication() {
    assert_eq!(
        get_inferred_fn_type("fn f(x) = 2 * x"),
        fn_type!(forall d_type(); dim d_type(); d_type() => d_dtype())
    );
    assert_eq!(
        get_inferred_fn_type("fn f(x) = x / 2"),
        fn_type!(forall d_type(); dim d_type(); d_type() => d_dtype())
    );

    assert_eq!(
        get_inferred_fn_type("fn f(x, y) = x * y"),
        fn_type!(forall d_type(), e_type(); dim d_type(), e_type(); d_type(), e_type() => d_times_e())
    );
}

#[test]
fn dimension_types_exponentiation() {
    assert_eq!(
        get_inferred_fn_type("fn f(x: Scalar, y) = x^y"),
        fn_type!(scalar(), scalar() => scalar())
    );
    assert_eq!(
        get_inferred_fn_type("fn f(x) = x^2"),
        fn_type!(forall d_type(); dim d_type(); d_type() => d_squared())
    );
    assert_eq!(
        get_inferred_fn_type("fn f(x) = 2^x"),
        fn_type!(scalar() => scalar())
    );

    assert!(matches!(
        get_typecheck_error("fn f(x, y) = x^y"),
        TypeCheckError::ExponentiationNeedsTypeAnnotation(..)
    ));
}

#[test]
fn dimension_types_combinations() {
    assert_eq!(
        get_inferred_fn_type("fn f(x) = (x + a) / a * b"),
        fn_type!(a() => b())
    );
    assert_eq!(
        get_inferred_fn_type("fn f(x) = (x * b) + c"),
        fn_type!(a() => c())
    );
    assert_eq!(
        get_inferred_fn_type("fn f(x) = x^2 + a^2"),
        fn_type!(a() => a_squared())
    );

    assert!(matches!(
        get_typecheck_error("fn f(x) = (x + a) * (x + b)"),
        TypeCheckError::ConstraintSolverError(..)
    ));
}

#[test]
fn dimension_types_gauss_elimination() {
    assert_eq!(
        get_inferred_fn_type("fn f(x, y) = x^2 + y^3"),
        fn_type!(forall d_type(); dim d_type(); d_cubed(), d_squared() => d_power6())
    );

    assert_eq!(
        get_inferred_fn_type("fn f(x) = x^2 + x"),
        fn_type!(scalar() => scalar())
    );
}

#[test]
fn function_types() {
    assert_eq!(
        get_inferred_fn_type("fn f(g) = g() + a"),
        fn_type!(concrete_fn_type!(/* no params */ => a()) => a())
    );
    assert_eq!(
        get_inferred_fn_type("fn f(g) = g(a) + a"),
        fn_type!(concrete_fn_type!(a() => a()) => a())
    );
    assert_eq!(
        get_inferred_fn_type("fn f(g) = g(a)"),
        fn_type!(forall t(); concrete_fn_type!(a() => t()) => t())
    );
    assert_eq!(
        get_inferred_fn_type("fn apply(g, x) = g(x)"),
        fn_type!(forall s(), t(); concrete_fn_type!(t() => s()), t() => s())
    );
    assert_eq!(
        get_inferred_fn_type("fn twice(g, x) = g(g(x))"),
        fn_type!(forall t(); concrete_fn_type!(t() => t()), t() => t())
    );

    assert!(matches!(
        get_typecheck_error("fn f(g) = if true then g() else g(1)"),
        TypeCheckError::ConstraintSolverError(..)
    ));
}

#[test]
fn recursive_functions() {
    assert_eq!(
        get_inferred_fn_type("fn absurd() = absurd()"),
        fn_type!(forall t(); /* no params */ => t())
    );
    assert_eq!(
        get_inferred_fn_type("fn loop(x) = loop(x)"),
        fn_type!(forall s(), t(); s() => t())
    );
    assert_eq!(
        get_inferred_fn_type("fn fac(n) = if n == 0 then 1 else n * fac(n - 1)"),
        fn_type!(scalar() => scalar())
    );
}

#[test]
fn typed_holes() {
    assert!(matches!(
        get_typecheck_error("a + ?"),
        TypeCheckError::TypedHoleInStatement(_, type_, _, _) if type_ == "A"
    ));

    assert!(matches!(
        get_typecheck_error("c + a × ?"),
        TypeCheckError::TypedHoleInStatement(_, type_, _, _) if type_ == "B"
    ));

    assert!(matches!(
        get_typecheck_error("let x: B = c / ?"),
        TypeCheckError::TypedHoleInStatement(_, type_, _, _) if type_ == "A"
    ));

    assert!(matches!(
        get_typecheck_error("if true then a else ?"),
        TypeCheckError::TypedHoleInStatement(_, type_, _, _) if type_ == "A"
    ));

    assert!(matches!(
        get_typecheck_error("let x: C = ?(a, b)"),
        TypeCheckError::TypedHoleInStatement(_, type_, _, _) if type_ == "Fn[(A, B) -> A × B]"
    ));
}
