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
fn dimension_types_combinations() {
    assert_eq!(
        get_inferred_fn_type("fn f(x) = (x + a) / a * b"),
        fn_type!(a() => b())
    );
    assert_eq!(
        get_inferred_fn_type("fn f(x) = (x * b) + c"),
        fn_type!(a() => c())
    );

    assert!(matches!(
        get_typecheck_error("fn f(x) = (x + a) * (x + b)"),
        TypeCheckError::ConstraintSolverError(..)
    ));
}

#[test]
fn function_types() {
    // TODO
    // λf. λx. f (f x)  :  (T0 -> T0) -> T0 -> T0
    // λf. λx. (f (1 m)) + x  :  Dim(T0) => (Length -> T0) -> T0 -> T0
}
