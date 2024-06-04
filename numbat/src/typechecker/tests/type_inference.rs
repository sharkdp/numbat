use qualified_type::Bounds;
use qualified_type::QualifiedType;
use tests::get_typecheck_error;
use tests::{type_a, type_b, type_c};

use super::super::*;

use super::get_inferred_fn_type;

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

// Type variables S and T
fn s() -> Type {
    Type::TVar(TypeVariable::new("S"))
}

fn t() -> Type {
    Type::TVar(TypeVariable::new("T"))
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
fn basic() {
    // if-then-else
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
}

#[test]
fn function_types() {
    // TODO
    // λf. λx. f (f x)  :  (T0 -> T0) -> T0 -> T0
    // λf. λx. (f (1 m)) + x  :  Dim(T0) => (Length -> T0) -> T0 -> T0
}

#[test]
fn instantiation() {
    // TODO
    // make sure that e.g. `id` can be used twice in the same expression,
    // but with different types
}
