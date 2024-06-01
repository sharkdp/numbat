use crate::parser::parse;
use crate::prefix_transformer::Transformer;
use crate::NameResolutionError;

use super::*;

const TEST_PRELUDE: &str = "
    dimension Scalar = 1
    dimension A
    dimension B
    dimension C = A * B
    unit a: A
    unit b: B
    unit c: C = a * b

    fn returns_a() -> A = a
    fn takes_a_returns_a(x: A) -> A = x
    fn takes_a_returns_b(x: A) -> B = b
    fn takes_a_and_b_returns_c(x: A, y: B) -> C = x * y

    struct SomeStruct { a: A, b: B }

    let callable = takes_a_returns_b

    fn atan2<T>(x: T, y: T) -> Scalar
    ";

fn type_a() -> DType {
    DType::base_dimension("A")
}

fn type_b() -> DType {
    DType::base_dimension("B")
}

fn type_c() -> DType {
    DType::base_dimension("A").multiply(&DType::base_dimension("B"))
}

fn run_typecheck(input: &str) -> Result<typed_ast::Statement> {
    let code = &format!("{prelude}\n{input}", prelude = TEST_PRELUDE, input = input);
    let statements = parse(code, 0).expect("No parse errors for inputs in this test suite");
    let transformed_statements = Transformer::new().transform(statements)?;

    TypeChecker::default()
        .check(transformed_statements)
        .map(|mut statements_checked| statements_checked.pop().unwrap())
}

fn assert_successful_typecheck(input: &str) {
    if let Err(err) = dbg!(run_typecheck(input)) {
        panic!("Input was expected to typecheck successfully, but failed with: {err:?}")
    }
}

#[track_caller]
fn get_typecheck_error(input: &str) -> TypeCheckError {
    if let Err(err) = dbg!(run_typecheck(input)) {
        err
    } else {
        panic!("Input was expected to yield a type check error");
    }
}

#[test]
fn basic_arithmetic() {
    assert_successful_typecheck("2 a + a");
    assert_successful_typecheck("2 a - a");

    assert_successful_typecheck("a * b");
    assert_successful_typecheck("a / b");

    assert_successful_typecheck("a * b + 2 c");
    assert_successful_typecheck("c / a + b");

    assert!(matches!(
        get_typecheck_error("a + b"),
        TypeCheckError::IncompatibleDimensions(IncompatibleDimensionsError {expected_type, actual_type, ..}) if expected_type == type_a().to_base_representation() && actual_type == type_b().to_base_representation()
    ));
}

#[test]
fn power_operator_with_scalar_base() {
    assert_successful_typecheck("2^2");
    assert_successful_typecheck("2^(2^2)");

    assert!(matches!(
        get_typecheck_error("2^a"),
        TypeCheckError::NonScalarExponent(_, t) if t == Type::Dimension(type_a())
    ));
    // TODO
    // assert!(matches!(
    //     get_typecheck_error("2^(c/b)"),
    //     TypeCheckError::NonScalarExponent(_, t) if t == Type::Dimension(type_a())
    // ));
    assert!(matches!(
        get_typecheck_error("2^(c/b)"),
        TypeCheckError::ConstraintSolverError(..)
    ));
}

#[test]
fn power_operator_with_dimensionful_base() {
    assert_successful_typecheck("a^2");
    assert_successful_typecheck("a^(2+3)");
    assert_successful_typecheck("a^(2-3)");
    assert_successful_typecheck("a^(2*3)");
    assert_successful_typecheck("a^(2/3)");
    assert_successful_typecheck("a^(2^3)");

    assert!(matches!(
        get_typecheck_error("a^b"),
        TypeCheckError::UnsupportedConstEvalExpression(_, desc) if desc == "unit identifier"
    ));

    // TODO: if we add ("constexpr") constants later, it would be great to support those in exponents.
    assert!(matches!(
        get_typecheck_error("let x=2
                             a^x"),
        TypeCheckError::UnsupportedConstEvalExpression(_, desc) if desc == "variable"
    ));

    assert!(matches!(
        get_typecheck_error("a^(3/(1-1))"),
        TypeCheckError::DivisionByZeroInConstEvalExpression(_)
    ));
}

#[test]
fn comparisons() {
    assert_successful_typecheck("2 a > a");
    assert_successful_typecheck("2 a / (3 a) > 3");

    assert!(matches!(
        get_typecheck_error("a > b"),
        TypeCheckError::IncompatibleDimensions(..)
    ));
}

#[test]
fn variable_definitions() {
    assert_successful_typecheck(
        "let x: A = a
             let y: B = b",
    );
    assert_successful_typecheck("let x: C = a * b");
    assert_successful_typecheck("let x: C = 2 * a * b^2 / b");
    assert_successful_typecheck("let x: A^3 = a^20 * a^(-17)");

    assert_successful_typecheck("let x: A = c / b");

    assert_successful_typecheck("let x: Bool = true");
    assert_successful_typecheck("let x: String = \"hello\"");

    assert!(matches!(
        get_typecheck_error("let x: A = b"),
        TypeCheckError::IncompatibleDimensions(IncompatibleDimensionsError {expected_type, actual_type, ..}) if expected_type == type_a().to_base_representation() && actual_type == type_b().to_base_representation()
    ));
    assert!(matches!(
        get_typecheck_error("let x: A = true"),
        TypeCheckError::IncompatibleTypesInAnnotation(_, _, annotated_type, _, actual_type, _) if annotated_type == Type::Dimension(type_a()) && actual_type == Type::Boolean
    ));
    assert!(matches!(
        get_typecheck_error("let x: A = \"foo\""),
        TypeCheckError::IncompatibleTypesInAnnotation(_, _, annotated_type, _, actual_type, _) if annotated_type == Type::Dimension(type_a()) && actual_type == Type::String
    ));
    assert!(matches!(
        get_typecheck_error("let x: Bool = a"),
        TypeCheckError::IncompatibleTypesInAnnotation(_, _, annotated_type, _, actual_type, _) if annotated_type == Type::Boolean && actual_type == Type::Dimension(type_a())
    ));
    assert!(matches!(
        get_typecheck_error("let x: String = true"),
        TypeCheckError::IncompatibleTypesInAnnotation(_, _, annotated_type, _, actual_type, _) if annotated_type == Type::String && actual_type == Type::Boolean
    ));
}

#[test]
fn unit_definitions() {
    assert_successful_typecheck("unit my_c: C = a * b");
    assert_successful_typecheck("unit foo: A*B^2 = a b^2");

    assert!(matches!(
        get_typecheck_error("unit my_c: C = a"),
        TypeCheckError::IncompatibleDimensions(IncompatibleDimensionsError {expected_type, actual_type, ..}) if expected_type == type_c().to_base_representation() && actual_type == type_a().to_base_representation()
    ));
}

#[test]
fn function_definitions() {
    assert_successful_typecheck("fn f(x: A) -> A = x");
    assert_successful_typecheck("fn f(x: A) -> A·B = 2 * x * b");
    assert_successful_typecheck("fn f(x: A, y: B) -> C = x * y");

    assert_successful_typecheck("fn f(x: A) = x");

    // assert!(matches!(
    //     get_typecheck_error("fn f(x: A, y: B) -> C = x / y"),
    //     TypeCheckError::IncompatibleDimensions(IncompatibleDimensionsError {expected_type, actual_type, ..}) if expected_type == type_c().to_base_representation() && actual_type == type_a().divide(&type_b()).to_base_representation()
    // ));

    assert!(matches!(
        get_typecheck_error("fn f(x: A) -> A = a\n\
                             f(b)"),
        TypeCheckError::IncompatibleDimensions(IncompatibleDimensionsError {expected_type, actual_type, ..}) if expected_type == type_a().to_base_representation() && actual_type == type_b().to_base_representation()
    ));
}

#[test]
fn recursive_functions() {
    assert_successful_typecheck("fn f(x: Scalar) -> Scalar = if x < 0 then f(-x) else x");
    assert_successful_typecheck("fn f(x) = if x < 0 then f(-x) else x");
    assert_successful_typecheck(
        "fn factorial(n: Scalar) -> Scalar = if n < 0 then 1 else factorial(n - 1) * n",
    );
    assert_successful_typecheck("fn factorial(n) = if n < 0 then 1 else factorial(n - 1) * n");

    // TODO
    // assert!(matches!(
    //     get_typecheck_error("fn f(x: Scalar) -> A = if x < 0 then f(-x) else 2 b"),
    //     TypeCheckError::IncompatibleTypesInCondition(_, lhs, _, rhs, _) if lhs == Type::Dimension(type_a()) && rhs == Type::Dimension(type_b())
    // ));
    assert!(matches!(
        get_typecheck_error("fn f(x: Scalar) -> A = if x < 0 then f(-x) else 2 b"),
        TypeCheckError::ConstraintSolverError(..)
    ));
}

#[test]
fn generics_basic() {
    assert_successful_typecheck(
        "
            fn f<D>(x: D) -> D = x
            f(2)
            f(2 a)
            ",
    );
    assert_successful_typecheck(
        "
            fn f<D>(x: D) -> D^2 = x*x
            f(2)
            f(2 a)
            ",
    );
    assert_successful_typecheck(
        "
            fn f<D0, D1>(x: D0, y: D1) -> D0/D1^2 = x/y^2
            f(2, 3)
            f(2 a, 2 b)
            ",
    );
    assert_successful_typecheck(
        "
        fn f3<T>(y: T, x: T) = atan2(y, x)
        ",
    );

    // assert!(matches!(
    //     get_typecheck_error("fn f<T1, T2>(x: T1, y: T2) -> T2/T1 = x/y"),
    //     TypeCheckError::IncompatibleDimensions(IncompatibleDimensionsError {expected_type, actual_type, ..})
    //         if expected_type == base_type("T2") / base_type("T1") &&
    //         actual_type == base_type("T1") / base_type("T2")
    // ));
}

// #[test]
// fn generics_multiple_unresolved_type_parameters() {
//     assert!(matches!(
//         get_typecheck_error(
//             "
//                 fn foo<D1, D2>(x: D1*D2) = 1
//                 foo(2)
//             "
//         ),
//         TypeCheckError::MultipleUnresolvedTypeParameters(..)
//     ));
// }

// #[test]
// fn generics_unused_type_parameter() {
//     assert!(matches!(
//         get_typecheck_error("
//                 fn foo<D0>(x: Scalar) -> Scalar = 1
//                 foo(2)
//             "),
//         TypeCheckError::CanNotInferTypeParameters(_, _, function_name, parameters) if function_name == "foo" && parameters == "D0"
//     ));

//     assert!(matches!(
//         get_typecheck_error("
//                 fn foo<D0, D1>(x: D0, y: D0) -> Scalar = 1
//                 foo(2, 3)
//             "),
//         TypeCheckError::CanNotInferTypeParameters(_, _, function_name, parameters) if function_name == "foo" && parameters == "D1"
//     ));

//     assert!(matches!(
//         get_typecheck_error("
//                 fn foo<D0, D1>(x: Scalar, y: Scalar) -> Scalar = 1
//                 foo(2, 3)
//             "),
//         TypeCheckError::CanNotInferTypeParameters(_, _, function_name, parameters) if function_name == "foo" && (parameters == "D1, D0" || parameters == "D0, D1")
//     ));
// }

#[test]
fn generics_type_parameter_name_clash() {
    assert!(matches!(
        get_typecheck_error("
                dimension Existing
                fn f<Existing>(x: Existing) = 1
            "),
        TypeCheckError::TypeParameterNameClash(_, name) if name == "Existing"
    ));

    assert!(matches!(
        get_typecheck_error("
                struct Existing {}
                fn f<Existing>(x: Existing) = 1
            "),
        TypeCheckError::TypeParameterNameClash(_, name) if name == "Existing"
    ));
}

#[test]
fn unknown_identifier() {
    assert!(matches!(
        get_typecheck_error("a + d"),
        TypeCheckError::UnknownIdentifier(_, ident, _) if ident == "d"
    ));
}

#[test]
fn unknown_function() {
    assert!(matches!(
        get_typecheck_error("foo(2)"),
        TypeCheckError::UnknownIdentifier(_, name, _) if name == "foo"
    ));
}

#[test]
fn incompatible_alternative_dimension_expression() {
    assert!(matches!(
        get_typecheck_error(
            "# wrong alternative expression: C / B^2
             dimension D = A / B = C / B^3"
        ),
        TypeCheckError::IncompatibleAlternativeDimensionExpression(t, ..) if t == "D",
    ));
}

#[test]
fn wrong_arity() {
    assert!(matches!(
        get_typecheck_error("
                fn f() = 1
                f(1)
            "),
        TypeCheckError::WrongArity{callable_span:_, callable_name, callable_definition_span: _, arity, num_args: 1} if arity == (0..=0) && callable_name == "f"
    ));

    assert!(matches!(
        get_typecheck_error("
                fn f(x: Scalar) = x
                f()
            "),
        TypeCheckError::WrongArity{callable_span:_, callable_name, callable_definition_span: _,  arity, num_args: 0} if arity == (1..=1) && callable_name == "f"
    ));

    assert!(matches!(
        get_typecheck_error("
                fn f(x: Scalar) = x
                f(2, 3)
            "),
        TypeCheckError::WrongArity{callable_span:_, callable_name, callable_definition_span: _,  arity, num_args: 2} if arity == (1..=1) && callable_name == "f"
    ));
}

#[test]
fn foreign_function_with_missing_return_type() {
    assert!(matches!(
        get_typecheck_error("fn sin(x: Scalar)"),
        TypeCheckError::ForeignFunctionNeedsTypeAnnotations(_, name) if name == "sin"
    ));
}

#[test]
fn unknown_foreign_function() {
    assert!(matches!(
        get_typecheck_error("fn foo(x: Scalar) -> Scalar"),
        TypeCheckError::UnknownForeignFunction(_, name) if name == "foo"
    ));
}

#[test]
fn arity_checks_in_procedure_calls() {
    assert!(matches!(
        get_typecheck_error("assert_eq(1)"),
        TypeCheckError::WrongArity{callable_span:_, callable_name, callable_definition_span: _,  arity, num_args: 1} if arity == (2..=3) && callable_name == "assert_eq"
    ));
    assert_successful_typecheck("assert_eq(1,2)");
    assert_successful_typecheck("assert_eq(1,2,3)");
    assert!(matches!(
        get_typecheck_error("assert_eq(1,2,3,4)"),
        TypeCheckError::WrongArity{callable_span:_, callable_name, callable_definition_span: _,  arity, num_args: 4} if arity == (2..=3) && callable_name == "assert_eq"
    ));
}

#[test]
fn boolean_values() {
    // assert!(matches!(
    //     get_typecheck_error("-true"),
    //     TypeCheckError::ExpectedDimensionType(_, _)
    // ));
}

#[test]
fn conditionals() {
    assert_successful_typecheck("if true then 1 else 2");
    assert_successful_typecheck("if true then true else false");

    assert!(matches!(
        get_typecheck_error("if 1 then 2 else 3"),
        TypeCheckError::ExpectedBool(_)
    ));

    assert!(matches!(
        get_typecheck_error("if true then a else b"),
        TypeCheckError::IncompatibleTypesInCondition(_, t1, _, t2, _) if t1 == Type::Dimension(DType::base_dimension("A")) && t2 == Type::Dimension(DType::base_dimension("B"))
    ));

    assert!(matches!(
        get_typecheck_error("if true then true else a"),
        TypeCheckError::IncompatibleTypesInCondition(_, t1, _, t2, _) if t1 == Type::Boolean && t2 == Type::Dimension(DType::base_dimension("A"))
    ));
}

#[test]
fn non_dtype_return_types() {
    assert!(matches!(
        get_typecheck_error("fn f() -> String = 1"),
        TypeCheckError::IncompatibleTypesInAnnotation(..)
    ));
    assert!(matches!(
        get_typecheck_error("fn f() -> Scalar = \"test\""),
        TypeCheckError::IncompatibleTypesInAnnotation(..)
    ));

    assert!(matches!(
        get_typecheck_error("fn f() -> Bool = 1"),
        TypeCheckError::IncompatibleTypesInAnnotation(..)
    ));
    assert!(matches!(
        get_typecheck_error("fn f() -> Scalar = true"),
        TypeCheckError::IncompatibleTypesInAnnotation(..)
    ));

    assert!(matches!(
        get_typecheck_error("fn f() -> String = true"),
        TypeCheckError::IncompatibleTypesInAnnotation(..)
    ));
    assert!(matches!(
        get_typecheck_error("fn f() -> Bool = \"test\""),
        TypeCheckError::IncompatibleTypesInAnnotation(..)
    ));
}

#[test]
fn function_types_basic() {
    assert_successful_typecheck(
        "
            let returns_a_ref1 = returns_a
            let returns_a_ref2: Fn[() -> A] = returns_a

            let takes_a_returns_a_ref1 = takes_a_returns_a
            let takes_a_returns_a_ref2: Fn[(A) -> A] = takes_a_returns_a

            let takes_a_returns_b_ref1 = takes_a_returns_b
            let takes_a_returns_b_ref2: Fn[(A) -> B] = takes_a_returns_b

            let takes_a_and_b_returns_C_ref1 = takes_a_and_b_returns_c
            let takes_a_and_b_returns_C_ref2: Fn[(A, B) -> C] = takes_a_and_b_returns_c
            let takes_a_and_b_returns_C_ref3: Fn[(A, B) -> A × B] = takes_a_and_b_returns_c
            ",
    );

    // assert!(matches!(
    //     get_typecheck_error("let wrong_return_type: Fn[() -> B] = returns_a"),
    //     TypeCheckError::IncompatibleTypesInAnnotation(..)
    // ));

    // assert!(matches!(
    //     get_typecheck_error("let wrong_argument_type: Fn[(B) -> A] = takes_a_returns_a"),
    //     TypeCheckError::IncompatibleTypesInAnnotation(..)
    // ));

    // assert!(matches!(
    //     get_typecheck_error("let wrong_argument_count: Fn[(A, B) -> C] = takes_a_returns_a"),
    //     TypeCheckError::IncompatibleTypesInAnnotation(..)
    // ));
}

#[test]
fn function_types_in_return_position() {
    assert_successful_typecheck(
        "
            fn returns_fn1() -> Fn[() -> A] = returns_a
            fn returns_fn2() -> Fn[(A) -> A] = takes_a_returns_a
            fn returns_fn3() -> Fn[(A) -> B] = takes_a_returns_b
            fn returns_fn4() -> Fn[(A, B) -> C] = takes_a_and_b_returns_c
            ",
    );

    // assert!(matches!(
    //     get_typecheck_error("fn returns_fn5() -> Fn[() -> B] = returns_a"),
    //     TypeCheckError::IncompatibleTypesInAnnotation(..)
    // ));
}

#[test]
fn function_types_in_argument_position() {
    assert_successful_typecheck(
        "
            fn takes_fn1(f: Fn[() -> A]) -> A = f()
            fn takes_fn2(f: Fn[(A) -> A]) -> A = f(a)
            fn takes_fn3(f: Fn[(A) -> B]) -> B = f(a)
            fn takes_fn4(f: Fn[(A, B) -> C]) -> C = f(a, b)

            takes_fn1(returns_a)
            takes_fn2(takes_a_returns_a)
            takes_fn3(takes_a_returns_b)
            takes_fn4(takes_a_and_b_returns_c)
            ",
    );

    // assert!(matches!(
    //     get_typecheck_error(
    //         "
    //             fn wrong_arity(f: Fn[(A) -> B]) -> B = f()
    //             "
    //     ),
    //     TypeCheckError::WrongArity { .. }
    // ));

    // assert!(matches!(
    //     get_typecheck_error(
    //         "
    //             fn wrong_argument_type(f: Fn[(A) -> B]) -> B = f(b)
    //             "
    //     ),
    //     TypeCheckError::IncompatibleTypesInFunctionCall(..)
    // ));

    // assert!(matches!(
    //     get_typecheck_error(
    //         "
    //             fn wrong_return_type(f: Fn[() -> A]) -> B = f()
    //             "
    //     ),
    //     TypeCheckError::IncompatibleDimensions(..)
    // ));

    // assert!(matches!(
    //     get_typecheck_error(
    //         "
    //             fn argument_mismatch(f: Fn[() -> A]) -> A = f()
    //             argument_mismatch(takes_a_returns_a)
    //             "
    //     ),
    //     TypeCheckError::IncompatibleTypesInFunctionCall(..)
    // ));
}

#[test]
fn no_dimensionless_base_units() {
    assert!(matches!(
        get_typecheck_error(
            "
                unit page: Scalar
                "
        ),
        TypeCheckError::NoDimensionlessBaseUnit { .. }
    ));
}

#[test]
fn callables() {
    assert_successful_typecheck("callable(a)");
    assert_successful_typecheck("a -> callable");
    // assert!(matches!(
    //     get_typecheck_error("callable(b)"),
    //     TypeCheckError::IncompatibleTypesInFunctionCall(..)
    // ));
    // assert!(matches!(
    //     get_typecheck_error("callable()"),
    //     TypeCheckError::WrongArity { .. }
    // ));
    // assert!(matches!(
    //     get_typecheck_error("callable(a, a)"),
    //     TypeCheckError::WrongArity { .. }
    // ));

    // assert!(matches!(
    //     get_typecheck_error("a + callable"),
    //     TypeCheckError::ExpectedDimensionType { .. }
    // ));
    // assert!(matches!(
    //     get_typecheck_error("callable == callable"),
    //     TypeCheckError::IncompatibleTypesInComparison { .. }
    // ));
}

#[test]
fn structs() {
    assert_successful_typecheck(
        "
          struct Foo {
            foo: A,
            bar: C
          }

          let s = Foo {
            foo: 1a,
            bar: 2c
          }

          let foo: A = s.foo
          let bar: C = s.bar
          ",
    );

    // assert!(matches!(
    //     get_typecheck_error("SomeStruct {a: 1, b: 1b}"),
    //     TypeCheckError::IncompatibleTypesForStructField(..)
    // ));

    // assert!(matches!(
    //     get_typecheck_error("NotAStruct {}"),
    //     TypeCheckError::UnknownStruct(_, name) if name == "NotAStruct"
    // ));

    // assert!(matches!(
    //     get_typecheck_error("SomeStruct {not_a_field: 1}"),
    //     TypeCheckError::UnknownFieldInStructInstantiation(_, _, field, _) if field == "not_a_field"
    // ));

    // assert!(matches!(
    //     get_typecheck_error("struct Foo { foo: A, foo: A }"),
    //     TypeCheckError::DuplicateFieldInStructDefinition(_, _, field) if field == "foo"
    // ));

    // assert!(matches!(
    //     get_typecheck_error("SomeStruct {a: 1a, a: 1a, b: 2b}"),
    //     TypeCheckError::DuplicateFieldInStructInstantiation(_, _, field) if field == "a"
    // ));

    // assert!(matches!(
    //     get_typecheck_error("SomeStruct {a: 1a, b: 1b}.foo"),
    //     TypeCheckError::UnknownFieldAccess(_, _, field, _) if field == "foo"
    // ));

    // assert!(matches!(
    //     get_typecheck_error("(1).foo"),
    //     TypeCheckError::FieldAccessOfNonStructType(_, _, field, _) if field == "foo"
    // ));

    // assert!(matches!(
    //     get_typecheck_error("SomeStruct {}"),
    //     TypeCheckError::MissingFieldsInStructInstantiation(..)
    // ));
}

#[test]
fn lists() {
    // assert_successful_typecheck("[]");
    assert_successful_typecheck("[1]");
    assert_successful_typecheck("[1, 2]");

    assert_successful_typecheck("[1 a]");
    assert_successful_typecheck("[1 a, 2 a]");

    assert_successful_typecheck("[[1 a, 2 a], [3 a]]");

    // assert!(matches!(
    //     get_typecheck_error("[1, a]"),
    //     TypeCheckError::IncompatibleTypesInList(..)
    // ));
    // assert!(matches!(
    //     get_typecheck_error("[[1 a], 2 a]"),
    //     TypeCheckError::IncompatibleTypesInList(..)
    // ));
    // assert!(matches!(
    //     get_typecheck_error("[[1 a], [1 b]]"),
    //     TypeCheckError::IncompatibleTypesInList(..)
    // ));
}

#[test]
fn name_resolution() {
    assert!(matches!(
        get_typecheck_error(
            "
                dimension Foo
                struct Foo {}
                "
        ),
        TypeCheckError::NameResolutionError(NameResolutionError::IdentifierClash { .. })
    ));

    assert!(matches!(
        get_typecheck_error(
            "
                struct Foo {}
                dimension Foo
                "
        ),
        TypeCheckError::NameResolutionError(NameResolutionError::IdentifierClash { .. })
    ));

    assert!(matches!(
        get_typecheck_error(
            "
                fn foo() -> Scalar = 1
                let foo = 1
                "
        ),
        TypeCheckError::NameResolutionError(NameResolutionError::IdentifierClash { .. })
    ));

    assert!(matches!(
        get_typecheck_error(
            "
                fn foo() -> Scalar = 1
                unit foo: Scalar
                "
        ),
        TypeCheckError::NameResolutionError(NameResolutionError::IdentifierClash { .. })
    ));

    assert_successful_typecheck(
        "
            let Foo = 1
            dimension Foo
            ",
    );

    assert_successful_typecheck(
        "
            fn Foo() -> Scalar = 1
            dimension Foo
            ",
    );

    assert_successful_typecheck(
        "
            fn Foo() -> Scalar = 1
            struct Foo {}
            ",
    );

    assert_successful_typecheck(
        "
            fn Foo() -> Scalar = 1
            fn Foo() -> Scalar = 2
            ",
    );
}
