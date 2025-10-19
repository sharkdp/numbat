mod type_checking;
mod type_inference;

use crate::Statement;
use crate::parser::parse;
use crate::prefix_transformer::Transformer;
use crate::typechecker::{Result, TypeCheckError};
use crate::typed_ast::{self, DType};

use super::TypeChecker;
use super::type_scheme::TypeScheme;

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

    fn len<T>(x: List<T>) -> Scalar
    fn head<T>(x: List<T>) -> T

    fn id<T>(x: T) -> T = x
    fn id_for_dim<T: Dim>(x: T) -> T = x
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

fn run_typecheck(input: &str) -> Result<typed_ast::Statement<'_>> {
    let statements = parse(TEST_PRELUDE, 0)
        .expect("No parse errors for inputs in this test suite")
        .into_iter()
        .chain(parse(input, 0).expect("No parse errors for inputs in this test suite"));

    let transformed_statements = Transformer::new()
        .transform(statements)
        .map_err(|err| Box::new(err.into()))?;

    TypeChecker::default()
        .check(&transformed_statements)
        .map(|mut statements_checked| statements_checked.pop().unwrap())
}

fn assert_successful_typecheck(input: &str) {
    if let Err(err) = dbg!(run_typecheck(input)) {
        panic!("Input was expected to typecheck successfully, but failed with: {err:?}")
    }
}

fn get_inferred_fn_type(input: &str) -> TypeScheme {
    let statement = run_typecheck(input).expect("Input was expected to type-check");
    match statement {
        Statement::DefineFunction(_, _, _, _, _, _, fn_type, _, _) => fn_type,
        _ => {
            unreachable!();
        }
    }
}

#[track_caller]
fn get_typecheck_error(input: &str) -> TypeCheckError {
    if let Err(err) = dbg!(run_typecheck(input)) {
        *err
    } else {
        panic!("Input was expected to yield a type check error");
    }
}
