use insect::{Insect, InsectError, InterpreterResult};

use std::ffi::OsStr;
use std::fs;

fn assert_typechecks_and_runs(code: &str) {
    let result = Insect::new_without_prelude(false).interpret(code);
    assert!(result.is_ok());
    assert!(matches!(
        result.unwrap().1,
        InterpreterResult::Quantity(_) | InterpreterResult::Continue
    ));
}

fn assert_parse_error(code: &str) {
    assert!(matches!(
        Insect::new_without_prelude(false).interpret(code),
        Err(InsectError::ParseError(_))
    ));
}

fn assert_name_resolution_error(code: &str) {
    assert!(matches!(
        Insect::new_without_prelude(false).interpret(code),
        Err(InsectError::NameResolutionError(_))
    ));
}

fn assert_typecheck_error(code: &str) {
    assert!(matches!(
        Insect::new_without_prelude(false).interpret(code),
        Err(InsectError::TypeCheckError(_))
    ));
}

fn assert_runtime_error(code: &str) {
    assert!(matches!(
        Insect::new_without_prelude(false).interpret(code),
        Err(InsectError::RuntimeError(_))
    ));
}

#[test]
fn prelude_can_be_parsed_and_interpreted() {
    let prelude_code = fs::read_to_string("../prelude.ins").unwrap();
    assert_typechecks_and_runs(&prelude_code);
}

fn run_for_each_insect_file_in(folder: &str, f: impl Fn(&str)) {
    let prelude_code = fs::read_to_string("../prelude.ins").unwrap();
    for entry in fs::read_dir(folder).unwrap() {
        let path = entry.unwrap().path();
        if path.extension() != Some(OsStr::new("ins")) {
            continue;
        }

        println!("Testing example {example:?}", example = path);
        let example_code = fs::read_to_string(path).unwrap();
        f(&format!(
            "{prelude}\n\
             {example}",
            prelude = prelude_code,
            example = example_code
        ));
    }
}

#[test]
fn examples_can_be_parsed_and_interpreted() {
    run_for_each_insect_file_in("../examples/", assert_typechecks_and_runs);
}

#[test]
fn parse_error_examples_fail_as_expected() {
    run_for_each_insect_file_in("../examples/parse_error", assert_parse_error);
}

#[test]
fn name_resolution_error_examples_fail_as_expected() {
    run_for_each_insect_file_in(
        "../examples/name_resolution_error",
        assert_name_resolution_error,
    );
}

#[test]
fn typecheck_error_examples_fail_as_expected() {
    run_for_each_insect_file_in("../examples/typecheck_error", assert_typecheck_error);
}

#[test]
fn runtime_error_examples_fail_as_expected() {
    run_for_each_insect_file_in("../examples/runtime_error", assert_runtime_error);
}
