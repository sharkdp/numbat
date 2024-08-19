mod common;

use common::get_test_context;

use numbat::resolver::{CodeSource, ResolverError};
use numbat::{InterpreterResult, NumbatError};

use std::ffi::OsStr;
use std::fs;

use crate::common::get_test_context_without_prelude;

fn assert_runs(code: &str) {
    let result = get_test_context().interpret(code, CodeSource::Internal);
    assert!(result.is_ok(), "Failed with: {result:#?}");
    assert!(matches!(
        result.unwrap().1,
        InterpreterResult::Value(_) | InterpreterResult::Continue
    ));
}

fn assert_runs_without_prelude(code: &str) {
    let result = get_test_context_without_prelude().interpret(code, CodeSource::Internal);
    assert!(result.is_ok(), "Failed with: {}", result.unwrap_err());
    assert!(matches!(
        result.unwrap().1,
        InterpreterResult::Value(_) | InterpreterResult::Continue
    ));
}

fn assert_parse_error(code: &str) {
    assert!(matches!(
        get_test_context().interpret(code, CodeSource::Internal),
        Err(NumbatError::ResolverError(
            ResolverError::ParseErrors { .. }
        ))
    ));
}

fn assert_name_resolution_error(code: &str) {
    assert!(matches!(
        get_test_context().interpret(code, CodeSource::Internal),
        Err(NumbatError::NameResolutionError(_))
    ));
}

fn assert_typecheck_error(code: &str) {
    assert!(matches!(
        get_test_context().interpret(code, CodeSource::Internal),
        Err(NumbatError::TypeCheckError(_))
    ));
}

fn assert_runtime_error(code: &str) {
    assert!(matches!(
        get_test_context().interpret(code, CodeSource::Internal),
        Err(NumbatError::RuntimeError(_))
    ));
}

fn run_for_each_file(glob_pattern: &str, f: impl Fn(&str)) {
    for entry in glob::glob(glob_pattern).unwrap() {
        let path = entry.unwrap();
        if path.extension() != Some(OsStr::new("nbt")) {
            continue;
        }

        println!("Testing example {path:?}");
        let example_code = fs::read_to_string(path).unwrap();

        f(&example_code);
    }
}

#[test]
fn modules_are_self_consistent() {
    run_for_each_file("modules/**/*.nbt", assert_runs_without_prelude);
}

#[test]
fn examples_can_be_parsed_and_interpreted() {
    run_for_each_file("../examples/*.nbt", assert_runs);
}

#[test]
fn numbat_tests_are_executed_successfully() {
    run_for_each_file("../examples/tests/*.nbt", assert_runs);
}

#[test]
fn parse_error_examples_fail_as_expected() {
    run_for_each_file("../examples/parse_error/*.nbt", assert_parse_error);
}

#[test]
fn name_resolution_error_examples_fail_as_expected() {
    run_for_each_file(
        "../examples/name_resolution_error/*.nbt",
        assert_name_resolution_error,
    );
}

#[test]
fn typecheck_error_examples_fail_as_expected() {
    run_for_each_file("../examples/typecheck_error/*.nbt", assert_typecheck_error);
}

#[test]
fn runtime_error_examples_fail_as_expected() {
    run_for_each_file("../examples/runtime_error/*.nbt", assert_runtime_error);
}
