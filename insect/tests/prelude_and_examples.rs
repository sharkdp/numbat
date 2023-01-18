use insect::{Insect, InsectError};

use std::ffi::OsStr;
use std::fs;

fn assert_typechecks_and_runs(code: &str) {
    assert!(Insect::new_without_prelude(false).interpret(code).is_ok())
}

fn assert_parse_error(code: &str) {
    assert!(matches!(Insect::new_without_prelude(false).interpret(code), Err(InsectError::ParseError(_))));
}

fn assert_typecheck_error(code: &str) {
    assert!(matches!(Insect::new_without_prelude(false).interpret(code), Err(InsectError::TypeCheckError(_))));
}

fn assert_interpreter_error(code: &str) {
    assert!(matches!(Insect::new_without_prelude(false).interpret(code), Err(InsectError::InterpreterError(_))));
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
fn typecheck_error_examples_fail_as_expected() {
    run_for_each_insect_file_in("../examples/typecheck_error", assert_typecheck_error);
}

#[test]
fn interpreter_error_examples_fail_as_expected() {
    run_for_each_insect_file_in("../examples/interpreter_error", assert_interpreter_error);
}
