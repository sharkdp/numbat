use insect::bytecode_interpreter::BytecodeInterpreter;
use insect::interpreter::Interpreter;
use insect::parser::parse;
use insect::typechecker::typecheck;

use std::ffi::OsStr;
use std::fs;

fn assert_typechecks_and_runs(code: &str) {
    let statements = parse(code).unwrap();
    let statements_checked = typecheck(statements).unwrap();
    assert!(BytecodeInterpreter::new(false)
        .interpret_statements(&statements_checked)
        .is_ok());
}

fn assert_typecheck_error(code: &str) {
    let statements = parse(code).unwrap();
    assert!(typecheck(statements).is_err());
}

fn assert_interpreter_error(code: &str) {
    let statements = parse(code).unwrap();
    let statements_checked = typecheck(statements).unwrap();
    assert!(BytecodeInterpreter::new(false)
        .interpret_statements(&statements_checked)
        .is_err());
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
fn typecheck_error_examples_fail_as_expected() {
    run_for_each_insect_file_in("../examples/typecheck_error", assert_typecheck_error);
}

#[test]
fn interpreter_error_examples_fail_as_expected() {
    run_for_each_insect_file_in("../examples/interpreter_error", assert_interpreter_error);
}
