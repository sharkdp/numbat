use insect::bytecode_interpreter::BytecodeInterpreter;
use insect::interpreter::Interpreter;
use insect::parser::parse;
use insect::typechecker::typecheck;

use std::ffi::OsStr;
use std::fs;
use std::fs::ReadDir;

fn assert_interpret_result_is_ok(code: &str) {
    let statements = parse(&code).unwrap();
    let statements_checked = typecheck(statements).unwrap();

    let mut interpreter = BytecodeInterpreter::new(false);

    let result = interpreter.interpret_statements(&statements_checked);

    assert!(result.is_ok());
}

#[test]
fn prelude_can_be_parsed_and_interpreted() {
    let prelude_code = fs::read_to_string("prelude.ins").unwrap();
    assert_interpret_result_is_ok(&prelude_code);
}

#[test]
fn examples_can_be_parsed_and_interpreted() {
    let prelude_code = fs::read_to_string("prelude.ins").unwrap();
    for example in fs::read_dir("examples/").unwrap() {
        let example = example.unwrap();

        if example.path().extension() != Some(OsStr::new("ins")) {
            continue;
        }
        println!("Testing example {:?}", example=example.path());
        let example_code = fs::read_to_string(example.path()).unwrap();
        assert_interpret_result_is_ok(&format!(
            "{prelude}\n\
             {example}",
            prelude = prelude_code,
            example = example_code
        ));
    }
}
