use insect::bytecode_interpreter::BytecodeInterpreter;
use insect::interpreter::Interpreter;
use insect::parser::parse;
use insect::typechecker::typecheck;

use std::fs;

#[test]
fn prelude_can_be_parsed_and_interpreted() {
    let prelude_code = fs::read_to_string("prelude.ins").unwrap();
    let statements = parse(&prelude_code).unwrap();
    let statements_checked = typecheck(statements);

    let mut interpreter = BytecodeInterpreter::new(false);

    let result = interpreter.interpret_statements(&statements_checked);

    assert!(result.is_ok());
}
