mod ast;
mod bytecode_interpreter;
mod dimension;
mod interpreter;
mod parser;
mod pretty_print;
mod span;
mod tokenizer;
mod treewalk_interpreter;
mod vm;

use interpreter::{Interpreter, InterpreterResult};
use parser::parse;
use pretty_print::PrettyPrint;

use anyhow::{Context, Result};
use rustyline::error::ReadlineError;
use rustyline::Editor;

use bytecode_interpreter::BytecodeInterpreter;

const HISTORY_FILE: &str = ".history";
const PROMPT: &str = ">>> ";

fn parse_and_evaluate(interpreter: &mut impl Interpreter, input: &str) -> bool {
    let result = parse(input);

    match result {
        Ok(statements) => {
            println!();
            for statement in &statements {
                println!("  {}", statement.pretty_print());
            }
            match interpreter.interpret_statemets(&statements) {
                Ok(InterpreterResult::Value(value)) => {
                    println!();
                    println!("    = {value:.6}", value = value);
                    println!();
                    true
                }
                Ok(InterpreterResult::Continue) => true,
                Ok(InterpreterResult::Exit) => false,
                Err(e) => {
                    eprintln!("Interpreter error: {:#}", e);
                    true
                }
            }
        }
        Err(ref e @ parser::ParseError { ref span, .. }) => {
            let line = input.lines().nth(span.line - 1).unwrap();

            eprintln!("  File \"<stdin>\", line {}", span.line);
            eprintln!("    {line}");
            eprintln!("    {offset}^", offset = " ".repeat(span.position - 1));
            eprintln!("{}", e);

            true
        }
    }
}

fn run() -> Result<()> {
    /*let mut interpreter: Box<dyn Interpreter> = if false {
        Box::new(TreewalkInterpreter::new())
    } else {
        Box::new(BytecodeInterpreter::new())
    };*/
    let mut interpreter = BytecodeInterpreter::new();

    let mut args = std::env::args();
    args.next();

    if let Some(code) = args.next() {
        parse_and_evaluate(&mut interpreter, &code);
        Ok(())
    } else {
        let mut rl = Editor::<()>::new()?;
        rl.load_history(HISTORY_FILE).ok();

        loop {
            let readline = rl.readline(PROMPT);
            match readline {
                Ok(line) => {
                    rl.add_history_entry(&line);
                    if !parse_and_evaluate(&mut interpreter, &line) {
                        break;
                    }
                }
                Err(ReadlineError::Eof) | Err(ReadlineError::Interrupted) => {
                    break;
                }
                Err(err) => {
                    println!("Error: {:?}", err);
                    break;
                }
            }
        }

        rl.save_history(HISTORY_FILE)
            .context("Error while saving history to file")
    }
}

fn main() {
    let result = run();

    if let Err(e) = result {
        eprintln!("{:#}", e);
    }
}
