mod ast;
mod interpreter;
mod parser;
mod pretty_print;
mod span;
mod tokenizer;

use interpreter::{interpret, NextAction};
use parser::parse;
use pretty_print::PrettyPrint;

use anyhow::{Context, Result};
use rustyline::error::ReadlineError;
use rustyline::Editor;

const HISTORY_FILE: &str = ".history";
const PROMPT: &str = ">>> ";

fn parse_and_evaluate(input: &str) -> NextAction {
    let result = parse(input);

    match result {
        Ok(statement) => {
            println!();
            println!("  {}", statement.pretty_print());
            match interpret(&statement) {
                Ok(next_action) => next_action,
                Err(e) => {
                    eprintln!("Interpreter error: {:#}", e);
                    NextAction::Continue
                }
            }
        }
        Err(ref e @ parser::ParseError { ref span, .. }) => {
            let line = input.lines().nth(span.line - 1).unwrap();

            eprintln!("  File \"<stdin>\", line {}", span.line);
            eprintln!("    {line}");
            eprintln!("    {offset}^", offset = " ".repeat(span.position - 1));
            eprintln!("{}", e);

            NextAction::Continue
        }
    }
}

fn run() -> Result<()> {
    let mut rl = Editor::<()>::new()?;
    rl.load_history(HISTORY_FILE).ok();

    loop {
        let readline = rl.readline(PROMPT);
        match readline {
            Ok(line) => {
                rl.add_history_entry(&line);
                match parse_and_evaluate(&line) {
                    NextAction::Continue => {}
                    NextAction::Quit => {
                        break;
                    }
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

fn main() {
    let result = run();

    if let Err(e) = result {
        eprintln!("{:#}", e);
    }
}
