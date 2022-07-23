mod ast;
mod interpreter;
mod parser;
mod pretty_print;
mod tokenizer;

use parser::parse;
use pretty_print::PrettyPrint;

use anyhow::{Context, Result};
use rustyline::error::ReadlineError;
use rustyline::Editor;

const HISTORY_FILE: &str = ".history";
const PROMPT: &str = ">>> ";

fn parse_and_evaluate(input: &str) {
    let result = parse(input).context("Error while parsing expression");

    match result {
        Ok(expression) => {
            println!("{}", expression.pretty_print());
            if let Err(e) = interpreter::run(&expression) {
                eprintln!("Interpreter error: {:#}", e);
            }
        }
        Err(e) => {
            eprintln!("{:#}", e)
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
                parse_and_evaluate(&line);
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
