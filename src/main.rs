mod ast;
mod parser;
mod pretty_print;
mod tokenizer;

use parser::Parser;
use pretty_print::PrettyPrint;
use tokenizer::tokenize;

use anyhow::{Result, Context};
use rustyline::error::ReadlineError;
use rustyline::Editor;

const HISTORY_FILE: &'static str = ".history";
const PROMPT: &'static str = ">>> ";

fn parse_and_evaluate(input: &str) -> Result<()>{
    let tokens = tokenize(input)?;
    // dbg!(&tokens);
    let mut parser = Parser::new(&tokens);
    let ast = parser.expression();
    println!("{}", ast.pretty_print());

    Ok(())
}

fn run() -> Result<()> {
    let mut rl = Editor::<()>::new()?;
    rl.load_history(HISTORY_FILE).ok();

    loop {
        let readline = rl.readline(PROMPT);
        match readline {
            Ok(line) => {
                rl.add_history_entry(&line);
                parse_and_evaluate(&line).unwrap();
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

    rl.save_history(HISTORY_FILE).context("Error while saving history to file")
}

fn main() {
    run().unwrap();
}
