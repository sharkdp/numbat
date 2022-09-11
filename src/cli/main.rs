use std::fs;
use std::path::PathBuf;

use insect::bytecode_interpreter::BytecodeInterpreter;
use insect::interpreter::{Interpreter, InterpreterResult};
use insect::parser::{parse, ParseError};
use insect::pretty_print::PrettyPrint;

use anyhow::{Context, Result};
use clap::{AppSettings, Parser};
use rustyline::error::ReadlineError;
use rustyline::Editor;

const HISTORY_FILE: &str = ".history";
const PROMPT: &str = ">>> ";

const PRETTY_PRINT: bool = false;

#[derive(Parser, Debug)]
#[clap(version, about)]
#[clap(global_setting(AppSettings::DeriveDisplayOrder))]
struct Cli {
    /// Path to source file with Insect code. If none is given, an interactive
    /// session is started.
    file: Option<PathBuf>,

    /// Evaluate a single expression
    #[clap(short, long, value_name = "CODE", conflicts_with = "file")]
    expression: Option<String>,

    /// Do not load Insects prelude with predefined physical dimensions and units.
    #[clap(long, action)]
    no_prelude: bool,

    /// Turn on debug mode (e.g. disassembler output).
    #[clap(long, short, action)]
    debug: bool,
}

fn parse_and_evaluate(interpreter: &mut impl Interpreter, input: &str) -> bool {
    let result = parse(input);

    match result {
        Ok(statements) => {
            if PRETTY_PRINT {
                println!();
                for statement in &statements {
                    println!("  {}", statement.pretty_print());
                }
            }
            match interpreter.interpret_statements(&statements) {
                Ok(InterpreterResult::Quantity(quantity)) => {
                    println!();
                    println!("    = {}", quantity);
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
        Err(ref e @ ParseError { ref span, .. }) => {
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
    let args = Cli::parse();

    let code: Option<String> = if let Some(ref path) = args.file {
        Some(fs::read_to_string(path).context(format!(
            "Could not load source file '{}'",
            path.to_string_lossy()
        ))?)
    } else {
        args.expression
    };

    let mut interpreter = BytecodeInterpreter::new(args.debug);

    if !args.no_prelude {
        let prelude_code = fs::read_to_string("prelude.ins")?; // TODO

        let statements = parse(&prelude_code).context("Parse error in prelude")?;
        interpreter
            .interpret_statements(&statements)
            .context("Interpreter error in prelude")?;
    }

    if let Some(code) = code {
        parse_and_evaluate(&mut interpreter, &code);
        Ok(())
    } else {
        let mut rl = Editor::<()>::new()?;
        rl.load_history(HISTORY_FILE).ok();

        loop {
            let readline = rl.readline(PROMPT);
            match readline {
                Ok(line) => {
                    if !line.trim().is_empty() {
                        rl.add_history_entry(&line);
                        if !parse_and_evaluate(&mut interpreter, &line) {
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
}

fn main() {
    let result = run();

    if let Err(e) = result {
        eprintln!("{:#}", e);
    }
}
