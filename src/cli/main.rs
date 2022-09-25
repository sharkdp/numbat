use std::fs;
use std::path::PathBuf;

use insect::bytecode_interpreter::BytecodeInterpreter;
use insect::interpreter::{Interpreter, InterpreterResult};
use insect::parser::{parse, ParseError};
use insect::pretty_print::PrettyPrint;

use anyhow::{Context, Result};
use clap::{AppSettings, Parser};
use insect::typechecker::TypeChecker;
use rustyline::error::ReadlineError;
use rustyline::Editor;

const HISTORY_FILE: &str = ".history";
const PROMPT: &str = ">>> ";

#[derive(Parser, Debug)]
#[clap(version, about)]
#[clap(global_setting(AppSettings::DeriveDisplayOrder))]
struct Args {
    /// Path to source file with Insect code. If none is given, an interactive
    /// session is started.
    file: Option<PathBuf>,

    /// Evaluate a single expression
    #[clap(short, long, value_name = "CODE", conflicts_with = "file")]
    expression: Option<String>,

    /// Do not load Insects prelude with predefined physical dimensions and units.
    #[clap(long, action)]
    no_prelude: bool,

    /// Whether or not to pretty-print every input expression.
    #[clap(long, action)]
    pretty_print: bool,

    /// Turn on debug mode (e.g. disassembler output).
    #[clap(long, short, action)]
    debug: bool,
}

struct Insect {
    args: Args,
    typechecker: TypeChecker,
    interpreter: BytecodeInterpreter,
}

impl Insect {
    fn new() -> Self {
        let args = Args::parse();
        Self {
            interpreter: BytecodeInterpreter::new(args.debug),
            typechecker: TypeChecker::default(),
            args,
        }
    }

    fn run(&mut self) -> Result<()> {
        println!(r" _                     _   ");
        println!(r"(_)_ __  ___  ___  ___| |_ ");
        println!(r"| | '_ \/ __|/ _ \/ __| __|   version 0.1");
        println!(r"| | | | \__ \  __/ (__| |_    enter '?' for help");
        println!(r"|_|_| |_|___/\___|\___|\__|");
        println!();

        let code: Option<String> = if let Some(ref path) = self.args.file {
            Some(fs::read_to_string(path).context(format!(
                "Could not load source file '{}'",
                path.to_string_lossy()
            ))?)
        } else {
            self.args.expression.clone()
        };

        if !self.args.no_prelude {
            let prelude_code = fs::read_to_string("prelude.ins")?; // TODO

            let statements = parse(&prelude_code).context("Parse error in prelude")?;
            let statements_checked = self.typechecker.check_statements(statements)?;
            self.interpreter
                .interpret_statements(&statements_checked)
                .context("Interpreter error in prelude")?;
        }

        if let Some(code) = code {
            self.parse_and_evaluate(&code);
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
                            if !self.parse_and_evaluate(&line) {
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

    fn parse_and_evaluate(&mut self, input: &str) -> bool {
        let result = parse(input);

        if self.args.debug {
            println!("{:#?}", &result);
        }

        match result {
            Ok(statements) => {
                if self.args.pretty_print {
                    println!();
                    for statement in &statements {
                        println!("  {}", statement.pretty_print());
                    }
                }

                match self.typechecker.check_statements(statements) {
                    Ok(statements_checked) => {
                        match self.interpreter.interpret_statements(&statements_checked) {
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
                    Err(e) => {
                        eprintln!("Type check error: {:#}", e);
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
}

fn main() {
    let mut insect = Insect::new();
    let result = insect.run();

    if let Err(e) = result {
        eprintln!("{:#}", e);
    }
}
