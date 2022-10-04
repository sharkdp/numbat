use std::fs;
use std::path::PathBuf;

use insect::bytecode_interpreter::BytecodeInterpreter;
use insect::interpreter::{Interpreter, InterpreterResult};
use insect::parser::{parse, ParseError};
use insect::pretty_print::PrettyPrint;
use insect::typechecker::TypeChecker;

use anyhow::{Context, Result};
use clap::Parser;
use rustyline::error::ReadlineError;
use rustyline::Editor;

const PROMPT: &str = ">>> ";

#[derive(Parser, Debug)]
#[command(version, about, name("insect"))]
struct Args {
    /// Path to source file with Insect code. If none is given, an interactive
    /// session is started.
    file: Option<PathBuf>,

    /// Evaluate a single expression
    #[arg(short, long, value_name = "CODE", conflicts_with = "file")]
    expression: Option<String>,

    /// Do not load Insects prelude with predefined physical dimensions and units.
    #[arg(long)]
    no_prelude: bool,

    /// Whether or not to pretty-print every input expression.
    #[arg(long)]
    pretty_print: bool,

    /// Turn on debug mode (e.g. disassembler output).
    #[arg(long, short)]
    debug: bool,
}

struct Insect {
    args: Args,
    typechecker: TypeChecker,
    interpreter: BytecodeInterpreter,
    current_filename: Option<PathBuf>,
}

impl Insect {
    fn new() -> Self {
        let args = Args::parse();
        Self {
            interpreter: BytecodeInterpreter::new(args.debug),
            typechecker: TypeChecker::default(),
            args,
            current_filename: None,
        }
    }

    fn run(&mut self) -> Result<()> {
        if !self.args.no_prelude {
            let prelude_path = self.get_prelude_path();

            self.current_filename = Some(prelude_path.clone());
            let prelude_code = fs::read_to_string(&prelude_path).context(format!(
                "Error while reading prelude from {}",
                prelude_path.to_string_lossy()
            ))?;
            self.parse_and_evaluate(&prelude_code);
        }

        let code: Option<String> = if let Some(ref path) = self.args.file {
            self.current_filename = Some(path.clone());
            Some(fs::read_to_string(path).context(format!(
                "Could not load source file '{}'",
                path.to_string_lossy()
            ))?)
        } else {
            self.current_filename = None;
            self.args.expression.clone()
        };

        if let Some(code) = code {
            self.parse_and_evaluate(&code);
            Ok(())
        } else {
            println!(r" _                     _   ");
            println!(r"(_)_ __  ___  ___  ___| |_ ");
            println!(r"| | '_ \/ __|/ _ \/ __| __|   version 0.1");
            println!(r"| | | | \__ \  __/ (__| |_    enter '?' for help");
            println!(r"|_|_| |_|___/\___|\___|\__|");
            println!();

            let history_path = self.get_history_path()?;

            let mut rl = Editor::<()>::new()?;
            rl.load_history(&history_path).ok();

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

            rl.save_history(&history_path).context(format!(
                "Error while saving history to '{}'",
                history_path.to_string_lossy()
            ))
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

                let filename = self
                    .current_filename
                    .as_deref()
                    .map(|p| p.to_string_lossy())
                    .unwrap_or_else(|| "<input>".into());
                eprintln!(
                    "File {filename}:{line_number}:{position}",
                    filename = filename,
                    line_number = span.line,
                    position = span.position
                );
                eprintln!("    {line}");
                eprintln!("    {offset}^", offset = " ".repeat(span.position - 1));
                eprintln!("{}", e);

                true
            }
        }
    }

    fn get_prelude_path(&self) -> PathBuf {
        let config_dir = dirs_next::config_dir().unwrap_or_else(|| PathBuf::from("."));
        config_dir.join("insect").join("prelude.ins") // TODO: allow for preludes in system paths, user paths, …
    }

    fn get_history_path(&self) -> Result<PathBuf> {
        let data_dir = dirs_next::data_dir()
            .unwrap_or_else(|| PathBuf::from("."))
            .join("insect");
        fs::create_dir(&data_dir).context("Error while creating directory for history")?;
        Ok(data_dir.join("history"))
    }
}

fn main() {
    let mut insect = Insect::new();
    let result = insect.run();

    if let Err(e) = result {
        eprintln!("{:#}", e);
        std::process::exit(1);
    }
}
