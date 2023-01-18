use std::fs;
use std::ops::ControlFlow;
use std::path::PathBuf;

use insect::pretty_print::PrettyPrint;
use insect::{Insect, InsectError, InterpreterResult, ParseError};

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

struct CLI {
    args: Args,
    insect: Insect,
    current_filename: Option<PathBuf>,
}

impl CLI {
    fn new() -> Self {
        let args = Args::parse();
        Self {
            insect: Insect::new(args.debug),
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
                            if self.parse_and_evaluate(&line).is_break() {
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

    fn parse_and_evaluate(&mut self, input: &str) -> ControlFlow<()> {
        let result = self.insect.interpret(input);

        match result {
            Ok((statements, interpreter_result)) => {
                if self.args.pretty_print {
                    println!();
                    for statement in &statements {
                        println!("  {}", statement.pretty_print());
                    }
                }

                match interpreter_result {
                    InterpreterResult::Quantity(quantity) => {
                        println!();
                        println!("    = {}", quantity);
                        println!();

                        ControlFlow::Continue(())
                    }
                    InterpreterResult::Continue => ControlFlow::Continue(()),
                    InterpreterResult::Exit => ControlFlow::Break(()),
                }
            }
            Err(InsectError::ParseError(ref e @ ParseError { ref span, .. })) => {
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

                ControlFlow::Continue(())
            }
            Err(InsectError::TypeCheckError(e)) => {
                eprintln!("Type check error: {:#}", e);
                ControlFlow::Continue(())
            }
            Err(InsectError::RuntimeError(e)) => {
                eprintln!("Runtime error: {:#}", e);
                ControlFlow::Continue(())
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
        fs::create_dir(&data_dir).ok();
        Ok(data_dir.join("history"))
    }
}

fn main() {
    let mut cli = CLI::new();

    if let Err(e) = cli.run() {
        eprintln!("{:#}", e);
        std::process::exit(1);
    }
}
