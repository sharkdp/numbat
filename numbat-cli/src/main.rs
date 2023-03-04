use std::fs;
use std::path::PathBuf;

use numbat::markup::{self, FormatType};
use numbat::pretty_print::PrettyPrint;
use numbat::{
    markup::FormattedString, markup::Formatter, ExitStatus, InterpreterResult, Numbat, NumbatError,
    ParseError,
};

use anyhow::{bail, Context, Result};
use clap::Parser;
use colored::Colorize;
use rustyline::error::ReadlineError;
use rustyline::Editor;

type ControlFlow = std::ops::ControlFlow<numbat::ExitStatus>;

const PROMPT: &str = ">>> ";

#[derive(Parser, Debug)]
#[command(version, about, name("numbat"))]
struct Args {
    /// Path to source file with Numbat code. If none is given, an interactive
    /// session is started.
    file: Option<PathBuf>,

    /// Evaluate a single expression
    #[arg(short, long, value_name = "CODE", conflicts_with = "file")]
    expression: Option<String>,

    /// Do not load the prelude with predefined physical dimensions and units.
    #[arg(long)]
    no_prelude: bool,

    /// Whether or not to pretty-print every input expression.
    #[arg(long)]
    pretty_print: bool,

    /// Turn on debug mode (e.g. disassembler output).
    #[arg(long, short)]
    debug: bool,
}

enum ExecutionMode {
    Normal,
    Interactive,
}

impl ExecutionMode {
    fn exit_status_in_case_of_error(&self) -> ControlFlow {
        if matches!(self, ExecutionMode::Normal) {
            ControlFlow::Break(ExitStatus::Error)
        } else {
            ControlFlow::Continue(())
        }
    }
}

struct ANSIFormatter;

impl Formatter for ANSIFormatter {
    fn format_part(
        &self,
        FormattedString(_output_type, format_type, text): &FormattedString,
    ) -> String {
        (match format_type {
            FormatType::Text => text.normal(),
            FormatType::Keyword => text.magenta(),
            FormatType::Value => text.yellow(),
            FormatType::Unit => text.cyan(),
            FormatType::Identifier => text.red(),
            FormatType::TypeIdentifier => text.bright_yellow(),
            FormatType::Operator => text.bold(),
            FormatType::Decorator => text.yellow(),
        })
        .to_string()
    }
}

struct Cli {
    args: Args,
    numbat: Numbat,
    current_filename: Option<PathBuf>,
}

impl Cli {
    fn new() -> Self {
        let args = Args::parse();
        Self {
            numbat: Numbat::new_without_prelude(args.debug),
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
            let result = self.parse_and_evaluate(&prelude_code, ExecutionMode::Normal, false);
            if result.is_break() {
                bail!("Interpreter error in Prelude code")
            }
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
            let result =
                self.parse_and_evaluate(&code, ExecutionMode::Normal, self.args.pretty_print);

            match result {
                std::ops::ControlFlow::Continue(()) => Ok(()),
                std::ops::ControlFlow::Break(ExitStatus::Success) => Ok(()),
                std::ops::ControlFlow::Break(ExitStatus::Error) => {
                    bail!("Interpreter stopped due to error")
                }
            }
        } else {
            self.repl()
        }
    }

    fn repl(&mut self) -> Result<()> {
        println!();
        println!(" █▄░█ █░█ █▀▄▀█ █▄▄ ▄▀█ ▀█▀");
        println!(" █░▀█ █▄█ █░▀░█ █▄█ █▀█ ░█░");
        println!();

        let history_path = self.get_history_path()?;

        let mut rl = Editor::<()>::new()?;
        rl.load_history(&history_path).ok();

        let result = self.repl_loop(&mut rl);

        rl.save_history(&history_path).context(format!(
            "Error while saving history to '{}'",
            history_path.to_string_lossy()
        ))?;

        result
    }

    fn repl_loop(&mut self, rl: &mut Editor<()>) -> Result<()> {
        loop {
            let readline = rl.readline(PROMPT);
            match readline {
                Ok(line) => {
                    if !line.trim().is_empty() {
                        rl.add_history_entry(&line);
                        let result = self.parse_and_evaluate(
                            &line,
                            ExecutionMode::Interactive,
                            self.args.pretty_print,
                        );

                        match result {
                            std::ops::ControlFlow::Continue(()) => {}
                            std::ops::ControlFlow::Break(ExitStatus::Success) => {
                                return Ok(());
                            }
                            std::ops::ControlFlow::Break(ExitStatus::Error) => {
                                bail!("Interpreter stopped due to error")
                            }
                        }
                    }
                }
                Err(ReadlineError::Eof) | Err(ReadlineError::Interrupted) => {
                    return Ok(());
                }
                Err(err) => {
                    eprintln!("Error: {:?}", err);
                    todo!()
                }
            }
        }
    }

    #[must_use]
    fn parse_and_evaluate(
        &mut self,
        input: &str,
        execution_mode: ExecutionMode,
        pretty_print: bool,
    ) -> ControlFlow {
        let result = self.numbat.interpret(input);

        match result {
            Ok((statements, interpreter_result)) => {
                if pretty_print {
                    println!();
                    for statement in &statements {
                        let repr = ANSIFormatter {}.format(statement.pretty_print(), true);
                        println!("{}", repr);
                    }
                }

                match interpreter_result {
                    InterpreterResult::Quantity(quantity) => {
                        println!();

                        let formatted_number = format!("{:.6}", quantity.unsafe_value().to_f64());
                        let formatted_number = formatted_number.trim_end_matches('0');
                        let formatted_number = if formatted_number.ends_with('.') {
                            format!("{}0", formatted_number)
                        } else {
                            formatted_number.to_string()
                        };

                        let output_markup = markup::text("    ")
                            + markup::operator("=")
                            + markup::text(" ")
                            + markup::value(formatted_number)
                            + markup::text(" ")
                            + markup::unit(format!("{}", quantity.unit()));
                        println!("{}", ANSIFormatter {}.format(output_markup, false));
                        println!();

                        ControlFlow::Continue(())
                    }
                    InterpreterResult::Continue => ControlFlow::Continue(()),
                    InterpreterResult::Exit(exit_status) => ControlFlow::Break(exit_status),
                }
            }
            Err(NumbatError::ParseError(ref e @ ParseError { ref span, .. })) => {
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

                execution_mode.exit_status_in_case_of_error()
            }
            Err(NumbatError::NameResolutionError(e)) => {
                eprintln!("Name resolution error: {:#}", e);
                execution_mode.exit_status_in_case_of_error()
            }
            Err(NumbatError::TypeCheckError(e)) => {
                eprintln!("Type check error: {:#}", e);
                execution_mode.exit_status_in_case_of_error()
            }
            Err(NumbatError::RuntimeError(e)) => {
                eprintln!("Runtime error: {:#}", e);
                execution_mode.exit_status_in_case_of_error()
            }
        }
    }

    fn get_prelude_path(&self) -> PathBuf {
        let config_dir = dirs_next::config_dir().unwrap_or_else(|| PathBuf::from("."));
        config_dir.join("numbat").join("prelude.nbt") // TODO: allow for preludes in system paths, user paths, …
    }

    fn get_history_path(&self) -> Result<PathBuf> {
        let data_dir = dirs_next::data_dir()
            .unwrap_or_else(|| PathBuf::from("."))
            .join("numbat");
        fs::create_dir(&data_dir).ok();
        Ok(data_dir.join("history"))
    }
}

fn main() {
    let mut cli = Cli::new();

    if let Err(e) = cli.run() {
        eprintln!("{:#}", e);
        std::process::exit(1);
    }
}
