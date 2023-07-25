mod ansi_formatter;
mod completer;
mod highlighter;
mod keywords;

use completer::NumbatCompleter;
use highlighter::NumbatHighlighter;

use numbat::diagnostic::ErrorDiagnostic;
use numbat::pretty_print::PrettyPrint;
use numbat::resolver::{CodeSource, FileSystemImporter, ResolverError};
use numbat::{markup, NameResolutionError, RuntimeError};
use numbat::{Context, ExitStatus, InterpreterResult, NumbatError};

use anyhow::{bail, Context as AnyhowContext, Result};
use clap::{Parser, ValueEnum};
use colored::Colorize;
use rustyline::config::Configurer;
use rustyline::{
    self, error::ReadlineError, history::DefaultHistory, Completer, Editor, Helper, Hinter,
    Validator,
};
use rustyline::{EventHandler, Highlighter, KeyCode, KeyEvent, Modifiers};

use std::io::IsTerminal;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use std::{fs, thread};

use crate::ansi_formatter::ansi_format;

type ControlFlow = std::ops::ControlFlow<numbat::ExitStatus>;

const PROMPT: &str = ">>> ";

#[derive(Debug, Clone, Copy, PartialEq, ValueEnum)]
enum PrettyPrintMode {
    Always,
    Never,
    Auto,
}

#[derive(Parser, Debug)]
#[command(version, about, name("numbat"))]
struct Args {
    /// Path to source file with Numbat code. If none is given, an interactive
    /// session is started.
    file: Option<PathBuf>,

    /// Evaluate a single expression
    #[arg(short, long, value_name = "CODE", conflicts_with = "file")]
    expression: Option<String>,

    /// Do not load the prelude with predefined physical dimensions and units. This implies --no-init.
    #[arg(long)]
    no_prelude: bool,

    /// Do not load the user init file.
    #[arg(long)]
    no_init: bool,

    /// Whether or not to pretty-print every input expression.
    #[arg(long, value_name = "WHEN", default_value = "auto")]
    pretty_print: PrettyPrintMode,

    /// Turn on debug mode (e.g. disassembler output).
    #[arg(long, short)]
    debug: bool,
}

#[derive(Debug, Clone, Copy, PartialEq)]
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

#[derive(Completer, Helper, Hinter, Validator, Highlighter)]
struct NumbatHelper {
    #[rustyline(Completer)]
    completer: NumbatCompleter,
    #[rustyline(Highlighter)]
    highlighter: NumbatHighlighter,
}

struct Cli {
    args: Args,
    context: Arc<Mutex<Context>>,
}

impl Cli {
    fn new() -> Self {
        let args = Args::parse();

        let mut importer = FileSystemImporter::default();
        for path in Self::get_modules_paths() {
            importer.add_path(path);
        }

        let mut context = Context::new(importer);
        context.set_debug(args.debug);

        Self {
            context: Arc::new(Mutex::new(context)),
            args,
        }
    }

    fn run(&mut self) -> Result<()> {
        let load_prelude = !self.args.no_prelude;
        let load_init = !(self.args.no_prelude || self.args.no_init);

        if load_prelude {
            let result = self.parse_and_evaluate(
                "use prelude",
                CodeSource::Text,
                ExecutionMode::Normal,
                PrettyPrintMode::Never,
            );
            if result.is_break() {
                bail!("Interpreter error in Prelude code")
            }
        }

        if load_init {
            let user_init_path = Self::get_config_path().join("init.nbt");

            if let Ok(user_init_code) = fs::read_to_string(&user_init_path) {
                let result = self.parse_and_evaluate(
                    &user_init_code,
                    CodeSource::File(user_init_path),
                    ExecutionMode::Normal,
                    PrettyPrintMode::Never,
                );
                if result.is_break() {
                    bail!("Interpreter error in user initialization code")
                }
            }
        }

        let ctx = self.context.clone();
        thread::spawn(move || {
            numbat::Context::fetch_exchange_rates();

            // After pre-fetching the exchange rates, we can load the 'non_euro_currencies'
            // module without blocking the context for long. This allows us to have fast
            // startup times of the CLI application, but still have currency units available
            // after a short delay (the limiting factor is the HTTP request).
            ctx.lock()
                .unwrap()
                .interpret("use units::non_euro_currencies", CodeSource::Text)
                .ok();
        });

        let pretty_print_mode =
            if !self.args.file.is_some() && self.args.pretty_print == PrettyPrintMode::Auto {
                PrettyPrintMode::Always
            } else {
                self.args.pretty_print
            };

        let (code, code_source): (Option<String>, CodeSource) =
            if let Some(ref path) = self.args.file {
                (
                    Some(fs::read_to_string(path).context(format!(
                        "Could not load source file '{}'",
                        path.to_string_lossy()
                    ))?),
                    CodeSource::File(path.clone()),
                )
            } else {
                (self.args.expression.clone(), CodeSource::Text)
            };

        if let Some(code) = code {
            let result = self.parse_and_evaluate(
                &code,
                code_source,
                ExecutionMode::Normal,
                pretty_print_mode,
            );

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
        let interactive = std::io::stdin().is_terminal();
        if interactive {
            println!();
            println!(" █▄░█ █░█ █▀▄▀█ █▄▄ ▄▀█ ▀█▀");
            println!(" █░▀█ █▄█ █░▀░█ █▄█ █▀█ ░█░");
            println!();
        }

        let history_path = self.get_history_path()?;

        let mut rl = Editor::<NumbatHelper, DefaultHistory>::new()?;
        rl.set_max_history_size(1000)
            .context("Error while configuring history size")?;
        rl.set_completion_type(rustyline::CompletionType::List);
        rl.set_helper(Some(NumbatHelper {
            completer: NumbatCompleter {
                context: self.context.clone(),
            },
            highlighter: NumbatHighlighter {
                context: self.context.clone(),
            },
        }));
        rl.bind_sequence(
            KeyEvent(KeyCode::Enter, Modifiers::ALT),
            EventHandler::Simple(rustyline::Cmd::Newline),
        );
        rl.load_history(&history_path).ok();

        let result = self.repl_loop(&mut rl);

        if interactive {
            rl.save_history(&history_path).context(format!(
                "Error while saving history to '{}'",
                history_path.to_string_lossy()
            ))?;
        }

        result
    }

    fn repl_loop(&mut self, rl: &mut Editor<NumbatHelper, DefaultHistory>) -> Result<()> {
        loop {
            let readline = rl.readline(PROMPT);
            match readline {
                Ok(line) => {
                    if !line.trim().is_empty() {
                        rl.add_history_entry(&line)?;

                        match line.trim() {
                            "list" | "ls" | "ll" => {
                                let ctx = self.context.lock().unwrap();

                                let mut functions = Vec::from(ctx.function_names());
                                functions.sort();
                                let mut dimensions = Vec::from(ctx.dimension_names());
                                dimensions.sort();
                                let mut units = Vec::from(ctx.unit_names());
                                units.sort();
                                let mut variables = Vec::from(ctx.variable_names());
                                variables.sort();

                                println!("{}", "List of functions:".bold());
                                for function in functions {
                                    println!("  {function}");
                                }
                                println!();
                                println!("{}", "List of dimensions:".bold());
                                for dimension in dimensions {
                                    println!("  {dimension}");
                                }
                                println!();
                                println!("{}", "List of units:".bold());
                                for unit_names in units {
                                    println!("  {}", itertools::join(unit_names.iter(), ", "));
                                }
                                println!();
                                println!("{}", "List of variables:".bold());
                                for variable in variables {
                                    println!("  {variable}");
                                }
                            }
                            "clear" => {
                                rl.clear_screen()?;
                            }
                            "quit" | "exit" => {
                                return Ok(());
                            }
                            _ => {
                                let result = self.parse_and_evaluate(
                                    &line,
                                    CodeSource::Text,
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
                    }
                }
                Err(ReadlineError::Interrupted) => {}
                Err(ReadlineError::Eof) => {
                    return Ok(());
                }
                Err(err) => {
                    bail!(err);
                }
            }
        }
    }

    #[must_use]
    fn parse_and_evaluate(
        &mut self,
        input: &str,
        code_source: CodeSource,
        execution_mode: ExecutionMode,
        pretty_print_mode: PrettyPrintMode,
    ) -> ControlFlow {
        let result = { self.context.lock().unwrap().interpret(input, code_source) };

        let pretty_print = match pretty_print_mode {
            PrettyPrintMode::Always => true,
            PrettyPrintMode::Never => false,
            PrettyPrintMode::Auto => execution_mode == ExecutionMode::Interactive,
        };

        match result {
            Ok((statements, interpreter_result)) => {
                if pretty_print {
                    println!();
                    for statement in &statements {
                        let repr = ansi_format(&statement.pretty_print(), true);
                        println!("{}", repr);
                        println!();
                    }
                } else if execution_mode == ExecutionMode::Interactive {
                    println!();
                }

                match interpreter_result {
                    InterpreterResult::Quantity(quantity) => {
                        let q_markup = markup::whitespace("    ")
                            + markup::operator("=")
                            + markup::space()
                            + quantity.pretty_print();
                        println!("{}", ansi_format(&q_markup, false));
                        println!();

                        ControlFlow::Continue(())
                    }
                    InterpreterResult::Continue => ControlFlow::Continue(()),
                    InterpreterResult::Exit(exit_status) => ControlFlow::Break(exit_status),
                }
            }
            Err(NumbatError::ResolverError(e)) => {
                self.print_diagnostic(e.clone());
                if matches!(&e, ResolverError::UnknownModule(_, module_path) if module_path.0 == &["prelude"])
                {
                    eprintln!("Make sure that you have properly set up Numbat's module path.");
                }
                execution_mode.exit_status_in_case_of_error()
            }
            Err(NumbatError::NameResolutionError(
                e @ (NameResolutionError::IdentifierClash { .. }
                | NameResolutionError::ReservedIdentifier(_)),
            )) => {
                self.print_diagnostic(e);
                execution_mode.exit_status_in_case_of_error()
            }
            Err(NumbatError::TypeCheckError(e)) => {
                self.print_diagnostic(e);
                execution_mode.exit_status_in_case_of_error()
            }
            Err(NumbatError::RuntimeError(e)) => {
                if execution_mode == ExecutionMode::Interactive
                    && matches!(e, RuntimeError::NoStatements)
                {
                    ControlFlow::Continue(())
                } else {
                    self.print_diagnostic(e);
                    execution_mode.exit_status_in_case_of_error()
                }
            }
        }
    }

    fn print_diagnostic(&mut self, error: impl ErrorDiagnostic) {
        self.context.lock().unwrap().print_diagnostic(error)
    }

    fn get_config_path() -> PathBuf {
        let config_dir = dirs::config_dir().unwrap_or_else(|| PathBuf::from("."));
        config_dir.join("numbat") // TODO: allow for preludes in system paths, user paths, …
    }

    fn get_modules_paths() -> Vec<PathBuf> {
        let mut paths = vec![];

        if let Some(path) = std::env::var_os("NUMBAT_MODULES_PATH") {
            paths.push(path.into());
        }

        paths.push(Self::get_config_path().join("modules"));
        if cfg!(unix) {
            paths.push("/usr/share/numbat/modules".into());
        } else {
            paths.push("C:\\Program Files\\numbat\\modules".into());
        }
        paths
    }

    fn get_history_path(&self) -> Result<PathBuf> {
        let data_dir = dirs::data_dir()
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
