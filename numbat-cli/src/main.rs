mod ansi_formatter;
mod completer;
mod highlighter;

use ansi_formatter::ansi_format;
use completer::NumbatCompleter;
use highlighter::NumbatHighlighter;

use itertools::Itertools;
use numbat::diagnostic::ErrorDiagnostic;
use numbat::markup as m;
use numbat::module_importer::{BuiltinModuleImporter, ChainedImporter, FileSystemImporter};
use numbat::pretty_print::PrettyPrint;
use numbat::resolver::CodeSource;
use numbat::{Context, ExitStatus, InterpreterResult, NumbatError};
use numbat::{InterpreterSettings, NameResolutionError, Type};

use anyhow::{bail, Context as AnyhowContext, Result};
use clap::{Parser, ValueEnum};
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

type ControlFlow = std::ops::ControlFlow<numbat::ExitStatus>;

const PROMPT: &str = ">>> ";

#[derive(Debug, Clone, Copy, PartialEq, ValueEnum)]
enum PrettyPrintMode {
    Always,
    Never,
    Auto,
}

#[derive(Parser, Debug)]
#[command(version, about, name("numbat"), max_term_width = 90)]
struct Args {
    /// Path to source file with Numbat code. If none is given, an interactive
    /// session is started.
    file: Option<PathBuf>,

    /// Evaluate a single expression. Can be specified multiple times to evaluate several expressions in sequence.
    #[arg(
        short,
        long,
        value_name = "CODE",
        conflicts_with = "file",
        action = clap::ArgAction::Append
    )]
    expression: Option<Vec<String>>,

    /// Do not load the prelude with predefined physical dimensions and units. This implies --no-init.
    #[arg(long)]
    no_prelude: bool,

    /// Do not load the user init file.
    #[arg(long)]
    no_init: bool,

    /// Whether or not to pretty-print every input expression.
    #[arg(long, value_name = "WHEN", default_value = "auto")]
    pretty_print: PrettyPrintMode,

    /// Quiet startup without intro banner.
    #[arg(short, long)]
    quiet: bool,

    /// Turn on debug mode (e.g. disassembler output).
    #[arg(long, short, hide = true)]
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

        let mut fs_importer = FileSystemImporter::default();
        for path in Self::get_modules_paths() {
            fs_importer.add_path(path);
        }

        let importer = ChainedImporter::new(
            Box::new(fs_importer),
            Box::new(BuiltinModuleImporter::default()),
        );

        let mut context = Context::new(importer);
        context.set_debug(args.debug);

        Self {
            context: Arc::new(Mutex::new(context)),
            args,
        }
    }

    fn help(&mut self) -> Result<()> {
        let output = m::nl()
            + m::keyword("numbat")
            + m::space()
            + m::text(env!("CARGO_PKG_DESCRIPTION"))
            + m::nl()
            + m::text("You can start by trying one of the examples:")
            + m::nl();

        println!("{}", ansi_format(&output, false));

        let examples = vec![
            "8 km / (1 h + 25 min)",
            "atan2(30 cm, 1 m) -> deg",
            r#"print("Energy of red photons: {ℏ 2 π c / 660 cm -> eV}")"#,
        ];
        for example in examples.iter() {
            println!("{}{}", PROMPT, example);
            let eg_result = self.parse_and_evaluate(
                example,
                CodeSource::Internal,
                ExecutionMode::Normal,
                self.args.pretty_print,
            );
            if eg_result.is_break() {
                bail!("Interpreter failed during help examples")
            }
        }

        let output = m::nl() // extra whitespace after last example
            +m::text("Full documentation:")
            +m::space()
            +m::keyword("https://numbat.dev/doc/")
            +m::nl();
        println!("{}", ansi_format(&output, false));

        Ok(())
    }

    fn run(&mut self) -> Result<()> {
        let load_prelude = !self.args.no_prelude;
        let load_init = !(self.args.no_prelude || self.args.no_init);

        if load_prelude {
            let result = self.parse_and_evaluate(
                "use prelude",
                CodeSource::Internal,
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

        if load_prelude {
            {
                self.context
                    .lock()
                    .unwrap()
                    .load_currency_module_on_demand(true);
            }
            thread::spawn(move || {
                numbat::Context::prefetch_exchange_rates();
            });
        }

        let pretty_print_mode =
            if self.args.file.is_none() && self.args.pretty_print == PrettyPrintMode::Auto {
                PrettyPrintMode::Always
            } else {
                self.args.pretty_print
            };

        let code_and_source: Option<(String, CodeSource)> = if let Some(ref path) = self.args.file {
            Some((
                fs::read_to_string(path).context(format!(
                    "Could not load source file '{}'",
                    path.to_string_lossy()
                ))?,
                CodeSource::File(path.clone()),
            ))
        } else if let Some(exprs) = &self.args.expression {
            Some((exprs.iter().join("\n"), CodeSource::Text))
        } else {
            None
        };

        if let Some((code, code_source)) = code_and_source {
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
        let history_path = self.get_history_path()?;
        let quiet_startup = self.args.quiet;

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

        if interactive && !quiet_startup {
            println!();
            println!(
                "  █▄░█ █░█ █▀▄▀█ █▄▄ ▄▀█ ▀█▀    Numbat {}",
                env!("CARGO_PKG_VERSION")
            );
            println!(
                "  █░▀█ █▄█ █░▀░█ █▄█ █▀█ ░█░    {}",
                env!("CARGO_PKG_HOMEPAGE")
            );
            println!();
        }

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

                                let markup = ctx.print_environment();
                                println!("{}", ansi_format(&markup, false));
                            }
                            "clear" => {
                                rl.clear_screen()?;
                            }
                            "quit" | "exit" => {
                                return Ok(());
                            }
                            "help" | "?" => {
                                // purposefully ignoring result of help
                                // because if the examples are failing I am assuming
                                // the user is a developer intentionally changing numbat
                                let _ = self.help();
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
        let to_be_printed: Arc<Mutex<Vec<m::Markup>>> = Arc::new(Mutex::new(vec![]));
        let to_be_printed_c = to_be_printed.clone();
        let mut settings = InterpreterSettings {
            print_fn: Box::new(move |s: &m::Markup| {
                to_be_printed_c.lock().unwrap().push(s.clone());
            }),
        };

        let (result, registry) = {
            let mut ctx = self.context.lock().unwrap();
            let registry = ctx.dimension_registry().clone(); // TODO: get rid of this clone
            (
                ctx.interpret_with_settings(&mut settings, input, code_source),
                registry,
            )
        };

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

                let to_be_printed = to_be_printed.lock().unwrap();
                for s in to_be_printed.iter() {
                    println!(
                        "{}",
                        ansi_format(s, execution_mode == ExecutionMode::Interactive)
                    );
                }
                if !to_be_printed.is_empty() && execution_mode == ExecutionMode::Interactive {
                    println!();
                }

                match interpreter_result {
                    InterpreterResult::Value(value) => {
                        let type_ = statements.last().map_or(m::empty(), |s| {
                            if let numbat::Statement::Expression(e) = s {
                                let type_ = e.get_type();

                                if type_ == Type::scalar() {
                                    m::empty()
                                } else {
                                    m::dimmed("    [")
                                        + e.get_type().to_readable_type(&registry)
                                        + m::dimmed("]")
                                }
                            } else {
                                m::empty()
                            }
                        });

                        let q_markup = m::whitespace("    ")
                            + m::operator("=")
                            + m::space()
                            + value.pretty_print()
                            + type_;
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
                self.print_diagnostic(e);
                execution_mode.exit_status_in_case_of_error()
            }
        }
    }

    fn print_diagnostic(&mut self, error: impl ErrorDiagnostic) {
        self.context.lock().unwrap().print_diagnostic(error)
    }

    fn get_config_path() -> PathBuf {
        let config_dir = dirs::config_dir().unwrap_or_else(|| PathBuf::from("."));
        config_dir.join("numbat")
    }

    fn get_modules_paths() -> Vec<PathBuf> {
        let mut paths = vec![];

        if let Some(modules_path) = std::env::var_os("NUMBAT_MODULES_PATH") {
            for path in modules_path.to_string_lossy().split(':') {
                paths.push(path.into());
            }
        }

        paths.push(Self::get_config_path().join("modules"));

        // We read the value of this environment variable at compile time to
        // allow package maintainers to control the system-wide module path
        // for Numbat.
        if let Some(system_module_path) = option_env!("NUMBAT_SYSTEM_MODULE_PATH") {
            if !system_module_path.is_empty() {
                paths.push(system_module_path.into());
            }
        } else {
            if cfg!(unix) {
                paths.push("/usr/share/numbat/modules".into());
            } else {
                paths.push("C:\\Program Files\\numbat\\modules".into());
            }
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
