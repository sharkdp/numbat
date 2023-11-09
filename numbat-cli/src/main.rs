mod ansi_formatter;
mod completer;
mod highlighter;

use ansi_formatter::ansi_format;
use completer::NumbatCompleter;
use highlighter::NumbatHighlighter;

use itertools::Itertools;
use numbat::diagnostic::ErrorDiagnostic;
use numbat::help::help_markup;
use numbat::markup as m;
use numbat::module_importer::{BuiltinModuleImporter, ChainedImporter, FileSystemImporter};
use numbat::pretty_print::PrettyPrint;
use numbat::resolver::CodeSource;
use numbat::{Context, InterpreterResult, NumbatError};
use numbat::{InterpreterSettings, NameResolutionError};

use anyhow::{bail, Context as AnyhowContext, Result};
use clap::{Parser, ValueEnum};
use rustyline::config::Configurer;
use rustyline::{
    self, error::ReadlineError, history::DefaultHistory, Completer, Editor, Helper, Hinter,
    Validator,
};
use rustyline::{EventHandler, Highlighter, KeyCode, KeyEvent, Modifiers};
use serde::Deserialize;
use toml;

use std::io::IsTerminal;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use std::{fs, thread};

#[derive(Deserialize, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
enum ExchangeRateFetchingPolicy {
    Prefetch,
    FetchOnFirstUse,
    DontFetch,
}

#[derive(Deserialize, PartialEq, Eq, Default, Debug, Clone, Copy, ValueEnum)]
#[serde(rename_all = "kebab-case")]
enum IntroBanner {
    #[default]
    Long,
    Short,
    Off,
}

#[derive(Deserialize)]
#[serde(rename_all = "kebab-case", default, deny_unknown_fields)]
struct Config {
    exchange_rate_fetching_policy: ExchangeRateFetchingPolicy,
    prompt: String,
    intro_banner: IntroBanner,
    pretty_print: PrettyPrintMode,
    load_prelude: bool,
    load_user_init: bool,
}

impl Default for Config {
    fn default() -> Self {
        Config {
            exchange_rate_fetching_policy: ExchangeRateFetchingPolicy::Prefetch,
            prompt: ">>> ".to_owned(),
            intro_banner: IntroBanner::default(),
            pretty_print: PrettyPrintMode::Auto,
            load_prelude: true,
            load_user_init: true,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ExitStatus {
    Success,
    Error,
}

type ControlFlow = std::ops::ControlFlow<ExitStatus>;

#[derive(Debug, Clone, Copy, PartialEq, ValueEnum, Deserialize)]
#[serde(rename_all = "kebab-case")]
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
    #[arg(long, value_name = "WHEN")]
    pretty_print: Option<PrettyPrintMode>,

    /// What kind of intro banner to show (if any).
    #[arg(long, value_name = "MODE")]
    intro_banner: Option<IntroBanner>,

    /// Turn on debug mode and print disassembler output (hidden, mainly for development)
    #[arg(long, short, hide = true)]
    debug: bool,

    /// Do not load the user configuration file (hidden, mainly for testing purposes)
    #[arg(long, hide = true)]
    no_config: bool,
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
    config: Config,
    context: Arc<Mutex<Context>>,
    file: Option<PathBuf>,
    expression: Option<Vec<String>>,
}

impl Cli {
    fn new() -> Result<Self> {
        let args = Args::parse();

        let user_config_path = Self::get_config_path().join("config.toml");

        let mut config = if args.no_config {
            Config::default()
        } else if let Ok(contents) = fs::read_to_string(&user_config_path) {
            toml::from_str(&contents).context(format!(
                "Error while loading {}",
                user_config_path.to_string_lossy()
            ))?
        } else {
            Config::default()
        };

        config.load_prelude = if args.no_prelude {
            false
        } else {
            config.load_prelude
        };
        config.load_user_init = if args.no_prelude || args.no_init {
            false
        } else {
            config.load_user_init
        };

        config.intro_banner = args.intro_banner.unwrap_or(config.intro_banner);
        config.pretty_print = args.pretty_print.unwrap_or(config.pretty_print);

        config.pretty_print = if args.file.is_none() && config.pretty_print == PrettyPrintMode::Auto
        {
            PrettyPrintMode::Always
        } else {
            config.pretty_print
        };

        let mut fs_importer = FileSystemImporter::default();
        for path in Self::get_modules_paths() {
            fs_importer.add_path(path);
        }

        let importer = ChainedImporter::new(
            Box::new(fs_importer),
            Box::<BuiltinModuleImporter>::default(),
        );

        let mut context = Context::new(importer);
        context.set_debug(args.debug);

        Ok(Self {
            context: Arc::new(Mutex::new(context)),
            config,
            file: args.file,
            expression: args.expression,
        })
    }

    fn run(&mut self) -> Result<()> {
        if self.config.load_prelude {
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

        if self.config.load_user_init {
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

        if self.config.load_prelude
            && self.config.exchange_rate_fetching_policy != ExchangeRateFetchingPolicy::DontFetch
        {
            {
                self.context
                    .lock()
                    .unwrap()
                    .load_currency_module_on_demand(true);
            }

            if self.config.exchange_rate_fetching_policy == ExchangeRateFetchingPolicy::Prefetch {
                thread::spawn(move || {
                    numbat::Context::prefetch_exchange_rates();
                });
            }
        }

        let code_and_source: Option<(String, CodeSource)> = if let Some(ref path) = self.file {
            Some((
                fs::read_to_string(path).context(format!(
                    "Could not load source file '{}'",
                    path.to_string_lossy()
                ))?,
                CodeSource::File(path.clone()),
            ))
        } else {
            self.expression
                .as_ref()
                .map(|exprs| (exprs.iter().join("\n"), CodeSource::Text))
        };

        if let Some((code, code_source)) = code_and_source {
            let result = self.parse_and_evaluate(
                &code,
                code_source,
                ExecutionMode::Normal,
                self.config.pretty_print,
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

        if interactive {
            match self.config.intro_banner {
                IntroBanner::Long => {
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
                IntroBanner::Short => {
                    println!("Numbat {}", env!("CARGO_PKG_VERSION"));
                }
                IntroBanner::Off => {}
            }
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
            let readline = rl.readline(&self.config.prompt);
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
                                let help = help_markup();
                                print!("{}", ansi_format(&help, true));
                                // currently, the ansi formatter adds indents
                                // _after_ each newline and so we need to manually
                                // add an extra blank line to absorb this indent
                                println!();
                            }
                            _ => {
                                let result = self.parse_and_evaluate(
                                    &line,
                                    CodeSource::Text,
                                    ExecutionMode::Interactive,
                                    self.config.pretty_print,
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

                let result_markup = interpreter_result.to_markup(statements.last(), &registry);
                print!("{}", ansi_format(&result_markup, false));

                match interpreter_result {
                    InterpreterResult::Value(_) => {
                        println!();
                        ControlFlow::Continue(())
                    }
                    InterpreterResult::Continue => ControlFlow::Continue(()),
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
        } else if cfg!(unix) {
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
    if let Err(e) = Cli::new().and_then(|mut cli| cli.run()) {
        eprintln!("{:#}", e);
        std::process::exit(1);
    }
}
