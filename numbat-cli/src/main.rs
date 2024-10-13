mod ansi_formatter;
mod completer;
mod config;
mod highlighter;

use ansi_formatter::ansi_format;
use colored::control::SHOULD_COLORIZE;
use completer::NumbatCompleter;
use config::{ColorMode, Config, ExchangeRateFetchingPolicy, IntroBanner, PrettyPrintMode};
use highlighter::NumbatHighlighter;

use itertools::Itertools;
use numbat::command::{self, CommandParser, SourcelessCommandParser};
use numbat::compact_str::{CompactString, ToCompactString};
use numbat::diagnostic::ErrorDiagnostic;
use numbat::help::help_markup;
use numbat::markup as m;
use numbat::module_importer::{BuiltinModuleImporter, ChainedImporter, FileSystemImporter};
use numbat::pretty_print::PrettyPrint;
use numbat::resolver::CodeSource;
use numbat::session_history::{ParseEvaluationResult, SessionHistory, SessionHistoryOptions};
use numbat::{Context, NumbatError};
use numbat::{InterpreterSettings, NameResolutionError};

use anyhow::{bail, Context as AnyhowContext, Result};
use clap::Parser;
use rustyline::config::Configurer;
use rustyline::{
    error::ReadlineError, history::DefaultHistory, Completer, Editor, Helper, Hinter, Validator,
};
use rustyline::{EventHandler, Highlighter, KeyCode, KeyEvent, Modifiers};

use std::io::IsTerminal;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use std::{fs, thread};

#[derive(Debug, PartialEq, Eq)]
pub enum ExitStatus {
    Success,
    Error,
}

type ControlFlow = std::ops::ControlFlow<ExitStatus>;

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
        action = clap::ArgAction::Append
    )]
    expression: Option<Vec<String>>,

    /// Enter interactive session after running a numbat script or expression
    #[arg(short, long)]
    inspect_interactively: bool,

    /// Do not load the user configuration file.
    #[arg(long, hide_short_help = true)]
    no_config: bool,

    /// Do not load the prelude with predefined physical dimensions and units. This implies --no-init.
    #[arg(short = 'N', long, hide_short_help = true)]
    no_prelude: bool,

    /// Do not load the user init file.
    #[arg(long, hide_short_help = true)]
    no_init: bool,

    /// Whether or not to pretty-print every input expression.
    #[arg(long, value_name = "WHEN")]
    pretty_print: Option<PrettyPrintMode>,

    /// Whether or not coloring should occur.
    #[arg(long, value_name = "WHEN")]
    color: Option<ColorMode>,

    /// What kind of intro banner to show (if any).
    #[arg(long, value_name = "MODE")]
    intro_banner: Option<IntroBanner>,

    /// Generate a default configuration file
    #[arg(long, hide_short_help = true)]
    generate_config: bool,

    /// Turn on debug mode and print disassembler output (hidden, mainly for development)
    #[arg(long, short, hide = true)]
    debug: bool,
}

struct ParseEvaluationOutcome {
    control_flow: ControlFlow,
    result: ParseEvaluationResult,
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
    fn new(args: Args) -> Result<Self> {
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

        config.load_prelude &= !args.no_prelude;
        config.load_user_init &= !(args.no_prelude || args.no_init);

        config.intro_banner = args.intro_banner.unwrap_or(config.intro_banner);
        config.pretty_print = args.pretty_print.unwrap_or(config.pretty_print);
        config.color = args.color.unwrap_or(config.color);

        config.enter_repl =
            (args.file.is_none() && args.expression.is_none()) || args.inspect_interactively;

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

        context.set_terminal_width(
            terminal_size::terminal_size().map(|(terminal_size::Width(w), _)| w as usize),
        );

        Ok(Self {
            context: Arc::new(Mutex::new(context)),
            config,
            file: args.file,
            expression: args.expression,
        })
    }

    fn run(&mut self) -> Result<()> {
        match self.config.color {
            ColorMode::Never => SHOULD_COLORIZE.set_override(false),
            ColorMode::Always => SHOULD_COLORIZE.set_override(true),
            ColorMode::Auto => (), // Let colored itself decide whether coloring should occur or not
        }

        if self.config.load_prelude {
            let result = self.parse_and_evaluate(
                "use prelude",
                CodeSource::Internal,
                ExecutionMode::Normal,
                PrettyPrintMode::Never,
            );
            if result.control_flow.is_break() {
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
                if result.control_flow.is_break() {
                    bail!("Interpreter error in user initialization code")
                }
            }
        }

        if self.config.load_prelude
            && self.config.exchange_rates.fetching_policy != ExchangeRateFetchingPolicy::Never
        {
            self.context
                .lock()
                .unwrap()
                .load_currency_module_on_demand(true);
        }

        let mut code_and_source = Vec::new();

        if let Some(ref path) = self.file {
            code_and_source.push((
                (fs::read_to_string(path).context(format!(
                    "Could not load source file '{}'",
                    path.to_string_lossy()
                ))?),
                CodeSource::File(path.clone()),
            ));
        };

        if let Some(expressions) = &self.expression {
            code_and_source.push((expressions.iter().join("\n"), CodeSource::Text));
        }

        let mut run_result = Ok(());

        if !code_and_source.is_empty() {
            for (code, code_source) in code_and_source {
                let result = self.parse_and_evaluate(
                    &code,
                    code_source,
                    ExecutionMode::Normal,
                    self.config.pretty_print,
                );

                let result_status = match result.control_flow {
                    std::ops::ControlFlow::Continue(()) => Ok(()),
                    std::ops::ControlFlow::Break(_) => {
                        bail!("Interpreter stopped")
                    }
                };

                run_result = run_result.and(result_status);
            }
        }

        if self.config.enter_repl {
            let mut currency_fetch_thread = if self.config.load_prelude
                && self.config.exchange_rates.fetching_policy
                    == ExchangeRateFetchingPolicy::OnStartup
            {
                Some(thread::spawn(move || {
                    numbat::Context::prefetch_exchange_rates();
                }))
            } else {
                None
            };

            let repl_result = self.repl();
            if let Some(thread) = currency_fetch_thread.take() {
                let _ = thread.join();
            }
            run_result = run_result.and(repl_result);
        }

        run_result
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
                modules: self.context.lock().unwrap().list_modules().collect(),
                all_timezones: jiff::tz::db()
                    .available()
                    .map(CompactString::from)
                    .collect(),
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

        let result = self.repl_loop(&mut rl, interactive);

        if interactive {
            rl.save_history(&history_path).context(format!(
                "Error while saving history to '{}'",
                history_path.to_string_lossy()
            ))?;
        }

        result
    }

    fn repl_loop(
        &mut self,
        rl: &mut Editor<NumbatHelper, DefaultHistory>,
        interactive: bool,
    ) -> Result<()> {
        let mut session_history = SessionHistory::default();

        loop {
            let readline = rl.readline(&self.config.prompt);
            match readline {
                Ok(line) => {
                    if !line.trim().is_empty() {
                        rl.add_history_entry(&line)?;

                        // if we enter here, the line looks like a command
                        if let Some(sourceless_parser) = SourcelessCommandParser::new(&line) {
                            let mut parser = CommandParser::new(
                                sourceless_parser,
                                self.context
                                    .lock()
                                    .unwrap()
                                    .resolver_mut()
                                    .add_code_source(CodeSource::Text, &line),
                            );

                            match parser.parse_command() {
                                Ok(command) => match command {
                                    command::Command::Help => {
                                        let help = help_markup();
                                        print!("{}", ansi_format(&help, true));
                                        // currently, the ansi formatter adds indents
                                        // _after_ each newline and so we need to manually
                                        // add an extra blank line to absorb this indent
                                        println!();
                                    }
                                    command::Command::Info { item } => {
                                        let help = self
                                            .context
                                            .lock()
                                            .unwrap()
                                            .print_info_for_keyword(item);
                                        println!("{}", ansi_format(&help, true));
                                    }
                                    command::Command::List { items } => {
                                        let context = self.context.lock().unwrap();
                                        let m = match items {
                                            None => context.print_environment(),
                                            Some(command::ListItems::Functions) => {
                                                context.print_functions()
                                            }
                                            Some(command::ListItems::Dimensions) => {
                                                context.print_dimensions()
                                            }
                                            Some(command::ListItems::Variables) => {
                                                context.print_variables()
                                            }
                                            Some(command::ListItems::Units) => {
                                                context.print_units()
                                            }
                                        };
                                        println!("{}", ansi_format(&m, false));
                                    }
                                    command::Command::Clear => rl.clear_screen()?,
                                    command::Command::Save { dst } => {
                                        let save_result = session_history.save(
                                            dst,
                                            SessionHistoryOptions {
                                                include_err_lines: false,
                                                trim_lines: true,
                                            },
                                        );
                                        match save_result {
                                            Ok(_) => {
                                                let m = m::text(
                                                    "successfully saved session history to",
                                                ) + m::space()
                                                    + m::string(dst.to_compact_string());
                                                println!("{}", ansi_format(&m, interactive));
                                            }
                                            Err(err) => {
                                                self.print_diagnostic(*err);
                                                continue;
                                            }
                                        }
                                    }
                                    command::Command::Quit => return Ok(()),
                                },
                                Err(e) => {
                                    self.print_diagnostic(e);
                                    continue;
                                }
                            }

                            continue;
                        }

                        let ParseEvaluationOutcome {
                            control_flow,
                            result,
                        } = self.parse_and_evaluate(
                            &line,
                            CodeSource::Text,
                            if interactive {
                                ExecutionMode::Interactive
                            } else {
                                ExecutionMode::Normal
                            },
                            self.config.pretty_print,
                        );

                        match control_flow {
                            std::ops::ControlFlow::Continue(()) => {}
                            std::ops::ControlFlow::Break(ExitStatus::Success) => {
                                return Ok(());
                            }
                            std::ops::ControlFlow::Break(ExitStatus::Error) => {
                                bail!("Interpreter stopped due to error")
                            }
                        }

                        session_history.push(CompactString::from(line), result);
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
    ) -> ParseEvaluationOutcome {
        let to_be_printed: Arc<Mutex<Vec<m::Markup>>> = Arc::new(Mutex::new(vec![]));
        let to_be_printed_c = to_be_printed.clone();
        let mut settings = InterpreterSettings {
            print_fn: Box::new(move |s: &m::Markup| {
                to_be_printed_c.lock().unwrap().push(s.clone());
            }),
        };

        let interpretation_result =
            self.context
                .lock()
                .unwrap()
                .interpret_with_settings(&mut settings, input, code_source);

        let interactive = execution_mode == ExecutionMode::Interactive;

        let pretty_print = match pretty_print_mode {
            PrettyPrintMode::Always => true,
            PrettyPrintMode::Never => false,
            PrettyPrintMode::Auto => interactive,
        };

        let parse_eval_result = match &interpretation_result {
            Ok(_) => Ok(()),
            Err(_) => Err(()),
        };

        let control_flow = match interpretation_result.map_err(|b| *b) {
            Ok((statements, interpreter_result)) => {
                if interactive || pretty_print {
                    println!();
                }

                if pretty_print {
                    for statement in &statements {
                        let repr = ansi_format(&statement.pretty_print(), true);
                        println!("{repr}");
                        println!();
                    }
                }

                let to_be_printed = to_be_printed.lock().unwrap();
                for s in to_be_printed.iter() {
                    println!("{}", ansi_format(s, interactive));
                }
                if interactive && !to_be_printed.is_empty() {
                    println!();
                }

                let ctx = self.context.lock().unwrap();
                let registry = ctx.dimension_registry();
                let result_markup = interpreter_result.to_markup(
                    statements.last(),
                    registry,
                    interactive || pretty_print,
                    interactive || pretty_print,
                );
                print!("{}", ansi_format(&result_markup, false));

                if (interactive || pretty_print) && interpreter_result.is_value() {
                    println!();
                }

                ControlFlow::Continue(())
            }
            Err(NumbatError::ResolverError(e)) => {
                self.print_diagnostic(e);
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
        };

        ParseEvaluationOutcome {
            control_flow,
            result: parse_eval_result,
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
        fs::create_dir_all(&data_dir).ok();
        Ok(data_dir.join("history"))
    }
}

fn generate_config() -> Result<()> {
    let config_folder_path = Cli::get_config_path();
    let config_file_path = config_folder_path.join("config.toml");

    if config_file_path.exists() {
        bail!(
            "The file '{}' exists already.",
            config_file_path.to_string_lossy()
        );
    }

    std::fs::create_dir_all(&config_folder_path).context(format!(
        "Error while creating folder '{}'",
        config_folder_path.to_string_lossy()
    ))?;

    let config = Config::default();
    let content = toml::to_string(&config).context("Error while creating TOML from config")?;

    std::fs::write(&config_file_path, content)?;

    println!(
        "A default configuration has been written to '{}'.",
        config_file_path.to_string_lossy()
    );
    println!("Open the file in a text editor. Modify whatever you want to change and remove the other fields");

    Ok(())
}

fn main() {
    let args = Args::parse();

    if args.generate_config {
        if let Err(e) = generate_config() {
            eprintln!("{e:#}");
            std::process::exit(1);
        }
        std::process::exit(0);
    }

    if let Err(e) = Cli::new(args).and_then(|mut cli| cli.run()) {
        eprintln!("{e:#}");
        std::process::exit(1);
    }
}
