use std::str::{FromStr, SplitWhitespace};

use compact_str::ToCompactString;

use crate::{
    Context, ParseError, RuntimeError,
    diagnostic::{ErrorDiagnostic, ResolverDiagnostic},
    help::help_markup,
    markup::{self as m, Markup},
    parser::ParseErrorKind,
    resolver::CodeSource,
    session_history::{SessionHistory, SessionHistoryOptions},
    span::{ByteIndex, Span},
};

#[derive(Debug, Clone, PartialEq)]
pub enum ListItems {
    Functions,
    Dimensions,
    Variables,
    Units,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum QuitAlias {
    Quit,
    Exit,
}

#[non_exhaustive]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CommandKind {
    Help,
    Info,
    List,
    Clear,
    Save,
    Reset,
    Quit(QuitAlias),
}

impl FromStr for CommandKind {
    type Err = ();

    fn from_str(word: &str) -> Result<Self, Self::Err> {
        use CommandKind::*;
        Ok(match word {
            "help" | "?" => Help,
            "info" => Info,
            "list" => List,
            "clear" => Clear,
            "save" => Save,
            "reset" => Reset,
            "quit" => Quit(QuitAlias::Quit),
            "exit" => Quit(QuitAlias::Exit),
            _ => return Err(()),
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum HelpKind {
    BasicHelp,
    AllCommands,
}

#[derive(Debug, PartialEq, Eq)]
#[must_use]
pub enum CommandControlFlow {
    Continue,
    Return,
    Reset,
    NotACommand,
}

#[derive(Debug)]
pub enum CommandError {
    Parse(ParseError),
    Runtime(RuntimeError),
}

impl From<ParseError> for CommandError {
    fn from(err: ParseError) -> Self {
        Self::Parse(err)
    }
}

impl From<RuntimeError> for CommandError {
    fn from(err: RuntimeError) -> Self {
        Self::Runtime(err)
    }
}

impl ErrorDiagnostic for ResolverDiagnostic<'_, CommandError> {
    fn diagnostics(&self) -> Vec<crate::Diagnostic> {
        match self.error {
            CommandError::Parse(parse_error) => parse_error.diagnostics(),
            CommandError::Runtime(runtime_error) => ResolverDiagnostic {
                resolver: self.resolver,
                error: runtime_error,
            }
            .diagnostics(),
        }
    }
}

enum ParsedCommand<'session, 'input> {
    Help { help_kind: HelpKind },
    Info { item: &'input str },
    List { items: Option<ListItems> },
    Clear,
    Save { session_history: &'session SessionHistory, dst: &'input str },
    Reset,
    Quit,
}

pub struct CommandRunner<'a, Editor = ()> {
    print_markup: Option<Box<dyn FnMut(&Markup) + 'a>>,
    clear: Option<Box<dyn FnMut(&mut Editor) -> CommandControlFlow + 'a>>,
    session_history: Option<SessionHistory>,
    reset: Option<()>,
    quit: Option<()>,
}

// cannot be derived because `#[derive(Default)]` introduces the bound `Editor:
// Default`, which is not necessary
impl<Editor> Default for CommandRunner<'_, Editor> {
    fn default() -> Self {
        Self {
            print_markup: None,
            clear: None,
            session_history: None,
            reset: None,
            quit: None,
        }
    }
}

impl<'a, Editor> CommandRunner<'a, Editor> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn print_with(mut self, action: impl FnMut(&Markup) + 'a) -> Self {
        self.print_markup = Some(Box::new(action));
        self
    }

    pub fn enable_clear(
        mut self,
        action: impl FnMut(&mut Editor) -> CommandControlFlow + 'a,
    ) -> Self {
        self.clear = Some(Box::new(action));
        self
    }

    pub fn enable_save(mut self, session_history: SessionHistory) -> Self {
        self.session_history = Some(session_history);
        self
    }

    pub fn enable_reset(mut self) -> Self {
        self.reset = Some(());
        self
    }

    pub fn enable_quit(mut self) -> Self {
        self.quit = Some(());
        self
    }

    pub fn push_to_history(&mut self, line: &str, result: Result<(), ()>) {
        let Some(session_history) = self.session_history.as_mut() else {
            return;
        };
        session_history.push(line.to_compact_string(), result);
    }

    fn help_markup(&self, help_kind: HelpKind) -> m::Markup {
        match help_kind {
            HelpKind::BasicHelp => help_markup(HelpKind::BasicHelp),
            HelpKind::AllCommands => self.all_commands_markup(),
        }
    }

    fn all_commands_markup(&self) -> m::Markup {
        fn m_cmd(cmd: &'static str) -> m::Markup {
            m::keyword(cmd)
        }

        fn m_arg(arg: &'static str) -> m::Markup {
            m::value(arg)
        }

        fn cmd(
            cmd: &'static str,
            args: impl AsRef<[&'static str]>,
            aliases: impl AsRef<[&'static str]>,
            help: &'static str,
        ) -> m::Markup {
            cmd_fmt(cmd, args, aliases, m::text(help))
        }

        fn cmd_fmt(
            cmd: &'static str,
            args: impl AsRef<[&'static str]>,
            aliases: impl AsRef<[&'static str]>,
            help: m::Markup,
        ) -> m::Markup {
            let indent = m::text("  ");
            let mut output = indent;
            output += m_cmd(cmd);
            for arg in args.as_ref() {
                output += m::space();
                output += m_arg(arg)
            }

            output += m::text(": ");
            output += help;

            let aliases = aliases.as_ref();
            let mut aliases_iter = aliases.iter();
            if let Some(first_alias) = aliases_iter.next() {
                if aliases.len() == 1 {
                    output += m::text(" (alias: ");
                } else {
                    output += m::text(" (aliases: ");
                }
                output += m_cmd(first_alias);

                for alias in aliases_iter {
                    output += m::text(", ");
                    output += m_cmd(alias);
                }
                output += m::text(")");
            }

            output += m::nl();

            output
        }

        let mut output = m::nl()
            + m::text("Numbat supports the following commands:")
            + m::nl()
            + cmd("help", [], ["?"], "show a basic introduction to Numbat")
            + cmd(
                "help",
                ["commands"],
                ["?"],
                "show the list of Numbat commands and information about them",
            )
            + cmd(
                "info",
                ["<identifier>"],
                [],
                "get more info about a particular item, such as a function, variable, or unit",
            )
            + cmd("list", [], [], "show all currently defined items")
            + cmd_fmt(
                "list",
                ["<what>"],
                [],
                m::text("show all currently defined items of the specified type, where ")
                    + m_arg("<what>")
                    + m::text(" is one of ")
                    + m_arg("functions")
                    + m::text(", ")
                    + m_arg("definitions")
                    + m::text(", ")
                    + m_arg("variables")
                    + m::text(", or ")
                    + m_arg("units"),
            );

        if self.clear.is_some() {
            output += cmd("clear", [], [], "clear the current screen contents");
        }

        if self.session_history.is_some() {
            output += cmd_fmt(
                "save",
                [],
                [],
                m::text("save the current session history to ")
                    + m_arg("history.nbt")
                    + m::text(" in the current directory"),
            );
            output += cmd_fmt(
                "save",
                ["<dst>"],
                [],
                m::text("save the current session history to file ")
                    + m_arg("<dst>")
                    + m::text("; the recommended file extension is ")
                    + m::string(".nbt"),
            );
        }

        if self.reset.is_some() {
            output += cmd("reset", [], [], "reset the interpreter state");
        }

        if self.quit.is_some() {
            output += cmd("exit", [], ["quit"], "exit Numbat");
        }

        output
    }

    fn parse_command<'s, 'b>(
        &'s self,
        line: &'b str,
        ctx: &mut Context,
    ) -> Result<Option<ParsedCommand<'s, 'b>>, Box<CommandError>> {
        let Some(mut parser) = CommandParser::new(
            line,
            ctx.resolver_mut().add_code_source(CodeSource::Text, line),
        ) else {
            return Ok(None);
        };

        let Self {
            print_markup,
            clear,
            session_history,
            reset,
            quit,
        } = self;

        // todo: replace all of the initial `let Some(...) = ... else { return Ok(None) };`
        // with an if-let guard. https://github.com/rust-lang/rust/issues/51114
        Ok(Some(match &parser.command_kind {
            CommandKind::Help => {
                if print_markup.is_none() {
                    return Ok(None);
                }

                let help_arg = parser.args.next();

                if parser.args.next().is_some() {
                    return Err(Box::new(
                        parser
                            .err_through_end_from(2, "`help` takes at most one argument")
                            .into(),
                    ));
                }

                let help_kind = match help_arg {
                    None => HelpKind::BasicHelp,
                    Some("commands") => HelpKind::AllCommands,
                    _ => {
                        return Err(Box::new(
                            parser
                                .err_at_idx(
                                    1,
                                    "if provided, the argument to `help` must be \"commands\"",
                                )
                                .into(),
                        ));
                    }
                };

                ParsedCommand::Help { help_kind }
            }
            CommandKind::Info => {
                if print_markup.is_none() {
                    return Ok(None);
                }
                let err_msg = "`info` requires exactly one argument, the item to get info on";
                let Some(item) = parser.args.next() else {
                    return Err(Box::new(parser.err_at_idx(0, err_msg).into()));
                };

                if parser.args.next().is_some() {
                    return Err(Box::new(parser.err_through_end_from(1, err_msg).into()));
                }

                ParsedCommand::Info { item }
            }
            CommandKind::List => {
                if print_markup.is_none() {
                    return Ok(None);
                }

                let items = parser.args.next();

                if parser.args.next().is_some() {
                    return Err(Box::new(
                        parser
                            .err_through_end_from(2, "`list` takes at most one argument")
                            .into(),
                    ));
                }

                let items = match items {
                    None => None,
                    Some("functions") => Some(ListItems::Functions),
                    Some("dimensions") => Some(ListItems::Dimensions),
                    Some("variables") => Some(ListItems::Variables),
                    Some("units") => Some(ListItems::Units),
                    _ => {
                        return Err(Box::new(
                            parser
                                .err_at_idx(
                                    1,
                                    "if provided, the argument to `list` must be \
                                             one of: functions, dimensions, variables, units",
                                )
                                .into(),
                        ));
                    }
                };

                ParsedCommand::List { items }
            }
            CommandKind::Clear => {
                if clear.is_none() {
                    return Ok(None);
                }

                parser
                    .ensure_zero_args("clear", "")
                    .map_err(|err| Box::new(err.into()))?;

                ParsedCommand::Clear
            }
            CommandKind::Save => {
                let Some(session_history) = session_history else {
                    return Ok(None);
                };

                let dst = parser.args.next().unwrap_or("history.nbt");

                if parser.args.next().is_some() {
                    return Err(Box::new(
                        parser
                            .err_through_end_from(
                                2,
                                "`save` takes at most one argument (the destination, which will be history.nbt if omitted)",
                            )
                            .into(),
                    ));
                }

                ParsedCommand::Save {
                    session_history,
                    dst,
                }
            }
            CommandKind::Reset => {
                if reset.is_none() {
                    return Ok(None);
                }

                parser
                    .ensure_zero_args("reset", "")
                    .map_err(|err| Box::new(err.into()))?;

                ParsedCommand::Reset
            }
            CommandKind::Quit(quit_alias) => {
                if quit.is_none() {
                    return Ok(None);
                }

                parser
                    .ensure_zero_args(
                        match quit_alias {
                            QuitAlias::Quit => "quit",
                            QuitAlias::Exit => "exit",
                        },
                        "",
                    )
                    .map_err(|err| Box::new(err.into()))?;

                ParsedCommand::Quit
            }
        }))
    }

    /// Try to run the input line as a command.
    ///
    /// If the line is recognized as an (enabled) command, this handles the happy path,
    /// but it is up to frontends to handle the error path if this returns an error,
    /// which can happen because arguments to the command were incorrect (eg `list
    /// foobar`) or because the command failed at runtime (eg `save /`). If the command
    /// was recognized then this returns one of `CommandControlFlow::Continue` or
    /// `CommandControlFlow::Return`. If the line is not an enabled command (whether
    /// that's because it's not a command at all, or it's a disabled command), then this
    /// returns `CommandControlFlow::NotACommand`.
    pub fn try_run_command(
        &mut self,
        line: &str,
        ctx: &mut Context,
        editor: &mut Editor,
    ) -> Result<CommandControlFlow, Box<CommandError>> {
        let Some(parsed) = self.parse_command(line, ctx)? else {
            return Ok(CommandControlFlow::NotACommand);
        };

        Ok(match parsed {
            ParsedCommand::Help { help_kind } => {
                let markup = self.help_markup(help_kind);
                if let Some(print_fn) = self.print_markup.as_mut() {
                    print_fn(&markup);
                }
                CommandControlFlow::Continue
            }
            ParsedCommand::Info { item } => {
                let markup = ctx.print_info_for_keyword(item);
                if let Some(print_fn) = self.print_markup.as_mut() {
                    print_fn(&markup);
                }
                CommandControlFlow::Continue
            }
            ParsedCommand::List { items } => {
                let markup = match items {
                    None => ctx.print_environment(),
                    Some(ListItems::Functions) => ctx.print_functions(),
                    Some(ListItems::Dimensions) => ctx.print_dimensions(),
                    Some(ListItems::Variables) => ctx.print_variables(),
                    Some(ListItems::Units) => ctx.print_units(),
                };
                if let Some(print_fn) = self.print_markup.as_mut() {
                    print_fn(&markup);
                }
                CommandControlFlow::Continue
            }
            ParsedCommand::Clear => {
                if let Some(clear_fn) = self.clear.as_mut() {
                    clear_fn(editor)
                } else {
                    CommandControlFlow::Continue
                }
            }
            ParsedCommand::Save { session_history, dst } => {
                session_history.save(
                    dst,
                    SessionHistoryOptions {
                        include_err_lines: false,
                        trim_lines: true,
                    },
                ).map_err(|err| CommandError::Runtime(ctx.interpreter.runtime_error(*err)))?;

                if let Some(print_fn) = self.print_markup.as_mut() {
                    let markup = m::text("successfully saved session history to")
                        + m::space()
                        + m::string(dst.to_compact_string());
                    print_fn(&markup);
                }
                CommandControlFlow::Continue
            }
            ParsedCommand::Reset => {
                if let Some(clear_fn) = self.clear.as_mut() {
                    let _ = clear_fn(editor);
                }
                CommandControlFlow::Reset
            }
            ParsedCommand::Quit => CommandControlFlow::Return,
        })
    }
}

/// The command parser
///
/// This contains the words of the input and, for reporting errors, their boundaries'
/// indices and the input's code_source_id. The args of the input are not validated for
/// correctness until running  [`CommandRunner::try_run_line`].
///
/// This can only be successfully constructed if the first word of the input is a valid
/// command name.
pub struct CommandParser<'a> {
    command_kind: CommandKind,
    /// The words in the input, not including the command itself, which has already been
    /// parsed into `self.command_kind`.
    args: SplitWhitespace<'a>,
    /// For tracking spans. Contains `(start, start+len)` for each (whitespace-separated)
    /// word in the input
    word_boundaries: Vec<(u32, u32)>,
    /// The source id of the input
    code_source_id: usize,
}

impl<'a> CommandParser<'a> {
    /// Construct a new `CommandParser` from the input line and a `code_source_id`
    ///
    /// This breaks the input down into words and parses the first into a
    /// `command_kind`, but doesn't check the subsequent ones for correctness. For error
    /// reporting, it also stores the word boundaries and code_source_id.
    ///
    /// Returns `Some(_)` if the first word of the input is a valid command, `None`
    /// otherwise. This is not aware of commands enabled by a command runner; that check
    /// happens later.
    pub fn new(input: &'a str, code_source_id: usize) -> Option<Self> {
        let mut words: SplitWhitespace<'_> = input.split_whitespace();
        let command_kind = words.next()?.parse().ok()?;

        let mut word_boundaries = Vec::new();
        let mut prev_char_was_whitespace = true;
        let mut start_idx = 0;

        for (i, c) in input
            .char_indices()
            // force trailing whitespace to get last word
            .chain(std::iter::once((input.len(), ' ')))
        {
            if prev_char_was_whitespace && !c.is_whitespace() {
                start_idx = u32::try_from(i).unwrap();
            } else if !prev_char_was_whitespace && c.is_whitespace() {
                word_boundaries.push((start_idx, u32::try_from(i).unwrap()));
            }
            prev_char_was_whitespace = c.is_whitespace();
        }

        Some(Self {
            command_kind,
            args: words,
            word_boundaries,
            code_source_id,
        })
    }

    /// Get the span starting at the start of the word at `word_index`, through the end of
    /// the last word represented by `word_boundaries`
    ///
    /// ## Panics
    /// If `word_index` is out of bounds, ie `word_index >= word_boundaries.len()`
    fn span_through_end(&self, word_index: usize) -> Span {
        let start = self.word_boundaries[word_index].0;
        let end = self.word_boundaries.last().unwrap().1;
        self.span_from_boundary((start, end))
    }

    /// Get the span between indices given by `start` and `end`
    ///
    /// The only role of `&self` here is to provide the `code_source_id`
    fn span_from_boundary(&self, (start, end): (u32, u32)) -> Span {
        Span {
            start: ByteIndex(start),
            end: ByteIndex(end),
            code_source_id: self.code_source_id,
        }
    }

    fn err_at_idx(&self, index: usize, err_msg: impl Into<String>) -> ParseError {
        ParseError {
            kind: ParseErrorKind::InvalidCommand(err_msg.into()),
            span: self.span_from_boundary(self.word_boundaries[index]),
        }
    }

    fn err_through_end_from(&self, index: usize, err_msg: impl Into<String>) -> ParseError {
        ParseError {
            kind: ParseErrorKind::InvalidCommand(err_msg.into()),
            span: self.span_through_end(index),
        }
    }

    fn ensure_zero_args(
        &mut self,
        cmd: &'static str,
        err_msg_suffix: &'static str,
    ) -> Result<(), ParseError> {
        if self.args.next().is_some() {
            let message = format!("`{}` takes 0 arguments{}", cmd, err_msg_suffix);
            return Err(self.err_through_end_from(1, message));
        }
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[derive(Debug, Clone, PartialEq)]
    pub enum BareCommand<'a> {
        Help { help_kind: HelpKind },
        Info { item: &'a str },
        List { items: Option<ListItems> },
        Clear,
        Save { dst: &'a str },
        Reset,
        Quit,
    }

    impl<'a> From<ParsedCommand<'_, 'a>> for BareCommand<'a> {
        fn from(cmd: ParsedCommand<'_, 'a>) -> Self {
            match cmd {
                ParsedCommand::Help { help_kind } => BareCommand::Help { help_kind },
                ParsedCommand::Info { item } => BareCommand::Info { item },
                ParsedCommand::List { items } => BareCommand::List { items },
                ParsedCommand::Clear => BareCommand::Clear,
                ParsedCommand::Save { dst, .. } => BareCommand::Save { dst },
                ParsedCommand::Reset => BareCommand::Reset,
                ParsedCommand::Quit => BareCommand::Quit,
            }
        }
    }

    fn new_runner() -> CommandRunner<'static, ()> {
        CommandRunner::new()
            .print_with(|_| {})
            .enable_clear(|_| CommandControlFlow::Continue)
            .enable_save(SessionHistory::new())
            .enable_reset()
            .enable_quit()
    }

    fn parser(input: &'static str) -> Option<CommandParser<'static>> {
        CommandParser::new(input, 0)
    }

    #[track_caller]
    fn expect_word_boundaries(input: &'static str) -> Vec<(u32, u32)> {
        parser(input).unwrap().word_boundaries
    }

    #[track_caller]
    fn expect_ok(
        runner: &CommandRunner<()>,
        ctx: &mut Context,
        input: &'static str,
        expected: BareCommand,
    ) {
        let cmd = runner.parse_command(input, ctx).unwrap().unwrap();
        assert_eq!(expected, cmd.into());
    }

    #[track_caller]
    fn expect_fail(runner: &CommandRunner<()>, ctx: &mut Context, input: &'static str) {
        assert!(runner.parse_command(input, ctx).is_err());
    }

    #[test]
    fn test_command_parser() {
        assert!(parser("").is_none());
        assert!(parser(" ").is_none());
        assert!(parser("  ").is_none());

        assert!(parser("x").is_none());
        assert!(parser("x ").is_none());
        assert!(parser(" x").is_none());
        assert!(parser(" x ").is_none());

        assert!(parser("xyz").is_none());
        assert!(parser("xyz  ").is_none());
        assert!(parser("  xyz").is_none());
        assert!(parser("  xyz  ").is_none());

        assert!(parser("abc x").is_none(),);
        assert!(parser("abc  x ").is_none(),);
        assert!(parser(" abc   x").is_none());
        assert!(parser("  abc   x  ").is_none());

        assert_eq!(expect_word_boundaries("list"), [(0, 4)]);
        assert_eq!(expect_word_boundaries("list "), [(0, 4)]);
        assert_eq!(expect_word_boundaries(" list"), [(1, 5)]);
        assert_eq!(expect_word_boundaries(" list "), [(1, 5)]);

        assert_eq!(expect_word_boundaries("list   ab"), [(0, 4), (7, 9)]);
        assert_eq!(expect_word_boundaries("list   ab "), [(0, 4), (7, 9)]);
        assert_eq!(expect_word_boundaries(" list   ab"), [(1, 5), (8, 10)]);
        assert_eq!(expect_word_boundaries(" list   ab "), [(1, 5), (8, 10)]);

        assert_eq!(
            expect_word_boundaries("list   ab xy"),
            [(0, 4), (7, 9), (10, 12)]
        );
        assert_eq!(
            expect_word_boundaries("list   ab   xy "),
            [(0, 4), (7, 9), (12, 14)]
        );
        assert_eq!(
            expect_word_boundaries("   list   ab    xy"),
            [(3, 7), (10, 12), (16, 18)]
        );
        assert_eq!(
            expect_word_boundaries("   list   ab    xy   "),
            [(3, 7), (10, 12), (16, 18)]
        );
    }

    #[test]
    fn test_existent_commands() {
        // these shouldn't happen at runtime because the REPL skips over all
        // whitespace lines, but we still want to handle them just in case
        assert!(parser("").is_none());
        assert!(parser(" ").is_none());

        // valid commands
        assert!(parser("help").is_some());
        assert!(parser("help arg").is_some());
        assert!(parser("help arg1 arg2").is_some());

        assert!(parser("info").is_some());
        assert!(parser("info arg").is_some());
        assert!(parser("info arg1 arg2").is_some());

        assert!(parser("clear").is_some());
        assert!(parser("clear arg").is_some());
        assert!(parser("clear arg1 arg2").is_some());

        assert!(parser("list").is_some());
        assert!(parser("list arg").is_some());
        assert!(parser("list arg1 arg2").is_some());

        assert!(parser("quit").is_some());
        assert!(parser("quit arg").is_some());
        assert!(parser("quit arg1 arg2").is_some());

        assert!(parser("exit").is_some());
        assert!(parser("exit arg").is_some());
        assert!(parser("exit arg1 arg2").is_some());

        assert!(parser("save").is_some());
        assert!(parser("save arg").is_some());
        assert!(parser("save arg1 arg2").is_some());

        assert!(parser("reset").is_some());
        assert!(parser("reset arg").is_some());
        assert!(parser("reset arg1 arg2").is_some());

        // invalid (nonempty) command names are all None so that parsing can continue on
        // what is presumably a math expression. case matters
        assert!(parser(".").is_none());
        assert!(parser(",").is_none());
        assert!(parser(";").is_none());
        assert!(parser("ls").is_none());
        assert!(parser("HELP").is_none());
        assert!(parser("List xyz").is_none());
        assert!(parser("qUIt abc").is_none());
        assert!(parser("listfunctions").is_none());
        assert!(parser("exitquit").is_none());
    }

    #[test]
    fn test_whitespace() {
        let mut ctx = Context::new_without_importer();
        let runner = new_runner();

        expect_ok(&runner, &mut ctx, "list", BareCommand::List { items: None });
        expect_ok(
            &runner,
            &mut ctx,
            " list",
            BareCommand::List { items: None },
        );
        expect_ok(
            &runner,
            &mut ctx,
            "list ",
            BareCommand::List { items: None },
        );
        expect_ok(
            &runner,
            &mut ctx,
            " list ",
            BareCommand::List { items: None },
        );
        expect_ok(
            &runner,
            &mut ctx,
            "list functions  ",
            BareCommand::List {
                items: Some(ListItems::Functions),
            },
        );
        expect_ok(
            &runner,
            &mut ctx,
            "  list    functions  ",
            BareCommand::List {
                items: Some(ListItems::Functions),
            },
        );
        expect_ok(
            &runner,
            &mut ctx,
            "  list    functions  ",
            BareCommand::List {
                items: Some(ListItems::Functions),
            },
        );
        expect_ok(
            &runner,
            &mut ctx,
            "list    functions",
            BareCommand::List {
                items: Some(ListItems::Functions),
            },
        );
    }

    #[test]
    fn test_args() {
        let mut ctx = Context::new_without_importer();
        let runner = new_runner();

        expect_ok(
            &runner,
            &mut ctx,
            "help",
            BareCommand::Help {
                help_kind: HelpKind::BasicHelp,
            },
        );
        expect_ok(
            &runner,
            &mut ctx,
            "help commands",
            BareCommand::Help {
                help_kind: HelpKind::AllCommands,
            },
        );
        expect_fail(&runner, &mut ctx, "help arg");
        expect_fail(&runner, &mut ctx, "help arg1 arg2");

        expect_fail(&runner, &mut ctx, "info");
        expect_ok(
            &runner,
            &mut ctx,
            "info arg",
            BareCommand::Info { item: "arg" },
        );
        expect_ok(&runner, &mut ctx, "info .", BareCommand::Info { item: "." });
        expect_fail(&runner, &mut ctx, "info arg1 arg2");

        expect_ok(&runner, &mut ctx, "clear", BareCommand::Clear);
        expect_fail(&runner, &mut ctx, "clear arg");
        expect_fail(&runner, &mut ctx, "clear arg1 arg2");

        expect_ok(&runner, &mut ctx, "list", BareCommand::List { items: None });
        expect_ok(
            &runner,
            &mut ctx,
            "list functions",
            BareCommand::List {
                items: Some(ListItems::Functions),
            },
        );
        expect_ok(
            &runner,
            &mut ctx,
            "list dimensions",
            BareCommand::List {
                items: Some(ListItems::Dimensions),
            },
        );
        expect_ok(
            &runner,
            &mut ctx,
            "list variables",
            BareCommand::List {
                items: Some(ListItems::Variables),
            },
        );
        expect_ok(
            &runner,
            &mut ctx,
            "list units",
            BareCommand::List {
                items: Some(ListItems::Units),
            },
        );

        expect_ok(&runner, &mut ctx, "reset", BareCommand::Reset);
        expect_fail(&runner, &mut ctx, "reset arg");
        expect_fail(&runner, &mut ctx, "reset arg1 arg2");

        expect_ok(&runner, &mut ctx, "quit", BareCommand::Quit);
        expect_fail(&runner, &mut ctx, "quit arg");
        expect_fail(&runner, &mut ctx, "quit arg1 arg2");

        expect_ok(&runner, &mut ctx, "exit", BareCommand::Quit);
        expect_fail(&runner, &mut ctx, "exit arg");
        expect_fail(&runner, &mut ctx, "exit arg1 arg2");

        expect_ok(
            &runner,
            &mut ctx,
            "save",
            BareCommand::Save { dst: "history.nbt" },
        );
        expect_ok(
            &runner,
            &mut ctx,
            "save arg",
            BareCommand::Save { dst: "arg" },
        );
        expect_ok(&runner, &mut ctx, "save .", BareCommand::Save { dst: "." });
        expect_fail(&runner, &mut ctx, "save arg1 arg2");
    }

    #[test]
    fn test_runner() {
        fn test_case(
            runner: &mut CommandRunner<()>,
            ctx: &mut Context,
            input: &'static str,
            expected: CommandControlFlow,
        ) {
            let editor = &mut ();
            assert_eq!(
                expected,
                runner.try_run_command(input, ctx, editor).unwrap()
            );
        }

        let mut ctx = Context::new_without_importer();

        let mut runner = CommandRunner::new().print_with(|_| {}).enable_quit();

        test_case(&mut runner, &mut ctx, "help", CommandControlFlow::Continue);
        test_case(&mut runner, &mut ctx, "list", CommandControlFlow::Continue);
        test_case(
            &mut runner,
            &mut ctx,
            // won't be found, but that's fine
            "info item",
            CommandControlFlow::Continue,
        );
        test_case(&mut runner, &mut ctx, "clear", CommandControlFlow::NotACommand);
        test_case(
            &mut runner,
            &mut ctx,
            "save dst",
            CommandControlFlow::NotACommand,
        );
        test_case(&mut runner, &mut ctx, "quit", CommandControlFlow::Return);
        test_case(&mut runner, &mut ctx, "exit", CommandControlFlow::Return);
    }
}
