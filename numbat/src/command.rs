use std::str::{FromStr, SplitWhitespace};

use compact_str::ToCompactString;

use crate::{
    diagnostic::ErrorDiagnostic,
    help::help_markup,
    markup::{self as m, Markup},
    parser::ParseErrorKind,
    resolver::CodeSource,
    session_history::{SessionHistory, SessionHistoryOptions},
    span::{ByteIndex, Span},
    Context, ParseError, RuntimeError,
};

#[derive(Debug, Clone, PartialEq)]
pub enum ListItems {
    Functions,
    Dimensions,
    Variables,
    Units,
}

enum QuitAlias {
    Quit,
    Exit,
}

enum CommandKind {
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

#[derive(Debug, PartialEq, Eq)]
#[must_use]
pub enum CommandControlFlow {
    Continue,
    Return,
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

impl ErrorDiagnostic for CommandError {
    fn diagnostics(&self) -> Vec<crate::Diagnostic> {
        match self {
            CommandError::Parse(parse_error) => parse_error.diagnostics(),
            CommandError::Runtime(runtime_error) => runtime_error.diagnostics(),
        }
    }
}

enum Command<'session, 'input, Editor> {
    Help {
        print_fn: fn(&Markup),
    },
    Info {
        item: &'input str,
        print_fn: fn(&Markup),
    },
    List {
        items: Option<ListItems>,
        print_fn: fn(&Markup),
    },
    Clear {
        clear_fn: fn(&mut Editor) -> CommandControlFlow,
    },
    Save(SaveCmdArgs<'session, 'input>),
    Reset {
        ctx_ctor: fn() -> Context,
        clear_fn: Option<fn(&mut Editor) -> CommandControlFlow>,
    },
    Quit,
}

struct SaveCmdArgs<'session, 'input> {
    session_history: &'session SessionHistory,
    dst: &'input str,
    print_fn: Option<fn(&Markup)>,
}

impl SaveCmdArgs<'_, '_> {
    fn save(&self) -> Result<(), Box<RuntimeError>> {
        let Self {
            session_history,
            dst,
            print_fn,
        } = self;

        session_history.save(
            dst,
            SessionHistoryOptions {
                include_err_lines: false,
                trim_lines: true,
            },
        )?;

        if let Some(print_markup) = print_fn {
            let markup = m::text("successfully saved session history to")
                + m::space()
                + m::string(dst.to_compact_string());
            print_markup(&markup)
        }

        Ok(())
    }
}

pub struct CommandRunner<Editor = ()> {
    print_markup: Option<fn(&Markup)>,
    clear: Option<fn(&mut Editor) -> CommandControlFlow>,
    session_history: Option<SessionHistory>,
    ctx_ctor: Option<fn() -> Context>,
    quit: Option<()>,
}

// cannot be derived because `#[derive(Default)]` introduces the bound `Editor:
// Default`, which is not necessary
impl<Editor> Default for CommandRunner<Editor> {
    fn default() -> Self {
        Self {
            print_markup: None,
            clear: None,
            session_history: None,
            ctx_ctor: None,
            quit: None,
        }
    }
}

impl<Editor> CommandRunner<Editor> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn print_with(mut self, action: fn(&Markup)) -> Self {
        self.print_markup = Some(action);
        self
    }

    pub fn enable_clear(mut self, action: fn(&mut Editor) -> CommandControlFlow) -> Self {
        self.clear = Some(action);
        self
    }

    pub fn enable_save(mut self, session_history: SessionHistory) -> Self {
        self.session_history = Some(session_history);
        self
    }

    pub fn enable_reset(mut self, ctx_ctor: fn() -> Context) -> Self {
        self.ctx_ctor = Some(ctx_ctor);
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

    fn get_command<'a, 'b>(
        &'a self,
        line: &'b str,
        ctx: &mut Context,
    ) -> Result<Option<Command<'a, 'b, Editor>>, Box<CommandError>> {
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
            ctx_ctor,
            quit,
        } = self;

        // todo: replace all of the initial `let Some(...) = ... else { return Ok(None) };`
        // with an if-let guard. https://github.com/rust-lang/rust/issues/51114
        Ok(Some(match &parser.command_kind {
            CommandKind::Help => {
                let &Some(print_fn) = print_markup else {
                    return Ok(None);
                };

                parser
                    .ensure_zero_args("help", "; use `info <item>` for information about an item")
                    .map_err(|err| Box::new(err.into()))?;

                Command::Help { print_fn }
            }
            CommandKind::Info => {
                let &Some(print_fn) = print_markup else {
                    return Ok(None);
                };
                let err_msg = "`info` requires exactly one argument, the item to get info on";
                let Some(item) = parser.args.next() else {
                    return Err(Box::new(parser.err_at_idx(0, err_msg).into()));
                };

                if parser.args.next().is_some() {
                    return Err(Box::new(parser.err_through_end_from(1, err_msg).into()));
                }

                Command::Info { item, print_fn }
            }
            CommandKind::List => {
                let &Some(print_fn) = print_markup else {
                    return Ok(None);
                };

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

                Command::List { items, print_fn }
            }
            CommandKind::Clear => {
                let &Some(clear_fn) = clear else {
                    return Ok(None);
                };

                parser
                    .ensure_zero_args("clear", "")
                    .map_err(|err| Box::new(err.into()))?;

                Command::Clear { clear_fn }
            }
            CommandKind::Save => {
                let Some(session_history) = session_history else {
                    return Ok(None);
                };

                let print_fn = print_markup.as_ref().copied();

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

                Command::Save(SaveCmdArgs {
                    session_history,
                    dst,
                    print_fn,
                })
            }
            CommandKind::Reset => {
                let &Some(ctx_ctor) = ctx_ctor else {
                    return Ok(None);
                };

                parser
                    .ensure_zero_args("reset", "")
                    .map_err(|err| Box::new(err.into()))?;

                Command::Reset {
                    ctx_ctor,
                    clear_fn: *clear,
                }
            }
            CommandKind::Quit(quit_alias) => {
                let Some(_) = quit else {
                    return Ok(None);
                };

                parser
                    .ensure_zero_args(
                        match quit_alias {
                            QuitAlias::Quit => "quit",
                            QuitAlias::Exit => "exit",
                        },
                        "",
                    )
                    .map_err(|err| Box::new(err.into()))?;

                Command::Quit
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
        &self,
        line: &str,
        ctx: &mut Context,
        editor: &mut Editor,
    ) -> Result<CommandControlFlow, Box<CommandError>> {
        let Some(output) = self.get_command(line, ctx)? else {
            return Ok(CommandControlFlow::NotACommand);
        };

        Ok(match output {
            Command::Help { print_fn } => {
                print_fn(&help_markup());
                CommandControlFlow::Continue
            }
            Command::Info { item, print_fn } => {
                print_fn(&ctx.print_info_for_keyword(item));
                CommandControlFlow::Continue
            }
            Command::List { items, print_fn } => {
                let markup = match items {
                    None => ctx.print_environment(),
                    Some(ListItems::Functions) => ctx.print_functions(),
                    Some(ListItems::Dimensions) => ctx.print_dimensions(),
                    Some(ListItems::Variables) => ctx.print_variables(),
                    Some(ListItems::Units) => ctx.print_units(),
                };
                print_fn(&markup);
                CommandControlFlow::Continue
            }
            Command::Clear { clear_fn } => clear_fn(editor),
            Command::Save(save_args) => {
                save_args.save().map_err(|err| Box::new((*err).into()))?;
                CommandControlFlow::Continue
            }
            Command::Reset { ctx_ctor, clear_fn } => {
                *ctx = ctx_ctor();
                match clear_fn {
                    Some(clear_fn) => clear_fn(editor),
                    None => CommandControlFlow::Continue,
                }
            }
            Command::Quit => CommandControlFlow::Return,
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
        Help,
        Info { item: &'a str },
        List { items: Option<ListItems> },
        Clear,
        Save { dst: &'a str },
        Reset,
        Quit,
    }

    impl<'b, Editor> Command<'_, 'b, Editor> {
        fn into_bare(self) -> BareCommand<'b> {
            match self {
                Command::Help { print_fn: _ } => BareCommand::Help,
                Command::Info { item, .. } => BareCommand::Info { item },
                Command::List { items, .. } => BareCommand::List { items },
                Command::Clear { clear_fn: _ } => BareCommand::Clear,
                Command::Save(SaveCmdArgs { dst, .. }) => BareCommand::Save { dst },
                Command::Reset { .. } => BareCommand::Reset,
                Command::Quit => BareCommand::Quit,
            }
        }
    }

    fn new_runner() -> CommandRunner<()> {
        CommandRunner::new()
            .print_with(|_| {})
            .enable_clear(|_| CommandControlFlow::Continue)
            .enable_save(SessionHistory::new())
            .enable_reset(Context::new_without_importer)
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
        let cmd = runner.get_command(input, ctx).unwrap().unwrap();
        assert_eq!(expected, cmd.into_bare());
    }

    #[track_caller]
    fn expect_fail(runner: &CommandRunner<()>, ctx: &mut Context, input: &'static str) {
        assert!(runner.get_command(input, ctx).is_err());
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

        expect_ok(&runner, &mut ctx, "help", BareCommand::Help);
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
            runner: &CommandRunner<()>,
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

        let runner = CommandRunner::new().print_with(|_| {}).enable_quit();

        test_case(&runner, &mut ctx, "help", CommandControlFlow::Continue);
        test_case(&runner, &mut ctx, "list", CommandControlFlow::Continue);
        test_case(
            &runner,
            &mut ctx,
            // won't be found, but that's fine
            "info item",
            CommandControlFlow::Continue,
        );
        test_case(&runner, &mut ctx, "clear", CommandControlFlow::NotACommand);
        test_case(
            &runner,
            &mut ctx,
            "save dst",
            CommandControlFlow::NotACommand,
        );
        test_case(&runner, &mut ctx, "quit", CommandControlFlow::Return);
        test_case(&runner, &mut ctx, "exit", CommandControlFlow::Return);
    }
}
