#![cfg_attr(test, allow(unused_variables))]

use std::{
    ops::DerefMut,
    str::{FromStr, SplitWhitespace},
};

use compact_str::ToCompactString;

#[cfg(test)]
use crate::RuntimeError;
#[cfg(not(test))]
use crate::{help::help_markup, session_history::SessionHistoryOptions};

use crate::{
    markup::{self as m, Markup},
    parser::ParseErrorKind,
    resolver::CodeSource,
    session_history::SessionHistory,
    span::{ByteIndex, Span},
    Context, ParseError,
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
            "quit" => Quit(QuitAlias::Quit),
            "exit" => Quit(QuitAlias::Exit),
            _ => return Err(()),
        })
    }
}

#[derive(Debug, Default, PartialEq, Eq)]
#[must_use]
pub enum CommandControlFlow {
    #[default]
    Continue,
    Return,
    NotACommand,
}

pub struct CommandContext<'editor, ContextMut, Editor> {
    pub ctx: ContextMut,
    pub editor: &'editor mut Editor,
}

pub struct CommandRunner<Editor> {
    print_markup: Option<fn(&Markup)>,
    clear: Option<fn(&mut Editor) -> CommandControlFlow>,
    session_history: Option<SessionHistory>,
    quit: Option<()>,
}

impl<Editor> CommandRunner<Editor> {
    pub fn new_all_disabled() -> Self {
        Self {
            print_markup: None,
            clear: None,
            session_history: None,
            quit: None,
        }
    }

    pub fn enable_print_markup(mut self, action: fn(&Markup)) -> Self {
        #[cfg(not(test))]
        {
            self.print_markup = Some(action);
        }
        #[cfg(test)]
        {
            self.print_markup = Some(|_| {});
        }
        self
    }

    pub fn enable_clear(mut self, action: fn(&mut Editor) -> CommandControlFlow) -> Self {
        #[cfg(not(test))]
        {
            self.clear = Some(action);
        }
        #[cfg(test)]
        {
            self.clear = Some(|_| CommandControlFlow::Continue);
        }
        self
    }

    pub fn enable_save(mut self, session_history: SessionHistory) -> Self {
        self.session_history = Some(session_history);
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

    /// Try to run the input line as a command.
    ///
    /// If the line is recognized as an (enabled) command, this handles everything that
    /// needs to be done, including printing markup or error messages, and simply
    /// returns a `CommandControlFlow`, returning one of `CommandControlFlow::Continue`
    /// or `CommandControlFlow::Return`. If the line is not an enabled command, then
    /// returns `CommandControlFlow::NotACommand`.
    pub fn try_run_line<ContextMut: DerefMut<Target = Context>>(
        &self,
        line: &str,
        args: CommandContext<ContextMut, Editor>,
    ) -> CommandControlFlow {
        let (cf, _) = self.try_run_line_internal(line, args);
        cf
    }

    /// Try to run the input line as a command. See [`Self::try_run_line`] for details.
    ///
    /// Returns both the control flow and the parsed command if it exists. Only used for
    /// testing; for production we only need the `CommandControlFlow`.
    fn try_run_line_internal<'a, ContextMut: DerefMut<Target = Context>>(
        &self,
        line: &'a str,
        args: CommandContext<ContextMut, Editor>,
    ) -> (CommandControlFlow, Option<Command<'a>>) {
        macro_rules! ensure_enabled {
            ($ident:ident, $if_some:tt) => {
                match $ident {
                    Some($ident) => $if_some,
                    None => return (CommandControlFlow::NotACommand, None),
                }
            };
            ($ident:ident: _, $if_some:tt) => {
                match $ident {
                    Some(_) => $if_some,
                    None => return (CommandControlFlow::NotACommand, None),
                }
            };
        }

        macro_rules! validate {
            ($ctx:expr, $ex:expr) => {
                match $ex {
                    Ok(ok) => ok,
                    Err(err) => {
                        #[cfg(not(test))]
                        {
                            $ctx.print_diagnostic(err);
                        }
                        return (CommandControlFlow::Continue, None);
                    }
                }
            };
        }

        let CommandContext { mut ctx, editor } = args;

        let ctx = &mut *ctx;

        let Some(mut parser) = CommandParser::new(
            line,
            ctx.resolver_mut().add_code_source(CodeSource::Text, line),
        ) else {
            return (CommandControlFlow::NotACommand, None);
        };

        let Self {
            clear,
            session_history,
            quit,
            print_markup,
        } = self;

        // ideally we could use if-let match guards, eg:
        // `CommandKind::Help if let Some(print_markup) = print_markup {...}`
        // and then at the bottom have a catch-all:
        // `_ => return CommandControlFlow::NotACommand`
        // but this syntax is currently experimental
        match &parser.command_kind {
            CommandKind::Help => ensure_enabled!(print_markup, {
                validate!(
                    ctx,
                    parser.ensure_zero_args(
                        "help",
                        "; use `info <item>` for information about an item",
                    )
                );

                #[cfg(not(test))]
                {
                    print_markup(&help_markup());
                }

                (CommandControlFlow::Continue, Some(Command::Help))
            }),
            CommandKind::Info => ensure_enabled!(print_markup, {
                let item = validate!(
                    ctx,
                    (|| {
                        let err_msg =
                            "`info` requires exactly one argument, the item to get info on";
                        let Some(item) = parser.args.next() else {
                            return Err(parser.err_at_idx(0, err_msg));
                        };

                        if parser.args.next().is_some() {
                            return Err(parser.err_through_end_from(1, err_msg));
                        }

                        Ok(item)
                    })()
                );
                #[cfg(not(test))]
                {
                    print_markup(&ctx.print_info_for_keyword(item));
                }

                (CommandControlFlow::Continue, Some(Command::Info { item }))
            }),
            CommandKind::List => ensure_enabled!(print_markup, {
                let items = validate!(
                    ctx,
                    (|| {
                        let items = parser.args.next();

                        if parser.args.next().is_some() {
                            return Err(
                                parser.err_through_end_from(2, "`list` takes at most one argument")
                            );
                        }

                        let items = match items {
                            None => None,
                            Some("functions") => Some(ListItems::Functions),
                            Some("dimensions") => Some(ListItems::Dimensions),
                            Some("variables") => Some(ListItems::Variables),
                            Some("units") => Some(ListItems::Units),
                            _ => {
                                return Err(parser.err_at_idx(
                                    1,
                                    "if provided, the argument to `list` must be \
                            one of: functions, dimensions, variables, units",
                                ));
                            }
                        };

                        Ok(items)
                    })()
                );

                let markup = match items {
                    None => ctx.print_environment(),
                    Some(ListItems::Functions) => ctx.print_functions(),
                    Some(ListItems::Dimensions) => ctx.print_dimensions(),
                    Some(ListItems::Variables) => ctx.print_variables(),
                    Some(ListItems::Units) => ctx.print_units(),
                };
                #[cfg(not(test))]
                {
                    print_markup(&markup);
                }

                (CommandControlFlow::Continue, Some(Command::List { items }))
            }),
            CommandKind::Clear => ensure_enabled!(clear, {
                validate!(ctx, parser.ensure_zero_args("clear", ""));
                (clear(editor), Some(Command::Clear))
            }),
            CommandKind::Save => ensure_enabled!(session_history, {
                let dst = validate!(
                    ctx,
                    (|| {
                        let Some(dst) = parser.args.next() else {
                            return Ok("history.nbt");
                        };

                        if parser.args.next().is_some() {
                            return Err(parser.err_through_end_from(
                                2,
                                "`save` requires exactly one argument, the destination",
                            ));
                        }

                        Ok(dst)
                    })()
                );

                #[cfg(not(test))]
                let save_result = session_history.save(
                    dst,
                    SessionHistoryOptions {
                        include_err_lines: false,
                        trim_lines: true,
                    },
                );

                #[cfg(test)]
                let save_result = Ok::<_, Box<RuntimeError>>(());

                match save_result {
                    Ok(_) => {
                        if let Some(print_markup) = print_markup {
                            let markup = m::text("successfully saved session history to")
                                + m::space()
                                + m::string(dst.to_compact_string());
                            print_markup(&markup)
                        }
                    }
                    Err(err) => {
                        ctx.print_diagnostic(*err);
                    }
                };
                (CommandControlFlow::Continue, Some(Command::Save { dst }))
            }),
            CommandKind::Quit(quit_alias) => ensure_enabled!(quit: _, {
                validate!(
                    ctx,
                    parser.ensure_zero_args(
                        match quit_alias {
                            QuitAlias::Quit => "quit",
                            QuitAlias::Exit => "exit",
                        },
                        "",
                    )
                );
                (CommandControlFlow::Return, Some(Command::Quit))
            }),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Command<'a> {
    Help,
    Info { item: &'a str },
    List { items: Option<ListItems> },
    Clear,
    Save { dst: &'a str },
    Quit,
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

    macro_rules! args {
        ($ctx:expr) => {{
            CommandContext {
                ctx: &mut $ctx,
                editor: &mut (),
            }
        }};
    }

    fn new_runner() -> CommandRunner<()> {
        CommandRunner::new_all_disabled()
            .enable_print_markup(|_| {})
            .enable_clear(|_| CommandControlFlow::Continue)
            .enable_save(SessionHistory::new())
            .enable_quit()
    }

    fn parser(input: &'static str) -> Option<CommandParser<'static>> {
        CommandParser::new(input, 0)
    }

    fn parser_uw(input: &'static str) -> CommandParser<'static> {
        parser(input).unwrap()
    }

    fn expect_command_ok(
        runner: &CommandRunner<()>,
        args: CommandContext<&mut Context, ()>,
        input: &'static str,
        expected: Command,
    ) {
        let (cf, cmd) = runner.try_run_line_internal(input, args);

        assert_eq!(cf, CommandControlFlow::Continue);
        assert_eq!(expected, cmd.unwrap());
    }

    fn expect_command_return(
        runner: &CommandRunner<()>,
        args: CommandContext<&mut Context, ()>,
        input: &'static str,
        expected: Command,
    ) {
        let (cf, cmd) = runner.try_run_line_internal(input, args);

        assert_eq!(cf, CommandControlFlow::Return);
        assert_eq!(expected, cmd.unwrap());
    }

    fn expect_command_fail(
        runner: &CommandRunner<()>,
        args: CommandContext<&mut Context, ()>,
        input: &'static str,
    ) {
        let (cf, cmd) = runner.try_run_line_internal(input, args);

        assert_eq!(cf, CommandControlFlow::Continue);
        assert_eq!(None, cmd);
    }

    // fn parse(input: &'static str) -> Result<Command, ParseError> {
    //     parser(input).unwrap().parse_command()
    // }

    // fn parse_uw(input: &'static str) -> Command {
    //     parse(input).unwrap()
    // }

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

        assert_eq!(&parser_uw("list").word_boundaries, &[(0, 4)]);
        assert_eq!(&parser_uw("list ").word_boundaries, &[(0, 4)]);
        assert_eq!(&parser_uw(" list").word_boundaries, &[(1, 5)]);
        assert_eq!(&parser_uw(" list ").word_boundaries, &[(1, 5)]);

        assert_eq!(&parser_uw("list   ab").word_boundaries, &[(0, 4), (7, 9)]);
        assert_eq!(&parser_uw("list   ab ").word_boundaries, &[(0, 4), (7, 9)]);
        assert_eq!(&parser_uw(" list   ab").word_boundaries, &[(1, 5), (8, 10)]);
        assert_eq!(
            &parser_uw(" list   ab ").word_boundaries,
            &[(1, 5), (8, 10)]
        );

        assert_eq!(
            &parser_uw("list   ab xy").word_boundaries,
            &[(0, 4), (7, 9), (10, 12)]
        );
        assert_eq!(
            &parser_uw("list   ab   xy ").word_boundaries,
            &[(0, 4), (7, 9), (12, 14)]
        );
        assert_eq!(
            parser_uw("   list   ab    xy").word_boundaries,
            &[(3, 7), (10, 12), (16, 18)]
        );
        assert_eq!(
            parser_uw("   list   ab    xy   ").word_boundaries,
            &[(3, 7), (10, 12), (16, 18)]
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

        expect_command_ok(&runner, args!(ctx), "list", Command::List { items: None });
        expect_command_ok(&runner, args!(ctx), " list", Command::List { items: None });
        expect_command_ok(&runner, args!(ctx), "list ", Command::List { items: None });
        expect_command_ok(&runner, args!(ctx), " list ", Command::List { items: None });
        expect_command_ok(
            &runner,
            args!(ctx),
            "list functions  ",
            Command::List {
                items: Some(ListItems::Functions),
            },
        );
        expect_command_ok(
            &runner,
            args!(ctx),
            "  list    functions  ",
            Command::List {
                items: Some(ListItems::Functions),
            },
        );
        expect_command_ok(
            &runner,
            args!(ctx),
            "  list    functions  ",
            Command::List {
                items: Some(ListItems::Functions),
            },
        );
        expect_command_ok(
            &runner,
            args!(ctx),
            "list    functions",
            Command::List {
                items: Some(ListItems::Functions),
            },
        );
    }

    #[test]
    fn test_args() {
        let mut ctx = Context::new_without_importer();
        let runner = new_runner();

        expect_command_ok(&runner, args!(ctx), "help", Command::Help);
        expect_command_fail(&runner, args!(ctx), "help arg");
        expect_command_fail(&runner, args!(ctx), "help arg1 arg2");

        expect_command_fail(&runner, args!(ctx), "info");
        expect_command_ok(
            &runner,
            args!(ctx),
            "info arg",
            Command::Info { item: "arg" },
        );
        expect_command_ok(&runner, args!(ctx), "info .", Command::Info { item: "." });
        expect_command_fail(&runner, args!(ctx), "info arg1 arg2");

        expect_command_ok(&runner, args!(ctx), "clear", Command::Clear);
        expect_command_fail(&runner, args!(ctx), "clear arg");
        expect_command_fail(&runner, args!(ctx), "clear arg1 arg2");

        expect_command_ok(&runner, args!(ctx), "list", Command::List { items: None });
        expect_command_ok(
            &runner,
            args!(ctx),
            "list functions",
            Command::List {
                items: Some(ListItems::Functions),
            },
        );
        expect_command_ok(
            &runner,
            args!(ctx),
            "list dimensions",
            Command::List {
                items: Some(ListItems::Dimensions),
            },
        );
        expect_command_ok(
            &runner,
            args!(ctx),
            "list variables",
            Command::List {
                items: Some(ListItems::Variables),
            },
        );
        expect_command_ok(
            &runner,
            args!(ctx),
            "list units",
            Command::List {
                items: Some(ListItems::Units),
            },
        );

        expect_command_return(&runner, args!(ctx), "quit", Command::Quit);
        expect_command_fail(&runner, args!(ctx), "quit arg");
        expect_command_fail(&runner, args!(ctx), "quit arg1 arg2");

        expect_command_return(&runner, args!(ctx), "exit", Command::Quit);
        expect_command_fail(&runner, args!(ctx), "exit arg");
        expect_command_fail(&runner, args!(ctx), "exit arg1 arg2");

        expect_command_ok(
            &runner,
            args!(ctx),
            "save",
            Command::Save { dst: "history.nbt" },
        );
        expect_command_ok(
            &runner,
            args!(ctx),
            "save arg",
            Command::Save { dst: "arg" },
        );
        expect_command_ok(&runner, args!(ctx), "save .", Command::Save { dst: "." });
        expect_command_fail(&runner, args!(ctx), "save arg1 arg2");
    }

    #[test]
    fn test_runner() {
        fn test_case(
            runner: &CommandRunner<()>,
            ctx: &mut Context,
            input: &'static str,
            expected: CommandControlFlow,
        ) {
            let args = CommandContext {
                ctx,
                editor: &mut (),
            };
            assert_eq!(runner.try_run_line(input, args), expected);
        }

        let mut ctx = Context::new_without_importer();

        let runner = CommandRunner::<()>::new_all_disabled()
            .enable_print_markup(|_| {})
            .enable_quit();

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
