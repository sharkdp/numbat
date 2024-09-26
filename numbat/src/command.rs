use std::str::{FromStr, SplitWhitespace};

use crate::{
    parser::ParseErrorKind,
    span::{ByteIndex, Span},
    ParseError,
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

#[derive(Debug, Clone, PartialEq)]
pub enum Command<'a> {
    Help,
    Info { item: &'a str },
    List { items: Option<ListItems> },
    Clear,
    Save { dst: &'a str },
    Quit,
}

/// Contains just the words and word boundaries of the input we're parsing, no
/// `code_source_id`
///
/// This type has a single method, a fallible initializer. If it succeeds, then we know
/// we're actually looking at a command (not necessarily a well-formed one, eg `list
/// foobar` is a command but will fail later on). Then we go and construct a new
/// `code_source_id`, and then use a full `CommandParser`, constructed from this and the
/// `code_source_id`, to do the actual command parsing. If the initializer fails, then
/// we proceed with parsing the input as a numbat expression (which will create its own
/// `code_source_id`).
pub struct SourcelessCommandParser<'a> {
    /// The command we're running ("list", "quit", etc) without any of its args
    command_kind: CommandKind,
    /// The words in the input. This does not include the command name, which has been
    /// stripped off and placed in `command_kind`
    args: SplitWhitespace<'a>,
    /// For tracking spans. Contains `(start, start+len)` for each (whitespace-separated)
    /// word in the input
    word_boundaries: Vec<(u32, u32)>,
}

impl<'a> SourcelessCommandParser<'a> {
    ///Fallibly construct a new `Self` from the input
    ///
    /// Returns:
    /// - `None`, if the first word of the input is not a command
    /// - `Some(Self)`, if the first word of the input is a command
    ///
    /// If this returns `None`, you should proceed with parsing the input as an ordinary
    /// numbat expression
    pub fn new(input: &'a str) -> Option<Self> {
        let mut words: SplitWhitespace<'_> = input.split_whitespace();
        let command_kind = words.next().and_then(|w| w.parse().ok())?;

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
        })
    }
}

/// A "full" command parser, containing both the state we need to parse (`inner:
/// SourcelessCommandParser`) and a `code_source_id` to report errors correctly
///
/// All actual parsing happens through this struct. Since we managed to obtain a
/// `SourcelessCommandParser`, we know that the input was a command and not a numbat
/// expression, so we can proceed with parsing the command further.
pub struct CommandParser<'a> {
    inner: SourcelessCommandParser<'a>,
    code_source_id: usize,
}

impl<'a> CommandParser<'a> {
    /// Construct a new `CommandParser` from an existing `SourcelessCommandParser`,
    /// which contains the input, and a `code_source_id`, for reporting errors
    pub fn new(inner: SourcelessCommandParser<'a>, code_source_id: usize) -> Self {
        Self {
            inner,
            code_source_id,
        }
    }

    /// Get the span starting at the start of the word at `word_index`, through the end of
    /// the last word represented by `word_boundaries`
    ///
    /// ## Panics
    /// If `word_index` is out of bounds, ie `word_index >= word_boundaries.len()`
    fn span_through_end(&self, word_index: usize) -> Span {
        let start = self.inner.word_boundaries[word_index].0;
        let end = self.inner.word_boundaries.last().unwrap().1;
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
            span: self.span_from_boundary(self.inner.word_boundaries[index]),
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
        if self.inner.args.next().is_some() {
            let message = format!("`{}` takes 0 arguments{}", cmd, err_msg_suffix);
            return Err(self.err_through_end_from(1, message));
        }
        Ok(())
    }

    /// Attempt to parse the input provided to Self::new as a command, such as "help",
    /// "list <args>", "quit", etc
    ///
    /// Returns:
    /// - `Ok(Command)`, if the input is a valid command with correct arguments
    /// - `Err(ParseError)`, if the input starts with a valid command but has the wrong
    ///   number or kind of arguments, e.g. `list foobar`
    pub fn parse_command(&mut self) -> Result<Command, ParseError> {
        let command = match &self.inner.command_kind {
            CommandKind::Help => {
                self.ensure_zero_args("help", "; use `info <item>` for information about an item")?;
                Command::Help
            }
            CommandKind::Clear => {
                self.ensure_zero_args("clear", "")?;
                Command::Clear
            }
            CommandKind::Quit(alias) => {
                self.ensure_zero_args(
                    match alias {
                        QuitAlias::Quit => "quit",
                        QuitAlias::Exit => "exit",
                    },
                    "",
                )?;

                Command::Quit
            }
            CommandKind::Info => {
                let err_msg = "`info` requires exactly one argument, the item to get info on";
                let Some(item) = self.inner.args.next() else {
                    return Err(self.err_at_idx(0, err_msg));
                };

                if self.inner.args.next().is_some() {
                    return Err(self.err_through_end_from(1, err_msg));
                }

                Command::Info { item }
            }
            CommandKind::List => {
                let items = self.inner.args.next();

                if self.inner.args.next().is_some() {
                    return Err(self.err_through_end_from(2, "`list` takes at most one argument"));
                }

                let items = match items {
                    None => None,
                    Some("functions") => Some(ListItems::Functions),
                    Some("dimensions") => Some(ListItems::Dimensions),
                    Some("variables") => Some(ListItems::Variables),
                    Some("units") => Some(ListItems::Units),
                    _ => {
                        return Err(self.err_at_idx(
                            1,
                            "if provided, the argument to `list` must be \
                            one of: functions, dimensions, variables, units",
                        ));
                    }
                };

                Command::List { items }
            }
            CommandKind::Save => {
                let Some(dst) = self.inner.args.next() else {
                    return Ok(Command::Save { dst: "history.nbt" });
                };

                if self.inner.args.next().is_some() {
                    return Err(self.err_through_end_from(
                        2,
                        "`save` requires exactly one argument, the destination",
                    ));
                }

                Command::Save { dst }
            }
        };

        Ok(command)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn parser(input: &'static str) -> Option<CommandParser<'static>> {
        Some(CommandParser::new(SourcelessCommandParser::new(input)?, 0))
    }

    // can't be a function due to lifetimes/borrow checker
    macro_rules! parse {
        ($input:literal) => {{
            parser($input).unwrap().parse_command()
        }};
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

        assert_eq!(&parser("list").unwrap().inner.word_boundaries, &[(0, 4)]);
        assert_eq!(&parser("list ").unwrap().inner.word_boundaries, &[(0, 4)]);
        assert_eq!(&parser(" list").unwrap().inner.word_boundaries, &[(1, 5)]);
        assert_eq!(&parser(" list ").unwrap().inner.word_boundaries, &[(1, 5)]);

        assert_eq!(
            &parser("list   ab").unwrap().inner.word_boundaries,
            &[(0, 4), (7, 9)]
        );
        assert_eq!(
            &parser("list   ab ").unwrap().inner.word_boundaries,
            &[(0, 4), (7, 9)]
        );
        assert_eq!(
            &parser(" list   ab").unwrap().inner.word_boundaries,
            &[(1, 5), (8, 10)]
        );
        assert_eq!(
            &parser(" list   ab ").unwrap().inner.word_boundaries,
            &[(1, 5), (8, 10)]
        );

        assert_eq!(
            &parser("list   ab xy").unwrap().inner.word_boundaries,
            &[(0, 4), (7, 9), (10, 12)]
        );
        assert_eq!(
            &parser("list   ab   xy ").unwrap().inner.word_boundaries,
            &[(0, 4), (7, 9), (12, 14)]
        );
        assert_eq!(
            parser("   list   ab    xy").unwrap().inner.word_boundaries,
            &[(3, 7), (10, 12), (16, 18)]
        );
        assert_eq!(
            parser("   list   ab    xy   ")
                .unwrap()
                .inner
                .word_boundaries,
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
        assert_eq!(parse!("list").unwrap(), Command::List { items: None });
        assert_eq!(parse!(" list").unwrap(), Command::List { items: None });
        assert_eq!(parse!("list ").unwrap(), Command::List { items: None });
        assert_eq!(parse!(" list ").unwrap(), Command::List { items: None });
        assert_eq!(
            parse!("list functions  ").unwrap(),
            Command::List {
                items: Some(ListItems::Functions)
            }
        );
        assert_eq!(
            parse!("  list    functions  ").unwrap(),
            Command::List {
                items: Some(ListItems::Functions)
            }
        );
        assert_eq!(
            parse!("  list    functions  ").unwrap(),
            Command::List {
                items: Some(ListItems::Functions)
            }
        );
        assert_eq!(
            parse!("list    functions").unwrap(),
            Command::List {
                items: Some(ListItems::Functions)
            }
        );
    }

    #[test]
    fn test_args() {
        assert_eq!(parse!("help").unwrap(), Command::Help);
        assert!(parse!("help arg").is_err());
        assert!(parse!("help arg1 arg2").is_err());

        assert!(parse!("info").is_err());
        assert_eq!(parse!("info arg").unwrap(), Command::Info { item: "arg" });
        assert_eq!(parse!("info .").unwrap(), Command::Info { item: "." });
        assert!(parse!("info arg1 arg2").is_err());

        assert_eq!(parse!("clear").unwrap(), Command::Clear);
        assert!(parse!("clear arg").is_err());
        assert!(parse!("clear arg1 arg2").is_err());

        assert_eq!(parse!("list").unwrap(), Command::List { items: None });
        assert_eq!(
            parse!("list functions").unwrap(),
            Command::List {
                items: Some(ListItems::Functions)
            }
        );
        assert_eq!(
            parse!("list dimensions").unwrap(),
            Command::List {
                items: Some(ListItems::Dimensions)
            }
        );
        assert_eq!(
            parse!("list variables").unwrap(),
            Command::List {
                items: Some(ListItems::Variables)
            }
        );
        assert_eq!(
            parse!("list units").unwrap(),
            Command::List {
                items: Some(ListItems::Units)
            }
        );

        assert_eq!(parse!("quit").unwrap(), Command::Quit);
        assert!(parse!("quit arg").is_err());
        assert!(parse!("quit arg1 arg2").is_err());

        assert_eq!(parse!("exit").unwrap(), Command::Quit);
        assert!(parse!("exit arg").is_err());
        assert!(parse!("exit arg1 arg2").is_err());

        assert_eq!(
            parse!("save").unwrap(),
            Command::Save { dst: "history.nbt" }
        );
        assert_eq!(parse!("save arg").unwrap(), Command::Save { dst: "arg" });
        assert_eq!(parse!("save .").unwrap(), Command::Save { dst: "." });
        assert!(parse!("save arg1 arg2").is_err());
    }
}
