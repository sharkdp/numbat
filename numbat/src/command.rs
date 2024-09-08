use std::str::SplitWhitespace;

use crate::{
    parser::ParseErrorKind,
    span::{SourceCodePositition, Span},
    ParseError,
};

#[derive(Debug, Clone, PartialEq)]
pub enum ListItems {
    Functions,
    Dimensions,
    Variables,
    Units,
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

struct CommandParser<'a> {
    /// The words in the input
    words: SplitWhitespace<'a>,
    /// For tracking spans. Contains `(start, start+len)` for each (whitespace-separated)
    /// word in the input
    word_boundaries: Vec<(u32, u32)>,
    code_source_id: usize,
}

impl<'a> CommandParser<'a> {
    fn new(input: &'a str, code_source_id: usize) -> Self {
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

        Self {
            words: input.split_whitespace(),
            word_boundaries,
            code_source_id,
        }
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
            start: SourceCodePositition {
                byte: start,
                line: 1,
                position: start,
            },
            end: SourceCodePositition {
                byte: end,
                line: 1,
                position: end,
            },
            code_source_id: self.code_source_id,
        }
    }

    fn err_at_idx(&self, index: usize, err_msg: &'static str) -> ParseError {
        ParseError {
            kind: ParseErrorKind::InvalidCommand(err_msg),
            span: self.span_from_boundary(self.word_boundaries[index]),
        }
    }

    fn err_through_end_from(&self, index: usize, err_msg: &'static str) -> ParseError {
        ParseError {
            kind: ParseErrorKind::InvalidCommand(err_msg),
            span: self.span_through_end(index),
        }
    }
}

macro_rules! ensure_zero_args {
    ($parser:expr, $cmd:literal) => {{
        ensure_zero_args!($parser, $cmd, "")
    }};
    ($parser:expr, $cmd: literal, $err_msg_suffix:literal) => {{
        if $parser.words.next().is_some() {
            return Some(Err($parser.err_through_end_from(
                1,
                concat!("`", $cmd, "` takes 0 arguments", $err_msg_suffix),
            )));
        }
    }};
}

/// Attempt to parse the input as a command, such as "help", "list <args>", "quit", etc
///
/// Returns:
/// - `None`, if the input does not begin with a command keyword
/// - `Some(Ok(Command))`, if the input is a valid command
/// - `Some(Err(_))`, if the input starts with a valid command but has the wrong number
///   or kind of arguments, e.g. `list foobar`
pub fn parse_command(input: &str, code_source_id: usize) -> Option<Result<Command, ParseError>> {
    let mut parser = CommandParser::new(input, code_source_id);

    let Some(command_str) = parser.words.next() else {
        // should never hit this branch in practice because all-whitespace inputs are
        // skipped over

        return Some(Err(ParseError {
            kind: ParseErrorKind::InvalidCommand("invalid empty command"),
            span: parser.span_from_boundary((0, u32::try_from(input.len()).unwrap())),
        }));
    };

    let command = match command_str {
        "help" => {
            ensure_zero_args!(
                parser,
                "help",
                "; use `info <item>` for information about an item"
            );
            Command::Help
        }
        "clear" => {
            ensure_zero_args!(parser, "clear");
            Command::Clear
        }
        "quit" => {
            ensure_zero_args!(parser, "quit");
            Command::Quit
        }
        "exit" => {
            ensure_zero_args!(parser, "exit");
            Command::Quit
        }
        "info" => {
            let err_msg = "`info` requires exactly one argument, the item to get info on";
            let Some(item) = parser.words.next() else {
                return Some(Err(parser.err_at_idx(0, err_msg)));
            };

            if parser.words.next().is_some() {
                return Some(Err(parser.err_through_end_from(1, err_msg)));
            }

            Command::Info { item }
        }
        "list" | "ls" => {
            let items = match parser.words.next() {
                None => None,
                Some("functions") => Some(ListItems::Functions),
                Some("dimensions") => Some(ListItems::Dimensions),
                Some("variables") => Some(ListItems::Variables),
                Some("units") => Some(ListItems::Units),
                _ => {
                    return Some(Err(parser.err_at_idx(
                        1,
                        "if provided, the argument to `list` or `ls` must be \
                        one of: functions, dimensions, variables, units",
                    )));
                }
            };
            if parser.words.next().is_some() {
                return Some(Err(
                    parser.err_through_end_from(2, "`list` takes at most one argument")
                ));
            }

            Command::List { items }
        }
        "save" => {
            let err_msg = "`save` requires exactly one argument, the destination";
            let Some(dst) = parser.words.next() else {
                return Some(Err(parser.err_at_idx(0, err_msg)));
            };

            if parser.words.next().is_some() {
                return Some(Err(parser.err_through_end_from(2, err_msg)));
            }

            Command::Save { dst }
        }

        _ => return None,
    };

    Some(Ok(command))
}

#[cfg(test)]
mod test {
    use super::*;

    fn parse_command(input: &str) -> Option<Result<Command, ParseError>> {
        super::parse_command(input, 0)
    }

    #[test]
    fn test_command_parser() {
        assert_eq!(&CommandParser::new("", 0).word_boundaries, &[]);
        assert_eq!(&CommandParser::new(" ", 0).word_boundaries, &[]);
        assert_eq!(&CommandParser::new("  ", 0).word_boundaries, &[]);

        assert_eq!(&CommandParser::new("x", 0).word_boundaries, &[(0, 1)]);
        assert_eq!(&CommandParser::new("x ", 0).word_boundaries, &[(0, 1)]);
        assert_eq!(&CommandParser::new(" x", 0).word_boundaries, &[(1, 2)]);
        assert_eq!(&CommandParser::new(" x ", 0).word_boundaries, &[(1, 2)]);

        assert_eq!(&CommandParser::new("xyz", 0).word_boundaries, &[(0, 3)]);
        assert_eq!(&CommandParser::new("xyz  ", 0).word_boundaries, &[(0, 3)]);
        assert_eq!(&CommandParser::new("  xyz", 0).word_boundaries, &[(2, 5)]);
        assert_eq!(&CommandParser::new("  xyz  ", 0).word_boundaries, &[(2, 5)]);

        assert_eq!(
            &CommandParser::new("abc x", 0).word_boundaries,
            &[(0, 3), (4, 5)]
        );
        assert_eq!(
            &CommandParser::new("abc  x ", 0).word_boundaries,
            &[(0, 3), (5, 6)]
        );
        assert_eq!(
            &CommandParser::new(" abc   x", 0).word_boundaries,
            &[(1, 4), (7, 8)]
        );
        assert_eq!(
            &CommandParser::new("  abc   x  ", 0).word_boundaries,
            &[(2, 5), (8, 9)]
        );

        assert_eq!(
            &CommandParser::new("abc x y z", 0).word_boundaries,
            &[(0, 3), (4, 5), (6, 7), (8, 9)]
        );
        assert_eq!(
            &CommandParser::new("abc x y z  ", 0).word_boundaries,
            &[(0, 3), (4, 5), (6, 7), (8, 9)]
        );
        assert_eq!(
            &CommandParser::new("  abc x y z", 0).word_boundaries,
            &[(2, 5), (6, 7), (8, 9), (10, 11)]
        );
        assert_eq!(
            &CommandParser::new("  abc x y z  ", 0).word_boundaries,
            &[(2, 5), (6, 7), (8, 9), (10, 11)]
        );
    }

    #[test]
    fn test_existent_commands() {
        // valid commands
        assert!(parse_command("help").is_some());
        assert!(parse_command("help arg").is_some());
        assert!(parse_command("help arg1 arg2").is_some());

        assert!(parse_command("info").is_some());
        assert!(parse_command("info arg").is_some());
        assert!(parse_command("info arg1 arg2").is_some());

        assert!(parse_command("clear").is_some());
        assert!(parse_command("clear arg").is_some());
        assert!(parse_command("clear arg1 arg2").is_some());

        assert!(parse_command("list").is_some());
        assert!(parse_command("list arg").is_some());
        assert!(parse_command("list arg1 arg2").is_some());

        assert!(parse_command("quit").is_some());
        assert!(parse_command("quit arg").is_some());
        assert!(parse_command("quit arg1 arg2").is_some());

        assert!(parse_command("exit").is_some());
        assert!(parse_command("exit arg").is_some());
        assert!(parse_command("exit arg1 arg2").is_some());

        assert!(parse_command("save").is_some());
        assert!(parse_command("save arg").is_some());
        assert!(parse_command("save arg1 arg2").is_some());

        // these shouldn't happen at runtime because the REPL skips over all
        // whitespace lines, but we still want to handle them just in case
        assert!(parse_command("").unwrap().is_err());
        assert!(parse_command(" ").unwrap().is_err());

        // invalid (nonempty) command names are all None so that parsing can continue on
        // what is presumably a math expression. case matters
        assert!(parse_command(".").is_none());
        assert!(parse_command(",").is_none());
        assert!(parse_command(";").is_none());
        assert!(parse_command("HELP").is_none());
        assert!(parse_command("List").is_none());
        assert!(parse_command("qUIt").is_none());
        assert!(parse_command("listfunctions").is_none());
        assert!(parse_command("exitquit").is_none());
    }

    #[test]
    fn test_whitespace() {
        assert_eq!(
            parse_command("list").unwrap().unwrap(),
            Command::List { items: None }
        );

        assert_eq!(
            parse_command(" list").unwrap().unwrap(),
            Command::List { items: None }
        );

        assert_eq!(
            parse_command("list ").unwrap().unwrap(),
            Command::List { items: None }
        );

        assert_eq!(
            parse_command(" list ").unwrap().unwrap(),
            Command::List { items: None }
        );

        assert_eq!(
            parse_command("list functions  ").unwrap().unwrap(),
            Command::List {
                items: Some(ListItems::Functions)
            }
        );

        assert_eq!(
            parse_command("  list    functions  ").unwrap().unwrap(),
            Command::List {
                items: Some(ListItems::Functions)
            }
        );

        assert_eq!(
            parse_command("  list    functions  ").unwrap().unwrap(),
            Command::List {
                items: Some(ListItems::Functions)
            }
        );

        assert_eq!(
            parse_command("list    functions").unwrap().unwrap(),
            Command::List {
                items: Some(ListItems::Functions)
            }
        );
    }

    #[test]
    fn test_args() {
        assert_eq!(parse_command("help").unwrap().unwrap(), Command::Help);
        assert!(parse_command("help arg").unwrap().is_err());
        assert!(parse_command("help arg1 arg2").unwrap().is_err());

        assert!(parse_command("info").unwrap().is_err());
        assert_eq!(
            parse_command("info arg").unwrap().unwrap(),
            Command::Info { item: "arg" }
        );
        assert_eq!(
            parse_command("info .").unwrap().unwrap(),
            Command::Info { item: "." }
        );
        assert!(parse_command("info arg1 arg2").unwrap().is_err());

        assert_eq!(parse_command("clear").unwrap().unwrap(), Command::Clear);
        assert!(parse_command("clear arg").unwrap().is_err());
        assert!(parse_command("clear arg1 arg2").unwrap().is_err());

        assert_eq!(
            parse_command("list").unwrap().unwrap(),
            Command::List { items: None }
        );
        assert_eq!(
            parse_command("list functions").unwrap().unwrap(),
            Command::List {
                items: Some(ListItems::Functions)
            }
        );
        assert_eq!(
            parse_command("list dimensions").unwrap().unwrap(),
            Command::List {
                items: Some(ListItems::Dimensions)
            }
        );
        assert_eq!(
            parse_command("list variables").unwrap().unwrap(),
            Command::List {
                items: Some(ListItems::Variables)
            }
        );
        assert_eq!(
            parse_command("list units").unwrap().unwrap(),
            Command::List {
                items: Some(ListItems::Units)
            }
        );

        assert_eq!(parse_command("quit").unwrap().unwrap(), Command::Quit);
        assert!(parse_command("quit arg").unwrap().is_err());
        assert!(parse_command("quit arg1 arg2").unwrap().is_err());

        assert_eq!(parse_command("exit").unwrap().unwrap(), Command::Quit);
        assert!(parse_command("exit arg").unwrap().is_err());
        assert!(parse_command("exit arg1 arg2").unwrap().is_err());

        assert!(parse_command("save").unwrap().is_err());
        assert_eq!(
            parse_command("save arg").unwrap().unwrap(),
            Command::Save { dst: "arg" }
        );
        assert_eq!(
            parse_command("save .").unwrap().unwrap(),
            Command::Save { dst: "." }
        );
        assert!(parse_command("save arg1 arg2").unwrap().is_err());
    }
}
