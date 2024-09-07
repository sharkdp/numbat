use crate::{
    parser::ParseErrorKind,
    resolver::{CodeSource, Resolver},
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

/// For tracking spans. Contains `(start, start+len)` for each (whitespace-separated)
/// word in the input
fn get_word_boundaries(input: &str) -> Vec<(u32, u32)> {
    let mut word_boundaries = Vec::new();
    let mut prev_char_was_whitespace = true;
    let mut start_idx = 0;
    for (i, c) in input.char_indices() {
        if prev_char_was_whitespace && !c.is_whitespace() {
            start_idx = u32::try_from(i).unwrap();
        } else if !prev_char_was_whitespace && c.is_whitespace() {
            word_boundaries.push((start_idx, u32::try_from(i).unwrap()));
        }
        prev_char_was_whitespace = c.is_whitespace();
    }

    // if no whitespace after last word, need to add last word
    if !prev_char_was_whitespace {
        word_boundaries.push((start_idx, u32::try_from(input.len()).unwrap()));
    }

    word_boundaries
}

/// Get the span starting at the start of the word at `word_index`, through the end of
/// the last word represented by `word_boundaries`
///
/// ## Panics
/// If `word_index` is out of bounds, ie `word_index >= word_boundaries.len()`
fn span_through_end(
    word_boundaries: &[(u32, u32)],
    word_index: usize,
    code_source_id: usize,
) -> Span {
    let start = word_boundaries[word_index].0;
    let end = word_boundaries.last().unwrap().1;
    span_from_boundary((start, end), code_source_id)
}

fn span_from_boundary((start, end): (u32, u32), code_source_id: usize) -> Span {
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
        code_source_id,
    }
}

macro_rules! handle_arg_count {
    (count = 0, $words:expr, $word_boundaries:expr, $code_source_id:expr, "help", $if_ok:expr $(,)?) => {{
        handle_arg_count!(
            count = 0,
            $words,
            $word_boundaries,
            $code_source_id,
            "help",
            Command::Help,
            "`help` takes 0 arguments; use `info <item>` for information about an item",
        )
    }};
    (count = 0, $words:expr, $word_boundaries:expr, $code_source_id:expr, $command:literal, $if_ok:expr $(,)?) => {{
        handle_arg_count!(
            count = 0,
            $words,
            $word_boundaries,
            $code_source_id,
            $command,
            $if_ok,
            concat!("`", $command, "` takes 0 arguments")
        )
    }};
    (count = 0, $words:expr, $word_boundaries:expr, $code_source_id:expr, $command:literal, $if_ok:expr, $err_msg:expr $(,)?) => {{
        if $words.next().is_some() {
            return Some(Err(ParseError {
                kind: ParseErrorKind::InvalidCommand($err_msg),
                span: span_through_end($word_boundaries, 1, $code_source_id),
            }));
        }

        $if_ok
    }};
}

/// this function primarily exists for testing, to not have to provide a resolver
///
/// for an actual build, a resolver is necessary for error reporting
fn parse_command_inner<'a>(
    input: &'a str,
    resolver: Option<&mut Resolver>,
) -> Option<Result<Command<'a>, ParseError>> {
    let word_boundaries = get_word_boundaries(input);

    let code_source_id = match resolver {
        Some(resolver) => resolver.add_code_source(CodeSource::Text, input),
        None => 0,
    };

    let mut words = input.split_whitespace();
    let Some(command_str) = words.next() else {
        // should never hit this branch in practice because all-whitespace inputs are
        // skipped over

        return Some(Err(ParseError {
            kind: ParseErrorKind::InvalidCommand("invalid empty command"),
            span: span_from_boundary((0, u32::try_from(input.len()).unwrap()), code_source_id),
        }));
    };

    let command = match command_str {
        "help" => handle_arg_count!(
            count = 0,
            words,
            &word_boundaries,
            code_source_id,
            "help",
            Command::Help,
        ),
        "clear" => handle_arg_count!(
            count = 0,
            words,
            &word_boundaries,
            code_source_id,
            "clear",
            Command::Clear,
        ),
        "quit" => handle_arg_count!(
            count = 0,
            words,
            &word_boundaries,
            code_source_id,
            "quit",
            Command::Quit,
        ),
        "exit" => handle_arg_count!(
            count = 0,
            words,
            &word_boundaries,
            code_source_id,
            "exit",
            Command::Quit,
        ),
        "info" => {
            let err_msg = "`info` requires exactly one argument, the item to get info on";
            let Some(item) = words.next() else {
                return Some(Err(ParseError {
                    kind: ParseErrorKind::InvalidCommand(err_msg),
                    span: span_from_boundary(word_boundaries[0], code_source_id),
                }));
            };

            if words.next().is_some() {
                return Some(Err(ParseError {
                    kind: ParseErrorKind::InvalidCommand(err_msg),
                    span: span_through_end(&word_boundaries, 1, code_source_id),
                }));
            }

            Command::Info { item }
        }
        "list" | "ls" => {
            let items = match words.next() {
                None => None,
                Some("functions") => Some(ListItems::Functions),
                Some("dimensions") => Some(ListItems::Dimensions),
                Some("variables") => Some(ListItems::Variables),
                Some("units") => Some(ListItems::Units),
                _ => {
                    return Some(Err(ParseError {
                        kind: ParseErrorKind::InvalidCommand(
                            "if provided, the argument to `list` or `ls` must be \
                             one of: functions, dimensions, variables, units",
                        ),
                        span: span_from_boundary(word_boundaries[1], code_source_id),
                    }));
                }
            };
            if words.next().is_some() {
                let start = word_boundaries[2].0;
                let end = word_boundaries.last().unwrap().1;
                return Some(Err(ParseError {
                    kind: ParseErrorKind::InvalidCommand("`list` takes at most one argument"),
                    span: span_from_boundary((start, end), code_source_id),
                }));
            }

            Command::List { items }
        }
        "save" => {
            let err_msg = "`save` requires exactly one argument, the destination";
            let Some(dst) = words.next() else {
                return Some(Err(ParseError {
                    kind: ParseErrorKind::InvalidCommand(err_msg),
                    span: span_from_boundary(word_boundaries[0], code_source_id),
                }));
            };

            if words.next().is_some() {
                let start = word_boundaries[2].0;
                let end = word_boundaries.last().unwrap().1;
                return Some(Err(ParseError {
                    kind: ParseErrorKind::InvalidCommand(err_msg),
                    span: span_from_boundary((start, end), code_source_id),
                }));
            }

            Command::Save { dst }
        }

        _ => return None,
    };

    Some(Ok(command))
}

/// Attempt to parse the input as a command, such as "help", "list <args>", "quit", etc
///
/// Returns:
/// - `None`, if the input does not begin with a command keyword
/// - `Some(Ok(Command))`, if the input is a valid command
/// - `Some(Err(_))`, if the input starts with a valid command but has the wrong number
///   or kind of arguments, e.g. `list foobar`
pub fn parse_command<'a>(
    input: &'a str,
    resolver: &mut Resolver,
) -> Option<Result<Command<'a>, ParseError>> {
    parse_command_inner(input, Some(resolver))
}

#[cfg(test)]
mod test {
    use super::*;

    fn parse_command(input: &str) -> Option<Result<Command, ParseError>> {
        parse_command_inner(input, None)
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
