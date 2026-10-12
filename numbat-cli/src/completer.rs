use std::sync::{Arc, Mutex};

use numbat::{Context, compact_str::CompactString, unicode_input::UNICODE_INPUT};
use rustyline::completion::{Completer, Pair, extract_word};

pub struct NumbatCompleter {
    pub context: Arc<Mutex<Context>>,
    pub modules: Vec<CompactString>,
    pub all_timezones: Vec<CompactString>,
}

// Complete names understood by the `info` command, without call parentheses
// or unrelated language keywords.
fn info_completions(line: &str, pos: usize, context: &Context) -> Option<(usize, Vec<Pair>)> {
    let word_part = line.get(..pos)?.strip_prefix("info ")?;

    // `info` accepts a single name, not an expression.
    if word_part.chars().any(char::is_whitespace) {
        return Some((pos, Vec::new()));
    }

    let mut names: Vec<String> = context
        .variable_names()
        .chain(context.function_names())
        .chain(context.dimension_names().iter().cloned())
        .chain(context.unit_names().iter().flatten().cloned())
        .filter(|name| name.starts_with(word_part))
        .map(|name| name.to_string())
        .collect();

    names.sort();
    names.dedup();

    Some((
        "info ".len(),
        names
            .into_iter()
            .map(|name| Pair {
                display: name.clone(),
                replacement: name,
            })
            .collect(),
    ))
}

impl Completer for NumbatCompleter {
    type Candidate = Pair;

    fn complete(
        &self,
        line: &str,
        pos: usize,
        _: &rustyline::Context<'_>,
    ) -> rustyline::Result<(usize, Vec<Self::Candidate>)> {
        for (patterns, replacement) in UNICODE_INPUT {
            for pattern in *patterns {
                let backslash_pattern = format!("\\{pattern}");
                if line[..pos].ends_with(&backslash_pattern) {
                    return Ok((
                        pos - (1 + pattern.len()),
                        vec![Pair {
                            display: backslash_pattern.to_string(),
                            replacement: replacement.to_string(),
                        }],
                    ));
                }
            }
        }

        if line.starts_with("info ") {
            let binding = self.context.lock().unwrap();
            if let Some(result) = info_completions(line, pos, &binding) {
                return Ok(result);
            }
        }

        if line.starts_with("use ") {
            return Ok((
                0,
                self.modules
                    .iter()
                    .map(|m| {
                        let line = format!("use {m}");
                        Pair {
                            display: m.to_string(),
                            replacement: line,
                        }
                    })
                    .filter(|p| p.replacement.starts_with(line))
                    .collect(),
            ));
        } else if line.starts_with("list ") || line.starts_with("ls ") {
            let command = if line.starts_with("list ") {
                "list"
            } else {
                "ls"
            };

            return Ok((
                0,
                ["functions", "dimensions", "units", "variables"]
                    .iter()
                    .map(|category| {
                        let line = format!("{command} {category}");
                        Pair {
                            display: category.to_string(),
                            replacement: line,
                        }
                    })
                    .filter(|p| p.replacement.starts_with(line))
                    .collect(),
            ));
        }

        // does it look like we're tab-completing a timezone?
        let complete_tz = line.find("tz(").and_then(|convert_pos| {
            if let Some(quote_pos) = line.rfind('"')
                && quote_pos > convert_pos
                && pos > quote_pos
            {
                return Some(quote_pos + 1);
            }
            None
        });
        if let Some(pos_word) = complete_tz {
            let word_part = &line[pos_word..];
            let matches = self
                .all_timezones
                .iter()
                .filter(|tz| tz.starts_with(word_part))
                .collect::<Vec<_>>();
            let append_closing_quote = matches.len() <= 1;

            return Ok((
                pos_word,
                matches
                    .into_iter()
                    .map(|tz| Pair {
                        display: tz.to_string(),
                        replacement: if append_closing_quote {
                            format!("{tz}\"")
                        } else {
                            tz.to_string()
                        },
                    })
                    .collect(),
            ));
        }

        let (pos_word, word_part) = extract_word(line, pos, None, |c| {
            // TODO: we could use is_identifier_char here potentially
            match c {
                c if c.is_alphanumeric() => false,
                '_' => false,
                _ => true,
            }
        });

        // don't add an opening paren if we're completing after a reverse function call
        // or when completing conversion functions
        let add_paren = !["|>", "->", "→", "➞", "to"]
            .iter()
            .any(|&s| line[..pos].contains(s));

        let binding = self.context.lock().unwrap();
        let candidates = binding.get_completions_for(word_part, add_paren);

        Ok((
            pos_word,
            candidates
                .map(|w| Pair {
                    display: w.to_string(),
                    replacement: w,
                })
                .collect(),
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::info_completions;
    use numbat::{Context, resolver::CodeSource};

    #[test]
    fn info_completion_uses_names_without_parentheses_or_keywords() {
        let mut context = Context::new_without_importer();
        let (_statements, _result) = context
            .interpret(
                "fn is_empty(x) = x\nlet is_example = 1",
                CodeSource::Internal,
            )
            .unwrap();

        let line = "info is_";
        let (start, candidates) = info_completions(line, line.len(), &context).unwrap();

        assert_eq!(start, "info ".len());

        let replacements: Vec<_> = candidates
            .iter()
            .map(|candidate| candidate.replacement.as_str())
            .collect();

        assert_eq!(replacements, ["is_empty", "is_example"]);
        assert!(!replacements.iter().any(|name| name.ends_with('(')));

        let (_, all_candidates) = info_completions("info ", "info ".len(), &context).unwrap();

        assert!(
            !all_candidates
                .iter()
                .any(|candidate| candidate.replacement == "let")
        );

        assert!(info_completions("is_", 3, &context).is_none());
    }
}
