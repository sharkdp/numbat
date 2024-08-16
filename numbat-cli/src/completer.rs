use std::sync::{Arc, Mutex};

use numbat::{unicode_input::UNICODE_INPUT, Context};
use rustyline::completion::{extract_word, Completer, Pair};

pub struct NumbatCompleter {
    pub context: Arc<Mutex<Context>>,
    pub modules: Vec<String>,
    pub all_timezones: Vec<String>,
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
            if let Some(quote_pos) = line.rfind('"') {
                if quote_pos > convert_pos && pos > quote_pos {
                    return Some(quote_pos + 1);
                }
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

        let candidates = self
            .context
            .lock()
            .unwrap()
            .get_completions_for(word_part, add_paren);

        Ok((
            pos_word,
            candidates
                .map(|w| Pair {
                    display: w.to_string(),
                    replacement: w.to_string(),
                })
                .collect(),
        ))
    }
}
