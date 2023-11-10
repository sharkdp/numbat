use std::sync::{Arc, Mutex};

use numbat::Context;
use rustyline::{
    self,
    completion::{extract_word, Completer, Pair},
};

pub struct NumbatCompleter {
    pub context: Arc<Mutex<Context>>,
    pub modules: Vec<String>,
}

impl Completer for NumbatCompleter {
    type Candidate = Pair;

    fn complete(
        &self,
        line: &str,
        pos: usize,
        _: &rustyline::Context<'_>,
    ) -> rustyline::Result<(usize, Vec<Self::Candidate>)> {
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
        }

        let (pos_word, word_part) = extract_word(line, pos, None, |c| {
            // TODO: we could use is_identifier_char here potentially
            match c {
                c if c.is_alphanumeric() => false,
                '_' => false,
                _ => true,
            }
        });

        let candidates = self.context.lock().unwrap().get_completions_for(word_part);

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
