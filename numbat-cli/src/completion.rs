use std::sync::{Arc, Mutex};

use numbat::Context;
use rustyline::{
    self,
    completion::{extract_word, Completer, Pair},
};

pub struct NumbatCompleter {
    pub context: Arc<Mutex<Context>>,
}

impl Completer for NumbatCompleter {
    type Candidate = Pair;

    fn complete(
        &self,
        line: &str,
        pos: usize,
        _: &rustyline::Context<'_>,
    ) -> rustyline::Result<(usize, Vec<Self::Candidate>)> {
        let (pos_word, word_part) = extract_word(line, pos, None, |c| {
            // TODO: we could use is_identifier_char here potentially
            match c {
                c if c.is_alphanumeric() => false,
                '_' => false,
                _ => true,
            }
        });

        let mut words = vec![
            // keywords
            "let ".into(),
            "fn ".into(),
            "dimension ".into(),
            "unit ".into(),
            "use ".into(),
            // 'inline' keywords
            "short".into(),
            "long".into(),
            // decorators
            "metric_prefixes".into(),
            "binary_prefixes".into(),
            "aliases".into(),
            "aliases_short".into(),
        ];

        {
            let ctx = self.context.lock().unwrap();

            for variable in ctx.variable_names() {
                words.push(variable.clone());
            }

            for function in ctx.function_names() {
                words.push(format!("{}(", function));
            }

            for dimension in ctx.dimension_names() {
                words.push(dimension.clone());
            }

            for unit_names in ctx.unit_names() {
                for unit in unit_names {
                    words.push(unit.clone());
                }
            }
        }

        words.sort();
        words.dedup();

        let candidates = words.iter().filter(|w| w.starts_with(word_part));

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
