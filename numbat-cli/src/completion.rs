use rustyline::{
    self,
    completion::{extract_word, Completer, Pair},
};

pub struct NumbatCompleter;

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

        // TODO, obviously
        let words = vec![
            "let ",
            "fn ",
            "dimension ",
            "unit ",
            //
            "sqrt(",
            "sqr(",
            "exp(",
            "ln(",
            "log(",
            "log10(",
            "log2(",
            "abs(",
            "round(",
            "floor(",
            "ceil(",
            "sin(",
            "cos(",
            "tan(",
            "asin(",
            "acos(",
            "atan(",
            "atan2(",
            "toCelsius(",
            "toKelvin(",
            "print(",
            "assert_eq(",
            "mean(",
            "minimum(",
            "maximum(",
            //
            "metric_prefixes",
            "binary_prefixes",
            "aliases",
            "aliases_short",
            //
            "gravity",
            "speed_of_light",
            //
            "Scalar",
            "Length",
            "Time",
            "Mass",
            //
            "meter",
            "second",
            "gram",
            //
            "micro",
            "milli",
            "centi",
            "deci",
            "kilo",
            "mega",
        ];

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
