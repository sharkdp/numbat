use compact_str::CompactString;
use std::{fs, io, path::Path};

use crate::interpreter::RuntimeErrorKind;

pub type ParseEvaluationResult = Result<(), ()>;

#[derive(Debug)]
struct SessionHistoryItem {
    input: CompactString,
    result: ParseEvaluationResult,
}

#[derive(Default)]
pub struct SessionHistory(Vec<SessionHistoryItem>);

impl SessionHistory {
    pub fn new() -> Self {
        Self::default()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct SessionHistoryOptions {
    pub include_err_lines: bool,
    pub trim_lines: bool,
}

impl SessionHistory {
    pub fn push(&mut self, input: CompactString, result: ParseEvaluationResult) {
        self.0.push(SessionHistoryItem { input, result });
    }

    fn save_inner(
        &self,
        mut w: impl io::Write,
        options: SessionHistoryOptions,
        err_fn: impl Fn(io::Error) -> RuntimeErrorKind,
    ) -> Result<(), Box<RuntimeErrorKind>> {
        let SessionHistoryOptions {
            include_err_lines,
            trim_lines,
        } = options;

        for item in &self.0 {
            if item.result.is_err() && !include_err_lines {
                continue;
            }

            let input = if trim_lines {
                item.input.trim()
            } else {
                &item.input
            };

            writeln!(w, "{input}").map_err(&err_fn)?
        }
        Ok(())
    }

    pub fn save(
        &self,
        dst: impl AsRef<Path>,
        options: SessionHistoryOptions,
    ) -> Result<(), Box<RuntimeErrorKind>> {
        let dst = dst.as_ref();
        let err_fn = |_: io::Error| RuntimeErrorKind::FileWrite(dst.to_owned());

        let f = fs::File::create(dst).map_err(err_fn)?;
        self.save_inner(f, options, err_fn)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::io::Cursor;

    #[test]
    fn test() {
        let mut sh = SessionHistory::new();

        // arbitrary non-ascii characters
        sh.push(CompactString::const_new("  a→  "), Ok(()));
        sh.push(CompactString::const_new("  b × c  "), Err(()));
        sh.push(CompactString::const_new("  d ♔ e ⚀ f  "), Err(()));
        sh.push(CompactString::const_new("  g ☼ h ▶︎ i ❖ j  "), Ok(()));

        let test_cases = [
            (
                SessionHistoryOptions {
                    include_err_lines: false,
                    trim_lines: false,
                },
                "  a→  \n  g ☼ h ▶︎ i ❖ j  \n",
            ),
            (
                SessionHistoryOptions {
                    include_err_lines: true,
                    trim_lines: false,
                },
                "  a→  \n  b × c  \n  d ♔ e ⚀ f  \n  g ☼ h ▶︎ i ❖ j  \n",
            ),
            (
                SessionHistoryOptions {
                    include_err_lines: false,
                    trim_lines: true,
                },
                "a→\ng ☼ h ▶︎ i ❖ j\n",
            ),
            (
                SessionHistoryOptions {
                    include_err_lines: true,
                    trim_lines: true,
                },
                "a→\nb × c\nd ♔ e ⚀ f\ng ☼ h ▶︎ i ❖ j\n",
            ),
        ];

        for (options, expected) in test_cases {
            let mut s = Cursor::new(Vec::<u8>::new());
            sh.save_inner(&mut s, options, |_| unreachable!()).unwrap();
            assert_eq!(expected, String::from_utf8(s.into_inner()).unwrap())
        }
    }

    #[test]
    fn test_error() {
        let sh = SessionHistory::new();
        assert!(
            sh.save(
                ".", // one place we know writing will fail
                SessionHistoryOptions {
                    include_err_lines: false,
                    trim_lines: false
                }
            )
            .is_err()
        )
    }
}
