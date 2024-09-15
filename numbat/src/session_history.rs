use std::{
    fs,
    io::{self, Write as _},
    path::Path,
};

use crate::RuntimeError;

#[derive(Debug)]
pub struct SessionHistoryItem {
    pub input: String,
    pub errored: bool,
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
    pub fn push(&mut self, item: SessionHistoryItem) {
        self.0.push(item);
    }

    pub fn save(
        &self,
        dst: impl AsRef<Path>,
        options: SessionHistoryOptions,
    ) -> Result<(), RuntimeError> {
        let SessionHistoryOptions {
            include_err_lines,
            trim_lines,
        } = options;

        let dst = dst.as_ref();
        let err_fn = |_: io::Error| RuntimeError::FileWrite(dst.to_owned());

        let mut f = fs::File::create(dst).map_err(err_fn)?;
        for item in &self.0 {
            if item.errored && !include_err_lines {
                continue;
            }

            let input = if trim_lines {
                item.input.trim()
            } else {
                &item.input
            };

            writeln!(f, "{}", input).map_err(err_fn)?
        }
        Ok(())
    }
}
