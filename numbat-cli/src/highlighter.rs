use colored::Colorize;
use numbat::keywords::KEYWORDS;
use numbat::{markup, Context};
use rustyline::{highlight::Highlighter, CompletionType};

use std::{
    borrow::Cow,
    sync::{Arc, Mutex},
};

use crate::ansi_formatter::ansi_format;

pub struct NumbatHighlighter {
    pub context: Arc<Mutex<Context>>,
}

impl Highlighter for NumbatHighlighter {
    fn highlight<'l>(&self, line: &'l str, _pos: usize) -> Cow<'l, str> {
        Cow::Borrowed(line)
    }

    fn highlight_prompt<'b, 's: 'b, 'p: 'b>(
        &self,
        prompt: &'p str,
        _default: bool,
    ) -> Cow<'b, str> {
        Cow::Owned(format!("{}", prompt.bold()))
    }

    fn highlight_hint<'h>(&self, hint: &'h str) -> Cow<'h, str> {
        Cow::Borrowed(hint)
    }

    fn highlight_candidate<'c>(
        &self,
        candidate: &'c str,
        _completion: CompletionType,
    ) -> Cow<'c, str> {
        let ctx = self.context.lock().unwrap();
        if ctx.variable_names().any(|n| n == candidate)
            || ctx.function_names().any(|n| format!("{n}(") == candidate)
        {
            Cow::Owned(ansi_format(&markup::identifier(candidate), false))
        } else if ctx
            .unit_names()
            .iter()
            .any(|un| un.iter().any(|n| n == candidate))
        {
            Cow::Owned(ansi_format(&markup::unit(candidate), false))
        } else if ctx.dimension_names().iter().any(|n| n == candidate) {
            Cow::Owned(ansi_format(&markup::type_identifier(candidate), false))
        } else if KEYWORDS.iter().any(|k| k == &candidate) {
            Cow::Owned(ansi_format(&markup::keyword(candidate), false))
        } else {
            Cow::Borrowed(candidate)
        }
    }
}
