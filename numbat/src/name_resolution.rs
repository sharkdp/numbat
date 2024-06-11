use std::collections::HashMap;

use thiserror::Error;

use crate::span::Span;

pub const LAST_RESULT_IDENTIFIERS: &[&str] = &["ans", "_"];

#[derive(Debug, Clone, Error, PartialEq, Eq)]
pub enum NameResolutionError {
    #[error("Identifier is already in use{}: '{conflicting_identifier}'.",
            if let Some(t) = .original_item_type { format!(" by the {t}") } else { "".to_owned() })]
    IdentifierClash {
        conflicting_identifier: String,
        conflict_span: Span,
        original_span: Span,
        original_item_type: Option<String>,
    },

    #[error("Reserved identifier")]
    ReservedIdentifier(Span),
}

#[derive(Debug, Clone, Default)]
pub struct Namespace {
    seen: HashMap<String, (String, Span)>,
}

impl Namespace {
    pub fn add_identifier_allow_override(
        &mut self,
        name: String,
        span: Span,
        item_type: String,
    ) -> Result<(), NameResolutionError> {
        self.add_impl(name, span, item_type, true)
    }

    pub fn add_identifier(
        &mut self,
        name: String,
        span: Span,
        item_type: String,
    ) -> Result<(), NameResolutionError> {
        self.add_impl(name, span, item_type, false)
    }

    pub fn has_identifier(&self, name: &str) -> bool {
        self.seen.contains_key(name)
    }

    fn add_impl(
        &mut self,
        name: String,
        span: Span,
        item_type: String,
        allow_override: bool,
    ) -> Result<(), NameResolutionError> {
        if let Some((original_item_type, original_span)) = self.seen.get(&name) {
            if original_span == &span {
                return Ok(());
            }

            if allow_override && original_item_type == &item_type {
                return Ok(());
            }

            return Err(NameResolutionError::IdentifierClash {
                conflicting_identifier: name,
                conflict_span: span,
                original_span: *original_span,
                original_item_type: Some(original_item_type.clone()),
            });
        }

        self.seen.insert(name, (item_type, span));

        Ok(())
    }
}
