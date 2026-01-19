use compact_str::CompactString;
use thiserror::Error;

use crate::ast::Visibility;
use crate::{span::Span, typechecker::map_stack::MapStack};

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

/// Information about an identifier in the namespace.
#[derive(Debug, Clone)]
struct NamespaceEntry {
    item_type: CompactString,
    span: Span,
    visibility: Visibility,
}

#[derive(Debug, Clone, Default)]
pub struct Namespace {
    seen: MapStack<CompactString, NamespaceEntry>,
}

impl Namespace {
    pub(crate) fn save(&mut self) {
        self.seen.save()
    }

    pub(crate) fn restore(&mut self) {
        self.seen.restore()
    }

    pub fn add_identifier_allow_override(
        &mut self,
        name: CompactString,
        span: Span,
        item_type: CompactString,
        visibility: Visibility,
        current_source_id: usize,
    ) -> Result<(), NameResolutionError> {
        self.add_impl(name, span, item_type, visibility, current_source_id, true)
    }

    pub fn add_identifier(
        &mut self,
        name: CompactString,
        span: Span,
        item_type: CompactString,
        visibility: Visibility,
        current_source_id: usize,
    ) -> Result<(), NameResolutionError> {
        self.add_impl(name, span, item_type, visibility, current_source_id, false)
    }

    pub fn has_identifier(&self, name: &str) -> bool {
        self.seen.contains_key(name)
    }

    /// Check if an identifier exists and is visible from the given source.
    /// Private identifiers from different sources are not considered visible.
    #[allow(dead_code)]
    pub fn has_visible_identifier(&self, name: &str, current_source_id: usize) -> bool {
        if let Some(entry) = self.seen.get(name) {
            // Visible if public OR from the same source
            entry.visibility == Visibility::Public || entry.span.code_source_id == current_source_id
        } else {
            false
        }
    }

    fn add_impl(
        &mut self,
        name: CompactString,
        span: Span,
        item_type: CompactString,
        visibility: Visibility,
        current_source_id: usize,
        allow_override: bool,
    ) -> Result<(), NameResolutionError> {
        if let Some(existing) = self.seen.get(&name) {
            // Same span means same definition, no conflict
            if existing.span == span {
                return Ok(());
            }

            // Check if the existing entry is visible from the current source.
            // Private entries from different sources don't cause conflicts.
            let existing_is_visible = existing.visibility == Visibility::Public
                || existing.span.code_source_id == current_source_id;

            if !existing_is_visible {
                // The existing entry is private and from a different source.
                // We can shadow it with our new definition.
                self.seen.insert(
                    name,
                    NamespaceEntry {
                        item_type,
                        span,
                        visibility,
                    },
                );
                return Ok(());
            }

            // The existing entry is visible, check for override permission
            if allow_override && existing.item_type == item_type {
                return Ok(());
            }

            return Err(NameResolutionError::IdentifierClash {
                conflicting_identifier: name.to_string(),
                conflict_span: span,
                original_span: existing.span,
                original_item_type: Some(existing.item_type.to_string()),
            });
        }

        self.seen.insert(
            name,
            NamespaceEntry {
                item_type,
                span,
                visibility,
            },
        );

        Ok(())
    }
}
