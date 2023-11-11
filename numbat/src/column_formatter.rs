use itertools::Itertools;
use unicode_width::UnicodeWidthStr;

use crate::markup as m;
use crate::markup::{FormatType, FormattedString, Markup, OutputType};

pub struct ColumnFormatter {
    terminal_width: usize,
    min_padding: usize,
}

impl ColumnFormatter {
    pub fn new(terminal_width: usize) -> Self {
        Self {
            terminal_width,
            min_padding: 2,
        }
    }

    pub fn format<S: AsRef<str>>(
        &self,
        entries: impl IntoIterator<Item = S> + Clone,
        format: FormatType,
    ) -> Markup {
        let mut result = m::empty();

        if let Some(max_entry_width) = entries
            .clone()
            .into_iter()
            .map(|s| s.as_ref().width())
            .max()
        {
            let column_width = max_entry_width + self.min_padding as usize;
            let columns_per_row = (self.terminal_width as usize / column_width).max(1);

            for row_entries in &entries.into_iter().chunks(columns_per_row) {
                for entry in row_entries {
                    let width = entry.as_ref().width();
                    let whitespace_length = column_width - width;

                    result += Markup::from(FormattedString(
                        OutputType::Normal,
                        format.clone(),
                        entry.as_ref().into(),
                    ));
                    result += m::whitespace(" ".repeat(whitespace_length));
                }
                result += m::nl();
            }

            result
        } else {
            result
        }
    }
}

#[cfg(test)]
fn format(width: usize, entries: &[&str]) -> String {
    use crate::markup::{Formatter, PlainTextFormatter};

    let formatter = ColumnFormatter::new(width);
    PlainTextFormatter {}.format(&formatter.format(entries, FormatType::Text), false)
}

#[test]
fn test_column_formatter() {
    use insta::assert_snapshot;

    {
        let elements = &["one", "two", "three", "four", "five", "six", "seven"];

        assert_snapshot!(format(80, elements), @"one    two    three  four   five   six    seven  ");

        assert_snapshot!(format(42, elements), @r###"
        one    two    three  four   five   six    
        seven  
        "###);

        assert_snapshot!(format(21, elements), @r###"
        one    two    three  
        four   five   six    
        seven  
        "###);

        assert_snapshot!(format(20, elements), @r###"
        one    two    
        three  four   
        five   six    
        seven  
        "###);

        assert_snapshot!(format(10, elements), @r###"
        one    
        two    
        three  
        four   
        five   
        six    
        seven  
        "###);

        assert_snapshot!(format(4, elements), @r###"
        one    
        two    
        three  
        four   
        five   
        six    
        seven  
        "###);
    }
}
