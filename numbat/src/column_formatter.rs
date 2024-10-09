use unicode_width::UnicodeWidthStr;

use crate::markup as m;
use crate::markup::{FormatType, FormattedString, Markup, OutputType};

/// Do not show tables wider than this for readabilty reasons
const MAX_WIDTH: usize = 160;

pub struct ColumnFormatter {
    terminal_width: usize,
    padding: usize,
}

impl ColumnFormatter {
    pub fn new(terminal_width: usize) -> Self {
        Self {
            terminal_width: terminal_width.min(MAX_WIDTH),
            padding: 2,
        }
    }

    pub fn format<S: AsRef<str>>(
        &self,
        entries: impl IntoIterator<Item = S>,
        format: FormatType,
    ) -> Markup {
        let mut result = m::empty();

        let entries: Vec<_> = entries
            .into_iter()
            .map(|s| s.as_ref().to_string())
            .collect();

        if let Some(max_entry_width) = entries.iter().map(|s| s.width()).max() {
            let column_width = max_entry_width + self.padding;
            let min_num_columns = self.terminal_width / column_width;

            if min_num_columns < 1 {
                for entry in entries {
                    result +=
                        Markup::from(FormattedString(OutputType::Normal, format, entry.into()))
                            + m::whitespace(" ".repeat(self.padding));
                }
                return result;
            }

            for num_columns in min_num_columns..=self.terminal_width {
                let num_rows = entries.len().div_ceil(num_columns);

                let mut table: Vec<Vec<Option<&str>>> = vec![vec![None; num_columns]; num_rows];
                for (idx, entry) in entries.iter().enumerate() {
                    let row = idx % num_rows;
                    let col = idx / num_rows;

                    table[row][col] = Some(entry);
                }

                let column_widths: Vec<usize> = (0..num_columns)
                    .map(|c| {
                        (0..num_rows)
                            .map(|r| table[r][c].map(|e| e.width()).unwrap_or(0))
                            .max()
                            .unwrap_or(0)
                            + self.padding
                    })
                    .collect();

                if column_widths.iter().sum::<usize>() > self.terminal_width {
                    // Return result from previous iteration
                    return result;
                }
                result = m::empty();

                for row in table {
                    for (col, entry) in row.iter().enumerate() {
                        if let Some(entry) = entry {
                            let width = entry.width();
                            let whitespace_length = column_widths[col] - width;

                            result += Markup::from(FormattedString(
                                OutputType::Normal,
                                format,
                                entry.to_string().into(),
                            ));
                            result += m::whitespace(" ".repeat(whitespace_length));
                        } else {
                            break;
                        }
                    }
                    result += m::nl();
                }
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

        assert_snapshot!(format(80, elements), @r###"
        one  two  three  four  five  six  seven  
        "###);

        assert_snapshot!(format(42, elements), @r###"
        one  two  three  four  five  six  seven  
        "###);

        assert_snapshot!(format(21, elements), @r###"
        one    four  seven  
        two    five  
        three  six   
        "###);

        assert_snapshot!(format(20, elements), @r###"
        one    four  seven  
        two    five  
        three  six   
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

        assert_snapshot!(format(4, elements), @"one  two  three  four  five  six  seven  ");
    }

    assert_snapshot!(format(80, &["one"]), @"one");

    assert_snapshot!(format(80, &[]), @"");

    assert_snapshot!(format(12, &["aaaa", "bbbb", "cccc", "dddd"]), @r###"
    aaaa  cccc  
    bbbb  dddd  
    "###);
}
