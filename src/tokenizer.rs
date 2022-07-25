use crate::span::Span;

use once_cell::sync::OnceCell;
use std::collections::HashMap;
use thiserror::Error;

#[derive(Debug, Error, PartialEq, Eq)]
pub enum TokenizerError {
    #[error("Unexpected character: '{character}'")]
    UnexpectedCharacter { character: char, span: Span },
}

type Result<T> = std::result::Result<T, TokenizerError>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    // Operators
    LeftParen,
    RightParen,
    Plus,
    Minus,
    Multiply,
    Divide,
    Comma,
    Arrow,

    // Variable-length tokens
    Number,
    Identifier,

    // End of file
    Eof,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String, // TODO: could be a &'str view into the input
    pub span: Span,
}

struct Tokenizer {
    input: Vec<char>,

    token_start_index: usize,
    token_start_position: usize,

    current_index: usize,
    current_line: usize,
    current_position: usize,
}

fn is_identifier_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

impl Tokenizer {
    fn new(input: &str) -> Self {
        Tokenizer {
            input: input.chars().collect(),
            token_start_index: 0,
            token_start_position: 0,
            current_index: 0,
            current_position: 1,
            current_line: 1,
        }
    }

    fn scan(&mut self) -> Result<Vec<Token>> {
        let mut tokens = vec![];
        while !self.at_end() {
            self.token_start_index = self.current_index;
            self.token_start_position = self.current_position;
            if let Some(token) = self.scan_single_token()? {
                tokens.push(token);
            }
        }

        tokens.push(Token {
            kind: TokenKind::Eof,
            lexeme: "".into(),
            span: Span {
                line: self.current_line,
                position: self.current_position,
                index: self.current_index,
            },
        });

        Ok(tokens)
    }

    fn scan_single_token(&mut self) -> Result<Option<Token>> {
        static KEYWORDS: OnceCell<HashMap<&'static str, TokenKind>> = OnceCell::new();
        let keywords = KEYWORDS.get_or_init(|| {
            let mut m = HashMap::new();
            m.insert("per", TokenKind::Divide);
            m.insert("to", TokenKind::Arrow);
            m
        });

        let current_position = self.current_position;
        let current_char = self.advance();

        let kind = match current_char {
            '(' => TokenKind::LeftParen,
            ')' => TokenKind::RightParen,
            c if c.is_digit(10) => {
                while let Some(c) = self.peek() {
                    if c.is_digit(10) {
                        self.advance();
                    } else {
                        break;
                    }
                }
                TokenKind::Number
            }
            ' ' | '\t' | '\r' => {
                return Ok(None);
            }
            '\n' => {
                self.current_line += 1;
                self.current_position = 1;
                return Ok(None);
            }
            '+' => TokenKind::Plus,
            '*' | '·' | '×' => TokenKind::Multiply,
            '/' | '÷' => TokenKind::Divide,
            ',' => TokenKind::Comma,
            '→' | '➞' => TokenKind::Arrow,
            '-' => {
                if self.match_char('>') {
                    TokenKind::Arrow
                } else {
                    TokenKind::Minus
                }
            }
            '°' => TokenKind::Identifier, // '°' is not an alphanumeric character, so we treat it as a special case here
            c if is_identifier_char(c) => {
                while self.peek().map(is_identifier_char).unwrap_or(false) {
                    self.advance();
                }

                if let Some(kind) = keywords.get(self.lexeme().as_str()) {
                    *kind
                } else {
                    TokenKind::Identifier
                }
            }
            c => {
                return Err(TokenizerError::UnexpectedCharacter {
                    character: c,
                    span: Span {
                        line: self.current_line,
                        position: current_position,
                        index: self.token_start_index,
                    },
                });
            }
        };

        Ok(Some(Token {
            kind,
            lexeme: self.lexeme(),
            span: Span {
                line: self.current_line,
                position: self.token_start_position,
                index: self.token_start_index,
            },
        }))
    }

    fn lexeme(&self) -> String {
        self.input[self.token_start_index..self.current_index]
            .iter()
            .collect()
    }

    fn advance(&mut self) -> char {
        let c = self.input[self.current_index];
        self.current_index += 1;
        self.current_position += 1;
        c
    }

    fn peek(&self) -> Option<char> {
        self.input.get(self.current_index).copied()
    }

    fn match_char(&mut self, c: char) -> bool {
        if self.peek() == Some(c) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn at_end(&self) -> bool {
        self.current_index >= self.input.len()
    }
}

pub fn tokenize(input: &str) -> Result<Vec<Token>> {
    let mut tokenizer = Tokenizer::new(input);
    tokenizer.scan()
}

#[cfg(test)]
fn token_stream(input: &[(&str, TokenKind, (usize, usize, usize))]) -> Vec<Token> {
    input
        .iter()
        .map(|(lexeme, kind, (line, position, index))| Token {
            kind: kind.clone(),
            lexeme: lexeme.to_string(),
            span: Span {
                line: *line,
                position: *position,
                index: *index,
            },
        })
        .collect()
}

#[test]
fn tokenize_basic() {
    use TokenKind::*;

    assert_eq!(
        tokenize("  12 + 34  ").unwrap(),
        token_stream(&[
            ("12", Number, (1, 3, 2)),
            ("+", Plus, (1, 6, 5)),
            ("34", Number, (1, 8, 7)),
            ("", Eof, (1, 12, 11))
        ])
    );

    assert_eq!(
        tokenize("1 2").unwrap(),
        token_stream(&[
            ("1", Number, (1, 1, 0)),
            ("2", Number, (1, 3, 2)),
            ("", Eof, (1, 4, 3))
        ])
    );

    assert_eq!(
        tokenize("12 × (3 - 4)").unwrap(),
        token_stream(&[
            ("12", Number, (1, 1, 0)),
            ("×", Multiply, (1, 4, 3)),
            ("(", LeftParen, (1, 6, 5)),
            ("3", Number, (1, 7, 6)),
            ("-", Minus, (1, 9, 8)),
            ("4", Number, (1, 11, 10)),
            (")", RightParen, (1, 12, 11)),
            ("", Eof, (1, 13, 12))
        ])
    );

    assert_eq!(
        tokenize("foo to bar").unwrap(),
        token_stream(&[
            ("foo", Identifier, (1, 1, 0)),
            ("to", Arrow, (1, 5, 4)),
            ("bar", Identifier, (1, 8, 7)),
            ("", Eof, (1, 11, 10))
        ])
    );

    assert_eq!(
        tokenize("1 -> 2").unwrap(),
        token_stream(&[
            ("1", Number, (1, 1, 0)),
            ("->", Arrow, (1, 3, 2)),
            ("2", Number, (1, 6, 5)),
            ("", Eof, (1, 7, 6))
        ])
    );

    assert_eq!(
        tokenize("45°").unwrap(),
        token_stream(&[
            ("45", Number, (1, 1, 0)),
            ("°", Identifier, (1, 3, 2)),
            ("", Eof, (1, 4, 3))
        ])
    );

    assert_eq!(
        tokenize("1+\n  2").unwrap(),
        token_stream(&[
            ("1", Number, (1, 1, 0)),
            ("+", Plus, (1, 2, 1)),
            ("2", Number, (2, 3, 5)),
            ("", Eof, (2, 4, 6))
        ])
    );

    assert!(tokenize("$").is_err());
}
