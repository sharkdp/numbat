use once_cell::sync::OnceCell;
use std::collections::HashMap;
use thiserror::Error;

#[derive(Debug, Error, PartialEq, Eq)]
pub enum TokenizerError {
    #[error("Unexpected character: '{0}'")]
    UnexpectedCharacter(char),
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
    pub line: usize,
}

struct Tokenizer {
    input: Vec<char>,
    token_start: usize,
    current: usize,
    line: usize,
}

fn is_identifier_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

impl Tokenizer {
    fn new(input: &str) -> Self {
        Tokenizer {
            input: input.chars().collect(),
            token_start: 0,
            current: 0,
            line: 1,
        }
    }

    fn scan(&mut self) -> Result<Vec<Token>> {
        let mut tokens = vec![];
        while !self.at_end() {
            self.token_start = self.current;
            if let Some(token) = self.scan_single_token()? {
                tokens.push(token);
            }
        }

        tokens.push(Token {
            kind: TokenKind::Eof,
            lexeme: "".into(),
            line: self.line,
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
                self.line += 1;
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
                return Err(TokenizerError::UnexpectedCharacter(c));
            }
        };

        Ok(Some(Token {
            kind,
            lexeme: self.lexeme(),
            line: self.line,
        }))
    }

    fn lexeme(&self) -> String {
        self.input[self.token_start..self.current].iter().collect()
    }

    fn advance(&mut self) -> char {
        let c = self.input[self.current];
        self.current += 1;
        c
    }

    fn peek(&self) -> Option<char> {
        self.input.get(self.current).copied()
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
        self.current >= self.input.len()
    }
}

pub fn tokenize(input: &str) -> Result<Vec<Token>> {
    let mut tokenizer = Tokenizer::new(input);
    tokenizer.scan()
}

#[cfg(test)]
fn token_stream(input: &[(&str, TokenKind)]) -> Vec<Token> {
    input
        .iter()
        .map(|(lexeme, kind)| Token {
            kind: kind.clone(),
            lexeme: lexeme.to_string(),
            line: 1,
        })
        .collect()
}

#[test]
fn tokenize_basic() {
    use TokenKind::*;

    assert_eq!(
        tokenize("  12 + 34  ").unwrap(),
        token_stream(&[("12", Number), ("+", Plus), ("34", Number), ("", Eof)])
    );

    assert_eq!(
        tokenize("1 2").unwrap(),
        token_stream(&[("1", Number), ("2", Number), ("", Eof)])
    );

    assert_eq!(
        tokenize("12 × (3 - 4)").unwrap(),
        token_stream(&[
            ("12", Number),
            ("×", Multiply),
            ("(", LeftParen),
            ("3", Number),
            ("-", Minus),
            ("4", Number),
            (")", RightParen),
            ("", Eof)
        ])
    );

    assert_eq!(
        tokenize("foo to bar").unwrap(),
        token_stream(&[
            ("foo", Identifier),
            ("to", Arrow),
            ("bar", Identifier),
            ("", Eof)
        ])
    );

    assert_eq!(
        tokenize("1 -> 2").unwrap(),
        token_stream(&[("1", Number), ("->", Arrow), ("2", Number), ("", Eof)])
    );

    assert_eq!(
        tokenize("45°").unwrap(),
        token_stream(&[("45", Number), ("°", Identifier), ("", Eof)])
    );

    assert!(tokenize("$").is_err());
}
