use anyhow::Result;
use thiserror::Error;

#[derive(Error, Debug, PartialEq, Eq)]
pub enum Error {
    #[error("Unexpected character: '{0}'")]
    UnexpectedCharacter(char),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    LeftParen,
    RightParen,
    Number,
    Plus,
    Minus,
    Times,
    Arrow,
    EOF,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    kind: TokenKind,
    lexeme: String, // TODO: could be a &'str view into the input
    line: usize,
}

pub struct Tokenizer {
    input: Vec<char>,
    token_start: usize,
    current: usize,
    line: usize,
}

impl Tokenizer {
    pub fn new(input: &str) -> Self {
        Tokenizer {
            input: input.chars().collect(),
            token_start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn scan<'a>(&mut self) -> Result<Vec<Token>> {
        let mut tokens = vec![];
        while !self.at_end() {
            self.token_start = self.current;
            if let Some(token) = self.scan_single_token()? {
                tokens.push(token);
            }
        }

        tokens.push(Token {
            kind: TokenKind::EOF,
            lexeme: "".into(),
            line: self.line,
        });

        Ok(tokens)
    }

    fn scan_single_token(&mut self) -> Result<Option<Token>> {
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
            '*' => TokenKind::Times,
            '-' => {
                if self.match_char('>') {
                    TokenKind::Arrow
                } else {
                    TokenKind::Minus
                }
            }
            c => {
                return Err(Error::UnexpectedCharacter(c).into());
            }
        };

        Ok(Some(Token {
            kind,
            lexeme: self.input[self.token_start..self.current].iter().collect(),
            line: self.line,
        }))
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

    let tokenize = |input| {
        let mut tokenizer = Tokenizer::new(input);
        tokenizer.scan()
    };

    assert_eq!(
        tokenize("  12 + 34  ").unwrap(),
        token_stream(&[("12", Number), ("+", Plus), ("34", Number), ("", EOF)])
    );

    assert_eq!(
        tokenize("12 * (3 - 4)").unwrap(),
        token_stream(&[
            ("12", Number),
            ("*", Times),
            ("(", LeftParen),
            ("3", Number),
            ("-", Minus),
            ("4", Number),
            (")", RightParen),
            ("", EOF)
        ])
    );

    assert_eq!(
        tokenize("1 -> 2").unwrap(),
        token_stream(&[("1", Number), ("->", Arrow), ("2", Number), ("", EOF)])
    );

    assert!(tokenize("$").is_err());
}
