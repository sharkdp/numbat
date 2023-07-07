use crate::span::Span;

use std::collections::HashMap;
use std::sync::OnceLock;
use thiserror::Error;

#[derive(Debug, Error, PartialEq, Eq)]
pub enum TokenizerErrorKind {
    #[error("Unexpected character: '{character}'")]
    UnexpectedCharacter { character: char },

    #[error("Unexpected character in negative exponent")]
    UnexpectedCharacterInNegativeExponent { character: Option<char> },

    #[error("Unexpected character in number literal: '{0}'")]
    UnexpectedCharacterInNumberLiteral(char),

    #[error("Unexpected character in scientific notation: '{0}'")]
    UnexpectedCharacterInScientificNotation(char),

    #[error("Unexpected character in identifier: '{0}'")]
    UnexpectedCharacterInIdentifier(char),

    #[error("Expected digit")]
    ExpectedDigit { character: Option<char> },

    #[error("Expected base-{base} digit")]
    ExpectedDigitInBase {
        base: usize,
        character: Option<char>,
    },

    #[error("Unterminated string")]
    UnterminatedString,
}

#[derive(Debug, Error, PartialEq, Eq)]
#[error("{kind}")]
pub struct TokenizerError {
    pub kind: TokenizerErrorKind,
    pub span: Span,
}

type Result<T> = std::result::Result<T, TokenizerError>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    // Brackets
    LeftParen,
    RightParen,
    LeftAngleBracket,
    RightAngleBracket,

    // Operators and special signs
    Plus,
    Minus,
    Multiply,
    Power,
    Divide,
    Per,
    Modulo,
    Comma,
    Arrow,
    Equal,
    Colon,
    ColonColon,
    PostfixApply,
    UnicodeExponent,
    At,
    Ellipsis,

    // Keywords
    Let,
    Fn,
    Dimension,
    Unit,
    Use,

    Long,
    Short,
    Both,
    None,

    // Procedure calls
    ProcedurePrint,
    ProcedureAssertEq,

    // Variable-length tokens
    Number,
    IntegerWithBase(usize),
    Identifier,
    String,

    // Other
    Newline,
    Eof,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String, // TODO(minor): could be a &'str view into the input
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

fn is_exponent_char(c: char) -> bool {
    matches!(c, '¹' | '²' | '³' | '⁴' | '⁵')
}

fn is_currency_char(c: char) -> bool {
    let c_u32 = c as u32;

    // See https://en.wikipedia.org/wiki/Currency_Symbols_(Unicode_block)
    (0x20A0..=0x20CF).contains(&c_u32) || c == '£' || c == '¥' || c == '$' || c == '฿'
}

fn is_identifier_char(c: char) -> bool {
    (c.is_alphanumeric() || c == '_' || is_currency_char(c)) && !is_exponent_char(c)
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

    fn consume_stream_of_digits(
        &mut self,
        at_least_one_digit: bool,
        disallow_dot_and_digits_after_stream: bool,
    ) -> Result<()> {
        if at_least_one_digit && !self.peek().map(|c| c.is_ascii_digit()).unwrap_or(false) {
            return Err(TokenizerError {
                kind: TokenizerErrorKind::ExpectedDigit {
                    character: self.peek(),
                },
                span: Span {
                    line: self.current_line,
                    position: self.current_position,
                    index: self.token_start_index,
                },
            });
        }

        while self.peek().map(|c| c.is_ascii_digit()).unwrap_or(false) {
            self.advance();
        }

        if disallow_dot_and_digits_after_stream
            && self
                .peek()
                .map(|c| c.is_ascii_digit() || c == '.')
                .unwrap_or(false)
        {
            return Err(TokenizerError {
                kind: TokenizerErrorKind::UnexpectedCharacterInNumberLiteral(self.peek().unwrap()),
                span: Span {
                    line: self.current_line,
                    position: self.current_position,
                    index: self.current_index,
                },
            });
        }

        Ok(())
    }

    fn scientific_notation(&mut self) -> Result<()> {
        if self
            .peek2()
            .map(|c| c.is_ascii_digit() || c == '+' || c == '-')
            .unwrap_or(false)
            && (self.match_char('e') || self.match_char('E'))
        {
            let _ = self.match_char('+') || self.match_char('-');

            self.consume_stream_of_digits(true, true)?;

            if self
                .peek()
                .map(|c| c.is_ascii_digit() || c == '.')
                .unwrap_or(false)
            {
                return Err(TokenizerError {
                    kind: TokenizerErrorKind::UnexpectedCharacterInScientificNotation(
                        self.peek().unwrap(),
                    ),
                    span: Span {
                        line: self.current_line,
                        position: self.current_position,
                        index: self.current_index,
                    },
                });
            }
        }

        Ok(())
    }

    fn scan_single_token(&mut self) -> Result<Option<Token>> {
        static KEYWORDS: OnceLock<HashMap<&'static str, TokenKind>> = OnceLock::new();
        let keywords = KEYWORDS.get_or_init(|| {
            let mut m = HashMap::new();
            m.insert("per", TokenKind::Per);
            m.insert("to", TokenKind::Arrow);
            m.insert("let", TokenKind::Let);
            m.insert("fn", TokenKind::Fn);
            m.insert("dimension", TokenKind::Dimension);
            m.insert("unit", TokenKind::Unit);
            m.insert("use", TokenKind::Use);
            m.insert("long", TokenKind::Long);
            m.insert("short", TokenKind::Short);
            m.insert("both", TokenKind::Both);
            m.insert("none", TokenKind::None);
            m.insert("print", TokenKind::ProcedurePrint);
            m.insert("assert_eq", TokenKind::ProcedureAssertEq);
            m
        });

        if self.peek() == Some('#') {
            // skip over comment until newline
            loop {
                match self.peek() {
                    None => return Ok(None),
                    Some('\n') => break,
                    _ => {
                        self.advance();
                    }
                }
            }
        }

        let current_char = self.advance();

        let tokenizer_error = |line, position, index, kind| -> Result<Option<Token>> {
            Err(TokenizerError {
                kind,
                span: Span {
                    line,
                    position,
                    index,
                },
            })
        };

        let kind = match current_char {
            '(' => TokenKind::LeftParen,
            ')' => TokenKind::RightParen,
            '<' => TokenKind::LeftAngleBracket,
            '>' => TokenKind::RightAngleBracket,
            '0' if self
                .peek()
                .map(|c| c == 'x' || c == 'o' || c == 'b')
                .unwrap_or(false) =>
            {
                let (base, is_digit_in_base): (_, Box<dyn Fn(char) -> bool>) =
                    match self.peek().unwrap() {
                        'x' => (16, Box::new(|c| c.is_ascii_hexdigit())),
                        'o' => (8, Box::new(|c| ('0'..='7').contains(&c))),
                        'b' => (2, Box::new(|c| c == '0' || c == '1')),
                        _ => unreachable!(),
                    };

                self.advance(); // skip over the x/o/b

                let mut has_advanced = false;
                while self.peek().map(&is_digit_in_base).unwrap_or(false) {
                    self.advance();
                    has_advanced = true;
                }

                if !has_advanced || self.peek().map(is_identifier_char).unwrap_or(false) {
                    return tokenizer_error(
                        self.current_line,
                        self.current_position,
                        self.token_start_index,
                        TokenizerErrorKind::ExpectedDigitInBase {
                            base,
                            character: self.peek(),
                        },
                    );
                }

                TokenKind::IntegerWithBase(base)
            }
            c if c.is_ascii_digit() => {
                self.consume_stream_of_digits(false, false)?;

                // decimal part
                if self.match_char('.') {
                    self.consume_stream_of_digits(false, true)?;
                }

                self.scientific_notation()?;

                TokenKind::Number
            }
            '.' => {
                self.consume_stream_of_digits(true, true)?;
                self.scientific_notation()?;

                TokenKind::Number
            }
            ' ' | '\t' | '\r' => {
                return Ok(None);
            }
            '\n' => TokenKind::Newline,
            '*' if self.match_char('*') => TokenKind::Power,
            '+' => TokenKind::Plus,
            '*' | '·' | '×' => TokenKind::Multiply,
            '/' if self.match_char('/') => TokenKind::PostfixApply,
            '/' => TokenKind::Divide,
            '÷' => TokenKind::Divide,
            '%' => TokenKind::Modulo,
            '^' => TokenKind::Power,
            ',' => TokenKind::Comma,
            '=' => TokenKind::Equal,
            ':' if self.match_char(':') => TokenKind::ColonColon,
            ':' => TokenKind::Colon,
            '@' => TokenKind::At,
            '→' | '➞' => TokenKind::Arrow,
            '-' if self.match_char('>') => TokenKind::Arrow,
            '-' => TokenKind::Minus,
            '⁻' => {
                let c = self.peek();
                if c.map(is_exponent_char).unwrap_or(false) {
                    self.advance();
                    TokenKind::UnicodeExponent
                } else {
                    return tokenizer_error(
                        self.current_line,
                        self.current_position,
                        self.current_index,
                        TokenizerErrorKind::UnexpectedCharacterInNegativeExponent { character: c },
                    );
                }
            }
            '¹' | '²' | '³' | '⁴' | '⁵' => TokenKind::UnicodeExponent,
            '°' => TokenKind::Identifier, // '°' is not an alphanumeric character, so we treat it as a special case here
            '"' => {
                while self.peek().map(|c| c != '"').unwrap_or(false) {
                    self.advance();
                }

                if self.match_char('"') {
                    TokenKind::String
                } else {
                    return tokenizer_error(
                        self.current_line,
                        self.current_position,
                        self.current_index,
                        TokenizerErrorKind::UnterminatedString,
                    );
                }
            }
            '…' => TokenKind::Ellipsis,
            c if is_identifier_char(c) => {
                while self.peek().map(is_identifier_char).unwrap_or(false) {
                    self.advance();
                }

                if self.peek().map(|c| c == '.').unwrap_or(false) {
                    return tokenizer_error(
                        self.current_line,
                        self.current_position,
                        self.current_index,
                        TokenizerErrorKind::UnexpectedCharacterInIdentifier(self.peek().unwrap()),
                    );
                }

                if let Some(kind) = keywords.get(self.lexeme().as_str()) {
                    *kind
                } else {
                    TokenKind::Identifier
                }
            }
            c => {
                return tokenizer_error(
                    self.current_line,
                    self.current_position - 1,
                    self.current_index - 1,
                    TokenizerErrorKind::UnexpectedCharacter { character: c },
                );
            }
        };

        let token = Some(Token {
            kind,
            lexeme: self.lexeme(),
            span: Span {
                line: self.current_line,
                position: self.token_start_position,
                index: self.token_start_index,
            },
        });

        if kind == TokenKind::Newline {
            self.current_line += 1;
            self.current_position = 1;
        }

        Ok(token)
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

    fn peek2(&self) -> Option<char> {
        self.input.get(self.current_index + 1).copied()
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
            kind: *kind,
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
fn test_tokenize_basic() {
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
        tokenize("1+2\n42").unwrap(),
        token_stream(&[
            ("1", Number, (1, 1, 0)),
            ("+", Plus, (1, 2, 1)),
            ("2", Number, (1, 3, 2)),
            ("\n", Newline, (1, 4, 3)),
            ("42", Number, (2, 1, 4)),
            ("", Eof, (2, 3, 6))
        ])
    );

    assert_eq!(
        tokenize("\"foo\"").unwrap(),
        token_stream(&[("\"foo\"", String, (1, 1, 0)), ("", Eof, (1, 6, 5))])
    );

    assert!(tokenize("~").is_err());
}

#[test]
fn test_is_currency_char() {
    assert!(is_currency_char('€'));
    assert!(is_currency_char('$'));
    assert!(is_currency_char('¥'));
    assert!(is_currency_char('£'));
    assert!(is_currency_char('฿'));
    assert!(is_currency_char('₿'));

    assert!(!is_currency_char('E'));
}
