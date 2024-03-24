use crate::span::{SourceCodePositition, Span};

use std::collections::HashMap;
use std::sync::OnceLock;
use thiserror::Error;

#[derive(Debug, Clone, Error, PartialEq, Eq)]
pub enum TokenizerErrorKind {
    #[error("Unexpected character: '{character}'")]
    UnexpectedCharacter { character: char },

    #[error("Unexpected character in negative exponent")]
    UnexpectedCharacterInNegativeExponent { character: Option<char> },

    #[error("Unexpected character in number literal: '{0}'")]
    UnexpectedCharacterInNumberLiteral(char),

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

    #[error("Unterminated string interpolation")]
    UnterminatedStringInterpolation,

    #[error("Unexpected '{{' inside string interpolation")]
    UnexpectedCurlyInInterpolation,
}

#[derive(Debug, Error, PartialEq, Eq)]
#[error("{kind}")]
pub struct TokenizerError {
    pub kind: TokenizerErrorKind,
    pub span: Span,
}

type Result<T, E = TokenizerError> = std::result::Result<T, E>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    // Brackets
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,

    // Operators and special signs
    Plus,
    Minus,
    Multiply,
    Power,
    Divide,
    Per,
    Comma,
    Arrow,
    Equal,
    Colon,
    DoubleColon,
    PostfixApply,
    UnicodeExponent,
    At,
    Ellipsis,
    ExclamationMark,
    EqualEqual,
    NotEqual,
    LessThan,
    GreaterThan,
    LessOrEqual,
    GreaterOrEqual,
    LogicalAnd,
    LogicalOr,

    // Keywords
    Let,
    Fn, // 'fn'
    Dimension,
    Unit,
    Use,

    To,

    Bool,
    True,
    False,
    If,
    Then,
    Else,

    String,
    DateTime,

    NaN,
    Inf,

    CapitalFn, // 'Fn'

    Long,
    Short,
    Both,
    None,

    // Procedure calls
    ProcedurePrint,
    ProcedureAssert,
    ProcedureAssertEq,
    ProcedureType,

    // Variable-length tokens
    Number,
    IntegerWithBase(usize),
    Identifier,

    // A normal string without interpolation: `"hello world"`
    StringFixed,
    // A part of a string which *starts* an interpolation: `"foo = {`
    StringInterpolationStart,
    // A part of a string between two interpolations: `}, and bar = {`
    StringInterpolationMiddle,
    // A part of a string which ends an interpolation: `}."`
    StringInterpolationEnd,

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

fn is_exponent_char(c: char) -> bool {
    matches!(c, '¹' | '²' | '³' | '⁴' | '⁵' | '⁶' | '⁷' | '⁸' | '⁹')
}

fn is_numerical_fraction_char(c: char) -> bool {
    matches!(
        c,
        '¼' | '½'
            | '¾'
            | '⅐'
            | '⅑'
            | '⅒'
            | '⅓'
            | '⅔'
            | '⅕'
            | '⅖'
            | '⅗'
            | '⅘'
            | '⅙'
            | '⅚'
            | '⅛'
            | '⅜'
            | '⅝'
            | '⅞'
    )
}

fn is_currency_char(c: char) -> bool {
    let c_u32 = c as u32;

    // See https://en.wikipedia.org/wiki/Currency_Symbols_(Unicode_block)
    (0x20A0..=0x20CF).contains(&c_u32) || c == '£' || c == '¥' || c == '$' || c == '฿'
}

fn is_other_allowed_identifier_char(c: char) -> bool {
    c == '%'
}

fn is_subscript_char(c: char) -> bool {
    let c_u32 = c as u32;

    // See https://en.wikipedia.org/wiki/Unicode_subscripts_and_superscripts#Superscripts_and_subscripts_block
    (0x2080..=0x209CF).contains(&c_u32)
}

fn is_identifier_start(c: char) -> bool {
    unicode_ident::is_xid_start(c)
        || is_numerical_fraction_char(c)
        || is_currency_char(c)
        || is_other_allowed_identifier_char(c)
        || c == '°'
        || c == '_'
}

fn is_identifier_continue(c: char) -> bool {
    (unicode_ident::is_xid_continue(c)
        || is_subscript_char(c)
        || is_currency_char(c)
        || is_other_allowed_identifier_char(c))
        && !is_exponent_char(c)
        && c != '·'
}

/// When scanning a string interpolation like `"foo = {foo}, and bar = {bar}."`,
/// the tokenizer needs to keep track of where it currently is, because we allow
/// for (almost) arbitrary expressions inside the {…} part.
enum InterpolationState {
    /// We are not inside curly braces.
    Outside,
    /// We are currently scanning the inner part of an interpolation.
    Inside,
}

impl InterpolationState {
    fn is_inside(&self) -> bool {
        matches!(self, InterpolationState::Inside)
    }
}

struct Tokenizer {
    input: Vec<char>,
    current: SourceCodePositition,
    last: SourceCodePositition,
    token_start: SourceCodePositition,
    current_index: usize,
    token_start_index: usize,
    code_source_id: usize,

    // Special fields / state for parsing string interpolations
    string_start: SourceCodePositition,
    interpolation_start: SourceCodePositition,
    interpolation_state: InterpolationState,
}

impl Tokenizer {
    fn new(input: &str, code_source_id: usize) -> Self {
        Tokenizer {
            input: input.chars().collect(),
            current: SourceCodePositition::start(),
            last: SourceCodePositition::start(),
            token_start: SourceCodePositition::start(),
            current_index: 0,
            token_start_index: 0,
            code_source_id,
            string_start: SourceCodePositition::start(),
            interpolation_start: SourceCodePositition::start(),
            interpolation_state: InterpolationState::Outside,
        }
    }

    fn scan(&mut self) -> Result<Vec<Token>> {
        let mut tokens = vec![];
        while !self.at_end() {
            self.token_start = self.current;
            self.token_start_index = self.current_index;
            if let Some(token) = self.scan_single_token()? {
                tokens.push(token);
            }
        }

        tokens.push(Token {
            kind: TokenKind::Eof,
            lexeme: "".into(),
            span: self.current.single_character_span(self.code_source_id),
        });

        Ok(tokens)
    }

    fn consume_stream_of_digits(
        &mut self,
        at_least_one_digit: bool,
        disallow_leading_underscore: bool,
        disallow_dot_after_stream: bool,
    ) -> Result<()> {
        if at_least_one_digit && !self.peek().map(|c| c.is_ascii_digit()).unwrap_or(false) {
            return Err(TokenizerError {
                kind: TokenizerErrorKind::ExpectedDigit {
                    character: self.peek(),
                },
                span: self.current.single_character_span(self.code_source_id),
            });
        }

        // Make sure we don't start with an underscore
        if disallow_leading_underscore && self.peek().map(|c| c == '_').unwrap_or(false) {
            return Err(TokenizerError {
                kind: TokenizerErrorKind::UnexpectedCharacterInNumberLiteral(self.peek().unwrap()),
                span: self.current.single_character_span(self.code_source_id),
            });
        }

        let mut last_char = None;
        while self
            .peek()
            .map(|c| c.is_ascii_digit() || c == '_')
            .unwrap_or(false)
        {
            last_char = Some(self.advance());
        }

        // Make sure we don't end with an underscore
        if last_char == Some('_') {
            return Err(TokenizerError {
                kind: TokenizerErrorKind::UnexpectedCharacterInNumberLiteral('_'),
                span: self.last.single_character_span(self.code_source_id),
            });
        }

        if disallow_dot_after_stream && self.peek().map(|c| c == '.').unwrap_or(false) {
            return Err(TokenizerError {
                kind: TokenizerErrorKind::UnexpectedCharacterInNumberLiteral(self.peek().unwrap()),
                span: self.current.single_character_span(self.code_source_id),
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

            self.consume_stream_of_digits(true, true, true)?;
        }

        Ok(())
    }

    fn scan_single_token(&mut self) -> Result<Option<Token>> {
        static KEYWORDS: OnceLock<HashMap<&'static str, TokenKind>> = OnceLock::new();
        let keywords = KEYWORDS.get_or_init(|| {
            let mut m = HashMap::new();
            m.insert("per", TokenKind::Per);
            m.insert("to", TokenKind::To);
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
            m.insert("assert", TokenKind::ProcedureAssert);
            m.insert("assert_eq", TokenKind::ProcedureAssertEq);
            m.insert("type", TokenKind::ProcedureType);
            m.insert("Bool", TokenKind::Bool);
            m.insert("true", TokenKind::True);
            m.insert("false", TokenKind::False);
            m.insert("if", TokenKind::If);
            m.insert("then", TokenKind::Then);
            m.insert("else", TokenKind::Else);
            m.insert("String", TokenKind::String);
            m.insert("DateTime", TokenKind::DateTime);
            m.insert("Fn", TokenKind::CapitalFn);
            m.insert("NaN", TokenKind::NaN);
            m.insert("inf", TokenKind::Inf);
            // Keep this list in sync with keywords::KEYWORDS!
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

        let code_source_id = self.code_source_id;
        let tokenizer_error = |position: &SourceCodePositition, kind| -> Result<Option<Token>> {
            Err(TokenizerError {
                kind,
                span: position.single_character_span(code_source_id),
            })
        };

        let kind = match current_char {
            '(' => TokenKind::LeftParen,
            ')' => TokenKind::RightParen,
            '[' => TokenKind::LeftBracket,
            ']' => TokenKind::RightBracket,
            '≤' => TokenKind::LessOrEqual,
            '<' if self.match_char('=') => TokenKind::LessOrEqual,
            '<' => TokenKind::LessThan,
            '≥' => TokenKind::GreaterOrEqual,
            '>' if self.match_char('=') => TokenKind::GreaterOrEqual,
            '>' => TokenKind::GreaterThan,
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

                // If the first character is not a digits, that's an error.
                if !self.peek().map(&is_digit_in_base).unwrap_or(false) {
                    return tokenizer_error(
                        &self.current,
                        TokenizerErrorKind::ExpectedDigitInBase {
                            base,
                            character: self.peek(),
                        },
                    );
                }

                let mut last_char = None;

                while self
                    .peek()
                    .map(|c| is_digit_in_base(c) || c == '_')
                    .unwrap_or(false)
                {
                    last_char = self.peek();
                    self.advance();
                }

                // Numeric literal should not end with a `_` either.
                if last_char == Some('_')
                    || self
                        .peek()
                        .map(|c| is_identifier_continue(c) || c == '.')
                        .unwrap_or(false)
                {
                    return tokenizer_error(
                        &self.current,
                        TokenizerErrorKind::ExpectedDigitInBase {
                            base,
                            character: self.peek(),
                        },
                    );
                }

                TokenKind::IntegerWithBase(base)
            }
            c if c.is_ascii_digit() => {
                self.consume_stream_of_digits(false, false, false)?;

                // decimal part
                if self.match_char('.') {
                    self.consume_stream_of_digits(false, true, true)?;
                }

                self.scientific_notation()?;

                TokenKind::Number
            }
            '.' => {
                self.consume_stream_of_digits(true, true, true)?;
                self.scientific_notation()?;

                TokenKind::Number
            }
            ' ' | '\t' | '\r' => {
                return Ok(None);
            }
            '\n' => TokenKind::Newline,
            '&' if self.match_char('&') => TokenKind::LogicalAnd,
            '|' if self.match_char('|') => TokenKind::LogicalOr,
            '*' if self.match_char('*') => TokenKind::Power,
            '+' => TokenKind::Plus,
            '*' | '·' | '⋅' | '×' => TokenKind::Multiply,
            '/' if self.match_char('/') => TokenKind::PostfixApply,
            '/' => TokenKind::Divide,
            '÷' => TokenKind::Divide,
            '^' => TokenKind::Power,
            ',' => TokenKind::Comma,
            '⩵' => TokenKind::EqualEqual,
            '=' if self.match_char('=') => TokenKind::EqualEqual,
            '=' => TokenKind::Equal,
            ':' if self.match_char(':') => TokenKind::DoubleColon,
            ':' => TokenKind::Colon,
            '@' => TokenKind::At,
            '→' | '➞' => TokenKind::Arrow,
            '-' if self.match_char('>') => TokenKind::Arrow,
            '-' | '−' => TokenKind::Minus,
            '≠' => TokenKind::NotEqual,
            '!' if self.match_char('=') => TokenKind::NotEqual,
            '!' => TokenKind::ExclamationMark,
            '⁻' => {
                let c = self.peek();
                if c.map(is_exponent_char).unwrap_or(false) {
                    self.advance();
                    TokenKind::UnicodeExponent
                } else {
                    return tokenizer_error(
                        &self.current,
                        TokenizerErrorKind::UnexpectedCharacterInNegativeExponent { character: c },
                    );
                }
            }
            '¹' | '²' | '³' | '⁴' | '⁵' | '⁶' | '⁷' | '⁸' | '⁹' => {
                TokenKind::UnicodeExponent
            }
            '"' => match self.interpolation_state {
                InterpolationState::Outside => {
                    self.string_start = self.token_start;

                    while self.peek().map(|c| c != '"' && c != '{').unwrap_or(false) {
                        self.advance();
                    }

                    if self.match_char('"') {
                        TokenKind::StringFixed
                    } else if self.match_char('{') {
                        self.interpolation_state = InterpolationState::Inside;
                        self.interpolation_start = self.last;
                        TokenKind::StringInterpolationStart
                    } else {
                        return Err(TokenizerError {
                            kind: TokenizerErrorKind::UnterminatedString,
                            span: Span {
                                start: self.token_start,
                                end: self.current,
                                code_source_id: self.code_source_id,
                            },
                        });
                    }
                }
                InterpolationState::Inside => {
                    return Err(TokenizerError {
                        kind: TokenizerErrorKind::UnterminatedStringInterpolation,
                        span: Span {
                            start: self.interpolation_start,
                            end: self.last,
                            code_source_id: self.code_source_id,
                        },
                    });
                }
            },
            '}' if self.interpolation_state.is_inside() => {
                while self.peek().map(|c| c != '"' && c != '{').unwrap_or(false) {
                    self.advance();
                }

                if self.match_char('"') {
                    self.interpolation_state = InterpolationState::Outside;
                    TokenKind::StringInterpolationEnd
                } else if self.match_char('{') {
                    self.interpolation_start = self.last;
                    TokenKind::StringInterpolationMiddle
                } else {
                    return Err(TokenizerError {
                        kind: TokenizerErrorKind::UnterminatedString,
                        span: Span {
                            start: self.string_start,
                            end: self.current,
                            code_source_id: self.code_source_id,
                        },
                    });
                }
            }
            '{' if self.interpolation_state.is_inside() => {
                return Err(TokenizerError {
                    kind: TokenizerErrorKind::UnexpectedCurlyInInterpolation,
                    span: self.last.single_character_span(code_source_id),
                });
            }
            '…' => TokenKind::Ellipsis,
            c if is_identifier_start(c) => {
                while self.peek().map(is_identifier_continue).unwrap_or(false) {
                    self.advance();
                }

                if self.peek().map(|c| c == '.').unwrap_or(false) {
                    return tokenizer_error(
                        &self.current,
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
                    &self.token_start,
                    TokenizerErrorKind::UnexpectedCharacter { character: c },
                );
            }
        };

        let token = Some(Token {
            kind,
            lexeme: self.lexeme(),
            span: Span {
                start: self.token_start,
                end: self.current,
                code_source_id: self.code_source_id,
            },
        });

        if kind == TokenKind::Newline {
            self.current.line += 1;
            self.current.position = 1;
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
        self.last = self.current;
        self.current_index += 1;
        self.current.byte += c.len_utf8() as u32;
        self.current.position += 1;
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

pub fn tokenize(input: &str, code_source_id: usize) -> Result<Vec<Token>> {
    let mut tokenizer = Tokenizer::new(input, code_source_id);
    tokenizer.scan()
}

#[cfg(test)]
fn tokenize_reduced(input: &str) -> Result<Vec<(String, TokenKind, (u32, u32))>, String> {
    Ok(tokenize(input, 0)
        .map_err(|e| {
            format!(
                "Error at {:?}: `{e}`",
                (e.span.start.line, e.span.start.position)
            )
        })?
        .iter()
        .map(|token| {
            (
                token.lexeme.to_string(),
                token.kind,
                (token.span.start.line, token.span.start.position),
            )
        })
        .collect())
}

#[cfg(test)]
fn tokenize_reduced_pretty(input: &str) -> Result<String, String> {
    use std::fmt::Write;
    let mut ret = String::new();
    for (lexeme, kind, pos) in tokenize_reduced(input)? {
        writeln!(ret, "{lexeme:?}, {kind:?}, {pos:?}").unwrap();
    }
    Ok(ret)
}

#[test]
fn test_tokenize_basic() {
    use TokenKind::*;

    assert_eq!(
        tokenize_reduced("  12 + 34  ").unwrap(),
        [
            ("12".to_string(), Number, (1, 3)),
            ("+".to_string(), Plus, (1, 6)),
            ("34".to_string(), Number, (1, 8)),
            ("".to_string(), Eof, (1, 12))
        ]
    );

    assert_eq!(
        tokenize_reduced("1 2").unwrap(),
        [
            ("1".to_string(), Number, (1, 1)),
            ("2".to_string(), Number, (1, 3)),
            ("".to_string(), Eof, (1, 4))
        ]
    );

    assert_eq!(
        tokenize_reduced("12 × (3 - 4)").unwrap(),
        [
            ("12".to_string(), Number, (1, 1)),
            ("×".to_string(), Multiply, (1, 4)),
            ("(".to_string(), LeftParen, (1, 6)),
            ("3".to_string(), Number, (1, 7)),
            ("-".to_string(), Minus, (1, 9)),
            ("4".to_string(), Number, (1, 11)),
            (")".to_string(), RightParen, (1, 12)),
            ("".to_string(), Eof, (1, 13))
        ]
    );

    assert_eq!(
        tokenize_reduced("foo to bar").unwrap(),
        [
            ("foo".to_string(), Identifier, (1, 1)),
            ("to".to_string(), To, (1, 5)),
            ("bar".to_string(), Identifier, (1, 8)),
            ("".to_string(), Eof, (1, 11))
        ]
    );

    assert_eq!(
        tokenize_reduced("1 -> 2").unwrap(),
        [
            ("1".to_string(), Number, (1, 1)),
            ("->".to_string(), Arrow, (1, 3)),
            ("2".to_string(), Number, (1, 6)),
            ("".to_string(), Eof, (1, 7))
        ]
    );

    assert_eq!(
        tokenize_reduced("45°").unwrap(),
        [
            ("45".to_string(), Number, (1, 1)),
            ("°".to_string(), Identifier, (1, 3)),
            ("".to_string(), Eof, (1, 4))
        ]
    );

    assert_eq!(
        tokenize_reduced("1+2\n42").unwrap(),
        [
            ("1".to_string(), Number, (1, 1)),
            ("+".to_string(), Plus, (1, 2)),
            ("2".to_string(), Number, (1, 3)),
            ("\n".to_string(), Newline, (1, 4)),
            ("42".to_string(), Number, (2, 1)),
            ("".to_string(), Eof, (2, 3))
        ]
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("~").unwrap_err(),
    @"Error at (1, 1): `Unexpected character: '~'`");
}

#[test]
fn test_tokenize_numbers() {
    // valid queries

    insta::assert_snapshot!(
        tokenize_reduced_pretty("12").unwrap(),
        @r###"
    "12", Number, (1, 1)
    "", Eof, (1, 3)
    "###
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("1_2").unwrap(),
        @r###"
    "1_2", Number, (1, 1)
    "", Eof, (1, 4)
    "###
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("1.").unwrap(),
        @r###"
    "1.", Number, (1, 1)
    "", Eof, (1, 3)
    "###
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("1.2").unwrap(),
        @r###"
    "1.2", Number, (1, 1)
    "", Eof, (1, 4)
    "###
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("1_1.2_2").unwrap(),
        @r###"
    "1_1.2_2", Number, (1, 1)
    "", Eof, (1, 8)
    "###
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("0b01").unwrap(),
        @r###"
    "0b01", IntegerWithBase(2), (1, 1)
    "", Eof, (1, 5)
    "###
    );
    insta::assert_snapshot!(
        tokenize_reduced_pretty("0b1_0").unwrap(),
        @r###"
    "0b1_0", IntegerWithBase(2), (1, 1)
    "", Eof, (1, 6)
    "###
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("0o01234567").unwrap(),
        @r###"
    "0o01234567", IntegerWithBase(8), (1, 1)
    "", Eof, (1, 11)
    "###
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("0o1_2").unwrap(),
        @r###"
    "0o1_2", IntegerWithBase(8), (1, 1)
    "", Eof, (1, 6)
    "###
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("0x1234567890abcdef").unwrap(),
        @r###"
    "0x1234567890abcdef", IntegerWithBase(16), (1, 1)
    "", Eof, (1, 19)
    "###
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("0x1_2").unwrap(),
        @r###"
    "0x1_2", IntegerWithBase(16), (1, 1)
    "", Eof, (1, 6)
    "###
    );

    // Failing queries
    insta::assert_snapshot!(
        tokenize_reduced_pretty("1_.2").unwrap_err(),
        @"Error at (1, 2): `Unexpected character in number literal: '_'`"
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("1.2_").unwrap_err(),
        @"Error at (1, 4): `Unexpected character in number literal: '_'`"
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("0b012").unwrap_err(),
        @"Error at (1, 5): `Expected base-2 digit`"
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("0b").unwrap_err(),
        @"Error at (1, 3): `Expected base-2 digit`"
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("0b_").unwrap_err(),
        @"Error at (1, 3): `Expected base-2 digit`"
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("0b_1").unwrap_err(),
        @"Error at (1, 3): `Expected base-2 digit`"
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("0b1_").unwrap_err(),
        @"Error at (1, 5): `Expected base-2 digit`"
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("0o012345678").unwrap_err(),
        @"Error at (1, 11): `Expected base-8 digit`"
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("0o").unwrap_err(),
        @"Error at (1, 3): `Expected base-8 digit`"
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("0o_").unwrap_err(),
        @"Error at (1, 3): `Expected base-8 digit`"
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("0o_1").unwrap_err(),
        @"Error at (1, 3): `Expected base-8 digit`"
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("0o1_").unwrap_err(),
        @"Error at (1, 5): `Expected base-8 digit`"
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("0x1234567890abcdefg").unwrap_err(),
        @"Error at (1, 19): `Expected base-16 digit`"
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("0x").unwrap_err(),
        @"Error at (1, 3): `Expected base-16 digit`"
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("0x_").unwrap_err(),
        @"Error at (1, 3): `Expected base-16 digit`"
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("0x_1").unwrap_err(),
        @"Error at (1, 3): `Expected base-16 digit`"
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("0x1_").unwrap_err(),
        @"Error at (1, 5): `Expected base-16 digit`"
    );
}

#[test]
fn test_tokenize_string() {
    use TokenKind::*;

    assert_eq!(
        tokenize_reduced("\"foo\"").unwrap(),
        [
            ("\"foo\"".to_string(), StringFixed, (1, 1)),
            ("".to_string(), Eof, (1, 6))
        ]
    );

    assert_eq!(
        tokenize_reduced("\"foo = {foo}\"").unwrap(),
        [
            ("\"foo = {".to_string(), StringInterpolationStart, (1, 1)),
            ("foo".to_string(), Identifier, (1, 9)),
            ("}\"".to_string(), StringInterpolationEnd, (1, 12)),
            ("".to_string(), Eof, (1, 14))
        ]
    );

    assert_eq!(
        tokenize_reduced("\"foo = {foo}, and bar = {bar}\"").unwrap(),
        [
            ("\"foo = {".to_string(), StringInterpolationStart, (1, 1)),
            ("foo".to_string(), Identifier, (1, 9)),
            (
                "}, and bar = {".to_string(),
                StringInterpolationMiddle,
                (1, 12)
            ),
            ("bar".to_string(), Identifier, (1, 26)),
            ("}\"".to_string(), StringInterpolationEnd, (1, 29)),
            ("".to_string(), Eof, (1, 31))
        ]
    );

    assert_eq!(
        tokenize_reduced("\"1 + 2 = {1 + 2}\"").unwrap(),
        [
            ("\"1 + 2 = {".to_string(), StringInterpolationStart, (1, 1)),
            ("1".to_string(), Number, (1, 11)),
            ("+".to_string(), Plus, (1, 13)),
            ("2".to_string(), Number, (1, 15)),
            ("}\"".to_string(), StringInterpolationEnd, (1, 16)),
            ("".to_string(), Eof, (1, 18))
        ]
    );

    assert_eq!(
        tokenize("\"foo", 0).unwrap_err().kind,
        TokenizerErrorKind::UnterminatedString
    );
    assert_eq!(
        tokenize("\"foo = {foo\"", 0).unwrap_err().kind,
        TokenizerErrorKind::UnterminatedStringInterpolation
    );
    assert_eq!(
        tokenize("\"foo = {foo}.", 0).unwrap_err().kind,
        TokenizerErrorKind::UnterminatedString
    );
    assert_eq!(
        tokenize("\"foo = {foo, bar = {bar}\"", 0).unwrap_err().kind,
        TokenizerErrorKind::UnexpectedCurlyInInterpolation
    );
}

#[test]
fn test_logical_operators() {
    insta::assert_snapshot!(
        tokenize_reduced_pretty("true || false").unwrap(),
        @r###"
    "true", True, (1, 1)
    "||", LogicalOr, (1, 6)
    "false", False, (1, 9)
    "", Eof, (1, 14)
    "###
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("true && false").unwrap(),
        @r###"
    "true", True, (1, 1)
    "&&", LogicalAnd, (1, 6)
    "false", False, (1, 9)
    "", Eof, (1, 14)
    "###
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("true | false").unwrap_err(),
        @"Error at (1, 6): `Unexpected character: '|'`"
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("true & false").unwrap_err(),
        @"Error at (1, 6): `Unexpected character: '&'`"
    );
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

#[test]
fn test_is_subscript_char() {
    assert!(is_subscript_char('₅'));
    assert!(is_subscript_char('₁'));
    assert!(is_subscript_char('ₓ'));
    assert!(is_subscript_char('ₘ'));
    assert!(is_subscript_char('₎'));
}
