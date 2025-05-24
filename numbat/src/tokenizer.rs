use crate::ast::ProcedureKind;
use crate::span::{ByteIndex, Span};

use std::collections::HashMap;
use std::sync::OnceLock;
use thiserror::Error;

#[derive(Debug, Clone, Error, PartialEq, Eq)]
pub enum TokenizerErrorKind {
    #[error("Unexpected character: {}",
        if *character == '\'' {
            r#""'""#.to_owned()
        } else if character.is_ascii() {
            format!("'{}'", character.escape_default())
        } else {
            format!("'{character}' (U+{:0>4X})", *character as u32)
        }
    )]
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
    ExpectedDigitInBase { base: u8, character: Option<char> },

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

    LeftCurly,
    RightCurly,

    // Operators and special signs
    Plus,
    Minus,
    Multiply,
    Power,
    Divide,
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
    Period,
    QuestionMark,

    // Keywords
    Per,
    To,
    Let,
    Fn, // 'fn'
    Where,
    And,
    Dimension,
    Unit,
    Use,
    Struct,

    Long,
    Short,
    Both,
    None,

    If,
    Then,
    Else,
    True,
    False,

    NaN,
    Inf,

    // Type names
    Bool,
    String,
    DateTime,
    CapitalFn, // 'Fn'
    List,

    // Procedure calls
    ProcedurePrint,
    ProcedureAssert,
    ProcedureAssertEq,
    ProcedureType,

    // Variable-length tokens
    Number,
    IntegerWithBase(u8),
    Identifier,

    // A normal string without interpolation: `"hello world"`
    StringFixed,
    // A part of a string which *starts* an interpolation: `"foo = {`
    StringInterpolationStart,
    // A part of a string between two interpolations: `}, and bar = {`
    StringInterpolationMiddle,
    // Format specifiers for an interpolation, e.g. `:.03f`
    StringInterpolationSpecifiers,
    // A part of a string which ends an interpolation: `}."`
    StringInterpolationEnd,

    // Other
    Newline,
    Eof,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub lexeme: &'a str,
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
    matches!(c, '%' | '‰')
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
        || c == '′'
        || c == '″'
        || c == '_'
}

fn is_identifier_continue(c: char) -> bool {
    (unicode_ident::is_xid_continue(c)
        || is_subscript_char(c)
        || is_currency_char(c)
        || is_other_allowed_identifier_char(c))
        && !is_exponent_char(c)
        && c != '·'
        && c != '⋅'
}

/// When scanning a string interpolation like `"foo = {foo}, and bar = {bar}."`,
/// the tokenizer needs to keep track of where it currently is, because we allow
/// for (almost) arbitrary expressions inside the {…} part.
#[cfg_attr(debug_assertions, derive(Debug))]
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

#[cfg_attr(debug_assertions, derive(Debug))]
struct Tokenizer {
    current: ByteIndex,
    last: ByteIndex,
    token_start: ByteIndex,
    code_source_id: usize,

    // Special fields / state for parsing string interpolations
    string_start: ByteIndex,
    interpolation_start: ByteIndex,
    interpolation_state: InterpolationState,
}

fn char_at(s: &str, byte_index: usize) -> Option<char> {
    s[byte_index..].chars().next()
}

impl Tokenizer {
    fn new(code_source_id: usize) -> Self {
        Tokenizer {
            current: ByteIndex(0),
            last: ByteIndex(0),
            token_start: ByteIndex(0),

            code_source_id,
            string_start: ByteIndex(0),
            interpolation_start: ByteIndex(0),
            interpolation_state: InterpolationState::Outside,
        }
    }

    fn scan<'a>(&mut self, input: &'a str) -> Result<Vec<Token<'a>>> {
        let mut tokens = vec![];
        while !self.at_end(input) {
            self.token_start = self.current;
            if let Some(token) = self.scan_single_token(input)? {
                tokens.push(token);
            }
        }

        tokens.push(Token {
            kind: TokenKind::Eof,
            lexeme: "",
            span: self.current.single_character_span(self.code_source_id),
        });

        Ok(tokens)
    }

    fn consume_stream_of_digits(
        &mut self,
        input: &str,
        at_least_one_digit: bool,
        disallow_leading_underscore: bool,
        disallow_dot_after_stream: bool,
    ) -> Result<()> {
        if at_least_one_digit
            && !self
                .peek(input)
                .map(|c| c.is_ascii_digit())
                .unwrap_or(false)
        {
            return Err(TokenizerError {
                kind: TokenizerErrorKind::ExpectedDigit {
                    character: self.peek(input),
                },
                span: self.current.single_character_span(self.code_source_id),
            });
        }

        // Make sure we don't start with an underscore
        if disallow_leading_underscore && self.peek(input).map(|c| c == '_').unwrap_or(false) {
            return Err(TokenizerError {
                kind: TokenizerErrorKind::UnexpectedCharacterInNumberLiteral(
                    self.peek(input).unwrap(),
                ),
                span: self.current.single_character_span(self.code_source_id),
            });
        }

        let mut last_char = None;
        while self
            .peek(input)
            .map(|c| c.is_ascii_digit() || c == '_')
            .unwrap_or(false)
        {
            last_char = Some(self.advance(input));
        }

        // Make sure we don't end with an underscore
        if last_char == Some('_') {
            return Err(TokenizerError {
                kind: TokenizerErrorKind::UnexpectedCharacterInNumberLiteral('_'),
                span: self.last.single_character_span(self.code_source_id),
            });
        }

        if disallow_dot_after_stream && self.peek(input).map(|c| c == '.').unwrap_or(false) {
            return Err(TokenizerError {
                kind: TokenizerErrorKind::UnexpectedCharacterInNumberLiteral(
                    self.peek(input).unwrap(),
                ),
                span: self.current.single_character_span(self.code_source_id),
            });
        }

        Ok(())
    }

    fn scientific_notation(&mut self, input: &str) -> Result<()> {
        if self
            .peek2(input)
            .map(|c| c.is_ascii_digit() || c == '+' || c == '-')
            .unwrap_or(false)
            && (self.match_char(input, 'e') || self.match_char(input, 'E'))
        {
            let _ = self.match_char(input, '+') || self.match_char(input, '-');

            self.consume_stream_of_digits(input, true, true, true)?;
        }

        Ok(())
    }

    fn consume_string(&mut self, input: &str) -> Result<()> {
        let mut escaped = false;
        loop {
            escaped = match self.peek(input) {
                None => {
                    break;
                }
                Some('\\') if !escaped => true,
                Some('"') if !escaped => {
                    break;
                }
                c @ (Some('{') | Some('}')) if c != self.peek2(input) => {
                    break;
                }
                Some('{') | Some('}') => {
                    // extra advance to skip both curly's making up the escape sequence
                    self.advance(input);
                    false
                }
                Some(_) => false,
            };

            self.advance(input);
        }

        Ok(())
    }

    fn scan_single_token<'a>(&mut self, input: &'a str) -> Result<Option<Token<'a>>> {
        fn is_ascii_hex_digit(c: char) -> bool {
            c.is_ascii_hexdigit()
        }

        fn is_ascii_octal_digit(c: char) -> bool {
            ('0'..='7').contains(&c)
        }

        fn is_ascii_binary_digit(c: char) -> bool {
            c == '0' || c == '1'
        }

        static KEYWORDS: OnceLock<HashMap<&'static str, TokenKind>> = OnceLock::new();
        let keywords = KEYWORDS.get_or_init(|| {
            let mut m = HashMap::new();
            // keywords
            m.insert("per", TokenKind::Per);
            m.insert("to", TokenKind::To);
            m.insert("let", TokenKind::Let);
            m.insert("fn", TokenKind::Fn);
            m.insert("where", TokenKind::Where);
            m.insert("and", TokenKind::And);
            m.insert("dimension", TokenKind::Dimension);
            m.insert("unit", TokenKind::Unit);
            m.insert("use", TokenKind::Use);
            m.insert("struct", TokenKind::Struct);
            m.insert("long", TokenKind::Long);
            m.insert("short", TokenKind::Short);
            m.insert("both", TokenKind::Both);
            m.insert("none", TokenKind::None);
            m.insert("if", TokenKind::If);
            m.insert("then", TokenKind::Then);
            m.insert("else", TokenKind::Else);
            m.insert("true", TokenKind::True);
            m.insert("false", TokenKind::False);
            m.insert("NaN", TokenKind::NaN);
            m.insert("inf", TokenKind::Inf);

            // procedures
            m.insert(ProcedureKind::Print.name(), TokenKind::ProcedurePrint);
            m.insert(ProcedureKind::Assert.name(), TokenKind::ProcedureAssert);
            m.insert(ProcedureKind::AssertEq.name(), TokenKind::ProcedureAssertEq);
            m.insert(ProcedureKind::Type.name(), TokenKind::ProcedureType);

            // type names
            m.insert("Bool", TokenKind::Bool);
            m.insert("String", TokenKind::String);
            m.insert("DateTime", TokenKind::DateTime);
            m.insert("Fn", TokenKind::CapitalFn);
            m.insert("List", TokenKind::List);

            // Keep this list in sync with keywords::KEYWORDS!
            m
        });

        if self.peek(input) == Some('#') {
            // skip over comment until newline
            loop {
                match self.peek(input) {
                    None => return Ok(None),
                    Some('\n') => break,
                    _ => {
                        self.advance(input);
                    }
                }
            }
        }

        let current_char = self.advance(input);

        let code_source_id = self.code_source_id;
        let tokenizer_error = |position: ByteIndex, kind| -> Result<Option<Token>> {
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
            '{' if !self.interpolation_state.is_inside() => TokenKind::LeftCurly,
            '}' if !self.interpolation_state.is_inside() => TokenKind::RightCurly,
            '≤' => TokenKind::LessOrEqual,
            '<' if self.match_char(input, '=') => TokenKind::LessOrEqual,
            '<' => TokenKind::LessThan,
            '≥' => TokenKind::GreaterOrEqual,
            '>' if self.match_char(input, '=') => TokenKind::GreaterOrEqual,
            '>' => TokenKind::GreaterThan,
            '?' => TokenKind::QuestionMark,
            '0' if self
                .peek(input)
                .map(|c| c == 'x' || c == 'o' || c == 'b')
                .unwrap_or(false) =>
            {
                let (base, is_digit_in_base) = match self.peek(input).unwrap() {
                    'x' => (16, is_ascii_hex_digit as fn(char) -> bool),
                    'o' => (8, is_ascii_octal_digit as _),
                    'b' => (2, is_ascii_binary_digit as _),
                    _ => unreachable!(),
                };

                self.advance(input); // skip over the x/o/b

                // If the first character is not a digit, that's an error.
                if !self.peek(input).map(is_digit_in_base).unwrap_or(false) {
                    return tokenizer_error(
                        self.current,
                        TokenizerErrorKind::ExpectedDigitInBase {
                            base,
                            character: self.peek(input),
                        },
                    );
                }

                let mut last_char = None;

                while self
                    .peek(input)
                    .map(|c| is_digit_in_base(c) || c == '_')
                    .unwrap_or(false)
                {
                    last_char = self.peek(input);
                    self.advance(input);
                }

                // Numeric literal should not end with a `_` either.
                if last_char == Some('_')
                    || self
                        .peek(input)
                        .map(|c| is_identifier_continue(c) || c == '.')
                        .unwrap_or(false)
                {
                    return tokenizer_error(
                        self.current,
                        TokenizerErrorKind::ExpectedDigitInBase {
                            base,
                            character: self.peek(input),
                        },
                    );
                }

                TokenKind::IntegerWithBase(base)
            }
            c if c.is_ascii_digit() => {
                self.consume_stream_of_digits(input, false, false, false)?;

                // decimal part
                if self.match_char(input, '.') {
                    self.consume_stream_of_digits(input, false, true, true)?;
                }

                self.scientific_notation(input)?;

                TokenKind::Number
            }
            '.' if self.peek(input) == Some('.') && self.peek2(input) == Some('.') => {
                self.advance(input);
                self.advance(input);

                TokenKind::Ellipsis
            }
            '.' if self.peek(input).is_some_and(is_identifier_start) => TokenKind::Period,
            '.' => {
                self.consume_stream_of_digits(input, true, true, true)?;
                self.scientific_notation(input)?;

                TokenKind::Number
            }
            ' ' | '\t' | '\r' => {
                return Ok(None);
            }
            '\n' => TokenKind::Newline,
            '&' if self.match_char(input, '&') => TokenKind::LogicalAnd,
            '|' if self.match_char(input, '|') => TokenKind::LogicalOr,
            '|' if self.match_char(input, '>') => TokenKind::PostfixApply,
            '*' if self.match_char(input, '*') => TokenKind::Power,
            '+' => TokenKind::Plus,
            '*' | '·' | '⋅' | '×' => TokenKind::Multiply,
            '/' => TokenKind::Divide,
            '÷' => TokenKind::Divide,
            '^' => TokenKind::Power,
            ',' => TokenKind::Comma,
            '⩵' => TokenKind::EqualEqual,
            '=' if self.match_char(input, '=') => TokenKind::EqualEqual,
            '=' => TokenKind::Equal,
            '@' => TokenKind::At,
            '→' | '➞' => TokenKind::Arrow,
            '-' if self.match_char(input, '>') => TokenKind::Arrow,
            '-' | '−' => TokenKind::Minus,
            '≠' => TokenKind::NotEqual,
            '!' if self.match_char(input, '=') => TokenKind::NotEqual,
            '!' => TokenKind::ExclamationMark,
            '⁻' => {
                let c = self.peek(input);
                if c.map(is_exponent_char).unwrap_or(false) {
                    self.advance(input);
                    TokenKind::UnicodeExponent
                } else {
                    return tokenizer_error(
                        self.current,
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

                    self.consume_string(input)?;

                    if self.match_char(input, '"') {
                        TokenKind::StringFixed
                    } else if self.match_char(input, '{') {
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
            ':' if self.interpolation_state.is_inside() => {
                while self
                    .peek(input)
                    .map(|c| c != '"' && c != '}')
                    .unwrap_or(false)
                {
                    self.advance(input);
                }

                if self.peek(input) == Some('"') {
                    return Err(TokenizerError {
                        kind: TokenizerErrorKind::UnterminatedStringInterpolation,
                        span: Span {
                            start: self.token_start,
                            end: self.current,
                            code_source_id: self.code_source_id,
                        },
                    });
                }
                if self.peek(input) == Some('}') {
                    TokenKind::StringInterpolationSpecifiers
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
            '}' if self.interpolation_state.is_inside() => {
                self.consume_string(input)?;

                if self.match_char(input, '"') {
                    self.interpolation_state = InterpolationState::Outside;
                    TokenKind::StringInterpolationEnd
                } else if self.match_char(input, '{') {
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
                while self
                    .peek(input)
                    .map(is_identifier_continue)
                    .unwrap_or(false)
                {
                    self.advance(input);
                }

                if self.peek(input).map(|c| c == '.').unwrap_or(false)
                    && self
                        .peek2(input)
                        .map(|c| !is_identifier_start(c))
                        .unwrap_or(true)
                {
                    return tokenizer_error(
                        self.current,
                        TokenizerErrorKind::UnexpectedCharacterInIdentifier(
                            self.peek(input).unwrap(),
                        ),
                    );
                }

                if let Some(kind) = keywords.get(self.lexeme(input)) {
                    *kind
                } else {
                    TokenKind::Identifier
                }
            }
            ':' if self.match_char(input, ':') => TokenKind::DoubleColon,
            ':' => TokenKind::Colon,
            c => {
                return tokenizer_error(
                    self.token_start,
                    TokenizerErrorKind::UnexpectedCharacter { character: c },
                );
            }
        };

        let token = Some(Token {
            kind,
            lexeme: self.lexeme(input),
            span: Span {
                start: self.token_start,
                end: self.current,
                code_source_id: self.code_source_id,
            },
        });

        Ok(token)
    }

    fn lexeme<'a>(&self, input: &'a str) -> &'a str {
        &input[self.token_start.as_usize()..self.current.as_usize()]
    }

    fn advance(&mut self, input: &str) -> char {
        let c = char_at(input, self.current.as_usize()).unwrap();
        self.last = self.current;
        self.current += c.len_utf8() as u32;
        c
    }

    fn peek(&self, input: &str) -> Option<char> {
        char_at(input, self.current.as_usize())
    }

    fn peek2(&self, input: &str) -> Option<char> {
        let next_char = self.peek(input)?;
        char_at(input, self.current.as_usize() + next_char.len_utf8())
    }

    fn match_char(&mut self, input: &str, c: char) -> bool {
        if self.peek(input) == Some(c) {
            self.advance(input);
            true
        } else {
            false
        }
    }

    fn at_end(&self, input: &str) -> bool {
        self.current.as_usize() >= input.len()
    }
}

pub fn tokenize(input: &str, code_source_id: usize) -> Result<Vec<Token>> {
    let mut tokenizer = Tokenizer::new(code_source_id);
    tokenizer.scan(input)
}

#[cfg(test)]
fn tokenize_reduced(input: &str) -> Result<Vec<(&str, TokenKind, ByteIndex)>, String> {
    Ok(tokenize(input, 0)
        .map_err(|e| format!("Error at index {:?}: `{e}`", e.span.start.0))?
        .iter()
        .map(|token| (token.lexeme, token.kind, token.span.start))
        .collect())
}

#[cfg(test)]
fn tokenize_reduced_pretty(input: &str) -> Result<String, String> {
    use std::fmt::Write;
    let mut ret = String::new();
    for (lexeme, kind, pos) in tokenize_reduced(input)? {
        writeln!(ret, "{lexeme:?}, {kind:?}, {:?}", pos.0).unwrap();
    }
    Ok(ret)
}

#[test]
fn test_tokenize_basic() {
    use TokenKind::*;

    assert_eq!(
        tokenize_reduced("  12 + 34  ").unwrap(),
        [
            ("12", Number, ByteIndex(2)),
            ("+", Plus, ByteIndex(5)),
            ("34", Number, ByteIndex(7)),
            ("", Eof, ByteIndex(11))
        ]
    );

    assert_eq!(
        tokenize_reduced("1 2").unwrap(),
        [
            ("1", Number, ByteIndex(0)),
            ("2", Number, ByteIndex(2)),
            ("", Eof, ByteIndex(3))
        ]
    );

    assert_eq!(
        tokenize_reduced("12 × (3 - 4)").unwrap(),
        [
            ("12", Number, ByteIndex(0)),
            // 2 bytes: std::str::from_utf8(b"\xc3\x97") == Ok("×")
            ("×", Multiply, ByteIndex(3)),
            ("(", LeftParen, ByteIndex(6)),
            ("3", Number, ByteIndex(7)),
            ("-", Minus, ByteIndex(9)),
            ("4", Number, ByteIndex(11)),
            (")", RightParen, ByteIndex(12)),
            ("", Eof, ByteIndex(13))
        ]
    );

    assert_eq!(
        tokenize_reduced("foo to bar").unwrap(),
        [
            ("foo", Identifier, ByteIndex(0)),
            ("to", To, ByteIndex(4)),
            ("bar", Identifier, ByteIndex(7)),
            ("", Eof, ByteIndex(10))
        ]
    );

    assert_eq!(
        tokenize_reduced("1 -> 2").unwrap(),
        [
            ("1", Number, ByteIndex(0)),
            ("->", Arrow, ByteIndex(2)),
            ("2", Number, ByteIndex(5)),
            ("", Eof, ByteIndex(6))
        ]
    );

    assert_eq!(
        tokenize_reduced("45°").unwrap(),
        [
            ("45", Number, ByteIndex(0)),
            // 2 bytes: std::str::from_utf8(b"\xc2\xb0") == Ok("°")
            ("°", Identifier, ByteIndex(2)),
            ("", Eof, ByteIndex(4))
        ]
    );

    assert_eq!(
        tokenize_reduced("1+2\n42").unwrap(),
        [
            ("1", Number, ByteIndex(0)),
            ("+", Plus, ByteIndex(1)),
            ("2", Number, ByteIndex(2)),
            ("\n", Newline, ByteIndex(3)),
            ("42", Number, ByteIndex(4)),
            ("", Eof, ByteIndex(6))
        ]
    );

    assert_eq!(
        tokenize_reduced("…").unwrap(),
        [
            // 3 bytes: std::str::from_utf8(b"\xe2\x80\xa6") == Ok("…")
            ("…", Ellipsis, ByteIndex(0)),
            ("", Eof, ByteIndex(3))
        ]
    );

    assert_eq!(
        tokenize_reduced("...").unwrap(),
        [("...", Ellipsis, ByteIndex(0)), ("", Eof, ByteIndex(3))]
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("~").unwrap_err(),
    @"Error at index 0: `Unexpected character: '~'`");
}

#[test]
fn test_tokenize_numbers() {
    // valid queries

    insta::assert_snapshot!(
        tokenize_reduced_pretty("12").unwrap(),
        @r###"
    "12", Number, 0
    "", Eof, 2
    "###
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("1_2").unwrap(),
        @r###"
    "1_2", Number, 0
    "", Eof, 3
    "###
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("1.").unwrap(),
        @r###"
    "1.", Number, 0
    "", Eof, 2
    "###
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("1.2").unwrap(),
        @r###"
    "1.2", Number, 0
    "", Eof, 3
    "###
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("1_1.2_2").unwrap(),
        @r###"
    "1_1.2_2", Number, 0
    "", Eof, 7
    "###
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("0b01").unwrap(),
        @r###"
    "0b01", IntegerWithBase(2), 0
    "", Eof, 4
    "###
    );
    insta::assert_snapshot!(
        tokenize_reduced_pretty("0b1_0").unwrap(),
        @r###"
    "0b1_0", IntegerWithBase(2), 0
    "", Eof, 5
    "###
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("0o01234567").unwrap(),
        @r###"
    "0o01234567", IntegerWithBase(8), 0
    "", Eof, 10
    "###
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("0o1_2").unwrap(),
        @r###"
    "0o1_2", IntegerWithBase(8), 0
    "", Eof, 5
    "###
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("0x1234567890abcdef").unwrap(),
        @r###"
    "0x1234567890abcdef", IntegerWithBase(16), 0
    "", Eof, 18
    "###
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("0x1_2").unwrap(),
        @r###"
    "0x1_2", IntegerWithBase(16), 0
    "", Eof, 5
    "###
    );

    // Failing queries
    insta::assert_snapshot!(
        tokenize_reduced_pretty("1_.2").unwrap_err(),
        @"Error at index 1: `Unexpected character in number literal: '_'`"
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("1.2_").unwrap_err(),
        @"Error at index 3: `Unexpected character in number literal: '_'`"
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("0b012").unwrap_err(),
        @"Error at index 4: `Expected base-2 digit`"
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("0b").unwrap_err(),
        @"Error at index 2: `Expected base-2 digit`"
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("0b_").unwrap_err(),
        @"Error at index 2: `Expected base-2 digit`"
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("0b_1").unwrap_err(),
        @"Error at index 2: `Expected base-2 digit`"
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("0b1_").unwrap_err(),
        @"Error at index 4: `Expected base-2 digit`"
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("0o012345678").unwrap_err(),
        @"Error at index 10: `Expected base-8 digit`"
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("0o").unwrap_err(),
        @"Error at index 2: `Expected base-8 digit`"
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("0o_").unwrap_err(),
        @"Error at index 2: `Expected base-8 digit`"
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("0o_1").unwrap_err(),
        @"Error at index 2: `Expected base-8 digit`"
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("0o1_").unwrap_err(),
        @"Error at index 4: `Expected base-8 digit`"
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("0x1234567890abcdefg").unwrap_err(),
        @"Error at index 18: `Expected base-16 digit`"
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("0x").unwrap_err(),
        @"Error at index 2: `Expected base-16 digit`"
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("0x_").unwrap_err(),
        @"Error at index 2: `Expected base-16 digit`"
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("0x_1").unwrap_err(),
        @"Error at index 2: `Expected base-16 digit`"
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("0x1_").unwrap_err(),
        @"Error at index 4: `Expected base-16 digit`"
    );
}

#[test]
fn test_tokenize_string() {
    use TokenKind::*;

    assert_eq!(
        tokenize_reduced("\"foo\"").unwrap(),
        [
            ("\"foo\"", StringFixed, ByteIndex(0)),
            ("", Eof, ByteIndex(5))
        ]
    );

    assert_eq!(
        tokenize_reduced("\"foo = {foo}\"").unwrap(),
        [
            ("\"foo = {", StringInterpolationStart, ByteIndex(0)),
            ("foo", Identifier, ByteIndex(8)),
            ("}\"", StringInterpolationEnd, ByteIndex(11)),
            ("", Eof, ByteIndex(13))
        ]
    );

    assert_eq!(
        tokenize_reduced("\"foo = {foo}, and bar = {bar}\"").unwrap(),
        [
            ("\"foo = {", StringInterpolationStart, ByteIndex(0)),
            ("foo", Identifier, ByteIndex(8)),
            ("}, and bar = {", StringInterpolationMiddle, ByteIndex(11)),
            ("bar", Identifier, ByteIndex(25)),
            ("}\"", StringInterpolationEnd, ByteIndex(28)),
            ("", Eof, ByteIndex(30))
        ]
    );

    assert_eq!(
        tokenize_reduced("\"1 + 2 = {1 + 2}\"").unwrap(),
        [
            ("\"1 + 2 = {", StringInterpolationStart, ByteIndex(0)),
            ("1", Number, ByteIndex(10)),
            ("+", Plus, ByteIndex(12)),
            ("2", Number, ByteIndex(14)),
            ("}\"", StringInterpolationEnd, ByteIndex(15)),
            ("", Eof, ByteIndex(17))
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

    insta::assert_snapshot!(
        tokenize_reduced_pretty(r#""start \"inner\" end""#).unwrap(),
        @r###"
    "\"start \\\"inner\\\" end\"", StringFixed, 0
    "", Eof, 21
    "###
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty(r#""start {{inner}} end""#).unwrap(),
        @r###"
    "\"start {{inner}} end\"", StringFixed, 0
    "", Eof, 21
    "###
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty(r#""start {1} \"inner\" end""#).unwrap(),
        @r###"
    "\"start {", StringInterpolationStart, 0
    "1", Number, 8
    "} \\\"inner\\\" end\"", StringInterpolationEnd, 9
    "", Eof, 25
    "###
    );
}

#[test]
fn test_logical_operators() {
    insta::assert_snapshot!(
        tokenize_reduced_pretty("true || false").unwrap(),
        @r###"
    "true", True, 0
    "||", LogicalOr, 5
    "false", False, 8
    "", Eof, 13
    "###
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("true && false").unwrap(),
        @r###"
    "true", True, 0
    "&&", LogicalAnd, 5
    "false", False, 8
    "", Eof, 13
    "###
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("true | false").unwrap_err(),
        @"Error at index 5: `Unexpected character: '|'`"
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("true & false").unwrap_err(),
        @"Error at index 5: `Unexpected character: '&'`"
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

#[test]
fn test_field_access() {
    insta::assert_snapshot!(
        tokenize_reduced_pretty("instance2.field").unwrap(),
        @r###"
    "instance2", Identifier, 0
    ".", Period, 9
    "field", Identifier, 10
    "", Eof, 15
    "###
    );

    insta::assert_snapshot!(
        tokenize_reduced_pretty("function().field").unwrap(),
        @r###"
    "function", Identifier, 0
    "(", LeftParen, 8
    ")", RightParen, 9
    ".", Period, 10
    "field", Identifier, 11
    "", Eof, 16
    "###
    );

    insta::assert_snapshot!(
    tokenize_reduced_pretty("instance.0").unwrap_err(),
        @"Error at index 8: `Unexpected character in identifier: '.'`"
    );

    insta::assert_snapshot!(
    tokenize_reduced_pretty("instance..field").unwrap_err(),
        @"Error at index 8: `Unexpected character in identifier: '.'`"
    );

    insta::assert_snapshot!(
    tokenize_reduced_pretty("instance . field").unwrap_err(),
        @"Error at index 10: `Expected digit`"
    );
}

#[test]
fn test_lists() {
    insta::assert_snapshot!(
        tokenize_reduced_pretty("[1, 2.3, 4]").unwrap(),
        @r###"
    "[", LeftBracket, 0
    "1", Number, 1
    ",", Comma, 2
    "2.3", Number, 4
    ",", Comma, 7
    "4", Number, 9
    "]", RightBracket, 10
    "", Eof, 11
    "###
    );
}
