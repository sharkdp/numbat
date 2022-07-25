//! Insect Parser
//!
//! Operator precedence, low to high
//! * conversion
//! * addition
//! * subtraction
//! * multiplication
//! * division
//! * unary
//!
//! Grammar:
//! ```txt
//! expression   →   conversion
//! conversion   →   term ( "→" term ) *
//! term         →   factor ( ( "+" | "-") factor ) *
//! factor       →   unary ( ( "*" | "/") unary ) *
//! unary        →   "-" unary | primary
//! primary      →   number | identifier | "(" expression ")"
//! ```

use crate::ast::{BinaryOperator, Expression, Number};
use crate::span::Span;
use crate::tokenizer::{Token, TokenKind, TokenizerError};

use thiserror::Error;

#[derive(Error, Debug, PartialEq)]
pub enum ParseErrorKind {
    #[error("Unexpected character '{0}'")]
    TokenizerUnexpectedCharacter(char),

    #[error("Expected one of: number, identifier, parenthesized expression")]
    ExpectedPrimary,

    #[error("Missing closing parenthesis ')'")]
    MissingClosingParen,

    #[error("Trailing characters: '{0}'")]
    TrailingCharacters(String),
}

#[derive(Debug, Error)]
#[error("Error at {span} while parsing expression: {kind}")]
pub struct ParseError {
    kind: ParseErrorKind,
    span: Span,
}

impl ParseError {
    fn new(kind: ParseErrorKind, span: Span) -> Self {
        ParseError { kind, span }
    }
}

type Result<T> = std::result::Result<T, ParseError>;

pub struct Parser<'a> {
    tokens: &'a [Token],
    current: usize,
}

impl<'a> Parser<'a> {
    fn new(tokens: &'a [Token]) -> Self {
        Parser { tokens, current: 0 }
    }

    fn parse(&mut self) -> Result<Expression> {
        let expr_or_error = self.expression();
        if !self.is_at_end() {
            Err(ParseError {
                kind: ParseErrorKind::TrailingCharacters(self.peek().lexeme.clone()),
                span: Span {
                    line: 0,
                    position: 0,
                },
            })
        } else {
            expr_or_error
        }
    }

    fn expression(&mut self) -> Result<Expression> {
        self.conversion()
    }

    fn conversion(&mut self) -> Result<Expression> {
        let mut expr = self.term()?;
        while self.match_exact(TokenKind::Arrow).is_some() {
            let rhs = self.term()?;

            expr = Expression::BinaryOperator(
                BinaryOperator::ConvertTo,
                Box::new(expr),
                Box::new(rhs),
            );
        }
        Ok(expr)
    }

    fn term(&mut self) -> Result<Expression> {
        let mut expr = self.factor()?;
        while let Some(operator_token) = self.match_any(&[TokenKind::Plus, TokenKind::Minus]) {
            let operator = if operator_token.kind == TokenKind::Plus {
                BinaryOperator::Add
            } else {
                BinaryOperator::Sub
            };

            let rhs = self.factor()?;

            expr = Expression::BinaryOperator(operator, Box::new(expr), Box::new(rhs));
        }
        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expression> {
        let mut expr = self.unary()?;
        while let Some(operator_token) = self.match_any(&[TokenKind::Multiply, TokenKind::Divide]) {
            let operator = if operator_token.kind == TokenKind::Multiply {
                BinaryOperator::Mul
            } else {
                BinaryOperator::Div
            };

            let rhs = self.unary()?;

            expr = Expression::BinaryOperator(operator, Box::new(expr), Box::new(rhs));
        }
        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expression> {
        if self.match_exact(TokenKind::Minus).is_some() {
            let rhs = self.unary()?;

            Ok(Expression::Negate(Box::new(rhs)))
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> Result<Expression> {
        if let Some(num) = self.match_exact(TokenKind::Number) {
            Ok(Expression::Scalar(Number::from_f64(
                num.lexeme.parse::<f64>().unwrap(),
            )))
        } else if let Some(identifier) = self.match_exact(TokenKind::Identifier) {
            Ok(Expression::Identifier(identifier.lexeme.clone()))
        } else if self.match_exact(TokenKind::LeftParen).is_some() {
            let inner = self.expression()?;

            if self.match_exact(TokenKind::RightParen).is_none() {
                return Err(ParseError::new(
                    ParseErrorKind::MissingClosingParen,
                    self.peek().span.clone(),
                ));
            }

            Ok(inner)
        } else {
            Err(ParseError::new(
                ParseErrorKind::ExpectedPrimary,
                self.peek().span.clone(),
            ))
        }
    }

    fn match_exact(&mut self, token_kind: TokenKind) -> Option<&'a Token> {
        let token = self.peek();
        if token.kind == token_kind {
            self.advance();
            Some(token)
        } else {
            None
        }
    }

    fn match_any(&mut self, kinds: &[TokenKind]) -> Option<&'a Token> {
        for kind in kinds {
            if let result @ Some(..) = self.match_exact(*kind) {
                return result;
            }
        }
        None
    }

    fn advance(&mut self) {
        if !self.is_at_end() {
            self.current += 1;
        }
    }

    fn peek(&self) -> &'a Token {
        &self.tokens[self.current]
    }

    fn is_at_end(&self) -> bool {
        self.peek().kind == TokenKind::Eof
    }
}

pub fn parse(input: &str) -> Result<Expression> {
    use crate::tokenizer::tokenize;

    let tokens = tokenize(input).map_err(
        |TokenizerError::UnexpectedCharacter {
             character,
             ref span,
         }| {
            ParseError::new(
                ParseErrorKind::TokenizerUnexpectedCharacter(character),
                span.clone(),
            )
        },
    )?;
    let mut parser = Parser::new(&tokens);
    parser.parse()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{binop, identifier, negate, scalar};

    fn all_parse_as(inputs: &[&str], expr_expected: Expression) {
        for input in inputs {
            let expr_parsed = parse(input).expect("parse error");

            assert_eq!(expr_parsed, expr_expected);
        }
    }

    #[cfg(test)]
    fn should_fail(inputs: &[&str]) {
        for input in inputs {
            assert!(parse(input).is_err());
        }
    }

    #[test]
    fn parse_invalid_input() {
        should_fail(&["", "+", "->", "§"]);
    }

    #[test]
    fn parse_numbers() {
        all_parse_as(&["1", "  1   "], scalar!(1.0));

        should_fail(&["123..", "0..", ".0.", ".", ". 2", ".."]);
    }

    #[test]
    fn parse_identifiers() {
        all_parse_as(&["foo", "  foo   "], identifier!("foo"));
        all_parse_as(&["foo_bar"], identifier!("foo_bar"));
        all_parse_as(&["MeineSchöneVariable"], identifier!("MeineSchöneVariable"));
        all_parse_as(&["°"], identifier!("°"));
    }

    #[test]
    fn parse_negation() {
        all_parse_as(&["-1", "  - 1   "], negate!(scalar!(1.0)));
        all_parse_as(&["--1", " -  - 1   "], negate!(negate!(scalar!(1.0))));
        all_parse_as(&["-x", " - x"], negate!(identifier!("x")));

        all_parse_as(
            &["-1 + 2"],
            binop!(negate!(scalar!(1.0)), Add, scalar!(2.0)),
        );
    }

    #[test]
    fn parse_addition_subtraction() {
        all_parse_as(
            &["1+2", "  1   +  2    "],
            binop!(scalar!(1.0), Add, scalar!(2.0)),
        );

        // Minus should be left-associative
        all_parse_as(
            &["1-2-3"],
            binop!(binop!(scalar!(1.0), Sub, scalar!(2.0)), Sub, scalar!(3.0)),
        );
    }

    #[test]
    fn parse_multiplication_division() {
        all_parse_as(
            &["1*2", "  1   *  2    ", "1 · 2", "1 × 2"],
            binop!(scalar!(1.0), Mul, scalar!(2.0)),
        );

        all_parse_as(
            &["1/2", "1 per 2", "1÷2"],
            binop!(scalar!(1.0), Div, scalar!(2.0)),
        );

        should_fail(&["1*@", "1*", "1 per", "÷", "×"]);
    }

    #[test]
    fn parse_conversion() {
        all_parse_as(
            &["1->2", "1→2"],
            binop!(scalar!(1.0), ConvertTo, scalar!(2.0)),
        );

        // Conversion is left-associative
        all_parse_as(
            &["1→2→3"],
            binop!(
                binop!(scalar!(1.0), ConvertTo, scalar!(2.0)),
                ConvertTo,
                scalar!(3.0)
            ),
        );

        should_fail(&["1 - > 2", "1 -> -> 2"]);
    }

    #[test]
    fn parse_grouping() {
        all_parse_as(
            &["1*(2+3)", "1 * ( 2 + 3 )"],
            binop!(scalar!(1.0), Mul, binop!(scalar!(2.0), Add, scalar!(3.0))),
        );

        should_fail(&["1 * (2 + 3", "2 + 3)"]);
    }
}
