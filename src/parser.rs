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
//! primary      →   number | "(" expression ")"
//! ```

use crate::ast::{BinaryOperator, Expression, Number};
use crate::tokenizer::{Token, TokenKind, TokenizerError};

use thiserror::Error;

#[derive(Debug, Error, PartialEq)]
pub enum ParseError {
    #[error("{0:#}")]
    TokenizerError(TokenizerError),

    #[error("Expected number")]
    ExpectedNumber,
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
        let num = self
            .match_exact(TokenKind::Number)
            .ok_or(ParseError::ExpectedNumber)?;
        Ok(Expression::Scalar(Number::from_f64(
            num.lexeme.parse::<f64>().unwrap(),
        )))
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

    let tokens = tokenize(input).map_err(ParseError::TokenizerError)?;
    let mut parser = Parser::new(&tokens);
    parser.expression()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{binop, negate, scalar};

    fn all_parse_as(inputs: &[&str], expr_expected: Expression) {
        for input in inputs {
            let expr_parsed = parse(input).expect("parse error");

            assert_eq!(expr_parsed, expr_expected);
        }
    }

    #[cfg(test)]
    fn assert_parse_error(inputs: &[&str]) {
        for input in inputs {
            assert!(parse(input).is_err());
        }
    }

    #[test]
    fn parse_invalid_input() {
        assert_parse_error(&["", "+", "->", "§"]);
    }

    #[test]
    fn parse_numbers() {
        all_parse_as(&["1", "  1   "], scalar!(1.0));

        assert_parse_error(&["123..", "0..", ".0.", ".", ". 2", ".."]);
    }

    #[test]
    fn parse_negation() {
        all_parse_as(&["-1", "  - 1   "], negate!(scalar!(1.0)));
        all_parse_as(&["--1", " -  - 1   "], negate!(negate!(scalar!(1.0))));

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
    }
}
