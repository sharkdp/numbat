use crate::ast::{BinaryOperator, Expression, Number};
use crate::tokenizer::{Token, TokenKind};

// precedence low to high
//
// conversion
// addition
// subtraction
// multiplication
// division
// unary
//
// Grammar:
//
// expression   →   conversion
// conversion   →   term → term | term
// term         →   factor ( ( "+" / "-") factor ) *
// factor       →   unary ( ( "*" / "/") unary ) *
// unary        →   "-" unary | primary
// primary      →   number | "(" expression ")"

pub struct Parser<'a> {
    tokens: &'a [Token],
    current: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Parser { tokens, current: 0 }
    }

    pub fn expression(&mut self) -> Expression {
        self.conversion()
    }

    fn conversion(&mut self) -> Expression {
        let lhs = self.term();
        if self.match_and_consume(TokenKind::Arrow).is_some() {
            let rhs = self.term();

            Expression::BinaryOperator(BinaryOperator::ConvertTo, Box::new(lhs), Box::new(rhs))
        } else {
            lhs
        }
    }

    fn term(&mut self) -> Expression {
        let lhs = self.factor();
        if self.match_and_consume(TokenKind::Plus).is_some() {
            let rhs = self.factor();

            Expression::BinaryOperator(BinaryOperator::Add, Box::new(lhs), Box::new(rhs))
        } else {
            lhs
        }
    }

    fn factor(&mut self) -> Expression {
        let lhs = self.unary();
        if self.match_and_consume(TokenKind::Multiply).is_some() {
            let rhs = self.unary();

            Expression::BinaryOperator(BinaryOperator::Mul, Box::new(lhs), Box::new(rhs))
        } else {
            lhs
        }
    }

    fn unary(&mut self) -> Expression {
        if self.match_and_consume(TokenKind::Minus).is_some() {
            let rhs = self.unary();

            Expression::Negate(Box::new(rhs))
        } else {
            self.primary()
        }
    }
    fn primary(&mut self) -> Expression {
        let num = self
            .match_and_consume(TokenKind::Number)
            .expect("Expected number");
        Expression::Scalar(Number::from_f64(num.lexeme.parse::<f64>().unwrap()))
    }

    fn match_and_consume(&mut self, token_kind: TokenKind) -> Option<&'a Token> {
        let token = &self.tokens[self.current];
        if token.kind == token_kind {
            self.current += 1;
            Some(token)
        } else {
            None
        }
    }
}
