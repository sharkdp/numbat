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
// conversion   →   term ( "→" term ) *
// term         →   factor ( ( "+" | "-") factor ) *
// factor       →   unary ( ( "*" | "/") unary ) *
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
        let mut expr = self.term();
        while self.match_exact(TokenKind::Arrow).is_some() {
            let rhs = self.term();

            expr = Expression::BinaryOperator(
                BinaryOperator::ConvertTo,
                Box::new(expr),
                Box::new(rhs),
            );
        }
        expr
    }

    fn term(&mut self) -> Expression {
        let mut expr = self.factor();
        while let Some(operator_token) = self.match_any(&[TokenKind::Plus, TokenKind::Minus]) {
            let operator = if operator_token.kind == TokenKind::Plus {
                BinaryOperator::Add
            } else {
                BinaryOperator::Sub
            };

            let rhs = self.factor();

            expr = Expression::BinaryOperator(operator, Box::new(expr), Box::new(rhs));
        }
        expr
    }

    fn factor(&mut self) -> Expression {
        let mut expr = self.unary();
        while let Some(operator_token) = self.match_any(&[TokenKind::Multiply, TokenKind::Divide]) {
            let operator = if operator_token.kind == TokenKind::Multiply {
                BinaryOperator::Mul
            } else {
                BinaryOperator::Div
            };

            let rhs = self.unary();

            expr = Expression::BinaryOperator(operator, Box::new(expr), Box::new(rhs));
        }
        expr
    }

    fn unary(&mut self) -> Expression {
        if self.match_exact(TokenKind::Minus).is_some() {
            let rhs = self.unary();

            Expression::Negate(Box::new(rhs))
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> Expression {
        let num = self
            .match_exact(TokenKind::Number)
            .expect("Expected number");
        Expression::Scalar(Number::from_f64(num.lexeme.parse::<f64>().unwrap()))
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
