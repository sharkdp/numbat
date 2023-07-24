//! Numbat Parser
//!
//! Operator precedence, low to high
//! * postfix apply ("//")
//! * conversion ("->")
//! * addition ("+")
//! * subtraction ("-")
//! * multiplication ("*")
//! * division ("/")
//! * exponentiation ("^")
//! * unary minus ("-")
//!
//! Grammar:
//! ```txt
//! statement       →   expression | variable_decl | function_decl | dimension_decl | unit_decl | procedure_call | module_import
//!
//! variable_decl   →   …
//! function_decl   →   …
//! dimension_decl  →   …
//! unit_decl       →   …
//!
//! expression      →   postfix_apply
//! postfix_apply   →   conversion ( "//" identifier ) *
//! conversion      →   term ( "→" term ) *
//! term            →   factor ( ( "+" | "-") factor ) *
//! factor          →   unary ( ( "*" | "/") per_factor ) *
//! per_factor      →   modulo ( "per" modulo ) *
//! modulo          →   unary ( "%" unary ) *
//! unary           →   "-" unary | ifactor
//! ifactor         →   power ( " " power ) *
//! power           →   unicode_power ( "^" power )
//! unicode_power   →   call ( "⁻" ? ("¹" | "²" | "³" | "⁴" | "⁵" ) ) ?
//! call            →   primary ( "(" arguments? ")" ) ?
//! arguments       →   expression ( "," expression ) *
//! primary         →   number | hex-number | oct-number | bin-number | identifier | "(" expression ")"
//! ```

use crate::arithmetic::{Exponent, Rational};
use crate::ast::{BinaryOperator, DimensionExpression, Expression, ProcedureKind, Statement};
use crate::decorator::Decorator;
use crate::number::Number;
use crate::prefix_parser::AcceptsPrefix;
use crate::resolver::ModulePath;
use crate::span::Span;
use crate::tokenizer::{Token, TokenKind, TokenizerError, TokenizerErrorKind};

use num_traits::{CheckedDiv, FromPrimitive, Zero};
use thiserror::Error;

#[derive(Error, Debug, Clone, PartialEq, Eq)]
pub enum ParseErrorKind {
    #[error("{0}")]
    TokenizerError(TokenizerErrorKind),

    #[error("Expected one of: number, identifier, parenthesized expression")]
    ExpectedPrimary,

    #[error("Missing closing parenthesis ')'")]
    MissingClosingParen,

    #[error("Trailing characters: '{0}'")]
    TrailingCharacters(String),

    #[error("Expected identifier after 'let' keyword")]
    ExpectedIdentifierAfterLet,

    #[error("Expected '=' or ':' after identifier (and type annotation) in 'let' assignment")]
    ExpectedEqualOrColonAfterLetIdentifier,

    #[error("Expected identifier after 'fn' keyword. Note that some reserved words can not be used as function names.")]
    ExpectedIdentifierAfterFn,

    #[error("Expected function name after '//' postfix apply operator")]
    ExpectedIdentifierInPostfixApply,

    #[error("Expected dimension identifier, '1', or opening parenthesis")]
    ExpectedDimensionPrimary,

    #[error("Expected ',' or '>' in type parameter list")]
    ExpectedCommaOrRightAngleBracket,

    #[error("Expected identifier (type parameter name)")]
    ExpectedTypeParameterName,

    #[error("Expected opening parenthesis '(' in function definition")]
    ExpectedLeftParenInFunctionDefinition,

    #[error("Expected ',', '…', or ')' in function parameter list")]
    ExpectedCommaEllipsisOrRightParenInFunctionDefinition,

    #[error("Expected parameter name in function definition")]
    ExpectedParameterNameInFunctionDefinition,

    #[error("Only a single variadic parameter is allowed in a function definition")]
    OnlySingleVariadicParameter,

    #[error("Variadic parameters are only allowed in foreign functions (without body)")]
    VariadicParameterOnlyAllowedInForeignFunction,

    #[error("Expected identifier (dimension name)")]
    ExpectedIdentifierAfterDimension,

    #[error("Expected identifier (unit name)")]
    ExpectedIdentifierAfterUnit,

    #[error("Expected '=' or ':' after identifier in unit definition")]
    ExpectedColonOrEqualAfterUnitIdentifier,

    #[error("Only functions can be called")]
    CanOnlyCallIdentifier,

    #[error("Division by zero in dimension exponent")]
    DivisionByZeroInDimensionExponent,

    #[error("Expected opening parenthesis '(' after procedure name")]
    ExpectedLeftParenAfterProcedureName,

    #[error("Procedures can not be used inside an expression")]
    InlineProcedureUsage,

    #[error("Expected decorator name")]
    ExpectedDecoratorName,

    #[error("Unknown decorator name")]
    UnknownDecorator,

    #[error("Expected module path after 'use'")]
    ExpectedModulePathAfterUse,

    #[error("Expected module name after double colon (::)")]
    ExpectedModuleNameAfterDoubleColon,

    #[error("Overflow in number literal")]
    OverflowInNumberLiteral,

    #[error("Expected dimension exponent")]
    ExpectedDimensionExponent,

    #[error("Double-underscore type names are reserved for internal use")]
    DoubleUnderscoreTypeNamesReserved,

    #[error("Only integer numbers (< 2^128) are allowed in dimension exponents")]
    NumberInDimensionExponentOutOfRange,

    #[error("Decorators can only be used on unit definitions")]
    DecoratorsCanOnlyBeUsedOnUnitDefinitions,

    #[error("Expected opening parenthesis after decorator")]
    ExpectedLeftParenAfterDecorator,

    #[error("Unknown alias annotation")]
    UnknownAliasAnnotation,

    #[error("Numerical overflow in dimension exponent")]
    OverflowInDimensionExponent,
}

#[derive(Debug, Clone, Error)]
#[error("{kind}")]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub span: Span,
}

impl ParseError {
    fn new(kind: ParseErrorKind, span: Span) -> Self {
        ParseError { kind, span }
    }
}

type Result<T> = std::result::Result<T, ParseError>;

struct Parser<'a> {
    tokens: &'a [Token],
    current: usize,
    decorator_stack: Vec<Decorator>,
}

impl<'a> Parser<'a> {
    pub(crate) fn new(tokens: &'a [Token]) -> Self {
        Parser {
            tokens,
            current: 0,
            decorator_stack: vec![],
        }
    }

    fn skip_empty_lines(&mut self) {
        while self.match_exact(TokenKind::Newline).is_some() {}
    }

    fn parse(&mut self) -> Result<Vec<Statement>> {
        let mut statements = vec![];

        self.skip_empty_lines();

        while !self.is_at_end() {
            match self.statement() {
                Ok(statement) => statements.push(statement),
                Err(e) => {
                    return Err(e);
                }
            }

            match self.peek().kind {
                TokenKind::Newline => {
                    // Skip over empty lines
                    while self.match_exact(TokenKind::Newline).is_some() {}
                }
                TokenKind::Eof => {
                    break;
                }
                _ => {
                    return Err(ParseError {
                        kind: ParseErrorKind::TrailingCharacters(self.peek().lexeme.clone()),
                        span: self.peek().span,
                    });
                }
            }
        }

        Ok(statements)
    }

    fn accepts_prefix(&mut self) -> Result<Option<AcceptsPrefix>> {
        if self.match_exact(TokenKind::Colon).is_some() {
            if self.match_exact(TokenKind::Long).is_some() {
                Ok(Some(AcceptsPrefix::only_long()))
            } else if self.match_exact(TokenKind::Short).is_some() {
                Ok(Some(AcceptsPrefix::only_short()))
            } else if self.match_exact(TokenKind::Both).is_some() {
                Ok(Some(AcceptsPrefix::both()))
            } else if self.match_exact(TokenKind::None).is_some() {
                Ok(Some(AcceptsPrefix::none()))
            } else {
                return Err(ParseError::new(
                    ParseErrorKind::UnknownAliasAnnotation,
                    self.peek().span,
                ));
            }
        } else {
            Ok(None)
        }
    }

    fn list_of_aliases(&mut self) -> Result<Vec<(String, Option<AcceptsPrefix>)>> {
        if self.match_exact(TokenKind::RightParen).is_some() {
            return Ok(vec![]);
        }

        let mut identifiers: Vec<(String, Option<AcceptsPrefix>)> =
            vec![(self.identifier()?, self.accepts_prefix()?)];
        while self.match_exact(TokenKind::Comma).is_some() {
            identifiers.push((self.identifier()?, self.accepts_prefix()?));
        }

        if self.match_exact(TokenKind::RightParen).is_none() {
            return Err(ParseError::new(
                ParseErrorKind::MissingClosingParen,
                self.peek().span,
            ));
        }

        Ok(identifiers)
    }

    fn statement(&mut self) -> Result<Statement> {
        if !(self.peek().kind == TokenKind::At
            || self.peek().kind == TokenKind::Unit
            || self.decorator_stack.is_empty())
        {
            return Err(ParseError {
                kind: ParseErrorKind::DecoratorsCanOnlyBeUsedOnUnitDefinitions,
                span: self.peek().span,
            });
        }

        if self.match_exact(TokenKind::Let).is_some() {
            if let Some(identifier) = self.match_exact(TokenKind::Identifier) {
                let identifier_span = self.last().unwrap().span;

                let dexpr = if self.match_exact(TokenKind::Colon).is_some() {
                    Some(self.dimension_expression()?)
                } else {
                    None
                };

                if self.match_exact(TokenKind::Equal).is_none() {
                    Err(ParseError {
                        kind: ParseErrorKind::ExpectedEqualOrColonAfterLetIdentifier,
                        span: self.peek().span,
                    })
                } else {
                    self.skip_empty_lines();
                    let expr = self.expression()?;

                    Ok(Statement::DefineVariable {
                        identifier_span,
                        identifier: identifier.lexeme.clone(),
                        expr,
                        type_annotation: dexpr,
                    })
                }
            } else {
                Err(ParseError {
                    kind: ParseErrorKind::ExpectedIdentifierAfterLet,
                    span: self.peek().span,
                })
            }
        } else if self.match_exact(TokenKind::Fn).is_some() {
            if let Some(fn_name) = self.match_exact(TokenKind::Identifier) {
                let function_name_span = self.last().unwrap().span;
                let mut type_parameters = vec![];
                if self.match_exact(TokenKind::LeftAngleBracket).is_some() {
                    while self.match_exact(TokenKind::RightAngleBracket).is_none() {
                        if let Some(type_parameter_name) = self.match_exact(TokenKind::Identifier) {
                            let span = self.last().unwrap().span;
                            type_parameters.push((span, type_parameter_name.lexeme.to_string()));

                            if self.match_exact(TokenKind::Comma).is_none()
                                && self.peek().kind != TokenKind::RightAngleBracket
                            {
                                return Err(ParseError {
                                    kind: ParseErrorKind::ExpectedCommaOrRightAngleBracket,
                                    span: self.peek().span,
                                });
                            }
                        } else {
                            return Err(ParseError {
                                kind: ParseErrorKind::ExpectedTypeParameterName,
                                span: self.peek().span,
                            });
                        }
                    }
                }

                if self.match_exact(TokenKind::LeftParen).is_none() {
                    return Err(ParseError {
                        kind: ParseErrorKind::ExpectedLeftParenInFunctionDefinition,
                        span: self.peek().span,
                    });
                }

                let mut parameter_span = self.peek().span;

                let mut parameters = vec![];
                while self.match_exact(TokenKind::RightParen).is_none() {
                    if let Some(param_name) = self.match_exact(TokenKind::Identifier) {
                        let span = self.last().unwrap().span;
                        let param_type_dexpr = if self.match_exact(TokenKind::Colon).is_some() {
                            Some(self.dimension_expression()?)
                        } else {
                            None
                        };

                        let is_variadic = self.match_exact(TokenKind::Ellipsis).is_some();

                        parameters.push((
                            span,
                            param_name.lexeme.to_string(),
                            param_type_dexpr,
                            is_variadic,
                        ));

                        parameter_span = parameter_span.extend(&self.last().unwrap().span);

                        if self.match_exact(TokenKind::Comma).is_none()
                            && self.peek().kind != TokenKind::RightParen
                        {
                            return Err(ParseError {
                                kind: ParseErrorKind::ExpectedCommaEllipsisOrRightParenInFunctionDefinition,
                                span: self.peek().span,
                            });
                        }
                    } else {
                        return Err(ParseError {
                            kind: ParseErrorKind::ExpectedParameterNameInFunctionDefinition,
                            span: self.peek().span,
                        });
                    }
                }

                let (return_type_span, return_type_annotation) =
                    if self.match_exact(TokenKind::Arrow).is_some() {
                        // Parse return type
                        let return_type_annotation = self.dimension_expression()?;
                        (
                            Some(self.last().unwrap().span),
                            Some(return_type_annotation),
                        )
                    } else {
                        (None, None)
                    };

                let fn_is_variadic = parameters.iter().any(|p| p.3);
                if fn_is_variadic && parameters.len() > 1 {
                    return Err(ParseError {
                        kind: ParseErrorKind::OnlySingleVariadicParameter,
                        span: parameter_span,
                    });
                }

                let body = if self.match_exact(TokenKind::Equal).is_none() {
                    None
                } else {
                    self.skip_empty_lines();
                    Some(self.expression()?)
                };

                if fn_is_variadic && body.is_some() {
                    return Err(ParseError {
                        kind: ParseErrorKind::VariadicParameterOnlyAllowedInForeignFunction,
                        span: parameter_span,
                    });
                }

                Ok(Statement::DefineFunction {
                    function_name_span,
                    function_name: fn_name.lexeme.clone(),
                    type_parameters,
                    parameters,
                    body,
                    return_type_span,
                    return_type_annotation,
                })
            } else {
                Err(ParseError {
                    kind: ParseErrorKind::ExpectedIdentifierAfterFn,
                    span: self.peek().span,
                })
            }
        } else if self.match_exact(TokenKind::Dimension).is_some() {
            if let Some(identifier) = self.match_exact(TokenKind::Identifier) {
                if identifier.lexeme.starts_with("__") {
                    return Err(ParseError::new(
                        ParseErrorKind::DoubleUnderscoreTypeNamesReserved,
                        self.peek().span,
                    ));
                }

                if self.match_exact(TokenKind::Equal).is_some() {
                    self.skip_empty_lines();
                    let mut dexprs = vec![self.dimension_expression()?];

                    while self.match_exact(TokenKind::Equal).is_some() {
                        self.skip_empty_lines();
                        dexprs.push(self.dimension_expression()?);
                    }

                    Ok(Statement::DefineDimension(
                        identifier.lexeme.clone(),
                        dexprs,
                    ))
                } else {
                    Ok(Statement::DefineDimension(
                        identifier.lexeme.clone(),
                        vec![],
                    ))
                }
            } else {
                Err(ParseError {
                    kind: ParseErrorKind::ExpectedIdentifierAfterDimension,
                    span: self.peek().span,
                })
            }
        } else if self.match_exact(TokenKind::At).is_some() {
            if let Some(decorator) = self.match_exact(TokenKind::Identifier) {
                let decorator = if decorator.lexeme == "metric_prefixes" {
                    Decorator::MetricPrefixes
                } else if decorator.lexeme == "binary_prefixes" {
                    Decorator::BinaryPrefixes
                } else if decorator.lexeme == "aliases" {
                    if self.match_exact(TokenKind::LeftParen).is_some() {
                        let aliases = self.list_of_aliases()?;
                        Decorator::Aliases(aliases)
                    } else {
                        return Err(ParseError {
                            kind: ParseErrorKind::ExpectedLeftParenAfterDecorator,
                            span: self.peek().span,
                        });
                    }
                } else {
                    return Err(ParseError {
                        kind: ParseErrorKind::UnknownDecorator,
                        span: decorator.span,
                    });
                };

                self.decorator_stack.push(decorator); // TODO: make sure that there are no duplicate decorators

                // A decorator is not yet a full statement. Continue parsing:
                self.skip_empty_lines();
                self.statement()
            } else {
                Err(ParseError {
                    kind: ParseErrorKind::ExpectedDecoratorName,
                    span: self.peek().span,
                })
            }
        } else if self.match_exact(TokenKind::Unit).is_some() {
            if let Some(identifier) = self.match_exact(TokenKind::Identifier) {
                let identifier_span = self.last().unwrap().span;
                let (type_annotation_span, dexpr) = if self.match_exact(TokenKind::Colon).is_some()
                {
                    let type_annotation = self.dimension_expression()?;
                    (Some(self.last().unwrap().span), Some(type_annotation))
                } else {
                    (None, None)
                };

                let unit_name = identifier.lexeme.clone();

                let mut decorators = vec![];
                std::mem::swap(&mut decorators, &mut self.decorator_stack);

                if self.match_exact(TokenKind::Equal).is_some() {
                    self.skip_empty_lines();
                    let expr = self.expression()?;
                    Ok(Statement::DefineDerivedUnit {
                        identifier_span,
                        identifier: unit_name,
                        expr,
                        type_annotation_span,
                        type_annotation: dexpr,
                        decorators,
                    })
                } else if let Some(dexpr) = dexpr {
                    Ok(Statement::DefineBaseUnit(
                        identifier_span,
                        unit_name,
                        Some(dexpr),
                        decorators,
                    ))
                } else if self.is_end_of_statement() {
                    Ok(Statement::DefineBaseUnit(
                        identifier_span,
                        unit_name,
                        None,
                        decorators,
                    ))
                } else {
                    Err(ParseError {
                        kind: ParseErrorKind::ExpectedColonOrEqualAfterUnitIdentifier,
                        span: self.peek().span,
                    })
                }
            } else {
                Err(ParseError {
                    kind: ParseErrorKind::ExpectedIdentifierAfterUnit,
                    span: self.peek().span,
                })
            }
        } else if self.match_exact(TokenKind::Use).is_some() {
            let mut span = self.peek().span;

            if let Some(identifier) = self.match_exact(TokenKind::Identifier) {
                let mut module_path = vec![identifier.lexeme.clone()];

                while self.match_exact(TokenKind::DoubleColon).is_some() {
                    if let Some(identifier) = self.match_exact(TokenKind::Identifier) {
                        module_path.push(identifier.lexeme.clone());
                    } else {
                        return Err(ParseError {
                            kind: ParseErrorKind::ExpectedModuleNameAfterDoubleColon,
                            span: self.peek().span,
                        });
                    }
                }
                span = span.extend(&self.last().unwrap().span);

                Ok(Statement::ModuleImport(span, ModulePath(module_path)))
            } else {
                Err(ParseError {
                    kind: ParseErrorKind::ExpectedModulePathAfterUse,
                    span: self.peek().span,
                })
            }
        } else if self
            .match_any(&[TokenKind::ProcedurePrint, TokenKind::ProcedureAssertEq])
            .is_some()
        {
            let span = self.last().unwrap().span;
            let procedure_kind = match self.last().unwrap().kind {
                TokenKind::ProcedurePrint => ProcedureKind::Print,
                TokenKind::ProcedureAssertEq => ProcedureKind::AssertEq,
                _ => unreachable!(),
            };

            if self.match_exact(TokenKind::LeftParen).is_none() {
                Err(ParseError {
                    kind: ParseErrorKind::ExpectedLeftParenAfterProcedureName,
                    span: self.peek().span,
                })
            } else {
                Ok(Statement::ProcedureCall(
                    span,
                    procedure_kind,
                    self.arguments()?,
                ))
            }
        } else {
            Ok(Statement::Expression(self.expression()?))
        }
    }

    pub fn expression(&mut self) -> Result<Expression> {
        self.postfix_apply()
    }

    fn function_name_from_primary(&self, primary: &Expression) -> Result<String> {
        if let Expression::Identifier(_, name) = primary {
            Ok(name.clone())
        } else {
            Err(ParseError::new(
                ParseErrorKind::CanOnlyCallIdentifier,
                primary.full_span(),
            ))
        }
    }

    fn identifier(&mut self) -> Result<String> {
        if let Some(identifier) = self.match_exact(TokenKind::Identifier) {
            Ok(identifier.lexeme.clone())
        } else {
            Err(ParseError::new(
                ParseErrorKind::ExpectedIdentifierInPostfixApply,
                self.peek().span,
            ))
        }
    }

    pub fn postfix_apply(&mut self) -> Result<Expression> {
        let mut expr = self.conversion()?;
        let mut full_span = expr.full_span();
        while self.match_exact(TokenKind::PostfixApply).is_some() {
            let identifier = self.identifier()?;
            let identifier_span = self.last().unwrap().span;
            full_span = full_span.extend(&identifier_span);

            expr = Expression::FunctionCall(identifier_span, full_span, identifier, vec![expr]);
        }
        Ok(expr)
    }

    fn conversion(&mut self) -> Result<Expression> {
        let mut expr = self.term()?;
        while self.match_exact(TokenKind::Arrow).is_some() {
            let span_op = Some(self.last().unwrap().span);
            let rhs = self.term()?;

            expr = Expression::BinaryOperator {
                op: BinaryOperator::ConvertTo,
                lhs: Box::new(expr),
                rhs: Box::new(rhs),
                span_op,
            };
        }
        Ok(expr)
    }

    fn term(&mut self) -> Result<Expression> {
        let mut expr = self.factor()?;
        while let Some(operator_token) = self.match_any(&[TokenKind::Plus, TokenKind::Minus]) {
            let span_op = Some(self.last().unwrap().span);
            let operator = if operator_token.kind == TokenKind::Plus {
                BinaryOperator::Add
            } else {
                BinaryOperator::Sub
            };

            let rhs = self.factor()?;

            expr = Expression::BinaryOperator {
                op: operator,
                lhs: Box::new(expr),
                rhs: Box::new(rhs),
                span_op,
            };
        }
        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expression> {
        let mut expr = self.per_factor()?;
        while let Some(operator_token) = self.match_any(&[TokenKind::Multiply, TokenKind::Divide]) {
            let span_op = Some(self.last().unwrap().span);
            let op = if operator_token.kind == TokenKind::Multiply {
                BinaryOperator::Mul
            } else {
                BinaryOperator::Div
            };

            let rhs = self.per_factor()?;

            expr = Expression::BinaryOperator {
                op,
                lhs: Box::new(expr),
                rhs: Box::new(rhs),
                span_op,
            };
        }
        Ok(expr)
    }

    fn per_factor(&mut self) -> Result<Expression> {
        let mut expr = self.modulo()?;

        while self.match_exact(TokenKind::Per).is_some() {
            let span_op = Some(self.last().unwrap().span);
            let rhs = self.modulo()?;

            expr = Expression::BinaryOperator {
                op: BinaryOperator::Div,
                lhs: Box::new(expr),
                rhs: Box::new(rhs),
                span_op,
            };
        }

        Ok(expr)
    }

    fn modulo(&mut self) -> Result<Expression> {
        let mut expr = self.unary()?;
        let mut full_span = expr.full_span();

        while self.match_exact(TokenKind::Modulo).is_some() {
            let op_span = self.last().unwrap().span;
            let rhs = self.modulo()?;

            full_span = full_span.extend(&rhs.full_span());

            expr = Expression::FunctionCall(op_span, full_span, "mod".into(), vec![expr, rhs]);
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expression> {
        if self.match_exact(TokenKind::Minus).is_some() {
            let span = self.last().unwrap().span;
            let rhs = self.unary()?;

            Ok(Expression::Negate(span, Box::new(rhs)))
        } else {
            self.ifactor()
        }
    }

    fn ifactor(&mut self) -> Result<Expression> {
        let mut expr = self.power()?;

        while self.next_token_could_start_power_expression() {
            let rhs = self.power()?;
            expr = Expression::BinaryOperator {
                op: BinaryOperator::Mul,
                lhs: Box::new(expr),
                rhs: Box::new(rhs),
                span_op: None,
            };
        }

        Ok(expr)
    }

    fn power(&mut self) -> Result<Expression> {
        let mut expr = self.unicode_power()?;
        if self.match_exact(TokenKind::Power).is_some() {
            let span_op = Some(self.last().unwrap().span);
            let rhs = self.power()?;

            expr = Expression::BinaryOperator {
                op: BinaryOperator::Power,
                lhs: Box::new(expr),
                rhs: Box::new(rhs),
                span_op,
            };
        }
        Ok(expr)
    }

    fn unicode_power(&mut self) -> Result<Expression> {
        let mut expr = self.call()?;

        if let Some(exponent) = self.match_exact(TokenKind::UnicodeExponent) {
            let exp = match exponent.lexeme.as_str() {
                "⁻¹" => -1,
                "⁻²" => -2,
                "⁻³" => -3,
                "⁻⁴" => -4,
                "⁻⁵" => -5,
                "⁻⁶" => -6,
                "⁻⁷" => -7,
                "⁻⁸" => -8,
                "⁻⁹" => -9,
                "¹" => 1,
                "²" => 2,
                "³" => 3,
                "⁴" => 4,
                "⁵" => 5,
                "⁶" => 6,
                "⁷" => 7,
                "⁸" => 8,
                "⁹" => 9,
                _ => unreachable!(
                    "Tokenizer should not generate unicode exponent tokens for anything else"
                ),
            };

            expr = Expression::BinaryOperator {
                op: BinaryOperator::Power,
                lhs: Box::new(expr),
                rhs: Box::new(Expression::Scalar(
                    exponent.span,
                    Number::from_f64(exp as f64),
                )),
                span_op: None,
            };
        }

        Ok(expr)
    }

    fn call(&mut self) -> Result<Expression> {
        let primary = self.primary()?;

        if self.match_exact(TokenKind::LeftParen).is_some() {
            let function_name = self.function_name_from_primary(&primary)?;

            let args = self.arguments()?;
            return Ok(Expression::FunctionCall(
                primary.full_span(),
                primary.full_span().extend(&self.last().unwrap().span),
                function_name,
                args,
            ));
        }
        Ok(primary)
    }

    fn arguments(&mut self) -> Result<Vec<Expression>> {
        if self.match_exact(TokenKind::RightParen).is_some() {
            return Ok(vec![]);
        }

        let mut args: Vec<Expression> = vec![self.expression()?];
        while self.match_exact(TokenKind::Comma).is_some() {
            args.push(self.expression()?);
        }

        if self.match_exact(TokenKind::RightParen).is_none() {
            return Err(ParseError::new(
                ParseErrorKind::MissingClosingParen,
                self.peek().span,
            ));
        }

        Ok(args)
    }

    fn primary(&mut self) -> Result<Expression> {
        // This function needs to be kept in sync with `next_token_could_start_primary` below.

        let overflow_error = |span| {
            Err(ParseError::new(
                ParseErrorKind::OverflowInNumberLiteral,
                span,
            ))
        };

        if let Some(num) = self.match_exact(TokenKind::Number) {
            let num_string = num.lexeme.replace("_", "");
            Ok(Expression::Scalar(
                self.last().unwrap().span,
                Number::from_f64(num_string.parse::<f64>().unwrap()),
            ))
        } else if let Some(hex_int) = self.match_exact(TokenKind::IntegerWithBase(16)) {
            let span = self.last().unwrap().span;
            Ok(Expression::Scalar(
                span,
                Number::from_f64(
                    i128::from_str_radix(&hex_int.lexeme[2..], 16)
                        .or_else(|_| overflow_error(span))? as f64, // TODO: i128 limits our precision here
                ),
            ))
        } else if let Some(oct_int) = self.match_exact(TokenKind::IntegerWithBase(8)) {
            let span = self.last().unwrap().span;
            Ok(Expression::Scalar(
                span,
                Number::from_f64(
                    i128::from_str_radix(&oct_int.lexeme[2..], 8)
                        .or_else(|_| overflow_error(span))? as f64, // TODO: i128 limits our precision here
                ),
            ))
        } else if let Some(bin_int) = self.match_exact(TokenKind::IntegerWithBase(2)) {
            let span = self.last().unwrap().span;
            Ok(Expression::Scalar(
                span,
                Number::from_f64(
                    i128::from_str_radix(&bin_int.lexeme[2..], 2)
                        .or_else(|_| overflow_error(span))? as f64, // TODO: i128 limits our precision here
                ),
            ))
        } else if let Some(identifier) = self.match_exact(TokenKind::Identifier) {
            let span = self.last().unwrap().span;
            Ok(Expression::Identifier(span, identifier.lexeme.clone()))
        } else if self.match_exact(TokenKind::LeftParen).is_some() {
            let inner = self.expression()?;

            if self.match_exact(TokenKind::RightParen).is_none() {
                return Err(ParseError::new(
                    ParseErrorKind::MissingClosingParen,
                    self.peek().span,
                ));
            }

            Ok(inner)
        } else if matches!(
            self.peek().kind,
            TokenKind::ProcedurePrint | TokenKind::ProcedureAssertEq
        ) {
            Err(ParseError::new(
                ParseErrorKind::InlineProcedureUsage,
                self.peek().span,
            ))
        } else {
            Err(ParseError::new(
                ParseErrorKind::ExpectedPrimary,
                self.peek().span,
            ))
        }
    }

    /// Returns true iff the upcoming token indicates the beginning of a 'power'
    /// expression (which needs to start with a 'primary' expression).
    fn next_token_could_start_power_expression(&self) -> bool {
        // This function needs to be kept in sync with `primary` above.

        matches!(
            self.peek().kind,
            TokenKind::Number | TokenKind::Identifier | TokenKind::LeftParen
        )
    }

    pub(crate) fn dimension_expression(&mut self) -> Result<DimensionExpression> {
        self.dimension_factor()
    }

    fn dimension_factor(&mut self) -> Result<DimensionExpression> {
        let mut expr = self.dimension_power()?;
        while let Some(operator_token) = self.match_any(&[TokenKind::Multiply, TokenKind::Divide]) {
            let span = self.last().unwrap().span;
            let rhs = self.dimension_power()?;

            expr = if operator_token.kind == TokenKind::Multiply {
                DimensionExpression::Multiply(span, Box::new(expr), Box::new(rhs))
            } else {
                DimensionExpression::Divide(span, Box::new(expr), Box::new(rhs))
            };
        }
        Ok(expr)
    }

    fn dimension_power(&mut self) -> Result<DimensionExpression> {
        let expr = self.dimension_primary()?;

        if self.match_exact(TokenKind::Power).is_some() {
            let span = self.last().unwrap().span;
            let (span_exponent, exponent) = self.dimension_exponent()?;

            Ok(DimensionExpression::Power(
                span,
                Box::new(expr),
                span_exponent,
                exponent,
            ))
        } else {
            Ok(expr)
        }
    }

    fn dimension_exponent(&mut self) -> Result<(Span, Exponent)> {
        // TODO: potentially allow for ², ³, etc.

        if let Some(token) = self.match_exact(TokenKind::Number) {
            let span = self.last().unwrap().span;
            let num_str = token.lexeme.replace("_", "");
            Ok((
                span,
                Rational::from_i128(num_str.parse::<i128>().map_err(|_| ParseError {
                    kind: ParseErrorKind::NumberInDimensionExponentOutOfRange,
                    span: token.span,
                })?)
                .ok_or_else(|| ParseError {
                    kind: ParseErrorKind::NumberInDimensionExponentOutOfRange,
                    span: token.span,
                })?,
            ))
        } else if self.match_exact(TokenKind::Minus).is_some() {
            let span = self.last().unwrap().span;
            let (span_inner, exponent) = self.dimension_exponent()?;
            Ok((span.extend(&span_inner), -exponent))
        } else if self.match_exact(TokenKind::LeftParen).is_some() {
            let mut span = self.last().unwrap().span;
            let (span_inner, exponent) = self.dimension_exponent()?;
            span = span.extend(&span_inner);
            if self.match_exact(TokenKind::RightParen).is_some() {
                span = span.extend(&self.last().unwrap().span);
                Ok((span, exponent))
            } else if self.match_exact(TokenKind::Divide).is_some() {
                let (span_rhs, rhs) = self.dimension_exponent()?;
                span = span.extend(&span_rhs);
                if rhs == Rational::zero() {
                    Err(ParseError::new(
                        ParseErrorKind::DivisionByZeroInDimensionExponent,
                        self.last().unwrap().span,
                    ))
                } else if self.match_exact(TokenKind::RightParen).is_none() {
                    Err(ParseError::new(
                        ParseErrorKind::MissingClosingParen,
                        self.peek().span,
                    ))
                } else {
                    span = span.extend(&self.last().unwrap().span);
                    Ok((
                        span,
                        exponent.checked_div(&rhs).ok_or_else(|| {
                            ParseError::new(ParseErrorKind::OverflowInDimensionExponent, span)
                        })?,
                    ))
                }
            } else {
                Err(ParseError::new(
                    ParseErrorKind::MissingClosingParen,
                    self.peek().span,
                ))
            }
        } else {
            Err(ParseError::new(
                ParseErrorKind::ExpectedDimensionExponent,
                self.peek().span,
            ))
        }
    }

    fn dimension_primary(&mut self) -> Result<DimensionExpression> {
        let e = Err(ParseError::new(
            ParseErrorKind::ExpectedDimensionPrimary,
            self.peek().span,
        ));
        if let Some(token) = self.match_exact(TokenKind::Identifier) {
            if token.lexeme.starts_with("__") {
                return Err(ParseError::new(
                    ParseErrorKind::DoubleUnderscoreTypeNamesReserved,
                    self.peek().span,
                ));
            }
            let span = self.last().unwrap().span;
            Ok(DimensionExpression::Dimension(span, token.lexeme.clone()))
        } else if let Some(number) = self.match_exact(TokenKind::Number) {
            let span = self.last().unwrap().span;
            if number.lexeme != "1" {
                e
            } else {
                Ok(DimensionExpression::Unity(span))
            }
        } else if self.match_exact(TokenKind::LeftParen).is_some() {
            let dexpr = self.dimension_expression()?;
            if self.match_exact(TokenKind::RightParen).is_none() {
                return Err(ParseError::new(
                    ParseErrorKind::MissingClosingParen,
                    self.peek().span,
                ));
            }
            Ok(dexpr)
        } else {
            e
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

    fn last(&self) -> Option<&'a Token> {
        self.tokens.get(self.current - 1)
    }

    pub fn is_end_of_statement(&self) -> bool {
        self.peek().kind == TokenKind::Newline || self.is_at_end()
    }

    pub fn is_at_end(&self) -> bool {
        self.peek().kind == TokenKind::Eof
    }
}

pub fn parse(input: &str, code_source_index: usize) -> Result<Vec<Statement>> {
    use crate::tokenizer::tokenize;

    let tokens = tokenize(input, code_source_index).map_err(|TokenizerError { kind, span }| {
        ParseError::new(ParseErrorKind::TokenizerError(kind), span)
    })?;
    let mut parser = Parser::new(&tokens);
    parser.parse()
}

#[cfg(test)]
pub fn parse_dexpr(input: &str) -> DimensionExpression {
    let tokens = crate::tokenizer::tokenize(input, 0).expect("No tokenizer errors in tests");
    let mut parser = crate::parser::Parser::new(&tokens);
    let expr = parser
        .dimension_expression()
        .expect("No parser errors in tests");
    assert!(parser.is_at_end());
    expr
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{binop, identifier, negate, scalar, ReplaceSpans};

    fn parse_as(inputs: &[&str], statement_expected: Statement) {
        for input in inputs {
            let statements = parse(input, 0).expect("parse error").replace_spans();

            assert!(statements.len() == 1);
            let statement = &statements[0];

            assert_eq!(*statement, statement_expected);
        }
    }

    fn parse_as_expression(inputs: &[&str], expr_expected: Expression) {
        parse_as(inputs, Statement::Expression(expr_expected));
    }

    fn should_fail(inputs: &[&str]) {
        for input in inputs {
            assert!(parse(input, 0).is_err());
        }
    }

    fn should_fail_with(inputs: &[&str], error_kind: ParseErrorKind) {
        for input in inputs {
            match parse(input, 0) {
                Err(e) => {
                    assert_eq!(e.kind, error_kind);
                }
                _ => {
                    panic!();
                }
            }
        }
    }

    #[test]
    fn invalid_input() {
        should_fail(&["+", "->", "§"]);

        should_fail_with(
            &["1)", "(1))"],
            ParseErrorKind::TrailingCharacters(")".into()),
        );
    }

    #[test]
    fn numbers_simple() {
        parse_as_expression(&["1", "1.0", "  1   ", " 1.0000   ", "1."], scalar!(1.0));

        parse_as_expression(&["0.2", "  0.2  ", ".2"], scalar!(0.2));

        parse_as_expression(&["3.5", "  3.5  ", "3.50"], scalar!(3.5));

        parse_as_expression(&["0.05"], scalar!(0.05));

        parse_as_expression(&["123.456"], scalar!(123.456));

        should_fail(&[
            "123..", "0..", ".0.", ".", ". 2", "..2", "..", ".1.1", "0.1.",
        ]);
    }

    #[test]
    fn large_numbers() {
        parse_as_expression(
            &["1234567890000000", "1234567890000000.0"],
            scalar!(1234567890000000.0),
        );
    }

    #[test]
    fn decimal_separator() {
        parse_as_expression(
            &["50_000_000", "50_000_000.0", "50_000000"],
            scalar!(50_000_000.0),
        );
        parse_as_expression(&["1_000"], scalar!(1000.0));
        parse_as_expression(&["1.000001", "1.000_001"], scalar!(1.000_001));
        parse_as_expression(&["1e1_0_0"], scalar!(1e100));

        // Leading underscores are not allowed / will result in parsing as identifier
        parse_as_expression(&["_50_000_000"], identifier!("_50_000_000"));
        should_fail(&["1._0", "1e+_0", "1e-_0"]);

        // Trailing underscores are not allowed
        should_fail(&["100_", "1.00_", "1e2_"]);
    }

    #[test]
    fn negation() {
        parse_as_expression(&["-1", "  - 1   "], negate!(scalar!(1.0)));
        parse_as_expression(&["-123.45"], negate!(scalar!(123.45)));
        parse_as_expression(&["--1", " -  - 1   "], negate!(negate!(scalar!(1.0))));
        parse_as_expression(&["-x", " - x"], negate!(identifier!("x")));
        parse_as_expression(&["-0.61", "-.61", "-  .61"], negate!(scalar!(0.61)));

        parse_as_expression(
            &["-1 + 2"],
            binop!(negate!(scalar!(1.0)), Add, scalar!(2.0)),
        );
    }

    #[test]
    fn scientific_notation() {
        parse_as_expression(&["1e3"], scalar!(1.0e3));
        parse_as_expression(&["1e+3"], scalar!(1.0e+3));
        parse_as_expression(&["1e-3"], scalar!(1.0e-3));

        parse_as_expression(&["-1e-3"], negate!(scalar!(1.0e-3)));

        parse_as_expression(&["123.456e12"], scalar!(123.456e12));
        parse_as_expression(&["123.456e+12"], scalar!(123.456e+12));
        parse_as_expression(&["123.456e-12"], scalar!(123.456e-12));

        should_fail(&["1e++2", "1e+-2", "1e+", "1e-"]);

        should_fail(&["2e1.5", "e.2e3e"]);

        parse_as_expression(&["1e", "1.0e"], binop!(scalar!(1.0), Mul, identifier!("e")));
        parse_as_expression(&["1ee"], binop!(scalar!(1.0), Mul, identifier!("ee")));
        parse_as_expression(&["1eV"], binop!(scalar!(1.0), Mul, identifier!("eV")));
        parse_as_expression(&["1erg"], binop!(scalar!(1.0), Mul, identifier!("erg")));
    }

    #[test]
    fn hex_oct_bin() {
        parse_as_expression(&["0x6A", "0x6a", "0b1101010", "0o152"], scalar!(106.0));
        parse_as_expression(&["0xFF", "0xff", "0b11111111", "0o377"], scalar!(255.0));

        parse_as_expression(&["-0x3", "-0b11", "-0o3"], negate!(scalar!(3.0)));

        should_fail(&["0x"]);
        should_fail(&["0o"]);
        should_fail(&["0b"]);
        should_fail(&["0xG"]);
        should_fail(&["0oG"]);
        should_fail(&["0oA"]);
        should_fail(&["0o8"]);
        should_fail(&["0bF"]);
        should_fail(&["0b2"]);
        should_fail(&["0xABCDU"]);
        should_fail(&["0o12348"]);
        should_fail(&["0b10102"]);

        // Until we support hexadecimal float notation
        should_fail(&["0x1.2", "0b1.0", "0o1.0", "0x.1", "0b.0", "0o.1"]);
    }

    #[test]
    fn identifiers() {
        parse_as_expression(&["foo", "  foo   "], identifier!("foo"));
        parse_as_expression(&["foo_bar"], identifier!("foo_bar"));
        parse_as_expression(&["MeineSchöneVariable"], identifier!("MeineSchöneVariable"));
        parse_as_expression(&["°"], identifier!("°"));
    }

    #[test]
    fn addition_and_subtraction() {
        parse_as_expression(
            &["1+2", "  1   +  2    "],
            binop!(scalar!(1.0), Add, scalar!(2.0)),
        );

        // Minus should be left-associative
        parse_as_expression(
            &["1-2-3", "1 - 2 - 3", "(1-2)-3"],
            binop!(binop!(scalar!(1.0), Sub, scalar!(2.0)), Sub, scalar!(3.0)),
        );

        should_fail(&["1+", "1-"]);
    }

    #[test]
    fn multiplication_and_division() {
        parse_as_expression(
            &["1*2", "  1   *  2    ", "1 · 2", "1 × 2"],
            binop!(scalar!(1.0), Mul, scalar!(2.0)),
        );

        parse_as_expression(
            &["1/2", "1 per 2", "1÷2"],
            binop!(scalar!(1.0), Div, scalar!(2.0)),
        );

        parse_as_expression(
            &["1/2/3", "(1/2)/3", "1 per 2 per 3"],
            binop!(binop!(scalar!(1.0), Div, scalar!(2.0)), Div, scalar!(3.0)),
        );

        should_fail(&["1*@", "1*", "1 per", "÷", "×"]);

        // 'per' is higher-precedence than '/'
        parse_as_expression(
            &["1 / meter per second"],
            binop!(
                scalar!(1.0),
                Div,
                binop!(identifier!("meter"), Div, identifier!("second"))
            ),
        );
    }

    #[test]
    fn addition_subtraction_multiplication_division_precedence() {
        parse_as_expression(
            &["5+3*2", "5+(3*2)"],
            binop!(scalar!(5.0), Add, binop!(scalar!(3.0), Mul, scalar!(2.0))),
        );

        parse_as_expression(
            &["5/3*2", "(5/3)*2"],
            binop!(binop!(scalar!(5.0), Div, scalar!(3.0)), Mul, scalar!(2.0)),
        );

        should_fail(&["3+*4", "3*/4", "(3+4", "3+4)", "3+(", "()", "(3+)4"])
    }

    #[test]
    fn implicit_multiplication() {
        parse_as_expression(
            &["1 2", "  1     2    "],
            binop!(scalar!(1.0), Mul, scalar!(2.0)),
        );

        parse_as_expression(
            &["2 meter"],
            binop!(scalar!(2.0), Mul, identifier!("meter")),
        );
    }

    #[test]
    fn exponentiation() {
        parse_as_expression(
            &["2^3", "  2   ^  3    ", "2**3"],
            binop!(scalar!(2.0), Power, scalar!(3.0)),
        );

        parse_as_expression(
            &["2^3^4", "  2   ^  3   ^ 4 ", "2**3**4"],
            binop!(
                scalar!(2.0),
                Power,
                binop!(scalar!(3.0), Power, scalar!(4.0))
            ),
        );

        parse_as_expression(
            &["(2^3)^4", "(2**3)**4"],
            binop!(
                binop!(scalar!(2.0), Power, scalar!(3.0)),
                Power,
                scalar!(4.0)
            ),
        );

        should_fail(&["1^", "1^^2", "1**", "1***3", "1****4"]);
    }

    #[test]
    fn unicode_exponentiation() {
        parse_as_expression(&["2³"], binop!(scalar!(2.0), Power, scalar!(3.0)));

        parse_as_expression(&["2⁻⁴"], binop!(scalar!(2.0), Power, scalar!(-4.0)));

        parse_as_expression(
            &["(2^3)²"],
            binop!(
                binop!(scalar!(2.0), Power, scalar!(3.0)),
                Power,
                scalar!(2.0)
            ),
        );

        parse_as_expression(
            &["2⁵^4"],
            binop!(
                binop!(scalar!(2.0), Power, scalar!(5.0)),
                Power,
                scalar!(4.0)
            ),
        );

        should_fail(&["1²³", "2⁻", "2⁻3", "²", "²3"]);
    }

    #[test]
    fn conversion() {
        parse_as_expression(
            &["1->2", "1→2"],
            binop!(scalar!(1.0), ConvertTo, scalar!(2.0)),
        );

        // Conversion is left-associative
        parse_as_expression(
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
    fn variable_definition() {
        parse_as(
            &["let foo = 1", "let foo=1"],
            Statement::DefineVariable {
                identifier_span: Span::dummy(),
                identifier: "foo".into(),
                expr: scalar!(1.0),
                type_annotation: None,
            },
        );

        parse_as(
            &["let x: Length = 1 * meter"],
            Statement::DefineVariable {
                identifier_span: Span::dummy(),
                identifier: "x".into(),
                expr: binop!(scalar!(1.0), Mul, identifier!("meter")),
                type_annotation: Some(DimensionExpression::Dimension(
                    Span::dummy(),
                    "Length".into(),
                )),
            },
        );

        should_fail_with(
            &["let (foo)=2", "let 2=3", "let = 2"],
            ParseErrorKind::ExpectedIdentifierAfterLet,
        );

        should_fail_with(
            &["let foo", "let foo 2"],
            ParseErrorKind::ExpectedEqualOrColonAfterLetIdentifier,
        );

        should_fail(&["let x²=2", "let x+y=2", "let 3=5", "let x=", "let x"])
    }

    #[test]
    fn dimension_definition() {
        parse_as(
            &["dimension px"],
            Statement::DefineDimension("px".into(), vec![]),
        );

        parse_as(
            &[
                "dimension Area = Length * Length",
                "dimension Area = Length × Length",
                "dimension Area =\n  Length × Length",
            ],
            Statement::DefineDimension(
                "Area".into(),
                vec![DimensionExpression::Multiply(
                    Span::dummy(),
                    Box::new(DimensionExpression::Dimension(
                        Span::dummy(),
                        "Length".into(),
                    )),
                    Box::new(DimensionExpression::Dimension(
                        Span::dummy(),
                        "Length".into(),
                    )),
                )],
            ),
        );

        parse_as(
            &["dimension Speed = Length / Time"],
            Statement::DefineDimension(
                "Speed".into(),
                vec![DimensionExpression::Divide(
                    Span::dummy(),
                    Box::new(DimensionExpression::Dimension(
                        Span::dummy(),
                        "Length".into(),
                    )),
                    Box::new(DimensionExpression::Dimension(Span::dummy(), "Time".into())),
                )],
            ),
        );

        parse_as(
            &["dimension Area = Length^2"],
            Statement::DefineDimension(
                "Area".into(),
                vec![DimensionExpression::Power(
                    Span::dummy(),
                    Box::new(DimensionExpression::Dimension(
                        Span::dummy(),
                        "Length".into(),
                    )),
                    Span::dummy(),
                    Rational::from_integer(2),
                )],
            ),
        );

        parse_as(
            &["dimension Energy = Mass * Length^2 / Time^2"],
            Statement::DefineDimension(
                "Energy".into(),
                vec![DimensionExpression::Divide(
                    Span::dummy(),
                    Box::new(DimensionExpression::Multiply(
                        Span::dummy(),
                        Box::new(DimensionExpression::Dimension(Span::dummy(), "Mass".into())),
                        Box::new(DimensionExpression::Power(
                            Span::dummy(),
                            Box::new(DimensionExpression::Dimension(
                                Span::dummy(),
                                "Length".into(),
                            )),
                            Span::dummy(),
                            Rational::from_integer(2),
                        )),
                    )),
                    Box::new(DimensionExpression::Power(
                        Span::dummy(),
                        Box::new(DimensionExpression::Dimension(Span::dummy(), "Time".into())),
                        Span::dummy(),
                        Rational::from_integer(2),
                    )),
                )],
            ),
        );

        parse_as(
            &["dimension X = Length^(12345/67890)"],
            Statement::DefineDimension(
                "X".into(),
                vec![DimensionExpression::Power(
                    Span::dummy(),
                    Box::new(DimensionExpression::Dimension(
                        Span::dummy(),
                        "Length".into(),
                    )),
                    Span::dummy(),
                    Rational::new(12345, 67890),
                )],
            ),
        );

        // Regression test, found using fuzzing. This should result in an error, but not panic
        should_fail_with(
            &["dimension X = Length^(6/(5/(99999999999999999999999999999999999999)))"],
            ParseErrorKind::OverflowInDimensionExponent,
        );
    }

    #[test]
    fn function_definition() {
        parse_as(
            &["fn foo() = 1", "fn foo() =\n  1"],
            Statement::DefineFunction {
                function_name_span: Span::dummy(),
                function_name: "foo".into(),
                type_parameters: vec![],
                parameters: vec![],
                body: Some(scalar!(1.0)),
                return_type_span: None,
                return_type_annotation: None,
            },
        );

        parse_as(
            &["fn foo() -> Scalar = 1"],
            Statement::DefineFunction {
                function_name_span: Span::dummy(),
                function_name: "foo".into(),
                type_parameters: vec![],
                parameters: vec![],
                body: Some(scalar!(1.0)),
                return_type_span: Some(Span::dummy()),
                return_type_annotation: Some(DimensionExpression::Dimension(
                    Span::dummy(),
                    "Scalar".into(),
                )),
            },
        );

        parse_as(
            &["fn foo(x) = 1"],
            Statement::DefineFunction {
                function_name_span: Span::dummy(),
                function_name: "foo".into(),
                type_parameters: vec![],
                parameters: vec![(Span::dummy(), "x".into(), None, false)],
                body: Some(scalar!(1.0)),
                return_type_span: None,
                return_type_annotation: None,
            },
        );

        parse_as(
            &["fn foo(x, y, z) = 1"],
            Statement::DefineFunction {
                function_name_span: Span::dummy(),
                function_name: "foo".into(),
                type_parameters: vec![],
                parameters: vec![
                    (Span::dummy(), "x".into(), None, false),
                    (Span::dummy(), "y".into(), None, false),
                    (Span::dummy(), "z".into(), None, false),
                ],
                body: Some(scalar!(1.0)),
                return_type_span: None,
                return_type_annotation: None,
            },
        );

        parse_as(
            &["fn foo(x: Length, y: Time, z: Length^3 · Time^2) -> Scalar = 1"],
            Statement::DefineFunction {
                function_name_span: Span::dummy(),
                function_name: "foo".into(),
                type_parameters: vec![],
                parameters: vec![
                    (
                        Span::dummy(),
                        "x".into(),
                        Some(DimensionExpression::Dimension(
                            Span::dummy(),
                            "Length".into(),
                        )),
                        false,
                    ),
                    (
                        Span::dummy(),
                        "y".into(),
                        Some(DimensionExpression::Dimension(Span::dummy(), "Time".into())),
                        false,
                    ),
                    (
                        Span::dummy(),
                        "z".into(),
                        Some(DimensionExpression::Multiply(
                            Span::dummy(),
                            Box::new(DimensionExpression::Power(
                                Span::dummy(),
                                Box::new(DimensionExpression::Dimension(
                                    Span::dummy(),
                                    "Length".into(),
                                )),
                                Span::dummy(),
                                Rational::new(3, 1),
                            )),
                            Box::new(DimensionExpression::Power(
                                Span::dummy(),
                                Box::new(DimensionExpression::Dimension(
                                    Span::dummy(),
                                    "Time".into(),
                                )),
                                Span::dummy(),
                                Rational::new(2, 1),
                            )),
                        )),
                        false,
                    ),
                ],
                body: Some(scalar!(1.0)),
                return_type_span: Some(Span::dummy()),
                return_type_annotation: Some(DimensionExpression::Dimension(
                    Span::dummy(),
                    "Scalar".into(),
                )),
            },
        );

        parse_as(
            &["fn foo<X>(x: X) = 1"],
            Statement::DefineFunction {
                function_name_span: Span::dummy(),
                function_name: "foo".into(),
                type_parameters: vec![(Span::dummy(), "X".into())],
                parameters: vec![(
                    Span::dummy(),
                    "x".into(),
                    Some(DimensionExpression::Dimension(Span::dummy(), "X".into())),
                    false,
                )],
                body: Some(scalar!(1.0)),
                return_type_span: None,
                return_type_annotation: None,
            },
        );

        parse_as(
            &["fn foo<D>(x: D…) -> D"],
            Statement::DefineFunction {
                function_name_span: Span::dummy(),
                function_name: "foo".into(),
                type_parameters: vec![(Span::dummy(), "D".into())],
                parameters: vec![(
                    Span::dummy(),
                    "x".into(),
                    Some(DimensionExpression::Dimension(Span::dummy(), "D".into())),
                    true,
                )],
                body: None,
                return_type_span: Some(Span::dummy()),
                return_type_annotation: Some(DimensionExpression::Dimension(
                    Span::dummy(),
                    "D".into(),
                )),
            },
        );

        should_fail_with(
            &[
                "fn foo<D>(x: D, y: D…) -> D",
                "fn foo<D>(y: D…, x: D) -> D",
                "fn foo<D>(y: D…, x: D…) -> D",
            ],
            ParseErrorKind::OnlySingleVariadicParameter,
        );

        should_fail_with(
            &[
                "fn foo(x: Scalar…) -> Scalar = 1",
                "fn foo<D>(x: D…) -> 1 = 1",
            ],
            ParseErrorKind::VariadicParameterOnlyAllowedInForeignFunction,
        );
    }

    #[test]
    fn function_call() {
        parse_as_expression(
            &["foo()"],
            Expression::FunctionCall(Span::dummy(), Span::dummy(), "foo".into(), vec![]),
        );

        parse_as_expression(
            &["foo(1)"],
            Expression::FunctionCall(
                Span::dummy(),
                Span::dummy(),
                "foo".into(),
                vec![scalar!(1.0)],
            ),
        );

        parse_as_expression(
            &["foo(1,2,3)"],
            Expression::FunctionCall(
                Span::dummy(),
                Span::dummy(),
                "foo".into(),
                vec![scalar!(1.0), scalar!(2.0), scalar!(3.0)],
            ),
        );

        should_fail(&["exp(,)", "exp(1,)"])
    }

    #[test]
    fn postfix_apply() {
        parse_as_expression(
            &["1 + 1 // foo"],
            Expression::FunctionCall(
                Span::dummy(),
                Span::dummy(),
                "foo".into(),
                vec![binop!(scalar!(1.0), Add, scalar!(1.0))],
            ),
        );
    }

    #[test]
    fn procedure_call() {
        parse_as(
            &["print(2)"],
            Statement::ProcedureCall(Span::dummy(), ProcedureKind::Print, vec![scalar!(2.0)]),
        );

        parse_as(
            &["print(2, 3, 4)"],
            Statement::ProcedureCall(
                Span::dummy(),
                ProcedureKind::Print,
                vec![scalar!(2.0), scalar!(3.0), scalar!(4.0)],
            ),
        );

        should_fail_with(
            &["print", "print 2"],
            ParseErrorKind::ExpectedLeftParenAfterProcedureName,
        );

        should_fail_with(&["1+print(2)"], ParseErrorKind::InlineProcedureUsage);

        should_fail_with(
            &["fn print() = 1"],
            ParseErrorKind::ExpectedIdentifierAfterFn,
        );
    }
}
