//! Numbat Parser
//!
//! Grammar:
//! ```txt
//! statement       ::=   variable_decl | struct_decl | function_decl | dimension_decl | unit_decl | module_import | procedure_call | expression
//!
//! variable_decl   ::=   "let" identifier ( ":" type_annotation ) ? "=" expression
//! struct_decl     ::=   "struct" identifier "{" ( identifier ":" type_annotation "," )* ( identifier ":" type_annotation "," ? ) ? "}"
//! function_decl   ::=   "fn" identifier ( fn_decl_generic ) ? fn_decl_param ( "->" type_annotation ) ? ( "=" expression ) ?
//! fn_decl_generic ::=   "<" ( identifier "," ) * identifier ">"
//! fn_decl_param   ::=   "(" ( identifier ( ":" type_annotation ) ? "," )* ( identifier ( ":" type_annotation ) ) ? ")"
//! dimension_decl  ::=   "dimension" identifier ( "=" dimension_expr ) *
//! unit_decl       ::=   decorator * "unit" ( ":" dimension_expr ) ? ( "=" expression ) ?
//! module_import   ::=   "use" ident ( "::" ident) *
//! procedure_call  ::=   ( "print" | "assert" | "assert_eq" | "type" ) "(" arguments? ")"
//!
//! decorator       ::=   "@" ( "metric_prefixes" | "binary_prefixes" | ( "aliases(" list_of_aliases ")" ) )
//!
//! type_annotation ::=   "Bool" | "String" | "List<" type ">" | dimension_expr
//! dimension_expr  ::=   dim_factor
//! dim_factor      ::=   dim_power ( (multiply | divide) dim_power ) *
//! dim_power       ::=   dim_primary ( power dim_exponent | unicode_exponent ) ?
//! dim_exponent    ::=   integer | minus dim_exponent | "(" dim_exponent ( divide dim_exponent ) ? ")"
//! dim_primary     ::=   identifier | "1" | "(" dimension_expr ")"
//!
//! expression      ::=   postfix_apply
//! postfix_apply   ::=   condition ( "|>" identifier ) *
//! condition       ::=   ( "if" conversion "then" condition "else" condition ) | conversion
//! conversion      ::=   logical_or ( ( "→" | "->" | "to" ) logical_or ) *
//! logical_or      ::=   logical_and ( "||" logical_and ) *
//! logical_and     ::=   logical_neg ( "&&" logical_neg ) *
//! logical_neg     ::=   ( "!" logical_neg) | comparison
//! comparison      ::=   term ( (">" | ">="| "≥" | "<" | "<=" | "≤" | "==" | "!=" | "≠" ) term ) *
//! term            ::=   factor ( ( "+" | "-") factor ) *
//! factor          ::=   unary ( ( "*" | "/") per_factor ) *
//! per_factor      ::=   unary ( "per" unary ) *
//! unary           ::=   ( ( minus | plus ) unary ) | ifactor
//! ifactor         ::=   power ( " " power ) *
//! power           ::=   factorial ( "^" "-" ? power ) ?
//! factorial       ::=   unicode_power "!" *
//! unicode_power   ::=   call ( "⁻" ? ( "¹" | "²" | "³" | "⁴" | "⁵" | "⁶" | "⁷" | "⁸" | "⁹" ) ) ?
//! call            ::=   primary ( ( "(" arguments? ")" ) | "." identifier ) *
//! arguments       ::=   expression ( "," expression ) *
//! primary         ::=   boolean | string | hex_number | oct_number | bin_number | number | identifier ( struct_expr ? ) | typed_hole | list_expr | "(" expression ")"
//! struct_expr     ::=   "{" ( identifier ":" type_annotation "," )* ( identifier ":" expression "," ? ) ? "}"
//! list_expr       ::=   "[]" | "[" expression ( "," expression ) * "]"
//!
//! number          ::=   [0-9][0-9_]*("." ([0-9][0-9_]*)?)?([eE][+-]?[0-9][0-9_]*)?
//! hex_number      ::=   "0x" [0-9a-fA-F]*
//! oct_number      ::=   "0o" [0-7]*
//! bin_number      ::=   "0b" [01]*
//! integer         ::=   [0-9]([0-9_]*[0-9])?
//! identifier      ::=   identifier_s identifier_c*
//! identifier_s    ::=   Unicode_XID_Start | Unicode_Currency | "%" | "°" | "′" | "″" | "_"
//! identifier_c    ::=   Unicode_XID_Continue | Unicode_Currency  | "%"
//! typed_hole      ::=   "?"
//! boolean         ::=   "true" | "false"
//! plus            ::=   "+"
//! minus           ::=   "-"
//! multiply        ::=   "*" | "×" | "·"
//! divide          ::=   "/" | "÷"
//! string          ::=   '"' [^"]* '"'
//! ```

use crate::arithmetic::{Exponent, Rational};
use crate::ast::{
    BinaryOperator, DefineVariable, Expression, ProcedureKind, Statement, StringPart,
    TypeAnnotation, TypeExpression, TypeParameterBound, UnaryOperator,
};
use crate::decorator::{self, Decorator};
use crate::number::Number;
use crate::prefix_parser::AcceptsPrefix;
use crate::resolver::ModulePath;
use crate::span::Span;
use crate::tokenizer::{Token, TokenKind, TokenizerError, TokenizerErrorKind};

use num_traits::{CheckedDiv, FromPrimitive, Zero};
use thiserror::Error;

#[derive(Error, Debug, Clone, PartialEq, Eq)]
pub enum ParseErrorKind {
    #[error(transparent)]
    TokenizerError(TokenizerErrorKind),

    #[error(
        "Expected one of: number, identifier, parenthesized expression, struct instantiation, list"
    )]
    ExpectedPrimary,

    #[error("Missing closing parenthesis ')'")]
    MissingClosingParen,

    #[error("Trailing characters: '{0}'")]
    TrailingCharacters(String),

    #[error("Trailing '=' sign. Use `let {0} = …` if you intended to define a new constant.")]
    TrailingEqualSign(String),

    #[error("Expected identifier after 'let' keyword")]
    ExpectedIdentifierAfterLet,

    #[error("Expected '=' or ':' after identifier (and type annotation) in 'let' assignment")]
    ExpectedEqualOrColonAfterLetIdentifier,

    #[error("Expected identifier after 'fn' keyword. Note that some reserved words can not be used as function names.")]
    ExpectedIdentifierAfterFn,

    #[error("Expected identifier")]
    ExpectedIdentifier,

    #[error("Expected identifier or function call after postfix apply (`|>`)")]
    ExpectedIdentifierOrCallAfterPostfixApply,

    #[error("Expected dimension identifier, '1', or opening parenthesis")]
    ExpectedDimensionPrimary,

    #[error("Expected ',' or '>' in type parameter list")]
    ExpectedCommaOrRightAngleBracket,

    #[error("Expected identifier (type parameter name)")]
    ExpectedTypeParameterName,

    #[error("Expected opening parenthesis '(' in function definition")]
    ExpectedLeftParenInFunctionDefinition,

    #[error("Expected ',' or ')' in function parameter list")]
    ExpectedCommaEllipsisOrRightParenInFunctionDefinition,

    #[error("Expected ',', or '}}' in struct field list")]
    ExpectedCommaOrRightCurlyInStructFieldList,

    #[error("Expected parameter name in function definition")]
    ExpectedParameterNameInFunctionDefinition,

    #[error("Expected field name in struct")]
    ExpectedFieldNameInStruct,

    #[error("Expected identifier (dimension name)")]
    ExpectedIdentifierAfterDimension,

    #[error("Expected identifier (unit name)")]
    ExpectedIdentifierAfterUnit,

    #[error("Expected '=' or ':' after identifier in unit definition")]
    ExpectedColonOrEqualAfterUnitIdentifier,

    #[error("Expected ':' after a field name")]
    ExpectedColonAfterFieldName,

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

    #[error("Decorators can only be used on unit, let or function definitions")]
    DecoratorUsedOnUnsuitableKind,

    #[error("Decorators on let definitions cannot have prefix information")]
    DecoratorsWithPrefixOnLetDefinition,

    #[error("Expected opening parenthesis after decorator")]
    ExpectedLeftParenAfterDecorator,

    #[error("Unknown alias annotation")]
    UnknownAliasAnnotation,

    #[error("Aliases cannot be used on functions.")]
    AliasUsedOnFunction,

    #[error("Numerical overflow in dimension exponent")]
    OverflowInDimensionExponent,

    #[error("Expected 'then' in if-then-else condition")]
    ExpectedThen,

    #[error("Expected 'else' in if-then-else condition")]
    ExpectedElse,

    #[error("Unterminated string")]
    UnterminatedString,

    #[error("Expected a string")]
    ExpectedString,

    #[error("Expected {0} in function type")]
    ExpectedTokenInFunctionType(&'static str),

    #[error("Expected {0} in list type")]
    ExpectedTokenInListType(&'static str),

    #[error("Expected '{{' after struct name")]
    ExpectedLeftCurlyAfterStructName,

    #[error("Expected ',' or ']' in list expression")]
    ExpectedCommaOrRightBracketInList,

    #[error("Unknown bound '{0}' in type parameter definition")]
    UnknownBound(String),

    #[error("Expected bound in type parameter definition")]
    ExpectedBoundInTypeParameterDefinition,

    #[error("Empty string interpolation")]
    EmptyStringInterpolation,

    #[error("Expected local variable definition after where/and")]
    ExpectedLocalVariableDefinition,
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

type Result<T, E = ParseError> = std::result::Result<T, E>;
type ParseResult = Result<Vec<Statement>, (Vec<Statement>, Vec<ParseError>)>;

static PROCEDURES: &[TokenKind] = &[
    TokenKind::ProcedurePrint,
    TokenKind::ProcedureAssert,
    TokenKind::ProcedureAssertEq,
    TokenKind::ProcedureType,
];

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

    /// Parse a token stream.
    /// If an error is encountered and `stop_on_error` is set to false, the parser
    /// will try to recover from the error and parse as many statements as possible
    /// while stacking all the errors in a `Vec`. At the end, it returns the complete
    /// list of statements parsed + the list of errors accumulated.
    fn parse(&mut self) -> ParseResult {
        let mut statements = vec![];
        let mut errors = vec![];

        self.skip_empty_lines();

        while !self.is_at_end() {
            match self.statement() {
                Ok(statement) => statements.push(statement),
                Err(e) => {
                    errors.push(e);
                    self.recover_from_error();
                }
            }

            match self.peek().kind {
                TokenKind::Newline => {
                    // Skip over empty lines
                    self.skip_empty_lines();
                }
                TokenKind::Eof => {
                    break;
                }
                TokenKind::Equal => {
                    errors.push(ParseError {
                        kind: ParseErrorKind::TrailingEqualSign(
                            self.last().unwrap().lexeme.clone(),
                        ),
                        span: self.peek().span,
                    });
                    self.recover_from_error();
                }
                _ => {
                    errors.push(ParseError {
                        kind: ParseErrorKind::TrailingCharacters(self.peek().lexeme.clone()),
                        span: self.peek().span,
                    });
                    self.recover_from_error();
                }
            }
        }

        if errors.is_empty() {
            Ok(statements)
        } else {
            Err((statements, errors))
        }
    }

    /// Must be called after encountering an error.
    fn recover_from_error(&mut self) {
        // Skip all the tokens until we encounter a newline or EoF.
        while !matches!(self.peek().kind, TokenKind::Newline | TokenKind::Eof) {
            self.advance()
        }
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
            || self.peek().kind == TokenKind::Let
            || self.peek().kind == TokenKind::Fn
            || self.decorator_stack.is_empty())
        {
            return Err(ParseError {
                kind: ParseErrorKind::DecoratorUsedOnUnsuitableKind,
                span: self.peek().span,
            });
        }

        if self.match_exact(TokenKind::Let).is_some() {
            self.parse_variable(true).map(Statement::DefineVariable)
        } else if self.match_exact(TokenKind::Fn).is_some() {
            self.parse_function_declaration()
        } else if self.match_exact(TokenKind::Dimension).is_some() {
            self.parse_dimension_declaration()
        } else if self.match_exact(TokenKind::At).is_some() {
            self.parse_decorators()
        } else if self.match_exact(TokenKind::Unit).is_some() {
            self.parse_unit_declaration()
        } else if self.match_exact(TokenKind::Use).is_some() {
            self.parse_use()
        } else if self.match_exact(TokenKind::Struct).is_some() {
            self.parse_struct()
        } else if self.match_any(PROCEDURES).is_some() {
            self.parse_procedure()
        } else {
            Ok(Statement::Expression(self.expression()?))
        }
    }

    fn parse_variable(&mut self, flush_decorators: bool) -> Result<DefineVariable> {
        if let Some(identifier) = self.match_exact(TokenKind::Identifier) {
            let identifier_span = self.last().unwrap().span;

            let type_annotation = if self.match_exact(TokenKind::Colon).is_some() {
                Some(self.type_annotation()?)
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

                let mut decorators = vec![];
                if flush_decorators {
                    if decorator::contains_aliases_with_prefixes(&self.decorator_stack) {
                        return Err(ParseError {
                            kind: ParseErrorKind::DecoratorsWithPrefixOnLetDefinition,
                            span: self.peek().span,
                        });
                    }
                    std::mem::swap(&mut decorators, &mut self.decorator_stack);
                }

                Ok(DefineVariable {
                    identifier_span,
                    identifier: identifier.lexeme.clone(),
                    expr,
                    type_annotation,
                    decorators,
                })
            }
        } else {
            Err(ParseError {
                kind: ParseErrorKind::ExpectedIdentifierAfterLet,
                span: self.peek().span,
            })
        }
    }

    fn parse_function_declaration(&mut self) -> Result<Statement> {
        if let Some(fn_name) = self.match_exact(TokenKind::Identifier) {
            let function_name_span = self.last().unwrap().span;
            let mut type_parameters = vec![];
            // Parsing the generic parameters if there are any
            if self.match_exact(TokenKind::LessThan).is_some() {
                while self.match_exact(TokenKind::GreaterThan).is_none() {
                    if let Some(type_parameter_name) = self.match_exact(TokenKind::Identifier) {
                        let bound = if self.match_exact(TokenKind::Colon).is_some() {
                            match self.match_exact(TokenKind::Identifier) {
                                Some(token) if token.lexeme == "Dim" => {
                                    Some(TypeParameterBound::Dim)
                                }
                                Some(token) => {
                                    return Err(ParseError {
                                        kind: ParseErrorKind::UnknownBound(token.lexeme.clone()),
                                        span: token.span,
                                    });
                                }
                                None => {
                                    return Err(ParseError {
                                        kind:
                                            ParseErrorKind::ExpectedBoundInTypeParameterDefinition,
                                        span: self.peek().span,
                                    });
                                }
                            }
                        } else {
                            None
                        };

                        let span = self.last().unwrap().span;
                        type_parameters.push((span, type_parameter_name.lexeme.to_string(), bound));

                        if self.match_exact(TokenKind::Comma).is_none()
                            && self.peek().kind != TokenKind::GreaterThan
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

            self.match_exact(TokenKind::Newline);
            let mut parameters = vec![];
            while self.match_exact(TokenKind::RightParen).is_none() {
                if let Some(param_name) = self.match_exact(TokenKind::Identifier) {
                    let span = self.last().unwrap().span;
                    let param_type_dexpr = if self.match_exact(TokenKind::Colon).is_some() {
                        Some(self.type_annotation()?)
                    } else {
                        None
                    };

                    parameters.push((span, param_name.lexeme.to_string(), param_type_dexpr));

                    parameter_span = parameter_span.extend(&self.last().unwrap().span);

                    self.skip_empty_lines();
                    let has_comma = self.match_exact(TokenKind::Comma).is_some();
                    self.skip_empty_lines();
                    if self.match_exact(TokenKind::RightParen).is_some() {
                        break;
                    }

                    if !has_comma && self.peek().kind != TokenKind::RightParen {
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

            let return_type_annotation = if self.match_exact(TokenKind::Arrow).is_some() {
                Some(self.type_annotation()?)
            } else {
                None
            };

            let (body, local_variables) = if self.match_exact(TokenKind::Equal).is_none() {
                (None, vec![])
            } else {
                self.skip_empty_lines();
                let body = self.expression()?;

                let mut local_variables = Vec::new();

                if self
                    .match_exact_beyond_linebreaks(TokenKind::Where)
                    .is_some()
                {
                    let keyword_span = self.last().unwrap().span;
                    self.skip_empty_lines();
                    if let Ok(local_variable) = self.parse_variable(false) {
                        local_variables.push(local_variable);
                    } else {
                        return Err(ParseError {
                            kind: ParseErrorKind::ExpectedLocalVariableDefinition,
                            span: keyword_span,
                        });
                    }

                    while self.match_exact_beyond_linebreaks(TokenKind::And).is_some() {
                        let keyword_span = self.last().unwrap().span;
                        self.skip_empty_lines();
                        if let Ok(local_variable) = self.parse_variable(false) {
                            local_variables.push(local_variable);
                        } else {
                            return Err(ParseError {
                                kind: ParseErrorKind::ExpectedLocalVariableDefinition,
                                span: keyword_span,
                            });
                        }
                    }
                }

                (Some(body), local_variables)
            };

            if decorator::contains_aliases(&self.decorator_stack) {
                return Err(ParseError {
                    kind: ParseErrorKind::AliasUsedOnFunction,
                    span: self.peek().span,
                });
            }

            let mut decorators = vec![];
            std::mem::swap(&mut decorators, &mut self.decorator_stack);

            Ok(Statement::DefineFunction {
                function_name_span,
                function_name: fn_name.lexeme.clone(),
                type_parameters,
                parameters,
                body,
                local_variables,
                return_type_annotation,
                decorators,
            })
        } else {
            Err(ParseError {
                kind: ParseErrorKind::ExpectedIdentifierAfterFn,
                span: self.peek().span,
            })
        }
    }

    fn parse_dimension_declaration(&mut self) -> Result<Statement> {
        if let Some(identifier) = self.match_exact(TokenKind::Identifier) {
            if identifier.lexeme.starts_with("__") {
                return Err(ParseError::new(
                    ParseErrorKind::DoubleUnderscoreTypeNamesReserved,
                    identifier.span,
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
                    identifier.span,
                    identifier.lexeme.clone(),
                    dexprs,
                ))
            } else {
                Ok(Statement::DefineDimension(
                    identifier.span,
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
    }

    fn parse_decorators(&mut self) -> Result<Statement> {
        if let Some(decorator) = self.match_exact(TokenKind::Identifier) {
            let decorator = match decorator.lexeme.as_str() {
                "metric_prefixes" => Decorator::MetricPrefixes,
                "binary_prefixes" => Decorator::BinaryPrefixes,
                "aliases" => {
                    if self.match_exact(TokenKind::LeftParen).is_some() {
                        let aliases = self.list_of_aliases()?;
                        Decorator::Aliases(aliases)
                    } else {
                        return Err(ParseError {
                            kind: ParseErrorKind::ExpectedLeftParenAfterDecorator,
                            span: self.peek().span,
                        });
                    }
                }
                "url" | "name" | "description" => {
                    if self.match_exact(TokenKind::LeftParen).is_some() {
                        if let Some(token) = self.match_exact(TokenKind::StringFixed) {
                            if self.match_exact(TokenKind::RightParen).is_none() {
                                return Err(ParseError::new(
                                    ParseErrorKind::MissingClosingParen,
                                    self.peek().span,
                                ));
                            }

                            let content = strip_and_escape(&token.lexeme);

                            match decorator.lexeme.as_str() {
                                "url" => Decorator::Url(content),
                                "name" => Decorator::Name(content),
                                "description" => Decorator::Description(content),
                                _ => unreachable!(),
                            }
                        } else {
                            return Err(ParseError {
                                kind: ParseErrorKind::ExpectedString,
                                span: self.peek().span,
                            });
                        }
                    } else {
                        return Err(ParseError {
                            kind: ParseErrorKind::ExpectedLeftParenAfterDecorator,
                            span: self.peek().span,
                        });
                    }
                }
                _ => {
                    return Err(ParseError {
                        kind: ParseErrorKind::UnknownDecorator,
                        span: decorator.span,
                    });
                }
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
    }

    fn parse_unit_declaration(&mut self) -> Result<Statement> {
        if let Some(identifier) = self.match_exact(TokenKind::Identifier) {
            let identifier_span = self.last().unwrap().span;
            let (type_annotation_span, dexpr) = if self.match_exact(TokenKind::Colon).is_some() {
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
                    type_annotation: dexpr.map(TypeAnnotation::TypeExpression),
                    decorators,
                })
            } else if dexpr.is_some() {
                Ok(Statement::DefineBaseUnit(
                    identifier_span,
                    unit_name,
                    dexpr,
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
    }

    fn parse_use(&mut self) -> Result<Statement> {
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
    }

    fn parse_struct(&mut self) -> Result<Statement> {
        let name = self.identifier()?;
        let name_span = self.last().unwrap().span;

        if self.match_exact(TokenKind::LeftCurly).is_none() {
            return Err(ParseError {
                kind: ParseErrorKind::ExpectedLeftCurlyAfterStructName,
                span: self.peek().span,
            });
        }

        self.skip_empty_lines();

        let mut fields = vec![];
        while self.match_exact(TokenKind::RightCurly).is_none() {
            self.skip_empty_lines();

            let Some(field_name) = self.match_exact(TokenKind::Identifier) else {
                return Err(ParseError {
                    kind: ParseErrorKind::ExpectedFieldNameInStruct,
                    span: self.peek().span,
                });
            };

            self.skip_empty_lines();

            if self.match_exact(TokenKind::Colon).is_none() {
                return Err(ParseError {
                    kind: ParseErrorKind::ExpectedColonAfterFieldName,
                    span: self.peek().span,
                });
            }
            self.skip_empty_lines();

            let attr_type = self.type_annotation()?;

            self.skip_empty_lines();

            let has_comma = self.match_exact(TokenKind::Comma).is_some();

            self.skip_empty_lines();

            if !has_comma && self.peek().kind != TokenKind::RightCurly {
                return Err(ParseError {
                    kind: ParseErrorKind::ExpectedCommaOrRightCurlyInStructFieldList,
                    span: self.peek().span,
                });
            }

            fields.push((field_name.span, field_name.lexeme.to_owned(), attr_type));
        }

        Ok(Statement::DefineStruct {
            struct_name_span: name_span,
            struct_name: name,
            fields,
        })
    }

    fn parse_procedure(&mut self) -> Result<Statement> {
        let span = self.last().unwrap().span;
        let procedure_kind = match self.last().unwrap().kind {
            TokenKind::ProcedurePrint => ProcedureKind::Print,
            TokenKind::ProcedureAssert => ProcedureKind::Assert,
            TokenKind::ProcedureAssertEq => ProcedureKind::AssertEq,
            TokenKind::ProcedureType => ProcedureKind::Type,
            _ => unreachable!(),
        };

        if self.match_exact(TokenKind::LeftParen).is_some() {
            Ok(Statement::ProcedureCall(
                span,
                procedure_kind,
                self.arguments()?,
            ))
        } else {
            Err(ParseError {
                kind: ParseErrorKind::ExpectedLeftParenAfterProcedureName,
                span: self.peek().span,
            })
        }
    }

    /// Helper function to parse binary operations
    /// - arg `op_symbol` specifiy the separator / symbol of your operation
    /// - arg `op` specifiy the operation you're currently parsing
    /// - arg `next` specifiy the next parser to call between each symbols
    fn parse_binop(
        &mut self,
        op_symbol: &[TokenKind],
        op: impl Fn(TokenKind) -> BinaryOperator,
        next_parser: impl Fn(&mut Self) -> Result<Expression>,
    ) -> Result<Expression> {
        let mut expr = next_parser(self)?;
        while let Some(matched) = self.match_any(op_symbol) {
            let span_op = Some(self.last().unwrap().span);
            let rhs = next_parser(self)?;

            expr = Expression::BinaryOperator {
                op: op(matched.kind),
                lhs: Box::new(expr),
                rhs: Box::new(rhs),
                span_op,
            };
        }
        Ok(expr)
    }

    pub fn expression(&mut self) -> Result<Expression> {
        self.postfix_apply()
    }

    fn identifier(&mut self) -> Result<String> {
        if let Some(identifier) = self.match_exact(TokenKind::Identifier) {
            Ok(identifier.lexeme.clone())
        } else {
            Err(ParseError::new(
                ParseErrorKind::ExpectedIdentifier,
                self.peek().span,
            ))
        }
    }

    pub fn postfix_apply(&mut self) -> Result<Expression> {
        let mut expr = self.condition()?;
        let mut full_span = expr.full_span();
        while self.match_exact(TokenKind::PostfixApply).is_some() {
            self.skip_empty_lines();
            match self.call()? {
                Expression::Identifier(span, ident) => {
                    full_span = full_span.extend(&span);

                    expr = Expression::FunctionCall(
                        span,
                        full_span,
                        Box::new(Expression::Identifier(span, ident)),
                        vec![expr],
                    );
                }
                Expression::FunctionCall(call_span, fn_full_span, call, mut params) => {
                    full_span = full_span.extend(&fn_full_span);

                    params.push(expr);
                    expr = Expression::FunctionCall(call_span, full_span, call, params);
                }
                _other => {
                    return Err(ParseError::new(
                        ParseErrorKind::ExpectedIdentifierOrCallAfterPostfixApply,
                        full_span,
                    ))
                }
            }
        }
        Ok(expr)
    }

    fn condition(&mut self) -> Result<Expression> {
        if self.match_exact(TokenKind::If).is_some() {
            let span_if = self.last().unwrap().span;
            let condition_expr = self.conversion()?;

            self.skip_empty_lines();

            if self.match_exact(TokenKind::Then).is_none() {
                return Err(ParseError::new(
                    ParseErrorKind::ExpectedThen,
                    self.peek().span,
                ));
            }

            self.skip_empty_lines();

            let then_expr = self.condition()?;

            self.skip_empty_lines();

            if self.match_exact(TokenKind::Else).is_none() {
                return Err(ParseError::new(
                    ParseErrorKind::ExpectedElse,
                    self.peek().span,
                ));
            }

            self.skip_empty_lines();

            let else_expr = self.condition()?;

            Ok(Expression::Condition(
                span_if,
                Box::new(condition_expr),
                Box::new(then_expr),
                Box::new(else_expr),
            ))
        } else {
            self.conversion()
        }
    }

    fn conversion(&mut self) -> Result<Expression> {
        self.parse_binop(
            &[TokenKind::Arrow, TokenKind::To],
            |_| BinaryOperator::ConvertTo,
            Self::logical_or,
        )
    }

    fn logical_or(&mut self) -> Result<Expression> {
        self.parse_binop(
            &[TokenKind::LogicalOr],
            |_| BinaryOperator::LogicalOr,
            Self::logical_and,
        )
    }

    fn logical_and(&mut self) -> Result<Expression> {
        self.parse_binop(
            &[TokenKind::LogicalAnd],
            |_| BinaryOperator::LogicalAnd,
            Self::logical_neg,
        )
    }

    fn logical_neg(&mut self) -> Result<Expression> {
        if self.match_exact(TokenKind::ExclamationMark).is_some() {
            let span = self.last().unwrap().span;
            let rhs = self.logical_neg()?;

            Ok(Expression::UnaryOperator {
                op: UnaryOperator::LogicalNeg,
                expr: Box::new(rhs),
                span_op: span,
            })
        } else {
            self.comparison()
        }
    }

    fn comparison(&mut self) -> Result<Expression> {
        self.parse_binop(
            &[
                TokenKind::LessThan,
                TokenKind::GreaterThan,
                TokenKind::LessOrEqual,
                TokenKind::GreaterOrEqual,
                TokenKind::EqualEqual,
                TokenKind::NotEqual,
            ],
            |matched| match matched {
                TokenKind::LessThan => BinaryOperator::LessThan,
                TokenKind::GreaterThan => BinaryOperator::GreaterThan,
                TokenKind::LessOrEqual => BinaryOperator::LessOrEqual,
                TokenKind::GreaterOrEqual => BinaryOperator::GreaterOrEqual,
                TokenKind::EqualEqual => BinaryOperator::Equal,
                TokenKind::NotEqual => BinaryOperator::NotEqual,
                _ => unreachable!(),
            },
            Self::term,
        )
    }

    fn term(&mut self) -> Result<Expression> {
        self.parse_binop(
            &[TokenKind::Plus, TokenKind::Minus],
            |matched| match matched {
                TokenKind::Plus => BinaryOperator::Add,
                TokenKind::Minus => BinaryOperator::Sub,
                _ => unreachable!(),
            },
            Self::factor,
        )
    }

    fn factor(&mut self) -> Result<Expression> {
        self.parse_binop(
            &[TokenKind::Multiply, TokenKind::Divide],
            |matched| match matched {
                TokenKind::Multiply => BinaryOperator::Mul,
                TokenKind::Divide => BinaryOperator::Div,
                _ => unreachable!(),
            },
            Self::per_factor,
        )
    }

    fn per_factor(&mut self) -> Result<Expression> {
        self.parse_binop(&[TokenKind::Per], |_| BinaryOperator::Div, Self::unary)
    }

    fn unary(&mut self) -> Result<Expression> {
        if self.match_exact(TokenKind::Minus).is_some() {
            let span = self.last().unwrap().span;
            let rhs = self.unary()?;

            Ok(Expression::UnaryOperator {
                op: UnaryOperator::Negate,
                expr: Box::new(rhs),
                span_op: span,
            })
        } else if self.match_exact(TokenKind::Plus).is_some() {
            // A unary `+` is equivalent to nothing. We can get rid of the
            // symbol without inserting any nodes in the AST.
            self.unary()
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
        let mut expr = self.factorial()?;

        if self.match_exact(TokenKind::Power).is_some() {
            let span_op = Some(self.last().unwrap().span);

            let unary_op = if self.match_exact(TokenKind::Minus).is_some() {
                let span_unary_minus = self.last().unwrap().span;

                Some((UnaryOperator::Negate, span_unary_minus))
            } else {
                None
            };

            let mut rhs = self.power()?;

            if let Some((op, span_op)) = unary_op {
                rhs = Expression::UnaryOperator {
                    op,
                    expr: Box::new(rhs),
                    span_op,
                };
            }

            expr = Expression::BinaryOperator {
                op: BinaryOperator::Power,
                lhs: Box::new(expr),
                rhs: Box::new(rhs),
                span_op,
            };
        }

        Ok(expr)
    }

    fn factorial(&mut self) -> Result<Expression> {
        let mut expr = self.unicode_power()?;

        while self.match_exact(TokenKind::ExclamationMark).is_some() {
            let span = self.last().unwrap().span;

            expr = Expression::UnaryOperator {
                op: UnaryOperator::Factorial,
                expr: Box::new(expr),
                span_op: span,
            };
        }

        Ok(expr)
    }

    fn unicode_exponent_to_int(lexeme: &str) -> i32 {
        match lexeme {
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
        }
    }

    fn unicode_power(&mut self) -> Result<Expression> {
        let mut expr = self.call()?;

        if let Some(exponent) = self.match_exact(TokenKind::UnicodeExponent) {
            let exp = Self::unicode_exponent_to_int(exponent.lexeme.as_str());

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
        let mut expr = self.primary()?;

        loop {
            if self.match_exact(TokenKind::LeftParen).is_some() {
                let args = self.arguments()?;
                expr = Expression::FunctionCall(
                    expr.full_span(),
                    expr.full_span().extend(&self.last().unwrap().span),
                    Box::new(expr),
                    args,
                );
            } else if self.match_exact(TokenKind::Period).is_some() {
                let ident = self.identifier()?;
                let ident_span = self.last().unwrap().span;
                let full_span = expr.full_span().extend(&ident_span);

                expr = Expression::AccessField(full_span, ident_span, Box::new(expr), ident)
            } else {
                return Ok(expr);
            }
        }
    }

    fn arguments(&mut self) -> Result<Vec<Expression>> {
        self.skip_empty_lines();
        if self.match_exact(TokenKind::RightParen).is_some() {
            return Ok(vec![]);
        }

        let mut args: Vec<Expression> = vec![self.expression()?];
        loop {
            self.skip_empty_lines();

            if self.match_exact(TokenKind::Comma).is_some() {
                self.skip_empty_lines();
                if self.match_exact(TokenKind::RightParen).is_some() {
                    break;
                }
                match self.expression() {
                    Ok(expr) => args.push(expr),
                    Err(_err) => {
                        return Err(ParseError::new(
                            ParseErrorKind::MissingClosingParen,
                            self.peek().span,
                        ))
                    }
                }
            } else if self.match_exact(TokenKind::RightParen).is_some() {
                break;
            } else {
                return Err(ParseError::new(
                    ParseErrorKind::MissingClosingParen,
                    self.peek().span,
                ));
            }
        }

        Ok(args)
    }

    fn primary(&mut self) -> Result<Expression> {
        // This function needs to be kept in sync with `next_token_could_start_power_expression` below.

        let overflow_error = |span| {
            Err(ParseError::new(
                ParseErrorKind::OverflowInNumberLiteral,
                span,
            ))
        };

        if let Some(num) = self.match_exact(TokenKind::Number) {
            let num_string = num.lexeme.replace('_', "");
            Ok(Expression::Scalar(
                self.last().unwrap().span,
                Number::from_f64(num_string.parse::<f64>().unwrap()),
            ))
        } else if let Some(hex_int) = self.match_exact(TokenKind::IntegerWithBase(16)) {
            let span = self.last().unwrap().span;
            Ok(Expression::Scalar(
                span,
                Number::from_f64(
                    i128::from_str_radix(&hex_int.lexeme[2..].replace('_', ""), 16)
                        .or_else(|_| overflow_error(span))? as f64, // TODO: i128 limits our precision here
                ),
            ))
        } else if let Some(oct_int) = self.match_exact(TokenKind::IntegerWithBase(8)) {
            let span = self.last().unwrap().span;
            Ok(Expression::Scalar(
                span,
                Number::from_f64(
                    i128::from_str_radix(&oct_int.lexeme[2..].replace('_', ""), 8)
                        .or_else(|_| overflow_error(span))? as f64, // TODO: i128 limits our precision here
                ),
            ))
        } else if let Some(bin_int) = self.match_exact(TokenKind::IntegerWithBase(2)) {
            let span = self.last().unwrap().span;
            Ok(Expression::Scalar(
                span,
                Number::from_f64(
                    i128::from_str_radix(&bin_int.lexeme[2..].replace('_', ""), 2)
                        .or_else(|_| overflow_error(span))? as f64, // TODO: i128 limits our precision here
                ),
            ))
        } else if self.match_exact(TokenKind::NaN).is_some() {
            let span = self.last().unwrap().span;
            Ok(Expression::Scalar(span, Number::from_f64(f64::NAN)))
        } else if self.match_exact(TokenKind::Inf).is_some() {
            let span = self.last().unwrap().span;
            Ok(Expression::Scalar(span, Number::from_f64(f64::INFINITY)))
        } else if self.match_exact(TokenKind::LeftBracket).is_some() {
            let span = self.last().unwrap().span;
            self.skip_empty_lines();

            let mut elements = vec![];
            while self.match_exact(TokenKind::RightBracket).is_none() {
                self.skip_empty_lines();

                elements.push(self.expression()?);

                self.skip_empty_lines();

                if self.match_exact(TokenKind::Comma).is_none()
                    && self.peek().kind != TokenKind::RightBracket
                {
                    return Err(ParseError {
                        kind: ParseErrorKind::ExpectedCommaOrRightBracketInList,
                        span: self.peek().span,
                    });
                }

                self.skip_empty_lines();
            }
            let span = span.extend(&self.last().unwrap().span);

            Ok(Expression::List(span, elements))
        } else if self.match_exact(TokenKind::QuestionMark).is_some() {
            let span = self.last().unwrap().span;
            Ok(Expression::TypedHole(span))
        } else if let Some(identifier) = self.match_exact(TokenKind::Identifier) {
            let span = self.last().unwrap().span;

            if self.match_exact(TokenKind::LeftCurly).is_some() {
                self.skip_empty_lines();

                let mut fields = vec![];
                while self.match_exact(TokenKind::RightCurly).is_none() {
                    self.skip_empty_lines();

                    let Some(field_name) = self.match_exact(TokenKind::Identifier) else {
                        return Err(ParseError {
                            kind: ParseErrorKind::ExpectedFieldNameInStruct,
                            span: self.peek().span,
                        });
                    };

                    self.skip_empty_lines();

                    if self.match_exact(TokenKind::Colon).is_none() {
                        return Err(ParseError {
                            kind: ParseErrorKind::ExpectedColonAfterFieldName,
                            span: self.peek().span,
                        });
                    }

                    self.skip_empty_lines();

                    let expr = self.expression()?;

                    self.skip_empty_lines();

                    let has_comma = self.match_exact(TokenKind::Comma).is_some();

                    self.skip_empty_lines();

                    if !has_comma && self.peek().kind != TokenKind::RightCurly {
                        return Err(ParseError {
                            kind: ParseErrorKind::ExpectedCommaOrRightCurlyInStructFieldList,
                            span: self.peek().span,
                        });
                    }

                    fields.push((field_name.span, field_name.lexeme.to_owned(), expr));
                }

                let full_span = span.extend(&self.last().unwrap().span);

                return Ok(Expression::InstantiateStruct {
                    full_span,
                    ident_span: span,
                    name: identifier.lexeme.clone(),
                    fields,
                });
            }

            Ok(Expression::Identifier(span, identifier.lexeme.clone()))
        } else if let Some(inner) = self.match_any(&[TokenKind::True, TokenKind::False]) {
            Ok(Expression::Boolean(
                inner.span,
                matches!(inner.kind, TokenKind::True),
            ))
        } else if let Some(token) = self.match_exact(TokenKind::StringFixed) {
            Ok(Expression::String(
                token.span,
                vec![StringPart::Fixed(strip_and_escape(&token.lexeme))],
            ))
        } else if let Some(token) = self.match_exact(TokenKind::StringInterpolationStart) {
            let mut parts = Vec::new();

            self.interpolation(&mut parts, token)?;

            let mut span_full_string = token.span;
            let mut has_end = false;
            while let Some(inner_token) = self.match_any(&[
                TokenKind::StringInterpolationMiddle,
                TokenKind::StringInterpolationEnd,
            ]) {
                span_full_string = span_full_string.extend(&inner_token.span);
                match inner_token.kind {
                    TokenKind::StringInterpolationMiddle => {
                        self.interpolation(&mut parts, inner_token)?;
                    }
                    TokenKind::StringInterpolationEnd => {
                        parts.push(StringPart::Fixed(strip_and_escape(&inner_token.lexeme)));
                        has_end = true;
                        break;
                    }
                    _ => unreachable!(),
                }
            }

            if !has_end {
                span_full_string = span_full_string.extend(&self.last().unwrap().span);
                return Err(ParseError::new(
                    ParseErrorKind::UnterminatedString,
                    span_full_string,
                ));
            }

            parts.retain(|p| !matches!(p, StringPart::Fixed(s) if s.is_empty()));

            Ok(Expression::String(span_full_string, parts))
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
        } else if self
            .last()
            .map(|t| {
                matches!(
                    t.kind,
                    TokenKind::StringInterpolationStart | TokenKind::StringInterpolationMiddle
                )
            })
            .unwrap_or(false)
        {
            let full_interpolation_end_span = self.peek().span;
            let closing_brace_span = full_interpolation_end_span
                .start
                .single_character_span(full_interpolation_end_span.code_source_id);

            Err(ParseError::new(
                ParseErrorKind::EmptyStringInterpolation,
                closing_brace_span,
            ))
        } else {
            Err(ParseError::new(
                ParseErrorKind::ExpectedPrimary,
                self.peek().span,
            ))
        }
    }

    fn interpolation(&mut self, parts: &mut Vec<StringPart>, token: &Token) -> Result<()> {
        parts.push(StringPart::Fixed(strip_and_escape(&token.lexeme)));

        let expr = self.expression()?;

        let format_specifiers = self
            .match_exact(TokenKind::StringInterpolationSpecifiers)
            .map(|token| token.lexeme.clone());

        parts.push(StringPart::Interpolation {
            span: expr.full_span(),
            expr: Box::new(expr),
            format_specifiers,
        });

        Ok(())
    }

    /// Returns true iff the upcoming token indicates the beginning of a 'power'
    /// expression (which needs to start with a 'primary' expression).
    fn next_token_could_start_power_expression(&self) -> bool {
        // This function needs to be kept in sync with `primary` above.

        matches!(
            self.peek().kind,
            TokenKind::Number
                | TokenKind::Identifier
                | TokenKind::LeftParen
                | TokenKind::QuestionMark
        )
    }

    fn type_annotation(&mut self) -> Result<TypeAnnotation> {
        if let Some(token) = self.match_exact(TokenKind::Bool) {
            Ok(TypeAnnotation::Bool(token.span))
        } else if let Some(token) = self.match_exact(TokenKind::String) {
            Ok(TypeAnnotation::String(token.span))
        } else if let Some(token) = self.match_exact(TokenKind::DateTime) {
            Ok(TypeAnnotation::DateTime(token.span))
        } else if self.match_exact(TokenKind::CapitalFn).is_some() {
            let span = self.last().unwrap().span;
            if self.match_exact(TokenKind::LeftBracket).is_none() {
                return Err(ParseError::new(
                    ParseErrorKind::ExpectedTokenInFunctionType("left bracket"),
                    self.peek().span,
                ));
            }
            if self.match_exact(TokenKind::LeftParen).is_none() {
                return Err(ParseError::new(
                    ParseErrorKind::ExpectedTokenInFunctionType("left parenthesis"),
                    self.peek().span,
                ));
            }

            let mut params = vec![];
            if self.peek().kind != TokenKind::RightParen {
                params.push(self.type_annotation()?);
                while self.match_exact(TokenKind::Comma).is_some() {
                    params.push(self.type_annotation()?);
                }
            }

            if self.match_exact(TokenKind::RightParen).is_none() {
                return Err(ParseError::new(
                    ParseErrorKind::MissingClosingParen,
                    self.peek().span,
                ));
            }

            if self.match_exact(TokenKind::Arrow).is_none() {
                return Err(ParseError::new(
                    ParseErrorKind::ExpectedTokenInFunctionType("arrow (->)"),
                    self.peek().span,
                ));
            }

            let return_type = self.type_annotation()?;

            if self.match_exact(TokenKind::RightBracket).is_none() {
                return Err(ParseError::new(
                    ParseErrorKind::ExpectedTokenInFunctionType("right bracket"),
                    self.peek().span,
                ));
            }

            let span = span.extend(&self.last().unwrap().span);

            Ok(TypeAnnotation::Fn(span, params, Box::new(return_type)))
        } else if self.match_exact(TokenKind::List).is_some() {
            let span = self.last().unwrap().span;

            if self.match_exact(TokenKind::LessThan).is_none() {
                return Err(ParseError::new(
                    ParseErrorKind::ExpectedTokenInListType("'<'"),
                    self.peek().span,
                ));
            }

            let element_type = self.type_annotation()?;

            if self.match_exact(TokenKind::GreaterThan).is_none() {
                return Err(ParseError::new(
                    ParseErrorKind::ExpectedTokenInListType("'>'"),
                    self.peek().span,
                ));
            }

            let span = span.extend(&self.last().unwrap().span);

            Ok(TypeAnnotation::List(span, Box::new(element_type)))
        } else {
            Ok(TypeAnnotation::TypeExpression(self.dimension_expression()?))
        }
    }

    fn dimension_expression(&mut self) -> Result<TypeExpression> {
        self.dimension_factor()
    }

    fn dimension_factor(&mut self) -> Result<TypeExpression> {
        let mut expr = self.dimension_power()?;
        while let Some(operator_token) = self.match_any(&[TokenKind::Multiply, TokenKind::Divide]) {
            let span = self.last().unwrap().span;
            let rhs = self.dimension_power()?;

            expr = if operator_token.kind == TokenKind::Multiply {
                TypeExpression::Multiply(span, Box::new(expr), Box::new(rhs))
            } else {
                TypeExpression::Divide(span, Box::new(expr), Box::new(rhs))
            };
        }
        Ok(expr)
    }

    fn dimension_power(&mut self) -> Result<TypeExpression> {
        let expr = self.dimension_primary()?;

        if self.match_exact(TokenKind::Power).is_some() {
            let span = self.last().unwrap().span;
            let (span_exponent, exponent) = self.dimension_exponent()?;

            Ok(TypeExpression::Power(
                Some(span),
                Box::new(expr),
                span_exponent,
                exponent,
            ))
        } else if let Some(exponent) = self.match_exact(TokenKind::UnicodeExponent) {
            let span_exponent = self.last().unwrap().span;
            let exp = Self::unicode_exponent_to_int(exponent.lexeme.as_str());

            Ok(TypeExpression::Power(
                None,
                Box::new(expr),
                span_exponent,
                Exponent::from_integer(exp as i128),
            ))
        } else {
            Ok(expr)
        }
    }

    fn dimension_exponent(&mut self) -> Result<(Span, Exponent)> {
        if let Some(token) = self.match_exact(TokenKind::Number) {
            let span = self.last().unwrap().span;
            let num_str = token.lexeme.replace('_', "");
            Ok((
                span,
                Rational::from_i128(num_str.parse::<i128>().map_err(|_| ParseError {
                    kind: ParseErrorKind::NumberInDimensionExponentOutOfRange,
                    span: token.span,
                })?)
                .ok_or(ParseError {
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

    fn dimension_primary(&mut self) -> Result<TypeExpression> {
        let e = Err(ParseError::new(
            ParseErrorKind::ExpectedDimensionPrimary,
            self.peek().span,
        ));
        if let Some(token) = self.match_exact(TokenKind::Identifier) {
            if token.lexeme.starts_with("__") {
                return Err(ParseError::new(
                    ParseErrorKind::DoubleUnderscoreTypeNamesReserved,
                    token.span,
                ));
            }
            let span = self.last().unwrap().span;
            Ok(TypeExpression::TypeIdentifier(span, token.lexeme.clone()))
        } else if let Some(number) = self.match_exact(TokenKind::Number) {
            let span = self.last().unwrap().span;
            if number.lexeme != "1" {
                e
            } else {
                Ok(TypeExpression::Unity(span))
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

    fn look_ahead_beyond_linebreak(&self, token_kind: TokenKind) -> bool {
        let mut i = self.current;
        while i < self.tokens.len() {
            if self.tokens[i].kind != TokenKind::Newline {
                return self.tokens[i].kind == token_kind;
            }
            i += 1;
        }
        false
    }

    /// Same as 'match_exact', but skips empty lines before matching. Note that this
    /// does *not* skip empty lines in case there is no match.
    fn match_exact_beyond_linebreaks(&mut self, token_kind: TokenKind) -> Option<&'a Token> {
        if self.look_ahead_beyond_linebreak(token_kind) {
            self.skip_empty_lines();
        }
        self.match_exact(token_kind)
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
        if self.current == 0 {
            None
        } else {
            self.tokens.get(self.current - 1)
        }
    }

    pub fn is_end_of_statement(&self) -> bool {
        self.peek().kind == TokenKind::Newline || self.is_at_end()
    }

    pub fn is_at_end(&self) -> bool {
        self.peek().kind == TokenKind::Eof
    }
}

fn strip_and_escape(s: &str) -> String {
    let trimmed = &s[1..(s.len() - 1)];

    let mut result = String::with_capacity(trimmed.len());
    let mut escaped = false;
    for c in trimmed.chars() {
        if escaped {
            // Keep this in sync with 'escape_numbat_string',
            // where the reverse replacement is needed
            match c {
                'n' => result.push('\n'),
                'r' => result.push('\r'),
                't' => result.push('\t'),
                '"' => result.push('"'),
                '0' => result.push('\0'),
                '\\' => result.push('\\'),
                '{' => result.push('{'),
                '}' => result.push('}'),
                _ => {
                    // We follow Python here, where an unknown escape sequence
                    // does not lead to an error, but is just passed through.
                    result.push('\\');
                    result.push(c)
                }
            }
            escaped = false;
        } else if c == '\\' {
            escaped = true;
        } else {
            result.push(c);
        }
    }

    result
}

/// Parse a string.
/// If an error is encountered and `stop_on_error` is set to false, the parser
/// will try to recover from the error and parse as many statements as possible
/// while stacking all the errors in a `Vec`. At the end, it returns the complete
/// list of statements parsed + the list of errors accumulated.
pub fn parse(input: &str, code_source_id: usize) -> ParseResult {
    use crate::tokenizer::tokenize;

    let tokens = tokenize(input, code_source_id)
        .map_err(|TokenizerError { kind, span }| {
            ParseError::new(ParseErrorKind::TokenizerError(kind), span)
        })
        .map_err(|e| (Vec::new(), vec![e]))?;
    let mut parser = Parser::new(&tokens);
    parser.parse()
}

#[cfg(test)]
pub fn parse_dexpr(input: &str) -> TypeExpression {
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
    use insta::assert_snapshot;
    use std::fmt::Write;

    use super::*;
    use crate::ast::{
        binop, boolean, conditional, factorial, identifier, list, logical_neg, negate, scalar,
        struct_, ReplaceSpans,
    };

    #[track_caller]
    fn parse_as(inputs: &[&str], statement_expected: Statement) {
        for input in inputs {
            let statements = parse(input, 0).expect("parse error").replace_spans();

            assert!(statements.len() == 1);
            let statement = &statements[0];

            assert_eq!(*statement, statement_expected);
        }
    }

    #[track_caller]
    fn parse_as_expression(inputs: &[&str], expr_expected: Expression) {
        parse_as(inputs, Statement::Expression(expr_expected));
    }

    #[track_caller]
    fn should_fail(inputs: &[&str]) {
        for input in inputs {
            if let Ok(v) = parse(input, 0) {
                panic!("Expected parse failure on {input:?} but got: {v:#?}")
            }
        }
    }

    #[track_caller]
    fn should_fail_with(inputs: &[&str], error_kind: ParseErrorKind) {
        for input in inputs {
            match parse(input, 0) {
                Err((_, errors)) => {
                    assert_eq!(errors[0].kind, error_kind, "Failed on {input}");
                }
                _ => {
                    panic!();
                }
            }
        }
    }

    #[track_caller]
    fn snap_parse(input: impl AsRef<str>) -> String {
        let mut ret = String::new();
        match parse(input.as_ref(), 0) {
            Ok(stmts) => {
                for stmt in stmts {
                    writeln!(&mut ret, "{stmt:?}").unwrap();
                }
            }
            Err((stmts, errors)) => {
                writeln!(&mut ret, "Successfully parsed:").unwrap();
                for stmt in stmts {
                    writeln!(&mut ret, "{stmt:?}").unwrap();
                }
                writeln!(&mut ret, "Errors encountered:").unwrap();
                for error in errors {
                    writeln!(&mut ret, "{error} - {error:?}").unwrap();
                }
            }
        }
        ret
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
    fn factorials() {
        parse_as_expression(
            &["4!", "4.0!", "4 !", " 4 !", "(4)!"],
            factorial!(scalar!(4.0)),
        );
        parse_as_expression(
            &["3!^3", "(3!)^3"],
            binop!(factorial!(scalar!(3.0)), Power, scalar!(3.0)),
        );
        parse_as_expression(
            &["3²!"],
            factorial!(binop!(scalar!(3.0), Power, scalar!(2.0))),
        );
        parse_as_expression(
            &["3^3!"],
            binop!(scalar!(3.0), Power, factorial!(scalar!(3.0))),
        );
        parse_as_expression(
            &["-5!", "-(5!)", "-(5)!"],
            negate!(factorial!(scalar!(5.0))),
        );
        parse_as_expression(&["5!!", "(5!)!"], factorial!(factorial!(scalar!(5.0))));
    }

    #[test]
    fn unary() {
        parse_as_expression(&["-1", "  - 1   "], negate!(scalar!(1.0)));
        parse_as_expression(&["-123.45"], negate!(scalar!(123.45)));
        parse_as_expression(&["--1", " -  - 1   "], negate!(negate!(scalar!(1.0))));
        parse_as_expression(&["-x", " - x"], negate!(identifier!("x")));
        parse_as_expression(&["-0.61", "-.61", "-  .61"], negate!(scalar!(0.61)));

        parse_as_expression(
            &["-1 + 2"],
            binop!(negate!(scalar!(1.0)), Add, scalar!(2.0)),
        );

        parse_as_expression(&["+1", "  + 1   "], scalar!(1.0));
        parse_as_expression(&["+123.45"], scalar!(123.45));
        parse_as_expression(&["++1", " +  + 1   "], scalar!(1.0));
        parse_as_expression(&["+x", " + x"], identifier!("x"));
        parse_as_expression(&["+0.61", "+.61", "+  .61"], scalar!(0.61));

        parse_as_expression(
            &["+1 + 2", "1 +++++ 2"],
            binop!(scalar!(1.0), Add, scalar!(2.0)),
        );

        parse_as_expression(
            &["+1 - 2", "+1 - +2 "],
            binop!(scalar!(1.0), Sub, scalar!(2.0)),
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
    fn nonfinite() {
        parse_as_expression(&["inf"], scalar!(f64::INFINITY));
        parse_as_expression(&["-inf"], negate!(scalar!(f64::INFINITY)));
    }

    #[test]
    fn identifiers() {
        parse_as_expression(&["foo", "  foo   "], identifier!("foo"));
        parse_as_expression(&["foo_bar"], identifier!("foo_bar"));
        parse_as_expression(&["MeineSchöneVariable"], identifier!("MeineSchöneVariable"));
        parse_as_expression(&["°"], identifier!("°"));
        parse_as_expression(&["′"], identifier!("′"));
        parse_as_expression(&["″"], identifier!("″"));
        parse_as_expression(&["Mass_H₂O"], identifier!("Mass_H₂O"));
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

        parse_as_expression(
            &["2^-3", "2**-3"],
            binop!(scalar!(2.0), Power, negate!(scalar!(3.0))),
        );

        parse_as_expression(
            &["2^-3^-4"],
            binop!(
                scalar!(2.0),
                Power,
                negate!(binop!(scalar!(3.0), Power, negate!(scalar!(4.0))))
            ),
        );

        parse_as_expression(
            &["2^-2-3"],
            binop!(
                binop!(scalar!(2.0), Power, negate!(scalar!(2.0))),
                Sub,
                scalar!(3.0)
            ),
        );

        should_fail(&[
            "1^", "1^^2", "1**", "1***3", "1****4", "2^-", "2^--", "2**--3",
        ]);
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
            Statement::DefineVariable(DefineVariable {
                identifier_span: Span::dummy(),
                identifier: "foo".into(),
                expr: scalar!(1.0),
                type_annotation: None,
                decorators: Vec::new(),
            }),
        );

        parse_as(
            &["let x: Length = 1 * meter"],
            Statement::DefineVariable(DefineVariable {
                identifier_span: Span::dummy(),
                identifier: "x".into(),
                expr: binop!(scalar!(1.0), Mul, identifier!("meter")),
                type_annotation: Some(TypeAnnotation::TypeExpression(
                    TypeExpression::TypeIdentifier(Span::dummy(), "Length".into()),
                )),
                decorators: Vec::new(),
            }),
        );

        // same as above, but with some decorators
        parse_as(
            &["@name(\"myvar\") @aliases(foo, bar) let x: Length = 1 * meter"],
            Statement::DefineVariable(DefineVariable {
                identifier_span: Span::dummy(),
                identifier: "x".into(),
                expr: binop!(scalar!(1.0), Mul, identifier!("meter")),
                type_annotation: Some(TypeAnnotation::TypeExpression(
                    TypeExpression::TypeIdentifier(Span::dummy(), "Length".into()),
                )),
                decorators: vec![
                    decorator::Decorator::Name("myvar".into()),
                    decorator::Decorator::Aliases(vec![("foo".into(), None), ("bar".into(), None)]),
                ],
            }),
        );

        should_fail_with(
            &["let (foo)=2", "let 2=3", "let = 2"],
            ParseErrorKind::ExpectedIdentifierAfterLet,
        );

        should_fail_with(
            &["let foo", "let foo 2"],
            ParseErrorKind::ExpectedEqualOrColonAfterLetIdentifier,
        );

        should_fail_with(
            &["foo = 2"],
            ParseErrorKind::TrailingEqualSign("foo".into()),
        );

        should_fail(&["let x²=2", "let x+y=2", "let 3=5", "let x=", "let x"]);

        should_fail_with(
            &["@aliases(foo, f: short) let foobar = 1"],
            ParseErrorKind::DecoratorsWithPrefixOnLetDefinition,
        );
    }

    #[test]
    fn dimension_definition() {
        parse_as(
            &["dimension px"],
            Statement::DefineDimension(Span::dummy(), "px".into(), vec![]),
        );

        parse_as(
            &[
                "dimension Area = Length * Length",
                "dimension Area = Length × Length",
                "dimension Area =\n  Length × Length",
            ],
            Statement::DefineDimension(
                Span::dummy(),
                "Area".into(),
                vec![TypeExpression::Multiply(
                    Span::dummy(),
                    Box::new(TypeExpression::TypeIdentifier(
                        Span::dummy(),
                        "Length".into(),
                    )),
                    Box::new(TypeExpression::TypeIdentifier(
                        Span::dummy(),
                        "Length".into(),
                    )),
                )],
            ),
        );

        parse_as(
            &["dimension Velocity = Length / Time"],
            Statement::DefineDimension(
                Span::dummy(),
                "Velocity".into(),
                vec![TypeExpression::Divide(
                    Span::dummy(),
                    Box::new(TypeExpression::TypeIdentifier(
                        Span::dummy(),
                        "Length".into(),
                    )),
                    Box::new(TypeExpression::TypeIdentifier(Span::dummy(), "Time".into())),
                )],
            ),
        );

        parse_as(
            &["dimension Area = Length^2"],
            Statement::DefineDimension(
                Span::dummy(),
                "Area".into(),
                vec![TypeExpression::Power(
                    Some(Span::dummy()),
                    Box::new(TypeExpression::TypeIdentifier(
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
                Span::dummy(),
                "Energy".into(),
                vec![TypeExpression::Divide(
                    Span::dummy(),
                    Box::new(TypeExpression::Multiply(
                        Span::dummy(),
                        Box::new(TypeExpression::TypeIdentifier(Span::dummy(), "Mass".into())),
                        Box::new(TypeExpression::Power(
                            Some(Span::dummy()),
                            Box::new(TypeExpression::TypeIdentifier(
                                Span::dummy(),
                                "Length".into(),
                            )),
                            Span::dummy(),
                            Rational::from_integer(2),
                        )),
                    )),
                    Box::new(TypeExpression::Power(
                        Some(Span::dummy()),
                        Box::new(TypeExpression::TypeIdentifier(Span::dummy(), "Time".into())),
                        Span::dummy(),
                        Rational::from_integer(2),
                    )),
                )],
            ),
        );

        parse_as(
            &["dimension X = Length^(12345/67890)"],
            Statement::DefineDimension(
                Span::dummy(),
                "X".into(),
                vec![TypeExpression::Power(
                    Some(Span::dummy()),
                    Box::new(TypeExpression::TypeIdentifier(
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
                local_variables: vec![],
                return_type_annotation: None,
                decorators: vec![],
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
                local_variables: vec![],
                return_type_annotation: Some(TypeAnnotation::TypeExpression(
                    TypeExpression::TypeIdentifier(Span::dummy(), "Scalar".into()),
                )),
                decorators: vec![],
            },
        );

        parse_as(
            &["fn foo(x) = 1"],
            Statement::DefineFunction {
                function_name_span: Span::dummy(),
                function_name: "foo".into(),
                type_parameters: vec![],
                parameters: vec![(Span::dummy(), "x".into(), None)],
                body: Some(scalar!(1.0)),
                local_variables: vec![],
                return_type_annotation: None,
                decorators: vec![],
            },
        );

        parse_as(
            &["fn foo(x,) = 1"],
            Statement::DefineFunction {
                function_name_span: Span::dummy(),
                function_name: "foo".into(),
                type_parameters: vec![],
                parameters: vec![(Span::dummy(), "x".into(), None)],
                body: Some(scalar!(1.0)),
                local_variables: vec![],
                return_type_annotation: None,
                decorators: vec![],
            },
        );

        parse_as(
            &["fn foo(
                x,
                y,
            ) = 1"],
            Statement::DefineFunction {
                function_name_span: Span::dummy(),
                function_name: "foo".into(),
                type_parameters: vec![],
                parameters: vec![
                    (Span::dummy(), "x".into(), None),
                    (Span::dummy(), "y".into(), None),
                ],
                body: Some(scalar!(1.0)),
                local_variables: vec![],
                return_type_annotation: None,
                decorators: vec![],
            },
        );

        parse_as(
            &["fn foo(x, y, z) = 1"],
            Statement::DefineFunction {
                function_name_span: Span::dummy(),
                function_name: "foo".into(),
                type_parameters: vec![],
                parameters: vec![
                    (Span::dummy(), "x".into(), None),
                    (Span::dummy(), "y".into(), None),
                    (Span::dummy(), "z".into(), None),
                ],
                body: Some(scalar!(1.0)),
                local_variables: vec![],
                return_type_annotation: None,
                decorators: vec![],
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
                        Some(TypeAnnotation::TypeExpression(
                            TypeExpression::TypeIdentifier(Span::dummy(), "Length".into()),
                        )),
                    ),
                    (
                        Span::dummy(),
                        "y".into(),
                        Some(TypeAnnotation::TypeExpression(
                            TypeExpression::TypeIdentifier(Span::dummy(), "Time".into()),
                        )),
                    ),
                    (
                        Span::dummy(),
                        "z".into(),
                        Some(TypeAnnotation::TypeExpression(TypeExpression::Multiply(
                            Span::dummy(),
                            Box::new(TypeExpression::Power(
                                Some(Span::dummy()),
                                Box::new(TypeExpression::TypeIdentifier(
                                    Span::dummy(),
                                    "Length".into(),
                                )),
                                Span::dummy(),
                                Rational::new(3, 1),
                            )),
                            Box::new(TypeExpression::Power(
                                Some(Span::dummy()),
                                Box::new(TypeExpression::TypeIdentifier(
                                    Span::dummy(),
                                    "Time".into(),
                                )),
                                Span::dummy(),
                                Rational::new(2, 1),
                            )),
                        ))),
                    ),
                ],
                body: Some(scalar!(1.0)),
                local_variables: vec![],
                return_type_annotation: Some(TypeAnnotation::TypeExpression(
                    TypeExpression::TypeIdentifier(Span::dummy(), "Scalar".into()),
                )),
                decorators: vec![],
            },
        );

        parse_as(
            &["fn foo<X>(x: X) = 1"],
            Statement::DefineFunction {
                function_name_span: Span::dummy(),
                function_name: "foo".into(),
                type_parameters: vec![(Span::dummy(), "X".into(), None)],
                parameters: vec![(
                    Span::dummy(),
                    "x".into(),
                    Some(TypeAnnotation::TypeExpression(
                        TypeExpression::TypeIdentifier(Span::dummy(), "X".into()),
                    )),
                )],
                body: Some(scalar!(1.0)),
                local_variables: vec![],
                return_type_annotation: None,
                decorators: vec![],
            },
        );

        parse_as(
            &["fn foo<X: Dim>(x: X) = 1"],
            Statement::DefineFunction {
                function_name_span: Span::dummy(),
                function_name: "foo".into(),
                type_parameters: vec![(Span::dummy(), "X".into(), Some(TypeParameterBound::Dim))],
                parameters: vec![(
                    Span::dummy(),
                    "x".into(),
                    Some(TypeAnnotation::TypeExpression(
                        TypeExpression::TypeIdentifier(Span::dummy(), "X".into()),
                    )),
                )],
                body: Some(scalar!(1.0)),
                local_variables: vec![],
                return_type_annotation: None,
                decorators: vec![],
            },
        );

        parse_as(
            &["@name(\"Some function\") @description(\"This is a description of some_function.\") fn some_function(x) = 1"],
            Statement::DefineFunction {
                function_name_span: Span::dummy(),
                function_name: "some_function".into(),
                type_parameters: vec![],
                parameters: vec![(Span::dummy(), "x".into(), None)],
                body: Some(scalar!(1.0)),
                local_variables: vec![],
                return_type_annotation: None,
                decorators: vec![
                    decorator::Decorator::Name("Some function".into()),
                    decorator::Decorator::Description(
                        "This is a description of some_function.".into(),
                    ),
                ],
            },
        );

        parse_as(
            &["fn double_kef(x) = y where y = x * 2"],
            Statement::DefineFunction {
                function_name_span: Span::dummy(),
                function_name: "double_kef".into(),
                type_parameters: vec![],
                parameters: vec![(Span::dummy(), "x".into(), None)],
                body: Some(identifier!("y")),
                local_variables: vec![DefineVariable {
                    identifier_span: Span::dummy(),
                    identifier: String::from("y"),
                    expr: binop!(identifier!("x"), Mul, scalar!(2.0)),
                    type_annotation: None,
                    decorators: vec![],
                }],
                return_type_annotation: None,
                decorators: vec![],
            },
        );

        parse_as(
            &["fn kefirausaure(x) = z + y
                 where y = x + x
                   and z = y + x"],
            Statement::DefineFunction {
                function_name_span: Span::dummy(),
                function_name: "kefirausaure".into(),
                type_parameters: vec![],
                parameters: vec![(Span::dummy(), "x".into(), None)],
                body: Some(binop!(identifier!("z"), Add, identifier!("y"))),
                local_variables: vec![
                    DefineVariable {
                        identifier_span: Span::dummy(),
                        identifier: String::from("y"),
                        expr: binop!(identifier!("x"), Add, identifier!("x")),
                        type_annotation: None,
                        decorators: vec![],
                    },
                    DefineVariable {
                        identifier_span: Span::dummy(),
                        identifier: String::from("z"),
                        expr: binop!(identifier!("y"), Add, identifier!("x")),
                        type_annotation: None,
                        decorators: vec![],
                    },
                ],
                return_type_annotation: None,
                decorators: vec![],
            },
        );

        should_fail_with(
            &["fn f(x) = x where"],
            ParseErrorKind::ExpectedLocalVariableDefinition,
        );

        should_fail_with(
            &["fn f(x) = x where z = 1 and"],
            ParseErrorKind::ExpectedLocalVariableDefinition,
        );

        should_fail_with(
            &["@aliases(foo) fn foobar(a: Scalar) -> Scalar"],
            ParseErrorKind::AliasUsedOnFunction,
        );
    }

    #[test]
    fn function_call() {
        parse_as_expression(
            &["foo()"],
            Expression::FunctionCall(
                Span::dummy(),
                Span::dummy(),
                Box::new(identifier!("foo")),
                vec![],
            ),
        );

        parse_as_expression(
            &["foo(1)"],
            Expression::FunctionCall(
                Span::dummy(),
                Span::dummy(),
                Box::new(identifier!("foo")),
                vec![scalar!(1.0)],
            ),
        );

        parse_as_expression(
            &["foo(1,2,3)"],
            Expression::FunctionCall(
                Span::dummy(),
                Span::dummy(),
                Box::new(identifier!("foo")),
                vec![scalar!(1.0), scalar!(2.0), scalar!(3.0)],
            ),
        );

        // https://github.com/sharkdp/numbat/issues/507
        assert_snapshot!(snap_parse(
            "tamo(
              2 m,
              5 m
            )"), @r###"
        Expression(FunctionCall(Span { start: SourceCodePositition { byte: 0, line: 1, position: 1 }, end: SourceCodePositition { byte: 4, line: 1, position: 5 }, code_source_id: 0 }, Span { start: SourceCodePositition { byte: 0, line: 1, position: 1 }, end: SourceCodePositition { byte: 56, line: 4, position: 14 }, code_source_id: 0 }, Identifier(Span { start: SourceCodePositition { byte: 0, line: 1, position: 1 }, end: SourceCodePositition { byte: 4, line: 1, position: 5 }, code_source_id: 0 }, "tamo"), [BinaryOperator { op: Mul, lhs: Scalar(Span { start: SourceCodePositition { byte: 20, line: 2, position: 15 }, end: SourceCodePositition { byte: 21, line: 2, position: 16 }, code_source_id: 0 }, Number(2.0)), rhs: Identifier(Span { start: SourceCodePositition { byte: 22, line: 2, position: 17 }, end: SourceCodePositition { byte: 23, line: 2, position: 18 }, code_source_id: 0 }, "m"), span_op: None }, BinaryOperator { op: Mul, lhs: Scalar(Span { start: SourceCodePositition { byte: 39, line: 3, position: 15 }, end: SourceCodePositition { byte: 40, line: 3, position: 16 }, code_source_id: 0 }, Number(5.0)), rhs: Identifier(Span { start: SourceCodePositition { byte: 41, line: 3, position: 17 }, end: SourceCodePositition { byte: 42, line: 3, position: 18 }, code_source_id: 0 }, "m"), span_op: None }]))
        "###);

        assert_snapshot!(snap_parse(
            "kefir(
              2 m,
              5 m,
            )"), @r###"
        Expression(FunctionCall(Span { start: SourceCodePositition { byte: 0, line: 1, position: 1 }, end: SourceCodePositition { byte: 5, line: 1, position: 6 }, code_source_id: 0 }, Span { start: SourceCodePositition { byte: 0, line: 1, position: 1 }, end: SourceCodePositition { byte: 58, line: 4, position: 14 }, code_source_id: 0 }, Identifier(Span { start: SourceCodePositition { byte: 0, line: 1, position: 1 }, end: SourceCodePositition { byte: 5, line: 1, position: 6 }, code_source_id: 0 }, "kefir"), [BinaryOperator { op: Mul, lhs: Scalar(Span { start: SourceCodePositition { byte: 21, line: 2, position: 15 }, end: SourceCodePositition { byte: 22, line: 2, position: 16 }, code_source_id: 0 }, Number(2.0)), rhs: Identifier(Span { start: SourceCodePositition { byte: 23, line: 2, position: 17 }, end: SourceCodePositition { byte: 24, line: 2, position: 18 }, code_source_id: 0 }, "m"), span_op: None }, BinaryOperator { op: Mul, lhs: Scalar(Span { start: SourceCodePositition { byte: 40, line: 3, position: 15 }, end: SourceCodePositition { byte: 41, line: 3, position: 16 }, code_source_id: 0 }, Number(5.0)), rhs: Identifier(Span { start: SourceCodePositition { byte: 42, line: 3, position: 17 }, end: SourceCodePositition { byte: 43, line: 3, position: 18 }, code_source_id: 0 }, "m"), span_op: None }]))
        "###);
        assert_snapshot!(snap_parse(
            "echo(
            )"), @r###"
        Expression(FunctionCall(Span { start: SourceCodePositition { byte: 0, line: 1, position: 1 }, end: SourceCodePositition { byte: 4, line: 1, position: 5 }, code_source_id: 0 }, Span { start: SourceCodePositition { byte: 0, line: 1, position: 1 }, end: SourceCodePositition { byte: 19, line: 2, position: 14 }, code_source_id: 0 }, Identifier(Span { start: SourceCodePositition { byte: 0, line: 1, position: 1 }, end: SourceCodePositition { byte: 4, line: 1, position: 5 }, code_source_id: 0 }, "echo"), []))
        "###);
        assert_snapshot!(snap_parse(
            "jax(
              2 m,
              5 m,
            "), @r###"
        Successfully parsed:
        Errors encountered:
        Missing closing parenthesis ')' - ParseError { kind: MissingClosingParen, span: Span { start: SourceCodePositition { byte: 55, line: 4, position: 13 }, end: SourceCodePositition { byte: 55, line: 4, position: 13 }, code_source_id: 0 } }
        "###);
    }

    #[test]
    fn callable_calls() {
        parse_as_expression(
            &["(returns_fn())()"],
            Expression::FunctionCall(
                Span::dummy(),
                Span::dummy(),
                Box::new(Expression::FunctionCall(
                    Span::dummy(),
                    Span::dummy(),
                    Box::new(identifier!("returns_fn")),
                    vec![],
                )),
                vec![],
            ),
        );

        parse_as_expression(
            &["returns_fn()()"],
            Expression::FunctionCall(
                Span::dummy(),
                Span::dummy(),
                Box::new(Expression::FunctionCall(
                    Span::dummy(),
                    Span::dummy(),
                    Box::new(identifier!("returns_fn")),
                    vec![],
                )),
                vec![],
            ),
        );
    }

    #[test]
    fn postfix_apply() {
        parse_as_expression(
            &["1 + 1 |> foo"],
            Expression::FunctionCall(
                Span::dummy(),
                Span::dummy(),
                Box::new(identifier!("foo")),
                vec![binop!(scalar!(1.0), Add, scalar!(1.0))],
            ),
        );
        parse_as_expression(
            &["1 + 1 |> kefir(2)"],
            Expression::FunctionCall(
                Span::dummy(),
                Span::dummy(),
                Box::new(identifier!("kefir")),
                vec![scalar!(2.0), binop!(scalar!(1.0), Add, scalar!(1.0))],
            ),
        );

        should_fail_with(&["1 |> print()"], ParseErrorKind::InlineProcedureUsage);

        should_fail_with(&["1 |> +"], ParseErrorKind::ExpectedPrimary);

        should_fail_with(
            &["1 |> 2", "1 |> 1 +"],
            ParseErrorKind::ExpectedIdentifierOrCallAfterPostfixApply,
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

        // https://github.com/sharkdp/numbat/issues/507
        assert_snapshot!(snap_parse(
            "print(
              2 m,
              5 m
            )"), @r###"
        ProcedureCall(Span { start: SourceCodePositition { byte: 0, line: 1, position: 1 }, end: SourceCodePositition { byte: 5, line: 1, position: 6 }, code_source_id: 0 }, Print, [BinaryOperator { op: Mul, lhs: Scalar(Span { start: SourceCodePositition { byte: 21, line: 2, position: 15 }, end: SourceCodePositition { byte: 22, line: 2, position: 16 }, code_source_id: 0 }, Number(2.0)), rhs: Identifier(Span { start: SourceCodePositition { byte: 23, line: 2, position: 17 }, end: SourceCodePositition { byte: 24, line: 2, position: 18 }, code_source_id: 0 }, "m"), span_op: None }, BinaryOperator { op: Mul, lhs: Scalar(Span { start: SourceCodePositition { byte: 40, line: 3, position: 15 }, end: SourceCodePositition { byte: 41, line: 3, position: 16 }, code_source_id: 0 }, Number(5.0)), rhs: Identifier(Span { start: SourceCodePositition { byte: 42, line: 3, position: 17 }, end: SourceCodePositition { byte: 43, line: 3, position: 18 }, code_source_id: 0 }, "m"), span_op: None }])
        "###);

        assert_snapshot!(snap_parse(
            "print(
              2 m,
              5 m,
            )"), @r###"
        ProcedureCall(Span { start: SourceCodePositition { byte: 0, line: 1, position: 1 }, end: SourceCodePositition { byte: 5, line: 1, position: 6 }, code_source_id: 0 }, Print, [BinaryOperator { op: Mul, lhs: Scalar(Span { start: SourceCodePositition { byte: 21, line: 2, position: 15 }, end: SourceCodePositition { byte: 22, line: 2, position: 16 }, code_source_id: 0 }, Number(2.0)), rhs: Identifier(Span { start: SourceCodePositition { byte: 23, line: 2, position: 17 }, end: SourceCodePositition { byte: 24, line: 2, position: 18 }, code_source_id: 0 }, "m"), span_op: None }, BinaryOperator { op: Mul, lhs: Scalar(Span { start: SourceCodePositition { byte: 40, line: 3, position: 15 }, end: SourceCodePositition { byte: 41, line: 3, position: 16 }, code_source_id: 0 }, Number(5.0)), rhs: Identifier(Span { start: SourceCodePositition { byte: 42, line: 3, position: 17 }, end: SourceCodePositition { byte: 43, line: 3, position: 18 }, code_source_id: 0 }, "m"), span_op: None }])
        "###);
        assert_snapshot!(snap_parse(
            "print(
            )"), @r###"
        ProcedureCall(Span { start: SourceCodePositition { byte: 0, line: 1, position: 1 }, end: SourceCodePositition { byte: 5, line: 1, position: 6 }, code_source_id: 0 }, Print, [])
        "###);
        println!("HEEEERE");
        assert_snapshot!(snap_parse(
            "print(
              2 m,
              5 m,
            "), @r###"
        Successfully parsed:
        Errors encountered:
        Missing closing parenthesis ')' - ParseError { kind: MissingClosingParen, span: Span { start: SourceCodePositition { byte: 57, line: 4, position: 13 }, end: SourceCodePositition { byte: 57, line: 4, position: 13 }, code_source_id: 0 } }
        "###);
    }

    #[test]
    fn logical_operation() {
        // basic
        assert_snapshot!(snap_parse(
            "true || false"), @r###"
        Expression(BinaryOperator { op: LogicalOr, lhs: Boolean(Span { start: SourceCodePositition { byte: 0, line: 1, position: 1 }, end: SourceCodePositition { byte: 4, line: 1, position: 5 }, code_source_id: 0 }, true), rhs: Boolean(Span { start: SourceCodePositition { byte: 8, line: 1, position: 9 }, end: SourceCodePositition { byte: 13, line: 1, position: 14 }, code_source_id: 0 }, false), span_op: Some(Span { start: SourceCodePositition { byte: 5, line: 1, position: 6 }, end: SourceCodePositition { byte: 7, line: 1, position: 8 }, code_source_id: 0 }) })
        "###);
        assert_snapshot!(snap_parse(
            "true && false"), @r###"
        Expression(BinaryOperator { op: LogicalAnd, lhs: Boolean(Span { start: SourceCodePositition { byte: 0, line: 1, position: 1 }, end: SourceCodePositition { byte: 4, line: 1, position: 5 }, code_source_id: 0 }, true), rhs: Boolean(Span { start: SourceCodePositition { byte: 8, line: 1, position: 9 }, end: SourceCodePositition { byte: 13, line: 1, position: 14 }, code_source_id: 0 }, false), span_op: Some(Span { start: SourceCodePositition { byte: 5, line: 1, position: 6 }, end: SourceCodePositition { byte: 7, line: 1, position: 8 }, code_source_id: 0 }) })
        "###);
        assert_snapshot!(snap_parse(
            "!true"), @r###"
        Expression(UnaryOperator { op: LogicalNeg, expr: Boolean(Span { start: SourceCodePositition { byte: 1, line: 1, position: 2 }, end: SourceCodePositition { byte: 5, line: 1, position: 6 }, code_source_id: 0 }, true), span_op: Span { start: SourceCodePositition { byte: 0, line: 1, position: 1 }, end: SourceCodePositition { byte: 1, line: 1, position: 2 }, code_source_id: 0 } })
        "###);

        // priority
        #[rustfmt::skip]
        parse_as_expression(
            &["true || false && true || false"],
            binop!( // the 'and' operator has the highest priority
                binop!(boolean!(true), LogicalOr, binop!(boolean!(false), LogicalAnd, boolean!(true))),
                LogicalOr,
                boolean!(false)
                )
            );

        #[rustfmt::skip]
        parse_as_expression(
            &["!true && false"],
            binop!( // The negation has precedence over the 'and'
                logical_neg!(boolean!(true)),
                LogicalAnd,
                boolean!(false)
                )
            );
    }

    #[test]
    fn comparisons() {
        parse_as_expression(&["1 < 2"], binop!(scalar!(1.0), LessThan, scalar!(2.0)));
        parse_as_expression(
            &["1 <= 2", "1 ≤ 2"],
            binop!(scalar!(1.0), LessOrEqual, scalar!(2.0)),
        );

        parse_as_expression(&["1 > 2"], binop!(scalar!(1.0), GreaterThan, scalar!(2.0)));
        parse_as_expression(
            &["1 >= 2", "1 ≥ 2"],
            binop!(scalar!(1.0), GreaterOrEqual, scalar!(2.0)),
        );

        parse_as_expression(
            &["1 == 2", "1 ⩵ 2"],
            binop!(scalar!(1.0), Equal, scalar!(2.0)),
        );
        parse_as_expression(
            &["1 != 2", "1 ≠ 2"],
            binop!(scalar!(1.0), NotEqual, scalar!(2.0)),
        );
    }

    #[test]
    fn conditionals() {
        parse_as_expression(
            &[
                "if 1 < 2 then 3 + 4 else 5 * 6",
                "if (1 < 2) then (3 + 4) else (5 * 6)",
                "if 1 < 2\n  then\n    3 + 4\n  else\n    5 * 6",
            ],
            conditional!(
                binop!(scalar!(1.0), LessThan, scalar!(2.0)),
                binop!(scalar!(3.0), Add, scalar!(4.0)),
                binop!(scalar!(5.0), Mul, scalar!(6.0))
            ),
        );

        parse_as_expression(
            &[
                "if true then if false then 1 else 2 else 3",
                "if true then (if false then 1 else 2) else 3",
            ],
            conditional!(
                Expression::Boolean(Span::dummy(), true),
                conditional!(
                    Expression::Boolean(Span::dummy(), false),
                    scalar!(1.0),
                    scalar!(2.0)
                ),
                scalar!(3.0)
            ),
        );

        should_fail_with(&["if true 1 else 2"], ParseErrorKind::ExpectedThen);
        should_fail_with(&["if true then 1"], ParseErrorKind::ExpectedElse);
    }

    #[test]
    fn strings() {
        parse_as_expression(
            &["\"hello world\""],
            Expression::String(Span::dummy(), vec![StringPart::Fixed("hello world".into())]),
        );

        parse_as_expression(
            &[r#""hello \"world\"!""#],
            Expression::String(
                Span::dummy(),
                vec![StringPart::Fixed("hello \"world\"!".into())],
            ),
        );

        parse_as_expression(
            &[r#""newline: \n, return: \r, tab: \t, quote: \", null: \0, backslash: \\, open_brace: \{, close brace: \}.""#],
            Expression::String(
                Span::dummy(),
                vec![StringPart::Fixed("newline: \n, return: \r, tab: \t, quote: \", null: \0, backslash: \\, open_brace: {, close brace: }.".into())],
            ),
        );

        parse_as_expression(
            &[r#""\"""#],
            Expression::String(Span::dummy(), vec![StringPart::Fixed("\"".into())]),
        );

        parse_as_expression(
            &[r#""\\""#],
            Expression::String(Span::dummy(), vec![StringPart::Fixed("\\".into())]),
        );

        parse_as_expression(
            &[r#""\\\"""#],
            Expression::String(Span::dummy(), vec![StringPart::Fixed("\\\"".into())]),
        );

        parse_as_expression(
            &[r#""\"\\""#],
            Expression::String(Span::dummy(), vec![StringPart::Fixed("\"\\".into())]),
        );

        parse_as_expression(
            &[r#""\\\n""#],
            Expression::String(Span::dummy(), vec![StringPart::Fixed("\\\n".into())]),
        );

        parse_as_expression(
            &[r#""\n\\""#],
            Expression::String(Span::dummy(), vec![StringPart::Fixed("\n\\".into())]),
        );

        parse_as_expression(
            &[r#""\\n""#],
            Expression::String(Span::dummy(), vec![StringPart::Fixed("\\n".into())]),
        );

        parse_as_expression(
            &["\"pi = {pi}\""],
            Expression::String(
                Span::dummy(),
                vec![
                    StringPart::Fixed("pi = ".into()),
                    StringPart::Interpolation {
                        span: Span::dummy(),
                        expr: Box::new(identifier!("pi")),
                        format_specifiers: None,
                    },
                ],
            ),
        );

        parse_as_expression(
            &["\"{pi}\""],
            Expression::String(
                Span::dummy(),
                vec![StringPart::Interpolation {
                    span: Span::dummy(),
                    expr: Box::new(identifier!("pi")),
                    format_specifiers: None,
                }],
            ),
        );

        parse_as_expression(
            &["\"{pi}{e}\""],
            Expression::String(
                Span::dummy(),
                vec![
                    StringPart::Interpolation {
                        span: Span::dummy(),
                        expr: Box::new(identifier!("pi")),
                        format_specifiers: None,
                    },
                    StringPart::Interpolation {
                        span: Span::dummy(),
                        expr: Box::new(identifier!("e")),
                        format_specifiers: None,
                    },
                ],
            ),
        );

        parse_as_expression(
            &["\"{pi} + {e}\""],
            Expression::String(
                Span::dummy(),
                vec![
                    StringPart::Interpolation {
                        span: Span::dummy(),
                        expr: Box::new(identifier!("pi")),
                        format_specifiers: None,
                    },
                    StringPart::Fixed(" + ".into()),
                    StringPart::Interpolation {
                        span: Span::dummy(),
                        expr: Box::new(identifier!("e")),
                        format_specifiers: None,
                    },
                ],
            ),
        );

        parse_as_expression(
            &["\"1 + 2 = {1 + 2:0.2}\""],
            Expression::String(
                Span::dummy(),
                vec![
                    StringPart::Fixed("1 + 2 = ".into()),
                    StringPart::Interpolation {
                        span: Span::dummy(),
                        expr: Box::new(binop!(scalar!(1.0), Add, scalar!(2.0))),
                        format_specifiers: Some(":0.2".to_string()),
                    },
                ],
            ),
        );

        should_fail_with(&["\"test {1"], ParseErrorKind::UnterminatedString);
        should_fail_with(
            &[
                "\"foo {} bar\"",
                "\"foo {1} bar {} baz\"",
                "\"foo {   } bar\"",
            ],
            ParseErrorKind::EmptyStringInterpolation,
        );
    }

    #[test]
    fn structs() {
        parse_as(
            &["struct Foo { foo: Scalar, bar: Scalar }"],
            Statement::DefineStruct {
                struct_name_span: Span::dummy(),
                struct_name: "Foo".to_owned(),
                fields: vec![
                    (
                        Span::dummy(),
                        "foo".to_owned(),
                        TypeAnnotation::TypeExpression(TypeExpression::TypeIdentifier(
                            Span::dummy(),
                            "Scalar".to_owned(),
                        )),
                    ),
                    (
                        Span::dummy(),
                        "bar".to_owned(),
                        TypeAnnotation::TypeExpression(TypeExpression::TypeIdentifier(
                            Span::dummy(),
                            "Scalar".to_owned(),
                        )),
                    ),
                ],
            },
        );

        parse_as_expression(
            &["Foo {foo: 1, bar: 2}"],
            struct_! {
                Foo,
                foo: scalar!(1.0),
                bar: scalar!(2.0)
            },
        );

        parse_as_expression(
            &["Foo {foo: 1, bar: 2}.foo"],
            Expression::AccessField(
                Span::dummy(),
                Span::dummy(),
                Box::new(struct_! {
                    Foo,
                    foo: scalar!(1.0),
                    bar: scalar!(2.0)
                }),
                "foo".to_owned(),
            ),
        );
    }

    #[test]
    fn lists() {
        parse_as_expression(&["[]"], list!());
        parse_as_expression(&["[1]", "[1,]"], list!(scalar!(1.0)));
        parse_as_expression(&["[1, 2]", "[1, 2, ]"], list!(scalar!(1.0), scalar!(2.0)));

        parse_as_expression(&["[\n]"], list!());
        parse_as_expression(&["[1\n]", "[1,\n]"], list!(scalar!(1.0)));
        parse_as_expression(
            &["[1\n,2\n]", "[1,\n2,\n]"],
            list!(scalar!(1.0), scalar!(2.0)),
        );

        parse_as_expression(
            &["[[1,2], [3]]"],
            list!(list!(scalar!(1.0), scalar!(2.0)), list!(scalar!(3.0))),
        );

        parse_as_expression(
            &["[[1,\n2\n],\n [3\n]\n]"],
            list!(list!(scalar!(1.0), scalar!(2.0)), list!(scalar!(3.0))),
        );

        should_fail_with(
            &["[1", "[1, 2, 3", "[1, 2)"],
            ParseErrorKind::ExpectedCommaOrRightBracketInList,
        );
        should_fail_with(&["[1, 2, ,"], ParseErrorKind::ExpectedPrimary);
        should_fail_with(
            &["[1, 2]]"],
            ParseErrorKind::TrailingCharacters("]".to_owned()),
        );

        should_fail_with(
            &["[1\n", "[1,\n 2,\n 3\n", "[1,\n 2\n)"],
            ParseErrorKind::ExpectedCommaOrRightBracketInList,
        );
        should_fail_with(&["[1,\n2,\n,\n"], ParseErrorKind::ExpectedPrimary);
    }

    #[test]
    fn accumulate_errors() {
        // error on the last character of a line
        assert_snapshot!(snap_parse(
            "1 + 
            2 + 3"), @r###"
        Successfully parsed:
        Expression(BinaryOperator { op: Add, lhs: Scalar(Span { start: SourceCodePositition { byte: 17, line: 2, position: 13 }, end: SourceCodePositition { byte: 18, line: 2, position: 14 }, code_source_id: 0 }, Number(2.0)), rhs: Scalar(Span { start: SourceCodePositition { byte: 21, line: 2, position: 17 }, end: SourceCodePositition { byte: 22, line: 2, position: 18 }, code_source_id: 0 }, Number(3.0)), span_op: Some(Span { start: SourceCodePositition { byte: 19, line: 2, position: 15 }, end: SourceCodePositition { byte: 20, line: 2, position: 16 }, code_source_id: 0 }) })
        Errors encountered:
        Expected one of: number, identifier, parenthesized expression, struct instantiation, list - ParseError { kind: ExpectedPrimary, span: Span { start: SourceCodePositition { byte: 4, line: 1, position: 5 }, end: SourceCodePositition { byte: 5, line: 1, position: 6 }, code_source_id: 0 } }
        "###);
        // error in the middle of something
        assert_snapshot!(snap_parse(
            "
            let cool = 50
            let tamo = * 30 
            assert_eq(tamo + cool == 80)
            30m"), @r###"
        Successfully parsed:
        DefineVariable(DefineVariable { identifier_span: Span { start: SourceCodePositition { byte: 17, line: 2, position: 17 }, end: SourceCodePositition { byte: 21, line: 2, position: 21 }, code_source_id: 0 }, identifier: "cool", expr: Scalar(Span { start: SourceCodePositition { byte: 24, line: 2, position: 24 }, end: SourceCodePositition { byte: 26, line: 2, position: 26 }, code_source_id: 0 }, Number(50.0)), type_annotation: None, decorators: [] })
        ProcedureCall(Span { start: SourceCodePositition { byte: 68, line: 4, position: 13 }, end: SourceCodePositition { byte: 77, line: 4, position: 22 }, code_source_id: 0 }, AssertEq, [BinaryOperator { op: Equal, lhs: BinaryOperator { op: Add, lhs: Identifier(Span { start: SourceCodePositition { byte: 78, line: 4, position: 23 }, end: SourceCodePositition { byte: 82, line: 4, position: 27 }, code_source_id: 0 }, "tamo"), rhs: Identifier(Span { start: SourceCodePositition { byte: 85, line: 4, position: 30 }, end: SourceCodePositition { byte: 89, line: 4, position: 34 }, code_source_id: 0 }, "cool"), span_op: Some(Span { start: SourceCodePositition { byte: 83, line: 4, position: 28 }, end: SourceCodePositition { byte: 84, line: 4, position: 29 }, code_source_id: 0 }) }, rhs: Scalar(Span { start: SourceCodePositition { byte: 93, line: 4, position: 38 }, end: SourceCodePositition { byte: 95, line: 4, position: 40 }, code_source_id: 0 }, Number(80.0)), span_op: Some(Span { start: SourceCodePositition { byte: 90, line: 4, position: 35 }, end: SourceCodePositition { byte: 92, line: 4, position: 37 }, code_source_id: 0 }) }])
        Expression(BinaryOperator { op: Mul, lhs: Scalar(Span { start: SourceCodePositition { byte: 109, line: 5, position: 13 }, end: SourceCodePositition { byte: 111, line: 5, position: 15 }, code_source_id: 0 }, Number(30.0)), rhs: Identifier(Span { start: SourceCodePositition { byte: 111, line: 5, position: 15 }, end: SourceCodePositition { byte: 112, line: 5, position: 16 }, code_source_id: 0 }, "m"), span_op: None })
        Errors encountered:
        Expected one of: number, identifier, parenthesized expression, struct instantiation, list - ParseError { kind: ExpectedPrimary, span: Span { start: SourceCodePositition { byte: 50, line: 3, position: 24 }, end: SourceCodePositition { byte: 51, line: 3, position: 25 }, code_source_id: 0 } }
        "###);
        // error on a multiline let
        assert_snapshot!(snap_parse(
            "
            let tamo =
                * cool
            "), @r###"
        Successfully parsed:
        Errors encountered:
        Expected one of: number, identifier, parenthesized expression, struct instantiation, list - ParseError { kind: ExpectedPrimary, span: Span { start: SourceCodePositition { byte: 40, line: 3, position: 17 }, end: SourceCodePositition { byte: 41, line: 3, position: 18 }, code_source_id: 0 } }
        "###);
        // error on a multiline if
        assert_snapshot!(snap_parse(
            "
            if a =
                then false
                else true
            "), @r###"
        Successfully parsed:
        Errors encountered:
        Expected 'then' in if-then-else condition - ParseError { kind: ExpectedThen, span: Span { start: SourceCodePositition { byte: 18, line: 2, position: 18 }, end: SourceCodePositition { byte: 19, line: 2, position: 19 }, code_source_id: 0 } }
        Expected one of: number, identifier, parenthesized expression, struct instantiation, list - ParseError { kind: ExpectedPrimary, span: Span { start: SourceCodePositition { byte: 36, line: 3, position: 17 }, end: SourceCodePositition { byte: 40, line: 3, position: 21 }, code_source_id: 0 } }
        Expected one of: number, identifier, parenthesized expression, struct instantiation, list - ParseError { kind: ExpectedPrimary, span: Span { start: SourceCodePositition { byte: 63, line: 4, position: 17 }, end: SourceCodePositition { byte: 67, line: 4, position: 21 }, code_source_id: 0 } }
        "###);

        // #260
        assert_snapshot!(snap_parse(
            "x = 3"), @r###"
        Successfully parsed:
        Expression(Identifier(Span { start: SourceCodePositition { byte: 0, line: 1, position: 1 }, end: SourceCodePositition { byte: 1, line: 1, position: 2 }, code_source_id: 0 }, "x"))
        Errors encountered:
        Trailing '=' sign. Use `let x = …` if you intended to define a new constant. - ParseError { kind: TrailingEqualSign("x"), span: Span { start: SourceCodePositition { byte: 2, line: 1, position: 3 }, end: SourceCodePositition { byte: 3, line: 1, position: 4 }, code_source_id: 0 } }
        "###);
    }
}
