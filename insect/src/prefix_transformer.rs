use crate::{
    ast::{Expression, Statement},
    name_resolution::NameResolutionError,
    prefix_parser::{PrefixParser, PrefixParserResult},
    typed_ast::Decorator,
};

type Result<T> = std::result::Result<T, NameResolutionError>;

pub struct Transformer {
    prefix_parser: PrefixParser,
}

// TODO: generalize this to a general-purpose transformer (not just for prefixes, could also be used for optimization)
impl Transformer {
    pub fn new() -> Self {
        Self {
            prefix_parser: PrefixParser::new(),
        }
    }

    fn transform_expression(&self, expression: Expression) -> Expression {
        match expression {
            expr @ Expression::Scalar(_) => expr,
            Expression::Identifier(identifier) => {
                if let PrefixParserResult::UnitIdentifier(prefix, unit_name) =
                    self.prefix_parser.parse(&identifier)
                {
                    Expression::UnitIdentifier(prefix, unit_name)
                } else {
                    Expression::Identifier(identifier)
                }
            }
            Expression::UnitIdentifier(_, _) => {
                unreachable!("Prefixed identifiers should not exist prior to this stage")
            }
            Expression::Negate(expr) => {
                Expression::Negate(Box::new(self.transform_expression(*expr)))
            }
            Expression::BinaryOperator(op, expr_lhs, expr_rhs) => Expression::BinaryOperator(
                op,
                Box::new(self.transform_expression(*expr_lhs)),
                Box::new(self.transform_expression(*expr_rhs)),
            ),
            Expression::FunctionCall(name, args) => Expression::FunctionCall(
                name,
                args.into_iter()
                    .map(|arg| self.transform_expression(arg))
                    .collect(),
            ),
        }
    }

    fn has_prefix_decorator(decorators: &[Decorator]) -> bool {
        decorators
            .iter()
            .any(|decorator| decorator == &Decorator::Prefixes("decimal".into()))
    }

    fn transform_statement(&mut self, statement: Statement) -> Result<Statement> {
        Ok(match statement {
            Statement::Expression(expr) => Statement::Expression(self.transform_expression(expr)),
            Statement::DeclareBaseUnit(name, dexpr, decorators) => {
                if Self::has_prefix_decorator(&decorators) {
                    self.prefix_parser.add_prefixable_unit(&name)?;
                } else {
                    self.prefix_parser.add_non_prefixable_unit(&name)?;
                }
                Statement::DeclareBaseUnit(name, dexpr, decorators)
            }
            Statement::DeclareDerivedUnit(name, expr, dexpr, decorators) => {
                // TODO(minor): same block as above. extract to function?
                if Self::has_prefix_decorator(&decorators) {
                    self.prefix_parser.add_prefixable_unit(&name)?;
                } else {
                    self.prefix_parser.add_non_prefixable_unit(&name)?;
                }
                Statement::DeclareDerivedUnit(
                    name,
                    self.transform_expression(expr),
                    dexpr,
                    decorators,
                )
            }
            Statement::DeclareVariable(name, expr, dexpr) => {
                self.prefix_parser.add_other_identifier(&name)?;
                Statement::DeclareVariable(name, self.transform_expression(expr), dexpr)
            }
            Statement::DeclareFunction(name, type_params, args, body, return_type) => {
                self.prefix_parser.add_other_identifier(&name)?;
                Statement::DeclareFunction(
                    name,
                    type_params,
                    args,
                    body.map(|expr| self.transform_expression(expr)),
                    return_type,
                )
            }
            statement @ Statement::DeclareDimension(_, _) => statement,
            Statement::ProcedureCall(procedure, args) => Statement::ProcedureCall(
                procedure,
                args.into_iter()
                    .map(|arg| self.transform_expression(arg))
                    .collect(),
            ),
        })
    }

    pub fn transform(
        &mut self,
        statements: impl IntoIterator<Item = Statement>,
    ) -> Result<Vec<Statement>> {
        statements
            .into_iter()
            .map(|statement| self.transform_statement(statement))
            .collect()
    }
}
