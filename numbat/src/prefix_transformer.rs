use crate::{
    ast::{Expression, Statement},
    decorator::{self, Decorator},
    name_resolution::NameResolutionError,
    prefix_parser::{PrefixParser, PrefixParserResult},
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

    fn has_decorator(decorators: &[Decorator], decorator: Decorator) -> bool {
        decorators.iter().any(|d| d == &decorator)
    }

    pub(crate) fn register_name_and_aliases(
        &mut self,
        name: &String,
        decorators: &[Decorator],
    ) -> Result<()> {
        let metric_prefixes = Self::has_decorator(decorators, Decorator::MetricPrefixes);
        let binary_prefixes = Self::has_decorator(decorators, Decorator::BinaryPrefixes);
        for (alias, accepts_prefix) in decorator::name_and_aliases(name, decorators) {
            self.prefix_parser
                .add_unit(alias, accepts_prefix, metric_prefixes, binary_prefixes)?;
        }

        Ok(())
    }

    fn transform_statement(&mut self, statement: Statement) -> Result<Statement> {
        Ok(match statement {
            Statement::Expression(expr) => Statement::Expression(self.transform_expression(expr)),
            Statement::DeclareBaseUnit(name, dexpr, decorators) => {
                self.register_name_and_aliases(&name, &decorators)?;
                Statement::DeclareBaseUnit(name, dexpr, decorators)
            }
            Statement::DeclareDerivedUnit(name, expr, dexpr, decorators) => {
                self.register_name_and_aliases(&name, &decorators)?;
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
