use crate::{
    ast::{Expression, Statement},
    decorator::{self, Decorator},
    name_resolution::NameResolutionError,
    prefix_parser::{PrefixParser, PrefixParserResult},
    span::Span,
};

type Result<T> = std::result::Result<T, NameResolutionError>;

#[derive(Debug, Clone)]
pub(crate) struct Transformer {
    prefix_parser: PrefixParser,

    pub variable_names: Vec<String>,
    pub function_names: Vec<String>,
    pub unit_names: Vec<Vec<String>>,
    pub dimension_names: Vec<String>,
}

// TODO: generalize this to a general-purpose transformer (not just for prefixes, could also be used for optimization)
impl Transformer {
    pub fn new() -> Self {
        Self {
            prefix_parser: PrefixParser::new(),
            variable_names: vec![],
            function_names: vec![],
            unit_names: vec![],
            dimension_names: vec![],
        }
    }

    fn transform_expression(&self, expression: Expression) -> Expression {
        match expression {
            expr @ Expression::Scalar(..) => expr,
            Expression::Identifier(span, identifier) => {
                if let PrefixParserResult::UnitIdentifier(prefix, unit_name, full_name) =
                    self.prefix_parser.parse(&identifier)
                {
                    Expression::UnitIdentifier(span, prefix, unit_name, full_name)
                } else {
                    Expression::Identifier(span, identifier)
                }
            }
            Expression::UnitIdentifier(_, _, _, _) => {
                unreachable!("Prefixed identifiers should not exist prior to this stage")
            }
            Expression::Negate(expr) => {
                Expression::Negate(Box::new(self.transform_expression(*expr)))
            }
            Expression::BinaryOperator {
                op,
                lhs,
                rhs,
                span_op,
            } => Expression::BinaryOperator {
                op,
                lhs: Box::new(self.transform_expression(*lhs)),
                rhs: Box::new(self.transform_expression(*rhs)),
                span_op,
            },
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
        definition_span: Span,
    ) -> Result<()> {
        let mut unit_names = vec![name.to_string()];
        let metric_prefixes = Self::has_decorator(decorators, Decorator::MetricPrefixes);
        let binary_prefixes = Self::has_decorator(decorators, Decorator::BinaryPrefixes);
        for (alias, accepts_prefix) in decorator::name_and_aliases(name, decorators) {
            self.prefix_parser.add_unit(
                alias,
                accepts_prefix,
                metric_prefixes,
                binary_prefixes,
                name,
                definition_span,
            )?;
            unit_names.push(alias.to_string());
        }

        unit_names.sort();
        self.unit_names.push(unit_names);

        Ok(())
    }

    fn transform_statement(&mut self, statement: Statement) -> Result<Statement> {
        Ok(match statement {
            Statement::Expression(expr) => Statement::Expression(self.transform_expression(expr)),
            Statement::DeclareBaseUnit(span, name, dexpr, decorators) => {
                self.register_name_and_aliases(&name, &decorators, span)?;
                Statement::DeclareBaseUnit(span, name, dexpr, decorators)
            }
            Statement::DeclareDerivedUnit(span, name, expr, dexpr, decorators) => {
                self.register_name_and_aliases(&name, &decorators, span)?;
                Statement::DeclareDerivedUnit(
                    span,
                    name,
                    self.transform_expression(expr),
                    dexpr,
                    decorators,
                )
            }
            Statement::DeclareVariable(span, name, expr, dexpr) => {
                self.variable_names.push(name.clone());
                self.prefix_parser.add_other_identifier(&name, span)?;
                Statement::DeclareVariable(span, name, self.transform_expression(expr), dexpr)
            }
            Statement::DeclareFunction(span, name, type_params, args, body, return_type) => {
                self.function_names.push(name.clone());
                self.prefix_parser.add_other_identifier(&name, span)?;
                Statement::DeclareFunction(
                    span,
                    name,
                    type_params,
                    args,
                    body.map(|expr| self.transform_expression(expr)),
                    return_type,
                )
            }
            Statement::DeclareDimension(name, dexprs) => {
                self.dimension_names.push(name.clone());
                Statement::DeclareDimension(name, dexprs)
            }
            Statement::ProcedureCall(procedure, args) => Statement::ProcedureCall(
                procedure,
                args.into_iter()
                    .map(|arg| self.transform_expression(arg))
                    .collect(),
            ),
            statement @ Statement::ModuleImport(_, _) => statement,
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
