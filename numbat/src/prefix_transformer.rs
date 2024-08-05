use crate::{
    ast::{Expression, Statement, StringPart},
    decorator::{self, Decorator},
    name_resolution::NameResolutionError,
    prefix_parser::{PrefixParser, PrefixParserResult},
    span::Span,
};

type Result<T> = std::result::Result<T, NameResolutionError>;

#[derive(Debug, Clone)]
pub(crate) struct Transformer {
    pub prefix_parser: PrefixParser,

    pub variable_names: Vec<String>,
    pub function_names: Vec<String>,
    pub unit_names: Vec<Vec<String>>,
    pub dimension_names: Vec<String>,
}

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
                if let PrefixParserResult::UnitIdentifier(
                    _definition_span,
                    prefix,
                    unit_name,
                    full_name,
                ) = self.prefix_parser.parse(&identifier)
                {
                    Expression::UnitIdentifier(span, prefix, unit_name, full_name)
                } else {
                    Expression::Identifier(span, identifier)
                }
            }
            Expression::UnitIdentifier(_, _, _, _) => {
                unreachable!("Prefixed identifiers should not exist prior to this stage")
            }
            Expression::UnaryOperator { op, expr, span_op } => Expression::UnaryOperator {
                op,
                expr: Box::new(self.transform_expression(*expr)),
                span_op,
            },
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
            Expression::FunctionCall(span, full_span, name, args) => Expression::FunctionCall(
                span,
                full_span,
                name,
                args.into_iter()
                    .map(|arg| self.transform_expression(arg))
                    .collect(),
            ),
            expr @ Expression::Boolean(_, _) => expr,
            Expression::Condition(span, condition, then, else_) => Expression::Condition(
                span,
                Box::new(self.transform_expression(*condition)),
                Box::new(self.transform_expression(*then)),
                Box::new(self.transform_expression(*else_)),
            ),
            Expression::String(span, parts) => Expression::String(
                span,
                parts
                    .into_iter()
                    .map(|p| match p {
                        f @ StringPart::Fixed(_) => f,
                        StringPart::Interpolation {
                            span,
                            expr,
                            format_specifiers,
                        } => StringPart::Interpolation {
                            span,
                            expr: Box::new(self.transform_expression(*expr)),
                            format_specifiers,
                        },
                    })
                    .collect(),
            ),
            Expression::InstantiateStruct {
                full_span,
                ident_span,
                name,
                fields,
            } => Expression::InstantiateStruct {
                full_span,
                ident_span,
                name,
                fields: fields
                    .into_iter()
                    .map(|(span, attr, arg)| (span, attr, self.transform_expression(arg)))
                    .collect(),
            },
            Expression::AccessField(full_span, ident_span, expr, attr) => Expression::AccessField(
                full_span,
                ident_span,
                Box::new(self.transform_expression(*expr)),
                attr,
            ),
            Expression::List(span, elements) => Expression::List(
                span,
                elements
                    .into_iter()
                    .map(|e| self.transform_expression(e))
                    .collect(),
            ),
            hole @ Expression::TypedHole(_) => hole,
        }
    }

    fn has_decorator(decorators: &[Decorator], decorator: Decorator) -> bool {
        decorators.iter().any(|d| d == &decorator)
    }

    pub(crate) fn register_name_and_aliases(
        &mut self,
        name: &String,
        decorators: &[Decorator],
        conflict_span: Span,
    ) -> Result<()> {
        let mut unit_names = vec![];
        let metric_prefixes = Self::has_decorator(decorators, Decorator::MetricPrefixes);
        let binary_prefixes = Self::has_decorator(decorators, Decorator::BinaryPrefixes);
        for (alias, accepts_prefix) in decorator::name_and_aliases(name, decorators) {
            self.prefix_parser.add_unit(
                alias,
                accepts_prefix,
                metric_prefixes,
                binary_prefixes,
                name,
                conflict_span,
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
            Statement::DefineBaseUnit(span, name, dexpr, decorators) => {
                self.register_name_and_aliases(&name, &decorators, span)?;
                Statement::DefineBaseUnit(span, name, dexpr, decorators)
            }
            Statement::DefineDerivedUnit {
                identifier_span,
                identifier,
                expr,
                type_annotation_span,
                type_annotation,
                decorators,
            } => {
                self.register_name_and_aliases(&identifier, &decorators, identifier_span)?;
                Statement::DefineDerivedUnit {
                    identifier_span,
                    identifier,
                    expr: self.transform_expression(expr),
                    type_annotation_span,
                    type_annotation,
                    decorators,
                }
            }
            Statement::DefineVariable {
                identifier_span,
                identifier,
                expr,
                type_annotation,
                decorators,
            } => {
                for (name, _) in decorator::name_and_aliases(&identifier, &decorators) {
                    self.variable_names.push(name.clone());
                }
                self.prefix_parser
                    .add_other_identifier(&identifier, identifier_span)?;
                Statement::DefineVariable {
                    identifier_span,
                    identifier,
                    expr: self.transform_expression(expr),
                    type_annotation,
                    decorators,
                }
            }
            Statement::DefineFunction {
                function_name_span,
                function_name,
                type_parameters,
                parameters,
                body,
                return_type_annotation,
                decorators,
            } => {
                self.function_names.push(function_name.clone());
                self.prefix_parser
                    .add_other_identifier(&function_name, function_name_span)?;

                // We create a clone of the full transformer for the purpose
                // of checking/transforming the function body. The reason for this
                // is that we don't want the parameter names to pollute the global
                // namespace. But we need to register parameter names as identifiers
                // because they could otherwise shadow global identifiers:
                //
                //   fn foo(t: Time) -> Time = t    # not okay: shadows 't' for ton
                //
                let mut fn_body_transformer = self.clone();
                for (param_span, param, _) in &parameters {
                    fn_body_transformer
                        .prefix_parser
                        .add_other_identifier(param, *param_span)?;
                }

                Statement::DefineFunction {
                    function_name_span,
                    function_name,
                    type_parameters,
                    parameters,
                    body: body.map(|expr| self.transform_expression(expr)),
                    return_type_annotation,
                    decorators,
                }
            }
            Statement::DefineStruct {
                struct_name_span,
                struct_name,
                type_parameters,
                fields,
            } => Statement::DefineStruct {
                struct_name_span,
                struct_name,
                type_parameters,
                fields,
            },
            Statement::DefineDimension(name_span, name, dexprs) => {
                self.dimension_names.push(name.clone());
                Statement::DefineDimension(name_span, name, dexprs)
            }
            Statement::ProcedureCall(span, procedure, args) => Statement::ProcedureCall(
                span,
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
