use crate::{
    ast::{BinOp, Condition, DefineVariable, Expression, Statement, StringPart},
    decorator::{self, Decorator},
    name_resolution::NameResolutionError,
    prefix_parser::{AliasSpanInfo, PrefixParser, PrefixParserResult},
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

    fn transform_expression(&self, expression: &mut Expression) {
        match expression {
            Expression::Scalar(..) | Expression::Boolean(_, _) | Expression::TypedHole(_) => {}
            Expression::Identifier(span, identifier) => {
                if let PrefixParserResult::UnitIdentifier(
                    _definition_span,
                    prefix,
                    unit_name,
                    full_name,
                ) = self.prefix_parser.parse(identifier)
                {
                    *expression = Expression::UnitIdentifier(*span, prefix, unit_name, full_name);
                } else {
                    *expression = Expression::Identifier(*span, identifier);
                }
            }
            Expression::UnitIdentifier(_, _, _, _) => {
                unreachable!("Prefixed identifiers should not exist prior to this stage")
            }
            Expression::UnaryOperator { expr, .. } => self.transform_expression(expr),

            Expression::BinaryOperator { bin_op, .. } => {
                let BinOp { lhs, rhs } = bin_op.as_mut();
                self.transform_expression(lhs);
                self.transform_expression(rhs);
            }
            Expression::FunctionCall(_, _, _, args) => {
                for arg in args {
                    self.transform_expression(arg);
                }
            }
            Expression::Condition(_, cond) => {
                let Condition {
                    condition,
                    then_expr,
                    else_expr,
                } = cond.as_mut();
                self.transform_expression(condition);
                self.transform_expression(then_expr);
                self.transform_expression(else_expr);
            }
            Expression::String(_, parts) => {
                for p in parts {
                    match p {
                        StringPart::Fixed(_) => {}
                        StringPart::Interpolation { expr, .. } => self.transform_expression(expr),
                    }
                }
            }
            Expression::InstantiateStruct { fields, .. } => {
                for (_, _, arg) in fields {
                    self.transform_expression(arg);
                }
            }
            Expression::AccessField(_, _, expr, _) => {
                self.transform_expression(expr);
            }
            Expression::List(_, elements) => {
                for e in elements {
                    self.transform_expression(e);
                }
            }
        }
    }

    fn has_decorator(decorators: &[Decorator], decorator: Decorator) -> bool {
        decorators.iter().any(|d| d == &decorator)
    }

    pub(crate) fn register_name_and_aliases(
        &mut self,
        name: &str,
        name_span: Span,
        decorators: &[Decorator],
    ) -> Result<()> {
        let mut unit_names = vec![];
        let metric_prefixes = Self::has_decorator(decorators, Decorator::MetricPrefixes);
        let binary_prefixes = Self::has_decorator(decorators, Decorator::BinaryPrefixes);

        for (alias, accepts_prefix, alias_span) in
            decorator::name_and_aliases_spans(name, name_span, decorators)
        {
            self.prefix_parser.add_unit(
                alias,
                accepts_prefix,
                metric_prefixes,
                binary_prefixes,
                name,
                AliasSpanInfo {
                    name_span,
                    alias_span,
                },
            )?;
            unit_names.push(alias.to_string());
        }

        unit_names.sort();
        self.unit_names.push(unit_names);

        Ok(())
    }

    fn transform_define_variable<'a>(
        &mut self,
        define_variable: DefineVariable<'a>,
    ) -> Result<DefineVariable<'a>> {
        let DefineVariable {
            identifier_span,
            identifier,
            mut expr,
            type_annotation,
            decorators,
        } = define_variable;

        for (name, _) in decorator::name_and_aliases(identifier, &decorators) {
            self.variable_names.push(name.to_owned());
        }
        self.prefix_parser
            .add_other_identifier(identifier, identifier_span)?;
        self.transform_expression(&mut expr);

        Ok(DefineVariable {
            identifier_span,
            identifier,
            expr,
            type_annotation,
            decorators,
        })
    }

    fn transform_statement<'a>(&mut self, statement: Statement<'a>) -> Result<Statement<'a>> {
        Ok(match statement {
            Statement::Expression(mut expr) => {
                self.transform_expression(&mut expr);
                Statement::Expression(expr)
            }
            Statement::DefineBaseUnit(span, name, dexpr, decorators) => {
                self.register_name_and_aliases(name, span, &decorators)?;
                Statement::DefineBaseUnit(span, name, dexpr, decorators)
            }
            Statement::DefineDerivedUnit {
                identifier_span,
                identifier,
                mut expr,
                type_annotation_span,
                type_annotation,
                decorators,
            } => {
                self.register_name_and_aliases(identifier, identifier_span, &decorators)?;
                self.transform_expression(&mut expr);
                Statement::DefineDerivedUnit {
                    identifier_span,
                    identifier,
                    expr,
                    type_annotation_span,
                    type_annotation,
                    decorators,
                }
            }
            Statement::DefineVariable(define_variable) => {
                Statement::DefineVariable(self.transform_define_variable(define_variable)?)
            }
            Statement::DefineFunction {
                function_name_span,
                function_name,
                type_parameters,
                parameters,
                mut body,
                local_variables,
                return_type_annotation,
                decorators,
            } => {
                self.function_names.push(function_name.to_owned());
                self.prefix_parser
                    .add_other_identifier(function_name, function_name_span)?;

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

                if let Some(expr) = &mut body {
                    self.transform_expression(expr);
                }

                Statement::DefineFunction {
                    function_name_span,
                    function_name,
                    type_parameters,
                    parameters,
                    body,
                    local_variables: local_variables
                        .into_iter()
                        .map(|def| self.transform_define_variable(def))
                        .collect::<Result<_>>()?,
                    return_type_annotation,
                    decorators,
                }
            }
            Statement::DefineStruct {
                struct_name_span,
                struct_name,
                fields,
            } => Statement::DefineStruct {
                struct_name_span,
                struct_name,
                fields,
            },
            Statement::DefineDimension(name_span, name, dexprs) => {
                self.dimension_names.push(name.to_owned());
                Statement::DefineDimension(name_span, name, dexprs)
            }
            Statement::ProcedureCall(span, procedure, mut args) => {
                for arg in &mut args {
                    self.transform_expression(arg);
                }
                Statement::ProcedureCall(span, procedure, args)
            }
            statement @ Statement::ModuleImport(_, _) => statement,
        })
    }

    pub fn transform<'a>(
        &mut self,
        statements: impl IntoIterator<Item = Statement<'a>>,
    ) -> Result<Vec<Statement<'a>>> {
        statements
            .into_iter()
            .map(|statement| self.transform_statement(statement))
            .collect()
    }
}
