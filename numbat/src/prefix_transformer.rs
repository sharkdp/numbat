use compact_str::{CompactString, ToCompactString};

use crate::{
    ast::{DefineVariable, Expression, Statement, StringPart},
    decorator::{self, Decorator},
    name_resolution::NameResolutionError,
    prefix_parser::{AliasSpanInfo, PrefixParser, PrefixParserResult},
    span::Span,
};

type Result<T> = std::result::Result<T, NameResolutionError>;

#[derive(Debug, Clone)]
pub(crate) struct Transformer {
    pub prefix_parser: PrefixParser,

    pub variable_names: Vec<CompactString>,
    pub function_names: Vec<CompactString>,
    pub unit_names: Vec<Vec<CompactString>>,
    pub dimension_names: Vec<CompactString>,
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

            Expression::BinaryOperator { lhs, rhs, .. } => {
                self.transform_expression(lhs);
                self.transform_expression(rhs);
            }
            Expression::FunctionCall(_, _, _, args) => {
                for arg in args {
                    self.transform_expression(arg);
                }
            }
            Expression::Condition(_, condition, then_expr, else_expr) => {
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
            unit_names.push(alias.to_compact_string());
        }

        unit_names.sort();
        self.unit_names.push(unit_names);

        Ok(())
    }

    fn transform_define_variable(&mut self, define_variable: &mut DefineVariable) -> Result<()> {
        let DefineVariable {
            identifier_span,
            identifier,
            expr,
            type_annotation: _,
            decorators,
        } = define_variable;

        for (name, _) in decorator::name_and_aliases(identifier, decorators) {
            self.variable_names.push(name.to_compact_string());
        }
        self.prefix_parser
            .add_other_identifier(identifier, *identifier_span)?;
        self.transform_expression(expr);

        Ok(())
    }

    fn transform_statement(&mut self, statement: &mut Statement) -> Result<()> {
        match statement {
            Statement::DefineStruct { .. } | Statement::ModuleImport(_, _) => {}

            Statement::Expression(expr) => {
                self.transform_expression(expr);
            }
            Statement::DefineBaseUnit(span, name, _, decorators) => {
                self.register_name_and_aliases(name, *span, decorators)?;
            }
            Statement::DefineDerivedUnit {
                identifier_span,
                identifier,
                expr,
                decorators,
                ..
            } => {
                self.register_name_and_aliases(identifier, *identifier_span, decorators)?;
                self.transform_expression(expr);
            }
            Statement::DefineVariable(define_variable) => {
                self.transform_define_variable(define_variable)?
            }
            Statement::DefineFunction {
                function_name_span,
                function_name,
                parameters,
                body,
                local_variables,
                ..
            } => {
                self.function_names.push(function_name.to_compact_string());
                self.prefix_parser
                    .add_other_identifier(function_name, *function_name_span)?;

                // We create a clone of the full transformer for the purpose
                // of checking/transforming the function body. The reason for this
                // is that we don't want the parameter names to pollute the global
                // namespace. But we need to register parameter names as identifiers
                // because they could otherwise shadow global identifiers:
                //
                //   fn foo(t: Time) -> Time = t    # not okay: shadows 't' for ton
                //
                let mut fn_body_transformer = self.clone();
                for (param_span, param, _) in &*parameters {
                    fn_body_transformer
                        .prefix_parser
                        .add_other_identifier(param, *param_span)?;
                }

                if let Some(expr) = body {
                    self.transform_expression(expr);
                }

                for def in local_variables {
                    self.transform_define_variable(def)?;
                }
            }
            Statement::DefineDimension(_, name, _) => {
                self.dimension_names.push(name.to_compact_string());
            }
            Statement::ProcedureCall(_, _, args) => {
                for arg in args {
                    self.transform_expression(arg);
                }
            }
        }

        Ok(())
    }

    pub fn transform<'a>(
        &mut self,
        statements: impl IntoIterator<Item = Statement<'a>>,
    ) -> Result<Vec<Statement<'a>>> {
        statements
            .into_iter()
            .map(|mut statement| {
                self.transform_statement(&mut statement)?;
                Ok(statement)
            })
            .collect()
    }
}
