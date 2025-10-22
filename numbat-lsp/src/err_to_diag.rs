use numbat::{
    pretty_print::PrettyPrint,
    span::Span,
    typechecker::{IncompatibleDimensionsError, TypeCheckError},
};
use tower_lsp_server::lsp_types::*;

use crate::{backend::span_to_range, file_mapper::FileMapping};

impl FileMapping {
    pub fn name_resolution_error_to_diag(
        &self,
        self_content: &str,
        error: &numbat::NameResolutionError,
    ) -> Vec<Diagnostic> {
        match error {
            numbat::NameResolutionError::IdentifierClash {
                conflicting_identifier,
                conflict_span,
                original_span,
                original_item_type: _,
            } => {
                let mut diags = Vec::new();

                let related_information =
                    if let Some(uri) = self.cs_id_to_uri.get(&original_span.code_source_id) {
                        let file = self.files.get(uri).unwrap();
                        Some(vec![DiagnosticRelatedInformation {
                            location: Location {
                                uri: uri.clone(),
                                range: span_to_range(*original_span, &file.content),
                            },
                            message: String::from("Already used here"),
                        }])
                    } else {
                        None
                    };
                diags.push(Diagnostic {
                    range: span_to_range(*conflict_span, self_content),
                    severity: Some(DiagnosticSeverity::ERROR),
                    code: None,
                    code_description: None,
                    source: Some("Name resolution".to_string()),
                    message: format!("Identifier {conflicting_identifier} already in use"),
                    related_information,
                    tags: None,
                    data: None,
                });
                diags
            }
            numbat::NameResolutionError::ReservedIdentifier(span) => vec![Diagnostic {
                range: span_to_range(*span, self_content),
                severity: Some(DiagnosticSeverity::ERROR),
                code: None,
                code_description: None,
                source: Some("Name resolution".to_string()),
                message: String::from("Reserved identifier"),
                related_information: None,
                tags: None,
                data: None,
            }],
        }
    }

    pub fn type_check_error_to_diag(
        &self,
        self_content: &str,
        error: &TypeCheckError,
    ) -> Vec<Diagnostic> {
        let helper_msg = |span: &Span, sev: DiagnosticSeverity, msg: String| Diagnostic {
            range: span_to_range(*span, self_content),
            severity: Some(sev),
            code: None,
            code_description: None,
            source: Some("Typechecker".to_string()),
            message: msg,
            related_information: None,
            tags: None,
            data: None,
        };
        let helper_only_span =
            |span: &Span| helper_msg(span, DiagnosticSeverity::ERROR, error.to_string());

        let mut diags = Vec::new();
        match error {
            TypeCheckError::UnsupportedConstEvalExpression(span, _)
            | TypeCheckError::TypeParameterNameClash(span, _)
            | TypeCheckError::ForeignFunctionNeedsTypeAnnotations(span, _)
            | TypeCheckError::UnknownForeignFunction(span, _)
            | TypeCheckError::NonRationalExponent(span)
            | TypeCheckError::OverflowInConstExpr(span)
            | TypeCheckError::ExpectedDimensionType(span, _)
            | TypeCheckError::ExpectedBool(span)
            | TypeCheckError::MissingDimension(span, _)
            | TypeCheckError::NoFunctionReferenceToGenericFunction(span)
            | TypeCheckError::OnlyFunctionsAndReferencesCanBeCalled(span)
            | TypeCheckError::UnknownStruct(span, _)
            | TypeCheckError::MissingDimBound(span)
            | TypeCheckError::ExponentiationNeedsTypeAnnotation(span)
            | TypeCheckError::DerivedUnitDefinitionMustNotBeGeneric(span)
            | TypeCheckError::MultipleTypedHoles(span)
            | TypeCheckError::DivisionByZeroInConstEvalExpression(span) => {
                diags.push(helper_only_span(span))
            }
            TypeCheckError::UnknownIdentifier(span, _, suggestion) => {
                diags.push(helper_msg(
                    span,
                    DiagnosticSeverity::ERROR,
                    "Unknown identifier".to_string(),
                ));
                if let Some(suggestion) = suggestion {
                    diags.push(helper_msg(
                        span,
                        DiagnosticSeverity::HINT,
                        format!("Did you mean '{suggestion}'?"),
                    ));
                }
            }
            TypeCheckError::IncompatibleDimensions(IncompatibleDimensionsError {
                span_operation,
                operation,
                span_expected,
                expected_type,
                expected_dimensions,
                span_actual,
                actual_type,
                actual_dimensions,
                ..
            }) => {
                let expected_type = if expected_dimensions.is_empty() {
                    format!("{expected_type}")
                } else {
                    expected_dimensions.join(" or ")
                };
                let actual_type = if actual_dimensions.is_empty() {
                    format!("{actual_type}")
                } else {
                    actual_dimensions.join(" or ")
                };

                if span_expected.code_source_id == span_operation.code_source_id {
                    diags.push(helper_msg(
                        span_expected,
                        DiagnosticSeverity::WARNING,
                        expected_type,
                    ));
                }
                if span_actual.code_source_id == span_operation.code_source_id {
                    diags.push(helper_msg(
                        span_actual,
                        DiagnosticSeverity::WARNING,
                        actual_type,
                    ));
                }
                diags.push(helper_msg(
                    span_operation,
                    DiagnosticSeverity::HINT,
                    format!("incompatible dimensions in {operation}"),
                ));

                diags.push(helper_only_span(span_operation))
            }
            TypeCheckError::NonScalarExponent(span, type_)
            | TypeCheckError::NonScalarFactorialArgument(span, type_) => {
                diags.push(helper_only_span(span));
                diags.push(helper_msg(
                    span,
                    DiagnosticSeverity::HINT,
                    format!("{type_}"),
                ));
            }
            // TODO: We should have a span
            TypeCheckError::RegistryError(registry_error) => match registry_error {
                numbat::RegistryError::EntryExists(_) => {
                    diags.push(helper_only_span(&Span::dummy()))
                }
                numbat::RegistryError::UnknownEntry(name, suggestion) => diags.push(helper_msg(
                    &Span::dummy(),
                    DiagnosticSeverity::ERROR,
                    format!(
                        "Unknown dimension '{name}'{maybe_suggestion}",
                        maybe_suggestion = if let Some(suggestion) = suggestion {
                            format!(" did you mean '{suggestion}'?")
                        } else {
                            "".into()
                        }
                    ),
                )),
            },
            TypeCheckError::IncompatibleAlternativeDimensionExpression(
                _name,
                span1,
                type1,
                span2,
                type2,
            ) => {
                diags.push(helper_only_span(&span1.extend(span2)));
                diags.push(helper_msg(
                    span1,
                    DiagnosticSeverity::WARNING,
                    type1.to_string(),
                ));
                diags.push(helper_msg(
                    span2,
                    DiagnosticSeverity::WARNING,
                    type2.to_string(),
                ));
            }
            TypeCheckError::WrongArity {
                callable_span,
                callable_name: _,
                callable_definition_span,
                arity,
                num_args,
            } => {
                diags.push(helper_msg(
                    callable_span,
                    DiagnosticSeverity::ERROR,
                    format!(
                        "{what}was called with {num}, but takes {range}",
                        what = if callable_definition_span.is_some() {
                            ""
                        } else {
                            "procedure or function object "
                        },
                        num = if *num_args == 1 {
                            "one argument".into()
                        } else {
                            format!("{num_args} arguments")
                        },
                        range = if arity.start() == arity.end() {
                            format!("{}", arity.start())
                        } else {
                            format!("{} to {}", arity.start(), arity.end())
                        }
                    ),
                ));
                if let Some(span) = callable_definition_span {
                    if span.code_source_id == callable_span.code_source_id {
                        diags.push(helper_msg(
                            span,
                            DiagnosticSeverity::HINT,
                            String::from("The function defined here"),
                        ));
                    }
                    // TODO: display the hint in the other file
                }
            }
            TypeCheckError::IncompatibleTypesInCondition(
                if_span,
                then_ty,
                then_span,
                else_ty,
                else_span,
            ) => {
                diags.push(helper_msg(
                    if_span,
                    DiagnosticSeverity::ERROR,
                    format!("Incompatible types `{then_ty}` in 'then' and `{else_ty}` in 'else' branches of conditional"),
                ));
                diags.push(helper_msg(
                    then_span,
                    DiagnosticSeverity::WARNING,
                    then_ty.to_string(),
                ));
                diags.push(helper_msg(
                    else_span,
                    DiagnosticSeverity::WARNING,
                    else_ty.to_string(),
                ));
            }
            TypeCheckError::IncompatibleTypesInComparison(
                op_span,
                lhs_ty,
                lhs_span,
                rhs_ty,
                rhs_span,
            ) => {
                diags.push(helper_msg(
                    op_span,
                    DiagnosticSeverity::ERROR,
                    format!("Incompatible types `{lhs_ty}` and `{rhs_ty}` in comparison operator"),
                ));
                diags.push(helper_msg(
                    lhs_span,
                    DiagnosticSeverity::WARNING,
                    lhs_ty.to_string(),
                ));
                diags.push(helper_msg(
                    rhs_span,
                    DiagnosticSeverity::WARNING,
                    rhs_ty.to_string(),
                ));
            }
            TypeCheckError::IncompatibleTypeInAssert(procedure_span, type_, type_span) => {
                diags.push(helper_msg(
                    procedure_span,
                    DiagnosticSeverity::ERROR,
                    format!("Non-boolean type `{type_}` in 'assert' call"),
                ));
                diags.push(helper_msg(
                    type_span,
                    DiagnosticSeverity::HINT,
                    type_.to_string(),
                ));
            }
            TypeCheckError::IncompatibleTypesInAssertEq(
                procedure_span,
                first_type,
                first_span,
                second_type,
                second_span,
            ) => {
                diags.push(helper_msg(
                    procedure_span,
                    DiagnosticSeverity::ERROR,
                    format!(
                        "Incompatible types `{first_type}` and `{second_type}` in 'assert_eq' call"
                    ),
                ));
                diags.push(helper_msg(
                    first_span,
                    DiagnosticSeverity::WARNING,
                    first_type.to_string(),
                ));
                diags.push(helper_msg(
                    second_span,
                    DiagnosticSeverity::WARNING,
                    second_type.to_string(),
                ));
            }

            TypeCheckError::IncompatibleTypesInAnnotation(
                what,
                what_span,
                annotation,
                annotation_span,
                deduced_type,
                body_span,
            ) => {
                diags.push(helper_msg(
                    what_span,
                    DiagnosticSeverity::ERROR,
                    format!("Incompatible types in {what}"),
                ));
                diags.push(helper_msg(
                    annotation_span,
                    DiagnosticSeverity::ERROR,
                    annotation.to_string(),
                ));
                diags.push(helper_msg(
                    body_span,
                    DiagnosticSeverity::ERROR,
                    deduced_type.to_string(),
                ));
            }
            TypeCheckError::IncompatibleTypesInFunctionCall(
                parameter_span,
                parameter_type,
                argument_span,
                _argument_type,
            ) => {
                diags.push(helper_only_span(argument_span));

                if let Some(parameter_span) = parameter_span {
                    // TODO: We should display the hint even when it's not in the same file
                    if parameter_span.code_source_id == argument_span.code_source_id {
                        diags.push(helper_msg(
                            parameter_span,
                            DiagnosticSeverity::HINT,
                            parameter_type.to_string(),
                        ));
                    }
                }
            }
            TypeCheckError::IncompatibleTypesInList(
                span_first,
                type_first,
                span_subsequent,
                _type_subsequent,
            ) => {
                diags.push(helper_only_span(span_subsequent));
                diags.push(helper_msg(
                    span_first,
                    DiagnosticSeverity::WARNING,
                    type_first.to_string(),
                ));
            }
            TypeCheckError::NoDimensionlessBaseUnit(span, unit_name) => {
                diags.push(helper_msg(
                    span,
                    DiagnosticSeverity::ERROR,
                    format!(
                        "{error}\
                        Use 'unit {unit_name}' for ad-hoc units.\
                        Use 'unit {unit_name}: Scalar = …' for derived units."
                    ),
                ));
            }
            TypeCheckError::IncompatibleTypesInOperator(
                span,
                op,
                lhs_type,
                lhs_span,
                rhs_type,
                rhs_span,
            ) => {
                diags.push(helper_msg(
                    span,
                    DiagnosticSeverity::ERROR,
                    format!(
                        "Operator {} can not be applied to these types",
                        op.pretty_print()
                    ),
                ));
                diags.push(helper_msg(
                    lhs_span,
                    DiagnosticSeverity::WARNING,
                    lhs_type.to_string(),
                ));
                diags.push(helper_msg(
                    rhs_span,
                    DiagnosticSeverity::WARNING,
                    rhs_type.to_string(),
                ));
            }
            TypeCheckError::DuplicateFieldInStructDefinition(
                this_field_span,
                that_field_span,
                _attr_name,
            )
            | TypeCheckError::DuplicateFieldInStructInstantiation(
                this_field_span,
                that_field_span,
                _attr_name,
            ) => {
                diags.push(helper_only_span(this_field_span));
                diags.push(helper_msg(
                    that_field_span,
                    DiagnosticSeverity::WARNING,
                    "Already defined here".to_string(),
                ));
            }
            TypeCheckError::FieldAccessOfNonStructType(ident_span, expr_span, _attr, type_) => {
                diags.push(helper_only_span(ident_span));
                diags.push(helper_msg(
                    expr_span,
                    DiagnosticSeverity::WARNING,
                    type_.to_string(),
                ));
            }
            TypeCheckError::UnknownFieldAccess(ident_span, expr_span, _attr, type_) => {
                diags.push(helper_only_span(ident_span));
                diags.push(helper_msg(
                    expr_span,
                    DiagnosticSeverity::WARNING,
                    type_.to_string(),
                ));
            }
            TypeCheckError::IncompatibleTypesForStructField(
                expected_field_span,
                _expected_type,
                expr_span,
                _found_type,
            ) => {
                diags.push(helper_only_span(expr_span));
                // TODO: Support multi-file diag
                if expected_field_span.code_source_id == expr_span.code_source_id {
                    diags.push(helper_msg(
                        expected_field_span,
                        DiagnosticSeverity::HINT,
                        "Defined here".to_string(),
                    ));
                }
            }
            TypeCheckError::UnknownFieldInStructInstantiation(field_span, defn_span, _, _) => {
                diags.push(helper_only_span(field_span));
                // TODO: Support multi-file diag
                if defn_span.code_source_id == field_span.code_source_id {
                    diags.push(helper_msg(
                        defn_span,
                        DiagnosticSeverity::HINT,
                        "Struct defined here".to_string(),
                    ));
                }
            }
            TypeCheckError::MissingFieldsInStructInstantiation(
                construction_span,
                defn_span,
                missing,
            ) => {
                diags.push(helper_msg(
                    construction_span,
                    DiagnosticSeverity::ERROR,
                    format!("{error}\nMissing fields: {missing:?}"),
                ));
                // TODO: Support multi-file diag
                if defn_span.code_source_id == construction_span.code_source_id {
                    diags.push(helper_msg(
                        defn_span,
                        DiagnosticSeverity::HINT,
                        "Struct defined here".to_string(),
                    ));
                }
            }
            TypeCheckError::NameResolutionError(name_resolution_error) => {
                match name_resolution_error {
                    numbat::NameResolutionError::IdentifierClash {
                        conflicting_identifier: _,
                        conflict_span,
                        original_span,
                        original_item_type,
                    } => {
                        diags.push(helper_msg(
                            conflict_span,
                            DiagnosticSeverity::ERROR,
                            "identifier is already in use".to_string(),
                        ));
                        diags.push(helper_msg(
                            original_span,
                            DiagnosticSeverity::HINT,
                            if let Some(t) = original_item_type.as_ref() {
                                format!("Previously defined {t} here")
                            } else {
                                "Previously defined here".to_owned()
                            },
                        ));
                    }

                    numbat::NameResolutionError::ReservedIdentifier(span) => {
                        diags.push(helper_msg(
                            span,
                            DiagnosticSeverity::ERROR,
                            "reserved identifier may not be used".to_string(),
                        ));
                    }
                }
            }
            // TODO: get a span out of these errors
            TypeCheckError::ConstraintSolverError(_, _)
            | TypeCheckError::SubstitutionError(_, _) => {
                diags.push(helper_msg(
                    &Span::dummy(),
                    DiagnosticSeverity::ERROR,
                    "Consider adding type annotations to get more precise error messages.".into(),
                ));
            }
            TypeCheckError::TypedHoleInStatement(span, type_, _statement, matches) => {
                let mut notes = format!("Hole of type '{type_}'");

                if !matches.is_empty() {
                    notes.push_str("\nRelevant matches for this hole include:");
                    notes.push_str(&format!(" {}", matches.join(", ")));
                }
                diags.push(helper_msg(span, DiagnosticSeverity::ERROR, notes));
            }
        }
        diags
    }
}
