use codespan_reporting::diagnostic::LabelStyle;

use crate::{
    NameResolutionError,
    interpreter::{RuntimeError, RuntimeErrorKind},
    parser::ParseError,
    pretty_print::PrettyPrint,
    resolver::{Resolver, ResolverError},
    span::Span,
    typechecker::{IncompatibleDimensionsError, TypeCheckError},
};

pub type Diagnostic = codespan_reporting::diagnostic::Diagnostic<usize>;

pub trait ErrorDiagnostic {
    fn diagnostics(&self) -> Vec<Diagnostic>;
}

impl ErrorDiagnostic for ParseError {
    fn diagnostics(&self) -> Vec<Diagnostic> {
        vec![
            Diagnostic::error()
                .with_message("while parsing")
                .with_labels(vec![
                    self.span
                        .diagnostic_label(LabelStyle::Primary)
                        .with_message(self.kind.to_string()),
                ]),
        ]
    }
}

impl ErrorDiagnostic for ResolverError {
    fn diagnostics(&self) -> Vec<Diagnostic> {
        match self {
            ResolverError::UnknownModule(span, _) => vec![
                Diagnostic::error()
                    .with_message("while resolving imports in")
                    .with_labels(vec![
                        span.diagnostic_label(LabelStyle::Primary)
                            .with_message("Unknown module"),
                    ]),
            ],
            ResolverError::ParseErrors(errors) => {
                errors.iter().flat_map(|e| e.diagnostics()).collect()
            }
        }
    }
}

impl ErrorDiagnostic for NameResolutionError {
    fn diagnostics(&self) -> Vec<Diagnostic> {
        match self {
            NameResolutionError::IdentifierClash {
                conflicting_identifier: _,
                original_item_type,
                conflict_span,
                original_span,
            } => vec![
                Diagnostic::error()
                    .with_message("identifier clash in definition")
                    .with_labels(vec![
                        original_span
                            .diagnostic_label(LabelStyle::Secondary)
                            .with_message(if let Some(t) = original_item_type.as_ref() {
                                format!("Previously defined {t} here")
                            } else {
                                "Previously defined here".to_owned()
                            }),
                        conflict_span
                            .diagnostic_label(LabelStyle::Primary)
                            .with_message("identifier is already in use"),
                    ]),
            ],
            NameResolutionError::ReservedIdentifier(span) => vec![
                Diagnostic::error()
                    .with_message("reserved identifier may not be used")
                    .with_labels(vec![
                        span.diagnostic_label(LabelStyle::Primary)
                            .with_message("reserved identifier"),
                    ]),
            ],
        }
    }
}

impl ErrorDiagnostic for TypeCheckError {
    fn diagnostics(&self) -> Vec<Diagnostic> {
        let d = Diagnostic::error().with_message("while type checking");
        let inner_error = format!("{self}");

        let d = match self {
            TypeCheckError::UnknownIdentifier(span, _, suggestion) => {
                let notes = if let Some(suggestion) = suggestion {
                    vec![format!("Did you mean '{suggestion}'?")]
                } else {
                    vec![]
                };
                d.with_labels(vec![
                    span.diagnostic_label(LabelStyle::Primary)
                        .with_message("unknown identifier"),
                ])
                .with_notes(notes)
            }
            TypeCheckError::IncompatibleDimensions(IncompatibleDimensionsError {
                operation,
                span_operation,
                span_actual,
                actual_type,
                actual_dimensions,
                span_expected,
                expected_type,
                expected_dimensions,
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

                let labels = vec![
                    span_expected
                        .diagnostic_label(LabelStyle::Primary)
                        .with_message(expected_type),
                    span_actual
                        .diagnostic_label(LabelStyle::Primary)
                        .with_message(actual_type),
                    span_operation
                        .diagnostic_label(LabelStyle::Secondary)
                        .with_message(format!("incompatible dimensions in {operation}")),
                ];
                d.with_labels(labels).with_notes(vec![inner_error])
            }
            TypeCheckError::NonScalarExponent(span, type_)
            | TypeCheckError::NonScalarFactorialArgument(span, type_) => d
                .with_labels(vec![
                    span.diagnostic_label(LabelStyle::Primary)
                        .with_message(format!("{type_}")),
                ])
                .with_notes(vec![inner_error]),
            TypeCheckError::UnsupportedConstEvalExpression(span, _) => d.with_labels(vec![
                span.diagnostic_label(LabelStyle::Primary)
                    .with_message(inner_error),
            ]),
            TypeCheckError::DivisionByZeroInConstEvalExpression(span) => d.with_labels(vec![
                span.diagnostic_label(LabelStyle::Primary)
                    .with_message(inner_error),
            ]),
            TypeCheckError::RegistryError(re) => match re {
                crate::registry::RegistryError::EntryExists(_) => d.with_notes(vec![inner_error]),
                crate::registry::RegistryError::UnknownEntry(name, suggestion) => {
                    d.with_notes(vec![format!(
                        "Unknown dimension '{name}'{maybe_suggestion}",
                        maybe_suggestion = if let Some(suggestion) = suggestion {
                            format!(" did you mean '{suggestion}'?")
                        } else {
                            "".into()
                        }
                    )])
                }
            },
            TypeCheckError::IncompatibleAlternativeDimensionExpression(
                _name,
                span1,
                type1,
                span2,
                type2,
            ) => d
                .with_labels(vec![
                    span1
                        .diagnostic_label(LabelStyle::Primary)
                        .with_message(type1.to_string()),
                    span2
                        .diagnostic_label(LabelStyle::Primary)
                        .with_message(type2.to_string()),
                ])
                .with_notes(vec![inner_error]),
            TypeCheckError::WrongArity {
                callable_span,
                callable_name: _,
                callable_definition_span,
                arity,
                num_args,
            } => {
                let mut labels = vec![
                    callable_span
                        .diagnostic_label(LabelStyle::Primary)
                        .with_message(format!(
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
                        )),
                ];
                if let Some(span) = callable_definition_span {
                    labels.insert(
                        0,
                        span.diagnostic_label(LabelStyle::Secondary)
                            .with_message("The function defined here"),
                    );
                }

                d.with_labels(labels)
            }
            TypeCheckError::TypeParameterNameClash(span, _) => d.with_labels(vec![
                span.diagnostic_label(LabelStyle::Primary)
                    .with_message(inner_error),
            ]),
            TypeCheckError::IncompatibleTypesInCondition(
                if_span,
                then_type,
                then_span,
                else_type,
                else_span,
            ) => d.with_labels(vec![
                then_span
                    .diagnostic_label(LabelStyle::Secondary)
                    .with_message(then_type.to_string()),
                else_span
                    .diagnostic_label(LabelStyle::Secondary)
                    .with_message(else_type.to_string()),
                if_span.diagnostic_label(LabelStyle::Primary).with_message(
                    "Incompatible types in 'then' and 'else' branches of conditional",
                ),
            ]),
            TypeCheckError::IncompatibleTypesInComparison(
                op_span,
                lhs_type,
                lhs_span,
                rhs_type,
                rhs_span,
            ) => d.with_labels(vec![
                lhs_span
                    .diagnostic_label(LabelStyle::Secondary)
                    .with_message(lhs_type.to_string()),
                rhs_span
                    .diagnostic_label(LabelStyle::Secondary)
                    .with_message(rhs_type.to_string()),
                op_span
                    .diagnostic_label(LabelStyle::Primary)
                    .with_message("Incompatible types in comparison operator"),
            ]),
            TypeCheckError::IncompatibleTypeInAssert(procedure_span, type_, type_span) => d
                .with_labels(vec![
                    type_span
                        .diagnostic_label(LabelStyle::Secondary)
                        .with_message(type_.to_string()),
                    procedure_span
                        .diagnostic_label(LabelStyle::Primary)
                        .with_message("Non-boolean type in 'assert' call"),
                ]),
            TypeCheckError::IncompatibleTypesInAssertEq(
                procedure_span,
                first_type,
                first_span,
                arg_type,
                arg_span,
            ) => d.with_labels(vec![
                first_span
                    .diagnostic_label(LabelStyle::Secondary)
                    .with_message(first_type.to_string()),
                arg_span
                    .diagnostic_label(LabelStyle::Secondary)
                    .with_message(arg_type.to_string()),
                procedure_span
                    .diagnostic_label(LabelStyle::Primary)
                    .with_message("Incompatible types in 'assert_eq' call"),
            ]),
            TypeCheckError::IncompatibleTypesInAnnotation(
                what,
                what_span,
                annotation,
                annotation_span,
                deduced_type,
                body_span,
            ) => d.with_labels(vec![
                annotation_span
                    .diagnostic_label(LabelStyle::Secondary)
                    .with_message(annotation.to_string()),
                body_span
                    .diagnostic_label(LabelStyle::Secondary)
                    .with_message(deduced_type.to_string()),
                what_span
                    .diagnostic_label(LabelStyle::Primary)
                    .with_message(format!("Incompatible types in {what}")),
            ]),
            TypeCheckError::IncompatibleTypesInFunctionCall(
                parameter_span,
                parameter_type,
                argument_span,
                argument_type,
            ) => {
                if let Some(parameter_span) = parameter_span {
                    d.with_labels(vec![
                        parameter_span
                            .diagnostic_label(LabelStyle::Secondary)
                            .with_message(parameter_type.to_string()),
                        argument_span
                            .diagnostic_label(LabelStyle::Primary)
                            .with_message(argument_type.to_string()),
                    ])
                    .with_notes(vec![inner_error])
                } else {
                    d.with_labels(vec![
                        argument_span
                            .diagnostic_label(LabelStyle::Primary)
                            .with_message(argument_type.to_string()),
                    ])
                    .with_notes(vec![inner_error])
                }
            }
            TypeCheckError::IncompatibleTypesInList(
                span_first,
                type_first,
                span_subsequent,
                type_subsequent,
            ) => d
                .with_labels(vec![
                    span_first
                        .diagnostic_label(LabelStyle::Secondary)
                        .with_message(type_first.to_string()),
                    span_subsequent
                        .diagnostic_label(LabelStyle::Primary)
                        .with_message(type_subsequent.to_string()),
                ])
                .with_notes(vec![inner_error]),
            TypeCheckError::NoDimensionlessBaseUnit(span, unit_name) => d
                .with_labels(vec![
                    span.diagnostic_label(LabelStyle::Primary)
                        .with_message(inner_error),
                ])
                .with_notes(vec![
                    format!("Use 'unit {unit_name}' for ad-hoc units."),
                    format!("Use 'unit {unit_name}: Scalar = …' for derived units."),
                ]),
            TypeCheckError::ForeignFunctionNeedsTypeAnnotations(span, _)
            | TypeCheckError::UnknownForeignFunction(span, _)
            | TypeCheckError::NonRationalExponent(span)
            | TypeCheckError::OverflowInConstExpr(span)
            | TypeCheckError::ExpectedDimensionType(span, _)
            | TypeCheckError::ExpectedBool(span)
            | TypeCheckError::NoFunctionReferenceToGenericFunction(span)
            | TypeCheckError::OnlyFunctionsAndReferencesCanBeCalled(span)
            | TypeCheckError::DerivedUnitDefinitionMustNotBeGeneric(span)
            | TypeCheckError::MultipleTypedHoles(span) => d.with_labels(vec![
                span.diagnostic_label(LabelStyle::Primary)
                    .with_message(inner_error),
            ]),
            TypeCheckError::MissingDimension(span, dim) => d
                .with_labels(vec![
                    span.diagnostic_label(LabelStyle::Primary)
                        .with_message(format!("Missing dimension '{dim}'")),
                ])
                .with_notes(vec![format!(
                    "This operation requires the '{dim}' dimension to be defined"
                )]),
            TypeCheckError::IncompatibleTypesInOperator(
                span,
                op,
                lhs_type,
                lhs_span,
                rhs_type,
                rhs_span,
            ) => d.with_labels(vec![
                span.diagnostic_label(LabelStyle::Primary)
                    .with_message(format!(
                        "Operator {} can not be applied to these types",
                        op.pretty_print()
                    )),
                lhs_span
                    .diagnostic_label(LabelStyle::Secondary)
                    .with_message(lhs_type.to_string()),
                rhs_span
                    .diagnostic_label(LabelStyle::Secondary)
                    .with_message(rhs_type.to_string()),
            ]),
            TypeCheckError::DuplicateFieldInStructInstantiation(
                this_field_span,
                that_field_span,
                _attr_name,
            ) => d.with_labels(vec![
                this_field_span
                    .diagnostic_label(LabelStyle::Primary)
                    .with_message(inner_error),
                that_field_span
                    .diagnostic_label(LabelStyle::Secondary)
                    .with_message("Already defined here"),
            ]),
            TypeCheckError::FieldAccessOfNonStructType(ident_span, expr_span, _attr, type_) => d
                .with_labels(vec![
                    ident_span
                        .diagnostic_label(LabelStyle::Primary)
                        .with_message(inner_error),
                    expr_span
                        .diagnostic_label(LabelStyle::Secondary)
                        .with_message(type_.to_string()),
                ]),
            TypeCheckError::UnknownFieldAccess(ident_span, expr_span, _attr, type_) => d
                .with_labels(vec![
                    ident_span
                        .diagnostic_label(LabelStyle::Primary)
                        .with_message(inner_error),
                    expr_span
                        .diagnostic_label(LabelStyle::Secondary)
                        .with_message(type_.to_string()),
                ]),
            TypeCheckError::IncompatibleTypesForStructField(
                expected_field_span,
                _expected_type,
                expr_span,
                _found_type,
            ) => d.with_labels(vec![
                expr_span
                    .diagnostic_label(LabelStyle::Primary)
                    .with_message(inner_error),
                expected_field_span
                    .diagnostic_label(LabelStyle::Secondary)
                    .with_message("Defined here"),
            ]),
            TypeCheckError::UnknownStruct(span, _name) => d.with_labels(vec![
                span.diagnostic_label(LabelStyle::Primary)
                    .with_message(inner_error),
            ]),
            TypeCheckError::UnknownFieldInStructInstantiation(field_span, defn_span, _, _) => d
                .with_labels(vec![
                    field_span
                        .diagnostic_label(LabelStyle::Primary)
                        .with_message(inner_error),
                    defn_span
                        .diagnostic_label(LabelStyle::Secondary)
                        .with_message("Struct defined here"),
                ]),
            TypeCheckError::DuplicateFieldInStructDefinition(
                this_field_span,
                that_field_span,
                _attr_name,
            ) => d.with_labels(vec![
                this_field_span
                    .diagnostic_label(LabelStyle::Primary)
                    .with_message(inner_error),
                that_field_span
                    .diagnostic_label(LabelStyle::Secondary)
                    .with_message("Already defined here"),
            ]),
            TypeCheckError::MissingFieldsInStructInstantiation(
                construction_span,
                defn_span,
                missing,
            ) => d
                .with_labels(vec![
                    construction_span
                        .diagnostic_label(LabelStyle::Primary)
                        .with_message(inner_error),
                    defn_span
                        .diagnostic_label(LabelStyle::Secondary)
                        .with_message("Struct defined here"),
                ])
                .with_notes(vec!["Missing fields: ".to_owned()])
                .with_notes(missing.iter().map(|(n, t)| format!("{n}: {t}")).collect()),
            TypeCheckError::NameResolutionError(inner) => {
                return inner.diagnostics();
            }
            TypeCheckError::ConstraintSolverError(..) | TypeCheckError::SubstitutionError(..) => {
                d.with_message(inner_error).with_notes(vec![
                    "Consider adding type annotations to get more precise error messages.".into(),
                ])
            }
            TypeCheckError::MissingDimBound(span) => d
                .with_labels(vec![
                    span.diagnostic_label(LabelStyle::Primary)
                        .with_message(inner_error),
                ])
                .with_notes(vec![
                    "Consider adding `: Dim` after the type parameter".to_owned(),
                ]),
            TypeCheckError::ExponentiationNeedsTypeAnnotation(span) => d.with_labels(vec![
                span.diagnostic_label(LabelStyle::Primary)
                    .with_message(inner_error),
            ]),
            TypeCheckError::TypedHoleInStatement(span, type_, statement, matches) => {
                let mut notes = vec![
                    format!("Found a hole of type '{type_}' in the statement:"),
                    format!("  {statement}"),
                ];

                if !matches.is_empty() {
                    notes.push("Relevant matches for this hole include:".into());
                    notes.push(format!("  {}", matches.join(", ")));
                }

                d.with_labels(vec![
                    span.diagnostic_label(LabelStyle::Primary)
                        .with_message(type_),
                ])
                .with_message("Found typed hole")
                .with_notes(notes)
            }
        };
        vec![d]
    }
}

/// Little helper to quickly implement diagnostic for types that requires the resolver
/// in order to emit a diagnostic.
pub struct ResolverDiagnostic<'a, E> {
    pub resolver: &'a Resolver,
    pub error: &'a E,
}

impl ErrorDiagnostic for ResolverDiagnostic<'_, RuntimeError> {
    fn diagnostics(&self) -> Vec<Diagnostic> {
        let mut diag = Vec::new();

        let inner = format!("{:#}", self.error.kind);
        match &self.error.kind {
            RuntimeErrorKind::AssertFailed(span) => diag.push(
                Diagnostic::error()
                    .with_message("Assertion failed")
                    .with_labels(vec![
                        span.diagnostic_label(LabelStyle::Primary)
                            .with_message("assertion failed"),
                    ]),
            ),
            RuntimeErrorKind::AssertEq2Failed(assert_eq2_error) => diag.push(
                Diagnostic::error()
                    .with_message("Assertion failed")
                    .with_labels(vec![
                        assert_eq2_error
                            .span_lhs
                            .diagnostic_label(LabelStyle::Secondary)
                            .with_message(format!("{}", assert_eq2_error.lhs)),
                        assert_eq2_error
                            .span_rhs
                            .diagnostic_label(LabelStyle::Primary)
                            .with_message(format!("{}", assert_eq2_error.rhs)),
                    ])
                    .with_notes(vec![inner]),
            ),
            RuntimeErrorKind::AssertEq3Failed(assert_eq3_error) => {
                let (lhs, rhs) = assert_eq3_error.fmt_comparands();

                diag.push(
                    Diagnostic::error()
                        .with_message("Assertion failed")
                        .with_labels(vec![
                            assert_eq3_error
                                .span_lhs
                                .diagnostic_label(LabelStyle::Secondary)
                                .with_message(lhs),
                            assert_eq3_error
                                .span_rhs
                                .diagnostic_label(LabelStyle::Primary)
                                .with_message(rhs),
                        ])
                        .with_notes(vec![inner]),
                )
            }
            _ => diag.push(
                Diagnostic::error()
                    .with_message("runtime error")
                    .with_notes(vec![inner])
                    .with_labels_iter(
                        // We're going to join the three first piece of user code that triggered the error
                        self.error
                            .backtrace
                            .iter()
                            .filter(|(_, span)| {
                                if cfg!(debug_assertions) {
                                    true
                                } else {
                                    let file =
                                        self.resolver.files.get(span.code_source_id).unwrap();
                                    // Everything that starts by prelude was not written by the user and must be ignored
                                    !file.name().contains("<builtin>")
                                }
                            })
                            .take(3)
                            .map(|(_, span)| span.diagnostic_label(LabelStyle::Primary)),
                    ),
            ),
        }

        diag.push(
            Diagnostic::help()
                .with_message("Backtrace:")
                .with_notes_iter(self.error.backtrace.iter().enumerate().map(
                    |(i, (fn_name, span))| {
                        let file = self.resolver.files.get(span.code_source_id).unwrap();
                        let file_name = file.name();
                        let (line, col) = position(*span, file.source());

                        // We want to retrieve a rough summary of the error that fits on a single line.
                        let error_cause =
                            &file.source()[span.start.as_usize()..span.end.as_usize()];
                        // " = {}: " => 5 character + the size of the idx in base 10
                        let target_width = 120 - 5 - (i + 1).ilog10() as usize;
                        let last_char_idx = error_cause
                            .char_indices()
                            .take(target_width)
                            .map(|(i, _)| i)
                            .last()
                            .unwrap_or_default();
                        let error_cause = &error_cause[..=last_char_idx].replace('\n', "\\n");

                        format!("{i}: {error_cause}\n\tat {fn_name} - {file_name}:{line}:{col}")
                    },
                )),
        );

        diag
    }
}

/// Return the position in terms of line number and column number
/// of the span starting position in the source.
fn position(span: Span, source: &str) -> (usize, usize) {
    let mut line = 1;
    let mut last_line_pos = 0;
    let start = span.start.as_usize();

    for (i, b) in source.bytes().enumerate() {
        if b == b'\n' {
            line += 1;
            last_line_pos = i;
        }
        if i == start {
            break;
        }
    }
    (line, start - last_line_pos)
}
