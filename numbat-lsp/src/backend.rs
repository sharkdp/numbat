use std::{
    collections::HashMap,
    str::FromStr,
    sync::atomic::{AtomicUsize, Ordering},
};

use numbat::{
    Context, ParseError,
    module_importer::{BuiltinModuleImporter, ChainedImporter, FileSystemImporter, ModuleImporter},
    pretty_print::PrettyPrint,
    resolver::{CodeSource, Resolver, ResolverError},
    span::Span,
    typechecker::{IncompatibleDimensionsError, TypeCheckError, TypeChecker},
};
use tokio::sync::RwLock;
use tower_lsp_server::{
    Client, LanguageServer, jsonrpc,
    lsp_types::{
        Diagnostic, DiagnosticOptions, DiagnosticRelatedInformation, DiagnosticServerCapabilities,
        DiagnosticSeverity, DidChangeTextDocumentParams, DidCloseTextDocumentParams,
        DidOpenTextDocumentParams, DocumentDiagnosticParams, DocumentDiagnosticReport,
        DocumentDiagnosticReportResult, FullDocumentDiagnosticReport, HoverProviderCapability,
        InitializeParams, InitializeResult, Location, MessageType, Position, Range,
        RelatedFullDocumentDiagnosticReport, ServerCapabilities, ServerInfo,
        TextDocumentSyncCapability, TextDocumentSyncKind, Uri, WorkDoneProgressOptions,
    },
};

#[derive(Debug)]
pub struct Backend {
    // All the raw files and their diagnostics
    // TODO: Should be a Rope probably
    pub files: RwLock<HashMap<Uri, (String, Vec<Diagnostic>)>>,

    // Mapping between the codesource id and the uri
    pub uri_to_cs_id: RwLock<HashMap<Uri, usize>>,
    pub cs_id_to_uri: RwLock<HashMap<usize, Uri>>,
    pub current_cs_id: AtomicUsize,

    // Client to communicate with the lsp
    pub client: Client,
}

impl LanguageServer for Backend {
    async fn initialize(
        &self,
        _params: InitializeParams,
    ) -> Result<InitializeResult, jsonrpc::Error> {
        let init = InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                completion_provider: None,
                signature_help_provider: None,
                definition_provider: None,
                type_definition_provider: None,
                implementation_provider: None,
                references_provider: None,
                workspace_symbol_provider: None,
                code_action_provider: None,
                rename_provider: None,
                declaration_provider: None,
                diagnostic_provider: Some(DiagnosticServerCapabilities::Options(
                    DiagnosticOptions {
                        identifier: Some(String::from("numbat")),
                        inter_file_dependencies: true,
                        workspace_diagnostics: false,
                        work_done_progress_options: WorkDoneProgressOptions {
                            // TODO: What is this?
                            work_done_progress: Some(false),
                        },
                    },
                )),
                ..Default::default()
            },

            server_info: Some(ServerInfo {
                name: "Numbat LSP".to_owned(),
                version: None,
            }),
        };

        Ok(init)
    }

    async fn shutdown(&self) -> Result<(), jsonrpc::Error> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let content = params.text_document.text;

        self.client
            .log_message(MessageType::LOG, format!("Document {uri:?} opened"))
            .await;
        self.parse_content(uri, content, Some(params.text_document.version))
            .await;
    }

    async fn did_change(&self, change: DidChangeTextDocumentParams) {
        let uri = change.text_document.uri;
        self.client
            .log_message(
                MessageType::LOG,
                format!(
                    "Document {uri:?} received {} changes",
                    change.content_changes.len()
                ),
            )
            .await;
        let mut content = String::new();
        // theoretically we should always have a single change since we asked to
        // only receive full changes
        for change in change.content_changes {
            content = change.text;
        }
        self.parse_content(uri, content, Some(change.text_document.version))
            .await;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;
        self.files.write().await.remove(&uri);
        if let Some(cs) = self.uri_to_cs_id.write().await.remove(&uri) {
            self.cs_id_to_uri.write().await.remove(&cs);
        }
    }

    async fn diagnostic(
        &self,
        params: DocumentDiagnosticParams,
    ) -> jsonrpc::Result<DocumentDiagnosticReportResult> {
        let uri = params.text_document.uri;
        let files = self.files.read().await;
        let Some((_, diags)) = files.get(&uri) else {
            let msg = format!("Diagnostic called for unopened file at {uri:?}");
            self.client
                .log_message(MessageType::WARNING, msg.clone())
                .await;
            return Err(jsonrpc::Error {
                code: jsonrpc::ErrorCode::ServerError(0),
                message: msg.into(),
                data: None,
            });
        };

        Ok(DocumentDiagnosticReportResult::Report(
            DocumentDiagnosticReport::Full(RelatedFullDocumentDiagnosticReport {
                related_documents: None,
                full_document_diagnostic_report: FullDocumentDiagnosticReport {
                    result_id: None,
                    items: diags.to_vec(),
                },
            }),
        ))
    }
}

impl Backend {
    async fn parse_content(&self, uri: Uri, content: String, version: Option<i32>) {
        let id = match self.uri_to_cs_id.read().await.get(&uri) {
            Some(id) => *id,
            None => self.current_cs_id.fetch_add(1, Ordering::Relaxed),
        };

        let (ret, err) = match numbat::parse(&content, id) {
            Ok(stmts) => (stmts, Vec::new()),
            Err(e) => e,
        };

        #[allow(clippy::mutable_key_type)]
        let mut diags: HashMap<Uri, Vec<Diagnostic>> = HashMap::new();

        for err in err.iter() {
            diags
                .entry(uri.clone())
                .or_default()
                .push(parse_error_to_diag(err, &content));
        }

        if err.is_empty() {
            let mut fs_importer = FileSystemImporter::default();
            for path in numbat_cli_helpers::get_modules_paths() {
                fs_importer.add_path(path);
            }

            let importer = ChainedImporter::new(
                Box::new(fs_importer),
                Box::<BuiltinModuleImporter>::default(),
            );

            let mut context = Context::new(importer);
            let statements = context
                .resolver_mut()
                .resolve(&content, numbat::resolver::CodeSource::Text);
            match statements {
                Err(e @ ResolverError::UnknownModule(span, _)) => {
                    diags.entry(uri.clone()).or_default().push(Diagnostic {
                        range: Range {
                            start: byte_index_to_pos(span.start.0 as usize, &content),
                            end: byte_index_to_pos(span.end.0 as usize, &content),
                        },
                        severity: Some(DiagnosticSeverity::ERROR),
                        code: None,
                        code_description: None,
                        source: Some("Resolver".to_string()),
                        message: e.to_string(),
                        related_information: None,
                        tags: None,
                        data: None,
                    });
                }
                Err(ResolverError::ParseErrors(errors)) => {
                    // In this case the error targets other files, we must
                    // load them before we're able to display them
                    for error in errors {
                        if let Ok(file) = context.resolver().files.get(error.span.code_source_id) {
                            let content = file.source().as_str();
                            let uri = match Uri::from_str(file.name()) {
                                Ok(uri) => uri,
                                Err(e) => {
                                    self.client.log_message(
                                        MessageType::WARNING,
                                        format!("Could not return parsing error of distant resource {} while parsing import in {uri:?}: {e}", file.name())
                                    ).await;
                                    continue;
                                }
                            };
                            diags
                                .entry(uri)
                                .or_default()
                                .push(parse_error_to_diag(&error, content));
                        }
                    }
                }
                Ok(stmts) => {
                    let result = context.prefix_transformer_mut().transform(stmts);
                    match result {
                        Err(e) => {
                            let mut diag = self
                                .name_resolution_error_to_diag(&context, &uri, &content, &e)
                                .await;
                            diags.entry(uri.clone()).or_default().append(&mut diag);
                        }
                        Ok(stmts) => {
                            let result = TypeChecker::default().check(&stmts);
                            match result {
                                Err(e) => {
                                    let mut diag = self
                                        .type_check_error_to_diag(&context, &uri, &content, &e)
                                        .await;
                                    diags.entry(uri.clone()).or_default().append(&mut diag);
                                }
                                Ok(_stmts) => (),
                            }
                        }
                    }
                }
            };
        }

        let mut updated_myself = false;
        for (other_uri, diags) in diags.iter() {
            let version = if other_uri == &uri {
                updated_myself = true;
                version
            } else {
                None
            };
            self.client
                .publish_diagnostics(other_uri.clone(), diags.clone(), version)
                .await;
        }
        if !updated_myself {
            // There is no error remaninig on ourselves. We can clear all diags
            self.client
                .publish_diagnostics(uri.clone(), Vec::new(), version)
                .await;
        }
        // we don't save others diagnostics, they'll be saved once the file is opened
        self.files.write().await.insert(
            uri.clone(),
            (content, diags.get(&uri).cloned().unwrap_or_default()),
        );
        self.uri_to_cs_id.write().await.insert(uri.clone(), id);
        self.cs_id_to_uri.write().await.insert(id, uri);
    }

    async fn name_resolution_error_to_diag(
        &self,
        context: &Context,
        self_uri: &Uri,
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

                let related_information = if original_span.code_source_id
                    == conflict_span.code_source_id
                {
                    let original_range = span_to_range(*original_span, self_content);

                    diags.push(Diagnostic {
                        range: original_range,
                        severity: Some(DiagnosticSeverity::HINT),
                        code: None,
                        code_description: None,
                        source: Some("Name resolution hint".to_string()),
                        message: String::from("Already used here"),
                        related_information: None,
                        tags: None,
                        data: None,
                    });
                    Some(vec![DiagnosticRelatedInformation {
                        location: Location {
                            uri: self_uri.clone(),
                            range: original_range,
                        },
                        message: String::from("Already used here"),
                    }])
                } else if let Ok(file) = context.resolver().files.get(original_span.code_source_id)
                {
                    let content = file.source().as_str();
                    match Uri::from_str(file.name()) {
                        Ok(uri) => Some(vec![DiagnosticRelatedInformation {
                            location: Location {
                                uri: uri.clone(),
                                range: span_to_range(*original_span, content),
                            },
                            message: String::from("Already used here"),
                        }]),
                        Err(e) => {
                            self.client.log_message(
                                MessageType::WARNING,
                                format!("Could not return name clash error of distant resource {} while checking {self_uri:?}: {e}", file.name())
                            ).await;
                            None
                        }
                    }
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

    async fn type_check_error_to_diag(
        &self,
        context: &Context,
        self_uri: &Uri,
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

fn byte_index_to_pos(bi: usize, content: &str) -> Position {
    if bi == 0 {
        return Position::new(0, 0);
    }

    let mut line = 0;
    let mut character = 0;
    let mut offset = 0;

    for c in content.chars() {
        if offset >= bi {
            return Position { line, character };
        }
        if c == '\n' {
            line += 1;
            character = 0;
        } else {
            character += 1;
        }
        offset += c.len_utf8();
    }

    // TODO: That's a failure
    Position { line, character }
}

fn span_to_range(span: numbat::span::Span, content: &str) -> Range {
    Range {
        start: byte_index_to_pos(span.start.as_usize(), content),
        end: byte_index_to_pos(span.end.as_usize(), content),
    }
}

fn pos_to_offset(pos: Position, content: &str) -> usize {
    if pos.line == 0 && pos.character == 0 {
        return 0;
    }

    let mut line = 0;
    let mut character = 0;
    let mut offset = 0;

    for c in content.chars() {
        if line == pos.line {
            if character == pos.character {
                return offset;
            }
            character += 1;
        }
        if c == '\n' {
            line += 1;
        }
        offset += c.len_utf8();
    }

    // TODO: That's a failure
    offset
}

fn parse_error_to_diag(err: &ParseError, content: &str) -> Diagnostic {
    Diagnostic {
        range: Range {
            start: byte_index_to_pos(err.span.start.0 as usize, &content),
            end: byte_index_to_pos(err.span.end.0 as usize, &content),
        },
        severity: Some(DiagnosticSeverity::ERROR),
        code: None,
        code_description: None,
        source: Some("Parse".to_string()),
        message: err.kind.to_string(),
        related_information: None,
        tags: None,
        data: None,
    }
}
