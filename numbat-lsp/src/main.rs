use std::collections::HashMap;
use std::path::PathBuf;
use std::str::FromStr;
use std::sync::Arc;

use dashmap::DashMap;
use numbat::module_importer::{BuiltinModuleImporter, ChainedImporter, FileSystemImporter};
use numbat::resolver::{CodeSource, ResolverError};
use numbat::{
    Context, IncompatibleDimensionsError, NameResolutionError, NumbatError, Span, TypeCheckError,
};
use numbat_lsp::chumsky::{type_inference, Func, ImCompleteSemanticToken};
use numbat_lsp::completion::completion;
use numbat_lsp::jump_definition::get_definition;
use numbat_lsp::reference::get_reference;
use numbat_lsp::semantic_token::{semantic_token_from_ast, LEGEND_TYPE};
use ropey::Rope;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use tokio::sync::Mutex;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

struct Backend {
    client: Client,
    ast_map: DashMap<String, HashMap<String, Func>>,
    document_map: DashMap<String, Rope>,
    semantic_token_map: DashMap<String, Vec<ImCompleteSemanticToken>>,

    context: Arc<Mutex<Context>>,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: None,
            offset_encoding: None,
            capabilities: ServerCapabilities {
                inlay_hint_provider: Some(OneOf::Left(true)),
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec!["|>".to_string()]),
                    work_done_progress_options: Default::default(),
                    all_commit_characters: None,
                    completion_item: None,
                }),
                execute_command_provider: Some(ExecuteCommandOptions {
                    commands: vec!["dummy.do_something".to_string()],
                    work_done_progress_options: Default::default(),
                }),

                workspace: Some(WorkspaceServerCapabilities {
                    workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                        supported: Some(true),
                        change_notifications: Some(OneOf::Left(true)),
                    }),
                    file_operations: None,
                }),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensRegistrationOptions(
                        SemanticTokensRegistrationOptions {
                            text_document_registration_options: {
                                TextDocumentRegistrationOptions {
                                    document_selector: Some(vec![DocumentFilter {
                                        language: Some("numbat".to_string()),
                                        scheme: Some("file".to_string()),
                                        pattern: Some("*.nbt".to_string()),
                                    }]),
                                }
                            },
                            semantic_tokens_options: SemanticTokensOptions {
                                work_done_progress_options: WorkDoneProgressOptions::default(),
                                legend: SemanticTokensLegend {
                                    token_types: LEGEND_TYPE.into(),
                                    token_modifiers: vec![],
                                },
                                range: Some(true),
                                full: Some(SemanticTokensFullOptions::Bool(true)),
                            },
                            static_registration_options: StaticRegistrationOptions::default(),
                        },
                    ),
                ),
                // definition: Some(GotoCapability::default()),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                rename_provider: Some(OneOf::Left(true)),
                ..ServerCapabilities::default()
            },
        })
    }
    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "initialized!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file opened!")
            .await;
        self.on_change(TextDocumentItem {
            uri: params.text_document.uri,
            text: params.text_document.text,
            version: params.text_document.version,
        })
        .await
    }

    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        self.on_change(TextDocumentItem {
            uri: params.text_document.uri,
            text: std::mem::take(&mut params.content_changes[0].text),
            version: params.text_document.version,
        })
        .await
    }

    async fn did_save(&self, _: DidSaveTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file saved!")
            .await;
    }
    async fn did_close(&self, _: DidCloseTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file closed!")
            .await;
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let definition = async {
            let uri = params.text_document_position_params.text_document.uri;
            let ast = self.ast_map.get(uri.as_str())?;
            let rope = self.document_map.get(uri.as_str())?;

            let position = params.text_document_position_params.position;
            let char = rope.try_line_to_char(position.line as usize).ok()?;
            let offset = char + position.character as usize;
            // self.client.log_message(MessageType::INFO, &format!("{:#?}, {}", ast.value(), offset)).await;
            let span = get_definition(&ast, offset);
            self.client
                .log_message(MessageType::INFO, &format!("{:?}, ", span))
                .await;
            span.and_then(|(_, range)| {
                let start_position = offset_to_position(range.start, &rope)?;
                let end_position = offset_to_position(range.end, &rope)?;

                let range = Range::new(start_position, end_position);

                Some(GotoDefinitionResponse::Scalar(Location::new(uri, range)))
            })
        }
        .await;
        Ok(definition)
    }
    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let reference_list = || -> Option<Vec<Location>> {
            let uri = params.text_document_position.text_document.uri;
            let ast = self.ast_map.get(&uri.to_string())?;
            let rope = self.document_map.get(&uri.to_string())?;

            let position = params.text_document_position.position;
            let char = rope.try_line_to_char(position.line as usize).ok()?;
            let offset = char + position.character as usize;
            let reference_list = get_reference(&ast, offset, false);
            let ret = reference_list
                .into_iter()
                .filter_map(|(_, range)| {
                    let start_position = offset_to_position(range.start, &rope)?;
                    let end_position = offset_to_position(range.end, &rope)?;

                    let range = Range::new(start_position, end_position);

                    Some(Location::new(uri.clone(), range))
                })
                .collect::<Vec<_>>();
            Some(ret)
        }();
        Ok(reference_list)
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = params.text_document.uri.to_string();
        self.client
            .log_message(MessageType::LOG, "semantic_token_full")
            .await;
        let semantic_tokens = || -> Option<Vec<SemanticToken>> {
            let mut im_complete_tokens = self.semantic_token_map.get_mut(&uri)?;
            let rope = self.document_map.get(&uri)?;
            let ast = self.ast_map.get(&uri)?;
            let extends_tokens = semantic_token_from_ast(&ast);
            im_complete_tokens.extend(extends_tokens);
            im_complete_tokens.sort_by(|a, b| a.start.cmp(&b.start));
            let mut pre_line = 0;
            let mut pre_start = 0;
            let semantic_tokens = im_complete_tokens
                .iter()
                .filter_map(|token| {
                    let line = rope.try_byte_to_line(token.start).ok()? as u32;
                    let first = rope.try_line_to_char(line as usize).ok()? as u32;
                    let start = rope.try_byte_to_char(token.start).ok()? as u32 - first;
                    let delta_line = line - pre_line;
                    let delta_start = if delta_line == 0 {
                        start - pre_start
                    } else {
                        start
                    };
                    let ret = Some(SemanticToken {
                        delta_line,
                        delta_start,
                        length: token.length as u32,
                        token_type: token.token_type as u32,
                        token_modifiers_bitset: 0,
                    });
                    pre_line = line;
                    pre_start = start;
                    ret
                })
                .collect::<Vec<_>>();
            Some(semantic_tokens)
        }();
        if let Some(semantic_token) = semantic_tokens {
            return Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
                result_id: None,
                data: semantic_token,
            })));
        }
        Ok(None)
    }

    async fn semantic_tokens_range(
        &self,
        params: SemanticTokensRangeParams,
    ) -> Result<Option<SemanticTokensRangeResult>> {
        let uri = params.text_document.uri.to_string();
        let semantic_tokens = || -> Option<Vec<SemanticToken>> {
            let im_complete_tokens = self.semantic_token_map.get(&uri)?;
            let rope = self.document_map.get(&uri)?;
            let mut pre_line = 0;
            let mut pre_start = 0;
            let semantic_tokens = im_complete_tokens
                .iter()
                .filter_map(|token| {
                    let line = rope.try_byte_to_line(token.start).ok()? as u32;
                    let first = rope.try_line_to_char(line as usize).ok()? as u32;
                    let start = rope.try_byte_to_char(token.start).ok()? as u32 - first;
                    let ret = Some(SemanticToken {
                        delta_line: line - pre_line,
                        delta_start: if start >= pre_start {
                            start - pre_start
                        } else {
                            start
                        },
                        length: token.length as u32,
                        token_type: token.token_type as u32,
                        token_modifiers_bitset: 0,
                    });
                    pre_line = line;
                    pre_start = start;
                    ret
                })
                .collect::<Vec<_>>();
            Some(semantic_tokens)
        }();
        if let Some(semantic_token) = semantic_tokens {
            return Ok(Some(SemanticTokensRangeResult::Tokens(SemanticTokens {
                result_id: None,
                data: semantic_token,
            })));
        }
        Ok(None)
    }

    async fn inlay_hint(
        &self,
        params: tower_lsp::lsp_types::InlayHintParams,
    ) -> Result<Option<Vec<InlayHint>>> {
        self.client
            .log_message(MessageType::INFO, "inlay hint")
            .await;
        let uri = &params.text_document.uri;
        let mut hashmap = HashMap::new();
        if let Some(ast) = self.ast_map.get(uri.as_str()) {
            ast.iter().for_each(|(_, v)| {
                type_inference(&v.body, &mut hashmap);
            });
        }

        let document = match self.document_map.get(uri.as_str()) {
            Some(rope) => rope,
            None => return Ok(None),
        };
        let inlay_hint_list = hashmap
            .into_iter()
            .map(|(k, v)| {
                (
                    k.start,
                    k.end,
                    match v {
                        numbat_lsp::chumsky::Value::Null => "null".to_string(),
                        numbat_lsp::chumsky::Value::Bool(_) => "bool".to_string(),
                        numbat_lsp::chumsky::Value::Num(_) => "number".to_string(),
                        numbat_lsp::chumsky::Value::Str(_) => "string".to_string(),
                        numbat_lsp::chumsky::Value::List(_) => "[]".to_string(),
                        numbat_lsp::chumsky::Value::Func(_) => v.to_string(),
                    },
                )
            })
            .filter_map(|item| {
                // let start_position = offset_to_position(item.0, document)?;
                let end_position = offset_to_position(item.1, &document)?;
                let inlay_hint = InlayHint {
                    text_edits: None,
                    tooltip: None,
                    kind: Some(InlayHintKind::TYPE),
                    padding_left: None,
                    padding_right: None,
                    data: None,
                    position: end_position,
                    label: InlayHintLabel::LabelParts(vec![InlayHintLabelPart {
                        value: item.2,
                        tooltip: None,
                        location: Some(Location {
                            uri: params.text_document.uri.clone(),
                            range: Range {
                                start: Position::new(0, 4),
                                end: Position::new(0, 5),
                            },
                        }),
                        command: None,
                    }]),
                };
                Some(inlay_hint)
            })
            .collect::<Vec<_>>();

        Ok(Some(inlay_hint_list))
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let completions = || -> Option<Vec<CompletionItem>> {
            let rope = self.document_map.get(&uri.to_string())?;
            let ast = self.ast_map.get(&uri.to_string())?;
            let char = rope.try_line_to_char(position.line as usize).ok()?;
            let offset = char + position.character as usize;
            let completions = completion(&ast, offset);
            let mut ret = Vec::with_capacity(completions.len());
            for (_, item) in completions {
                match item {
                    numbat_lsp::completion::ImCompleteCompletionItem::Variable(var) => {
                        ret.push(CompletionItem {
                            label: var.clone(),
                            insert_text: Some(var.clone()),
                            kind: Some(CompletionItemKind::VARIABLE),
                            detail: Some(var),
                            ..Default::default()
                        });
                    }
                    numbat_lsp::completion::ImCompleteCompletionItem::Function(name, args) => {
                        ret.push(CompletionItem {
                            label: name.clone(),
                            kind: Some(CompletionItemKind::FUNCTION),
                            detail: Some(name.clone()),
                            insert_text: Some(format!(
                                "{}({})",
                                name,
                                args.iter()
                                    .enumerate()
                                    .map(|(index, item)| { format!("${{{}:{}}}", index + 1, item) })
                                    .collect::<Vec<_>>()
                                    .join(",")
                            )),
                            insert_text_format: Some(InsertTextFormat::SNIPPET),
                            ..Default::default()
                        });
                    }
                }
            }
            Some(ret)
        }();
        Ok(completions.map(CompletionResponse::Array))
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        let workspace_edit = || -> Option<WorkspaceEdit> {
            let uri = params.text_document_position.text_document.uri;
            let ast = self.ast_map.get(&uri.to_string())?;
            let rope = self.document_map.get(&uri.to_string())?;

            let position = params.text_document_position.position;
            let char = rope.try_line_to_char(position.line as usize).ok()?;
            let offset = char + position.character as usize;
            let reference_list = get_reference(&ast, offset, true);
            let new_name = params.new_name;
            if !reference_list.is_empty() {
                let edit_list = reference_list
                    .into_iter()
                    .filter_map(|(_, range)| {
                        let start_position = offset_to_position(range.start, &rope)?;
                        let end_position = offset_to_position(range.end, &rope)?;
                        Some(TextEdit::new(
                            Range::new(start_position, end_position),
                            new_name.clone(),
                        ))
                    })
                    .collect::<Vec<_>>();
                let mut map = HashMap::new();
                map.insert(uri, edit_list);
                let workspace_edit = WorkspaceEdit::new(map);
                Some(workspace_edit)
            } else {
                None
            }
        }();
        Ok(workspace_edit)
    }

    async fn did_change_configuration(&self, _: DidChangeConfigurationParams) {
        self.client
            .log_message(MessageType::INFO, "configuration changed!")
            .await;
    }

    async fn did_change_workspace_folders(&self, _: DidChangeWorkspaceFoldersParams) {
        self.client
            .log_message(MessageType::INFO, "workspace folders changed!")
            .await;
    }

    async fn did_change_watched_files(&self, _: DidChangeWatchedFilesParams) {
        self.client
            .log_message(MessageType::INFO, "watched files have changed!")
            .await;
    }

    async fn execute_command(&self, _: ExecuteCommandParams) -> Result<Option<Value>> {
        self.client
            .log_message(MessageType::INFO, "command executed!")
            .await;

        match self.client.apply_edit(WorkspaceEdit::default()).await {
            Ok(res) if res.applied => self.client.log_message(MessageType::INFO, "applied").await,
            Ok(_) => self.client.log_message(MessageType::INFO, "rejected").await,
            Err(err) => self.client.log_message(MessageType::ERROR, err).await,
        }

        Ok(None)
    }
}
#[derive(Debug, Deserialize, Serialize)]
struct InlayHintParams {
    path: String,
}

struct TextDocumentItem {
    uri: Url,
    text: String,
    version: i32,
}

impl Backend {
    async fn on_change(&self, params: TextDocumentItem) {
        let rope = ropey::Rope::from_str(&params.text);
        self.document_map
            .insert(params.uri.to_string(), rope.clone());

        let Ok(filepath) = params.uri.to_file_path() else {
            return;
        };
        match self
            .context
            .lock()
            .await
            .interpret(&params.text, CodeSource::File(filepath))
        {
            Ok((_stmts, _results)) => {
                self.client
                    .publish_diagnostics(params.uri.clone(), vec![], Some(params.version))
                    .await;
            }
            Err(err) => {
                self.publish_numbat_error(params, err).await;
            }
        }

        // self.client
        //     .publish_diagnostics(params.uri.clone(), diagnostics, Some(params.version))
        //     .await;

        // if let Some(ast) = ast {
        //     self.ast_map.insert(params.uri.to_string(), ast);
        // }
        // self.client
        //     .log_message(MessageType::INFO, &format!("{:?}", semantic_tokens))
        //     .await;
        // self.semantic_token_map
        //     .insert(params.uri.to_string(), semantic_tokens);
    }

    async fn publish_numbat_error(&self, params: TextDocumentItem, error: NumbatError) {
        match error {
            NumbatError::ResolverError(err) => match err {
                ResolverError::UnknownModule(span, ref _path) => {
                    self.publish_simple_error(params, span, err).await
                }
                ResolverError::ParseErrors(errors) => {
                    let diagnostics = errors.iter().map(|error| Diagnostic {
                        range: span_to_range(error.span),
                        severity: Some(DiagnosticSeverity::ERROR),
                        code: None,
                        code_description: None,
                        source: Some("numbat".to_string()),
                        message: error.to_string(),
                        related_information: None,
                        tags: None,
                        data: None,
                    });

                    self.client
                        .publish_diagnostics(
                            params.uri.clone(),
                            diagnostics.collect(),
                            Some(params.version),
                        )
                        .await;
                }
            },
            NumbatError::NameResolutionError(err) => {
                self.publish_numbat_name_resolution_error(params, err).await
            }
            NumbatError::TypeCheckError(err) => {
                self.publish_numbat_typecheck_error(params, err).await
            }
            NumbatError::RuntimeError(err) => {
                self.publish_error_without_location(params, err).await
            }
        }
    }

    async fn publish_error_without_location(
        &self,
        params: TextDocumentItem,
        error: impl std::error::Error,
    ) {
        self.client
            .publish_diagnostics(
                params.uri.clone(),
                vec![Diagnostic {
                    // When we don't have a location to display the error, we
                    // always show the errors on the first line.
                    range: Range::new(Position::new(0, 0), Position::new(1, 0)),
                    severity: Some(DiagnosticSeverity::ERROR),
                    code: None,
                    code_description: None,
                    source: Some("numbat".to_string()),
                    message: error.to_string(),
                    related_information: None,
                    tags: None,
                    data: None,
                }],
                Some(params.version),
            )
            .await;
    }

    async fn publish_simple_error(
        &self,
        params: TextDocumentItem,
        location: Span,
        error: impl std::error::Error,
    ) {
        self.client
            .publish_diagnostics(
                params.uri.clone(),
                vec![Diagnostic {
                    range: span_to_range(location),
                    severity: Some(DiagnosticSeverity::ERROR),
                    code: None,
                    code_description: None,
                    source: Some("numbat".to_string()),
                    message: error.to_string(),
                    related_information: None,
                    tags: None,
                    data: None,
                }],
                Some(params.version),
            )
            .await;
    }

    async fn publish_defined_here_error(
        &self,
        params: TextDocumentItem,
        error_location: Span,
        defined_here: Span,
        error: impl std::error::Error,
    ) {
        self.client
            .publish_diagnostics(
                params.uri.clone(),
                vec![Diagnostic {
                    range: span_to_range(error_location),
                    severity: Some(DiagnosticSeverity::ERROR),
                    code: None,
                    code_description: None,
                    source: Some("numbat".to_string()),
                    message: error.to_string(),
                    related_information: Some(vec![DiagnosticRelatedInformation {
                        location: self.span_to_location(defined_here).await,
                        message: "Defined here".to_string(),
                    }]),
                    tags: None,
                    data: None,
                }],
                Some(params.version),
            )
            .await;
    }

    async fn publish_numbat_name_resolution_error(
        &self,
        params: TextDocumentItem,
        error: NameResolutionError,
    ) {
        match error {
            NameResolutionError::IdentifierClash {
                conflicting_identifier: _,
                conflict_span,
                original_span,
                original_item_type: _,
            } => {
                self.publish_defined_here_error(params, conflict_span, original_span, error)
                    .await
            }
            NameResolutionError::ReservedIdentifier(span) => {
                self.publish_simple_error(params, span, error).await
            }
        }
    }

    async fn publish_numbat_typecheck_error(
        &self,
        params: TextDocumentItem,
        error: TypeCheckError,
    ) {
        match error {
            TypeCheckError::UnknownIdentifier(span, _, _)
            | TypeCheckError::NonScalarExponent(span, _)
            | TypeCheckError::NonScalarFactorialArgument(span, _)
            | TypeCheckError::UnsupportedConstEvalExpression(span, _)
            | TypeCheckError::DivisionByZeroInConstEvalExpression(span)
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
            | TypeCheckError::NoDimensionlessBaseUnit(span, _)
            | TypeCheckError::UnknownStruct(span, _)
            | TypeCheckError::MissingDimBound(span)
            | TypeCheckError::ExponentiationNeedsTypeAnnotation(span)
            | TypeCheckError::DerivedUnitDefinitionMustNotBeGeneric(span)
            | TypeCheckError::TypedHoleInStatement(span, _, _, _)
            | TypeCheckError::MultipleTypedHoles(span)
            | TypeCheckError::WrongArity {
                callable_span: span,
                callable_name: _,
                callable_definition_span: None,
                arity: _,
                num_args: _,
            }
            | TypeCheckError::IncompatibleAlternativeDimensionExpression(_, span, _, _, _) => {
                self.publish_simple_error(params, span, error).await
            }

            // Defined here stuff
            TypeCheckError::WrongArity {
                callable_span: span,
                callable_name: _,
                callable_definition_span: Some(here),
                arity: _,
                num_args: _,
            } => {
                self.publish_defined_here_error(params, span, here, error)
                    .await
            }

            // Stuff with no span
            TypeCheckError::ConstraintSolverError(_, _)
            | TypeCheckError::RegistryError(_)
            | TypeCheckError::SubstitutionError(_, _) => {
                self.publish_error_without_location(params, error).await
            }

            // Complex stuff
            TypeCheckError::IncompatibleDimensions(IncompatibleDimensionsError {
                span_operation,
                operation: _,
                span_expected,
                expected_name: _,
                expected_type: _,
                expected_dimensions: _,
                span_actual,
                actual_name: _,
                actual_name_for_fix,
                actual_type: _,
                actual_dimensions: _,
            }) => {
                self.client
                    .publish_diagnostics(
                        params.uri.clone(),
                        vec![Diagnostic {
                            range: span_to_range(span_operation),
                            severity: Some(DiagnosticSeverity::ERROR),
                            code: None,
                            code_description: None,
                            source: Some("numbat".to_string()),
                            message: error.to_string(),
                            related_information: Some(vec![
                                DiagnosticRelatedInformation {
                                    location: self.span_to_location(span_actual).await,
                                    message: format!("Expected {actual_name_for_fix}"),
                                },
                                DiagnosticRelatedInformation {
                                    location: self.span_to_location(span_expected).await,
                                    message: "Defined here".to_string(),
                                },
                            ]),
                            tags: None,
                            data: None,
                        }],
                        Some(params.version),
                    )
                    .await;
            }
            TypeCheckError::NameResolutionError(err) => {
                self.publish_numbat_name_resolution_error(params, err).await
            }

            // TODO: Should be improved
            TypeCheckError::IncompatibleTypesInCondition(span, _, _, _, _)
            | TypeCheckError::IncompatibleTypeInAssert(span, _, _)
            | TypeCheckError::IncompatibleTypesInAssertEq(span, _, _, _, _)
            | TypeCheckError::IncompatibleTypesInAnnotation(_, span, _, _, _, _)
            | TypeCheckError::IncompatibleTypesInComparison(span, _, _, _, _)
            | TypeCheckError::IncompatibleTypesInOperator(span, _, _, _, _, _)
            | TypeCheckError::IncompatibleTypesInFunctionCall(_, _, span, _)
            | TypeCheckError::IncompatibleTypesForStructField(span, _, _, _)
            | TypeCheckError::UnknownFieldInStructInstantiation(span, _, _, _)
            | TypeCheckError::DuplicateFieldInStructDefinition(span, _, _)
            | TypeCheckError::DuplicateFieldInStructInstantiation(span, _, _)
            | TypeCheckError::FieldAccessOfNonStructType(span, _, _, _)
            | TypeCheckError::UnknownFieldAccess(span, _, _, _)
            | TypeCheckError::MissingFieldsInStructInstantiation(span, _, _)
            | TypeCheckError::IncompatibleTypesInList(span, _, _, _) => {
                self.publish_simple_error(params, span, error).await
            }
        }
    }

    async fn span_to_location(&self, span: Span) -> Location {
        let code_source = self
            .context
            .lock()
            .await
            .resolver()
            .get_code_source(span.code_source_id);
        let uri = match code_source {
            CodeSource::Text => unreachable!("You cannot run the lsp without a file right?"),
            CodeSource::Internal => PathBuf::from_str("<internal>").unwrap(),
            CodeSource::File(path) | CodeSource::Module(_, Some(path)) => path,
            CodeSource::Module(_, None) => todo!(),
        };

        Location {
            uri: Url::from_file_path(uri).unwrap(),
            range: span_to_range(span),
        }
    }
}

fn span_to_range(span: Span) -> Range {
    Range {
        start: Position {
            line: span.start.line - 1,
            character: span.start.position - 1,
        },
        end: Position {
            line: span.end.line - 1,
            character: span.end.position - 1,
        },
    }
}

#[tokio::main]
async fn main() {
    env_logger::init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let mut fs_importer = FileSystemImporter::default();
    for path in get_modules_paths().await {
        fs_importer.add_path(path);
    }

    let importer = ChainedImporter::new(
        Box::new(fs_importer),
        Box::<BuiltinModuleImporter>::default(),
    );

    let mut context = Context::new(importer);
    context.set_terminal_width(Some(60));
    /// TODO: Store everything for the goto-definition maybe?
    let _result = context
        .interpret("use prelude", CodeSource::Internal)
        .unwrap();

    /// TODO: Store everything for the goto-definition maybe?
    let user_init_path = get_config_path().await.join("init.nbt");
    if let Ok(user_init_code) = tokio::fs::read_to_string(&user_init_path).await {
        let _result = context
            .interpret(&user_init_code, CodeSource::File(user_init_path))
            .unwrap();
    }

    let (service, socket) = LspService::build(|client| Backend {
        client,
        ast_map: DashMap::new(),
        document_map: DashMap::new(),
        semantic_token_map: DashMap::new(),

        context: Arc::new(Mutex::new(context)),
    })
    .finish();

    Server::new(stdin, stdout, socket).serve(service).await;
}

fn offset_to_position(offset: usize, rope: &Rope) -> Option<Position> {
    let line = rope.try_char_to_line(offset).ok()?;
    let first_char_of_line = rope.try_line_to_char(line).ok()?;
    let column = offset - first_char_of_line;
    Some(Position::new(line as u32, column as u32))
}

async fn get_modules_paths() -> Vec<PathBuf> {
    let mut paths = vec![];

    if let Some(modules_path) = std::env::var_os("NUMBAT_MODULES_PATH") {
        for path in modules_path.to_string_lossy().split(':') {
            paths.push(path.into());
        }
    }

    paths.push(get_config_path().await.join("modules"));

    // We read the value of this environment variable at compile time to
    // allow package maintainers to control the system-wide module path
    // for Numbat.
    if let Some(system_module_path) = option_env!("NUMBAT_SYSTEM_MODULE_PATH") {
        if !system_module_path.is_empty() {
            paths.push(system_module_path.into());
        }
    } else if cfg!(unix) {
        paths.push("/usr/share/numbat/modules".into());
    } else {
        paths.push("C:\\Program Files\\numbat\\modules".into());
    }
    paths
}

async fn get_config_path() -> PathBuf {
    let config_dir = tokio::task::spawn_blocking(dirs::config_dir)
        .await
        .unwrap()
        .unwrap_or_else(|| PathBuf::from("."));
    config_dir.join("numbat")
}
