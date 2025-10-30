use std::collections::HashMap;

use numbat::ParseError;
use tokio::sync::RwLock;
use tower_lsp_server::{Client, LanguageServer, jsonrpc, lsp_types::*};

use crate::file_mapper::FileMapping;

#[derive(Debug)]
pub struct Backend {
    pub files: RwLock<FileMapping>,

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
        self.check_content(uri, content, Some(params.text_document.version))
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
        self.check_content(uri, content, Some(change.text_document.version))
            .await;
    }

    async fn did_close(&self, _params: DidCloseTextDocumentParams) {
        // TODO: If the file is not used by anything else we can close it maybe?
    }

    async fn diagnostic(
        &self,
        params: DocumentDiagnosticParams,
    ) -> jsonrpc::Result<DocumentDiagnosticReportResult> {
        let uri = params.text_document.uri;
        let files = self.files.read().await;
        let Some(file) = files.files.get(&uri) else {
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
                    items: file.diags.to_vec(),
                },
            }),
        ))
    }
}

impl Backend {
    async fn check_content(&self, uri: Uri, content: String, version: Option<i32>) {
        let module_path = FileMapping::uri_to_module(&uri);
        #[allow(clippy::mutable_key_type)]
        let mut diags = HashMap::new();
        self.files.write().await.load_module(
            module_path,
            Some(uri.path().as_str()),
            Some(content),
            &mut diags,
        );

        for (uri, diags) in diags {
            self.client.publish_diagnostics(uri, diags, version).await;
        }
    }
}

pub fn byte_index_to_pos(bi: usize, content: &str) -> Position {
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

pub fn span_to_range(span: numbat::span::Span, content: &str) -> Range {
    Range {
        start: byte_index_to_pos(span.start.as_usize(), content),
        end: byte_index_to_pos(span.end.as_usize(), content),
    }
}

#[allow(dead_code)] // TODO: We'll probably need it later
pub fn pos_to_offset(pos: Position, content: &str) -> usize {
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

pub fn parse_error_to_diag(err: &ParseError, content: &str) -> Diagnostic {
    Diagnostic {
        range: Range {
            start: byte_index_to_pos(err.span.start.0 as usize, content),
            end: byte_index_to_pos(err.span.end.0 as usize, content),
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
