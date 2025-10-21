use std::{
    collections::HashMap,
    sync::atomic::{AtomicUsize, Ordering},
};

use tokio::sync::RwLock;
use tower_lsp_server::{
    Client, LanguageServer, jsonrpc,
    lsp_types::{
        Diagnostic, DiagnosticSeverity, DidChangeTextDocumentParams, DidCloseTextDocumentParams,
        DidOpenTextDocumentParams, DocumentDiagnosticParams, DocumentDiagnosticReport,
        DocumentDiagnosticReportResult, FullDocumentDiagnosticReport, HoverProviderCapability,
        InitializeParams, InitializeResult, MessageType, Position, Range,
        RelatedFullDocumentDiagnosticReport, ServerCapabilities, ServerInfo,
        TextDocumentSyncCapability, TextDocumentSyncKind, Uri,
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
                diagnostic_provider: None,
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
        self.parse_content(uri, content).await;
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
        self.parse_content(uri, content).await;
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
    async fn parse_content(&self, uri: Uri, content: String) {
        let id = match self.uri_to_cs_id.read().await.get(&uri) {
            Some(id) => *id,
            None => self.current_cs_id.fetch_add(1, Ordering::Relaxed),
        };

        let (ret, err) = match numbat::parse(&content, id) {
            Ok(stmts) => (stmts, Vec::new()),
            Err(e) => e,
        };

        let mut diags = Vec::new();

        for err in err {
            let d = Diagnostic {
                range: Range {
                    start: byte_index_to_pos(err.span.start.0 as usize, &content),
                    end: byte_index_to_pos(err.span.end.0 as usize, &content),
                },
                severity: Some(DiagnosticSeverity::ERROR),
                code: None,
                code_description: None,
                source: Some("Parse error".to_string()),
                message: err.kind.to_string(),
                related_information: None,
                tags: None,
                data: None,
            };
            diags.push(d);
        }
        self.client
            .publish_diagnostics(uri.clone(), diags.clone(), None)
            .await;
        self.files
            .write()
            .await
            .insert(uri.clone(), (content, diags));
        self.uri_to_cs_id.write().await.insert(uri.clone(), id);
        self.cs_id_to_uri.write().await.insert(id, uri);
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
