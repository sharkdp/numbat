use std::collections::HashMap;
use numbat::{Context, NumbatError, NameResolutionError, InterpreterSettings};
use numbat::module_importer::BuiltinModuleImporter;
use numbat::resolver::CodeSource;
use numbat::diagnostic::ErrorDiagnostic;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};

pub struct NumbatBackend {
    client: Client,
    document_map: tokio::sync::RwLock<HashMap<Url, String>>,
}

impl NumbatBackend {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            document_map: tokio::sync::RwLock::new(HashMap::new()),
        }
    }

    async fn validate_document(&self, uri: &Url, text: &str) {
        let diagnostics = self.get_diagnostics(text).await;
        
        self.client
            .publish_diagnostics(uri.clone(), diagnostics, None)
            .await;
    }

    async fn get_diagnostics(&self, text: &str) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();

        // Create a new Numbat context
        let mut ctx = Context::new(BuiltinModuleImporter::default());
        
        // Load prelude for standard functionality
        let _ = ctx.interpret("use prelude", CodeSource::Internal);
        
        // Create settings for interpretation
        let mut settings = InterpreterSettings {
            print_fn: Box::new(|_| {}), // Ignore print output for LSP
        };
        
        // Try to interpret the text and collect errors
        match ctx.interpret_with_settings(&mut settings, text, CodeSource::Text) {
            Ok(_) => {
                // No errors, return empty diagnostics
            }
            Err(error) => {
                if let Some(diagnostic) = self.error_to_diagnostic(&error, text) {
                    diagnostics.push(diagnostic);
                }
            }
        }

        diagnostics
    }

    fn error_to_diagnostic(&self, error: &NumbatError, source_text: &str) -> Option<Diagnostic> {
        // Get error message
        let message = match error {
            NumbatError::ResolverError(e) => format!("Resolver error: {}", e),
            NumbatError::NameResolutionError(e) => format!("Name resolution error: {}", e),
            NumbatError::TypeCheckError(e) => format!("Type check error: {}", e),
            NumbatError::RuntimeError(e) => format!("Runtime error: {}", e),
        };

        // For now, we'll create a diagnostic that highlights the first line
        // TODO: Extract proper span information from the error
        Some(Diagnostic {
            range: Range {
                start: Position {
                    line: 0,
                    character: 0,
                },
                end: Position {
                    line: 0,
                    character: 10,
                },
            },
            severity: Some(DiagnosticSeverity::ERROR),
            code: None,
            code_description: None,
            source: Some("numbat".to_string()),
            message,
            related_information: None,
            tags: None,
            data: None,
        })
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for NumbatBackend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec![".".to_string()]),
                    work_done_progress_options: Default::default(),
                    all_commit_characters: None,
                    completion_item: None,
                }),
                ..ServerCapabilities::default()
            },
            server_info: None,
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "Numbat Language Server initialized!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let text = params.text_document.text;
        
        self.document_map.write().await.insert(uri.clone(), text.clone());
        self.validate_document(&uri, &text).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        
        if let Some(change) = params.content_changes.into_iter().next() {
            let text = change.text;
            self.document_map.write().await.insert(uri.clone(), text.clone());
            self.validate_document(&uri, &text).await;
        }
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        if let Some(text) = params.text {
            self.validate_document(&params.text_document.uri, &text).await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        self.document_map.write().await.remove(&params.text_document.uri);
    }

    async fn completion(&self, _: CompletionParams) -> Result<Option<CompletionResponse>> {
        Ok(Some(CompletionResponse::Array(vec![
            CompletionItem::new_simple("let".to_string(), "Variable declaration".to_string()),
            CompletionItem::new_simple("fn".to_string(), "Function declaration".to_string()),
            CompletionItem::new_simple("unit".to_string(), "Unit definition".to_string()),
            CompletionItem::new_simple("dimension".to_string(), "Dimension definition".to_string()),
            CompletionItem::new_simple("use".to_string(), "Import module".to_string()),
            CompletionItem::new_simple("where".to_string(), "Where clause".to_string()),
            CompletionItem::new_simple("if".to_string(), "Conditional expression".to_string()),
            CompletionItem::new_simple("then".to_string(), "Then branch".to_string()),
            CompletionItem::new_simple("else".to_string(), "Else branch".to_string()),
        ])))
    }
}