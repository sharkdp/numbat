use std::collections::HashMap;
use numbat::{Context, NumbatError, InterpreterSettings};
use numbat::module_importer::BuiltinModuleImporter;
use numbat::resolver::CodeSource;
use numbat::diagnostic::ErrorDiagnostic;
use codespan_reporting::files::Files;
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
                let error_diagnostics = self.error_to_diagnostics(&error, &ctx);
                diagnostics.extend(error_diagnostics);
            }
        }

        diagnostics
    }

    fn error_to_diagnostics(&self, error: &NumbatError, ctx: &Context) -> Vec<Diagnostic> {
        let error_diagnostic: &dyn ErrorDiagnostic = match error {
            NumbatError::ResolverError(e) => e,
            NumbatError::NameResolutionError(e) => e,
            NumbatError::TypeCheckError(e) => e,
            NumbatError::RuntimeError(e) => e,
        };

        // Get the concise error message from thiserror
        let error_message = error.to_string();
        
        let numbat_diagnostics = error_diagnostic.diagnostics();
        let files = &ctx.resolver().files;
        
        numbat_diagnostics
            .into_iter()
            .filter_map(|diag| self.convert_numbat_diagnostic(&diag, files, &error_message))
            .collect()
    }

    fn convert_numbat_diagnostic<'a>(
        &self,
        diag: &numbat::Diagnostic,
        files: &'a impl Files<'a, FileId = usize>,
        error_message: &str,
    ) -> Option<Diagnostic> {
        // Get all primary labels
        let primary_labels: Vec<_> = diag
            .labels
            .iter()
            .filter(|label| {
                matches!(
                    label.style,
                    codespan_reporting::diagnostic::LabelStyle::Primary
                )
            })
            .collect();

        if primary_labels.is_empty() {
            return None;
        }

        // Find the combined range that encompasses all primary labels
        let first_label = primary_labels[0];
        let mut min_start = first_label.range.start;
        let mut max_end = first_label.range.end;
        let file_id = first_label.file_id;

        // Extend the range to include all primary labels
        for label in &primary_labels[1..] {
            // Only combine labels from the same file
            if label.file_id == file_id {
                min_start = min_start.min(label.range.start);
                max_end = max_end.max(label.range.end);
            }
        }

        // Convert byte range to line/column using codespan
        let start_location = files.location(file_id, min_start).ok()?;
        let end_location = files.location(file_id, max_end).ok()?;

        Some(Diagnostic {
            range: Range {
                start: Position {
                    line: start_location.line_number as u32 - 1, // Convert to 0-based
                    character: start_location.column_number as u32 - 1, // Convert to 0-based
                },
                end: Position {
                    line: end_location.line_number as u32 - 1,
                    character: end_location.column_number as u32,
                },
            },
            severity: Some(DiagnosticSeverity::ERROR),
            code: None,
            code_description: None,
            source: Some("numbat".to_string()),
            message: error_message.to_string(),
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
            CompletionItem::new_simple("struct".to_string(), "Struct definition".to_string()),
        ])))
    }
}