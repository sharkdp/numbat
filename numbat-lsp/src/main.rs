use tower_lsp::{LspService, Server};

mod numbat_backend;

use numbat_backend::NumbatBackend;

#[tokio::main]
async fn main() {
    tracing_subscriber::fmt().init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| NumbatBackend::new(client));
    Server::new(stdin, stdout, socket).serve(service).await;
}