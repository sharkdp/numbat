use backend::Backend;
use file_mapper::FileMapping;
use tokio::sync::RwLock;

mod backend;
mod err_to_diag;
mod file_mapper;

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = tower_lsp_server::LspService::build(|client| Backend {
        client,
        files: RwLock::new(FileMapping::with_prelude()),
    })
    .finish();

    tower_lsp_server::Server::new(stdin, stdout, socket)
        .serve(service)
        .await;
}
