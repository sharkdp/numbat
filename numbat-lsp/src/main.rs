use backend::Backend;

mod backend;

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = tower_lsp_server::LspService::build(|client| Backend {
        client,
        files: Default::default(),
        uri_to_cs_id: Default::default(),
        cs_id_to_uri: Default::default(),
        current_cs_id: Default::default(),
    })
    .finish();

    tower_lsp_server::Server::new(stdin, stdout, socket)
        .serve(service)
        .await;
}
