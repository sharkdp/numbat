use std::path::Path;

use numbat::{module_importer::FileSystemImporter, resolver::CodeSource, Context};

pub fn get_test_context_without_prelude() -> Context {
    let module_path = Path::new(
        &std::env::var_os("CARGO_MANIFEST_DIR")
            .expect("CARGO_MANIFEST_DIR variable should be set when calling 'cargo test'"),
    )
    .join("..")
    .join("modules");

    let mut importer = FileSystemImporter::default();
    importer.add_path(module_path);

    Context::new(importer)
}

pub fn get_test_context() -> Context {
    let mut context = get_test_context_without_prelude();

    assert!(context
        .interpret("use prelude", CodeSource::Internal)
        .expect("Error while running prelude")
        .1
        .is_success());
    context
}
