use std::path::Path;

use numbat::{
    resolver::{CodeSource, FileSystemImporter},
    Context,
};

pub fn get_test_context() -> Context {
    let module_path = Path::new("../modules");

    let mut importer = FileSystemImporter::default();
    importer.add_path(module_path);

    let mut context = Context::new(importer);

    assert!(context
        .interpret(
            &std::fs::read_to_string(module_path.join("prelude.nbt")).unwrap(),
            CodeSource::Text
        )
        .expect("Error while running prelude")
        .1
        .is_success());
    context
}
