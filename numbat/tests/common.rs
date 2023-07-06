use std::path::Path;

use numbat::Context;

pub fn get_test_context() -> Context {
    let mut context = Context::new(false);

    let module_path = Path::new("../modules");
    context.add_module_path(module_path);

    assert!(context
        .interpret(&std::fs::read_to_string(module_path.join("prelude.nbt")).unwrap())
        .expect("Error while running prelude")
        .1
        .is_success());
    context
}
