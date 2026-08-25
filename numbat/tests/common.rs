use std::path::Path;

use numbat::{
    Context, NumbatError,
    currency::{CouldNotLoadCurrencyManager, CurrencyManager},
    module_importer::FileSystemImporter,
    resolver::CodeSource,
};
use once_cell::sync::Lazy;

pub struct TestCurrencyManager {}

impl CurrencyManager for TestCurrencyManager {
    fn get_rate(&self, _: &str) -> Option<f64> {
        Some(1.)
    }

    fn is_known(&self, _: &str) -> bool {
        true
    }

    fn is_loaded(&self) -> bool {
        true
    }

    fn load(&self) -> Result<Option<String>, CouldNotLoadCurrencyManager> {
        Ok(None)
    }
}

pub fn get_test_context_without_prelude() -> Context {
    let module_path = Path::new(
        &std::env::var_os("CARGO_MANIFEST_DIR")
            .expect("CARGO_MANIFEST_DIR variable should be set when calling 'cargo test'"),
    )
    .join("modules");

    let mut importer = FileSystemImporter::default();
    importer.add_path(module_path);

    Context::set_currency_manager(TestCurrencyManager {});
    Context::new(importer)
}

pub fn get_test_context() -> Context {
    static CONTEXT: Lazy<Result<Context, Box<NumbatError>>> = Lazy::new(|| {
        let mut context = get_test_context_without_prelude();

        let _ = context.interpret("use prelude", CodeSource::Internal)?;
        let _ = context.interpret("use units::currencies", CodeSource::Internal)?;
        Ok(context)
    });

    match CONTEXT.clone() {
        Ok(context) => context,
        Err(err) => panic!("\n{}\nError\n{}\n{}\n", "-".repeat(80), err, "-".repeat(80)),
    }
}
