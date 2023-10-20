use criterion::{criterion_group, criterion_main, Criterion};
use numbat::module_importer::BuiltinModuleImporter;
use numbat::resolver::CodeSource;
use numbat::Context;

fn import_prelude(c: &mut Criterion) {
    let importer = BuiltinModuleImporter::default();
    let context = Context::new(importer);
    c.bench_function("Import prelude", |b| {
        b.iter_with_setup(
            || context.clone(),
            |mut ctx| ctx.interpret("use prelude", CodeSource::Text),
        )
    });
}

criterion_group!(benches, import_prelude);
criterion_main!(benches);
