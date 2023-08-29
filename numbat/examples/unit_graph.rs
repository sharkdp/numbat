// You can run this program to generate a graph view of all Numbat units
// and their connections to base units:
//
//     cargo run --example=unit_graph | dot -Tsvg -o units.svg
//

use itertools::Itertools;
use numbat::{
    resolver::{CodeSource, FileSystemImporter},
    Context,
};

fn main() {
    let mut importer = FileSystemImporter::default();
    importer.add_path("modules");
    let mut ctx = Context::new(importer);
    let result = ctx
        .interpret(
            "use prelude\nuse units::non_euro_currencies",
            CodeSource::Internal,
        )
        .unwrap();
    assert!(result.1.is_success());

    println!("digraph G {{");
    println!("  layout=fdp;");
    println!("  splines=true;");
    println!("  rankdir=LR;");
    println!("  overlap=false;");

    for unit_name in ctx.base_units().sorted() {
        println!("  {unit_name} [color=\"#eaea5e\",style=filled,shape=doublecircle]");
    }

    for (ref unit_name, base_representation) in
        ctx.unit_representations().sorted_by_key(|(_, b)| b.clone())
    {
        let is_base = base_representation == vec![(unit_name.into(), 1i128)];

        if !is_base {
            for (base_factor, _) in base_representation {
                println!("  {} -> {}", unit_name, base_factor);
            }
        }
    }
    println!("}}");
}
