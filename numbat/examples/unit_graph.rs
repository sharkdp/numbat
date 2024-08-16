// You can run this program to generate a graph view of all Numbat units
// and their connections to base units:
//
//     cargo run --example=unit_graph | dot -Tsvg -o units.svg
//

use itertools::Itertools;
use numbat::{
    module_importer::FileSystemImporter, resolver::CodeSource, BaseRepresentationFactor, Context,
};

fn main() {
    let mut importer = FileSystemImporter::default();
    importer.add_path("numbat/modules");
    let mut ctx = Context::new(importer);
    let _ = ctx.interpret("use all", CodeSource::Internal).unwrap();

    println!("digraph G {{");
    println!("  layout=fdp;");
    println!("  splines=true;");
    println!("  rankdir=LR;");
    println!("  overlap=false;");

    for unit_name in ctx.base_units().sorted() {
        println!("  {unit_name} [color=\"#eaea5e\",style=filled,shape=doublecircle]");
    }

    for (ref unit_name, base_representation) in ctx.unit_representations().map(|(u, b)| {
        (
            u,
            b.0.iter()
                .map(|BaseRepresentationFactor(name, exp)| (name.clone(), exp.to_integer()))
                .collect::<Vec<_>>(),
        )
    })
    // TODO: check if to_integer can fail here.sorted_by_key(|(_, b)| b.clone())
    {
        let is_base = base_representation == vec![(unit_name.into(), 1i128)];

        if !is_base {
            for (base_factor, _) in base_representation {
                println!("  {unit_name} -> {base_factor}");
            }
        }
    }
    println!("}}");
}
