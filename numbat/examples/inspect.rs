use itertools::Itertools;
use numbat::{module_importer::FileSystemImporter, resolver::CodeSource, Context};
use std::path::Path;

const AUTO_GENERATED_HINT: &'static str = "<!-- NOTE! This file is auto-generated -->";

fn inspect_units(ctx: &Context) {
    println!("{AUTO_GENERATED_HINT}

# List of supported units

See also: [Unit notation](./unit-notation.md).

All SI-accepted units support [metric prefixes](https://en.wikipedia.org/wiki/Metric_prefix) (`mm`, `cm`, `km`, ... or `millimeter`, `centimeter`, `kilometer`, ...)
and — where sensible — units allow for [binary prefixes](https://en.wikipedia.org/wiki/Binary_prefix) (`MiB`, `GiB`, ... or `mebibyte`, `gibibyte`, ...).
");

    println!("| Dimension | Unit name | Identifier(s) |");
    println!("| --- | --- | --- |");
    for (ref unit_name, (_base_representation, unit_metadata)) in ctx
        .unit_representations()
        .sorted_by_key(|(u, (_, m))| (m.readable_type.to_string(), u.to_lowercase()))
    {
        let mut names = unit_metadata.aliases;
        names.sort_by_key(|(n, _)| n.to_lowercase());
        let names = names.iter().map(|(n, _)| n).join("`, `");

        let url = unit_metadata.url;
        let name = unit_metadata.name.unwrap_or(unit_name.clone());

        let name_with_url = if let Some(url) = url {
            format!("[{name}]({url})")
        } else {
            name.clone()
        };

        let readable_type = unit_metadata.readable_type;

        println!("| `{readable_type}` | {name_with_url} | `{names}` |");
    }
}

fn inspect_functions_in_module(ctx: &Context, module: String) {
    for (fn_name, name, signature, description, url, code_source) in ctx.functions() {
        let CodeSource::Module(module_path, _) = code_source else {
            unreachable!();
        };

        if module_path.to_string() != module {
            continue;
        }

        if let Some(name) = name {
            println!("### `{fn_name}` ({name})");
        } else {
            println!("### `{fn_name}`");
        }

        if let Some(ref description_raw) = description {
            let description_raw = description_raw.trim().to_string();

            // Replace $..$ with \\( .. \\) for mdbook.
            let mut description = String::new();
            for (i, part) in description_raw.split('$').enumerate() {
                if i % 2 == 0 {
                    description.push_str(part);
                } else {
                    description.push_str("\\\\( ");
                    description.push_str(part);
                    description.push_str(" \\\\)");
                }
            }

            if description.ends_with('.') {
                println!("{description}");
            } else {
                println!("{description}.");
            }
        }
        if let Some(url) = url {
            println!("More information [here]({url}).");
        }
        println!();

        println!("```nbt");
        println!("{signature}");
        println!("```");
        println!();
    }
}

fn main() {
    let module_path = Path::new(&std::env::var_os("CARGO_MANIFEST_DIR").unwrap()).join("modules");

    let mut importer = FileSystemImporter::default();
    importer.add_path(module_path);
    let mut ctx = Context::new(importer);
    let _result = ctx.interpret("use all", CodeSource::Internal).unwrap();

    let mut args = std::env::args();
    args.next();
    if let Some(arg) = args.next() {
        match arg.as_str() {
            "units" => inspect_units(&ctx),
            "functions" => {
                let module = args.next().unwrap();
                inspect_functions_in_module(&ctx, module)
            }
            _ => eprintln!("USAGE: inspect [units|functions <module>]"),
        }
    }
}
