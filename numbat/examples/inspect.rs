use itertools::Itertools;
use numbat::markup::plain_text_format;
use numbat::module_importer::FileSystemImporter;
use numbat::pretty_print::PrettyPrint;
use numbat::resolver::CodeSource;
use numbat::Context;
use percent_encoding;
use std::path::Path;

const AUTO_GENERATED_HINT: &str = "<!-- NOTE! This file is auto-generated -->";

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
    for (fn_name, name, signature, description, url, examples, code_source) in ctx.functions() {
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
            let description = replace_equation_delimiters(description_raw.trim().to_string());

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

        if !examples.is_empty() {
            println!("<details>");
            println!("<summary>Examples</summary>");
            println!();
        }

        for (example_code, example_description) in examples {
            let mut example_ctx = prepare_context();
            let _result = example_ctx
                .interpret("use prelude", CodeSource::Internal)
                .unwrap();

            let extra_import = if !example_ctx
                .resolver()
                .imported_modules
                .contains(&module_path)
            {
                format!("use {}\n", module)
            } else {
                "".into()
            };
            let _result = example_ctx
                .interpret(&extra_import, CodeSource::Internal)
                .unwrap();

            if let Ok((statements, results)) =
                example_ctx.interpret(&example_code, CodeSource::Internal)
            {
                //Format the example input
                let example_input = format!(">>> {}", example_code);

                //Encode the example url
                let url_code = extra_import + &example_code;
                let example_url = format!(
                    "https://numbat.dev/?q={}",
                    percent_encoding::utf8_percent_encode(
                        &url_code,
                        percent_encoding::NON_ALPHANUMERIC
                    )
                );

                //Assemble the example output
                let mut example_output = String::new();
                example_output += "\n";

                for statement in &statements {
                    example_output += &plain_text_format(&statement.pretty_print(), true);
                    example_output += "\n\n";
                }

                let result_markup = results.to_markup(
                    statements.last(),
                    &example_ctx.dimension_registry(),
                    true,
                    true,
                );
                example_output += &plain_text_format(&result_markup, false);

                if results.is_value() {
                    example_output += "\n";
                }

                //Print the example
                if let Some(example_description) = example_description {
                    println!(
                        "* {}\n\n  <a href=\"{}\"><i class=\"fa fa-play\"></i> Run this example</a>",
                        replace_equation_delimiters(example_description),
                        example_url
                    );
                } else {
                    println!(
                        "* <a href=\"{}\"><i class=\"fa fa-play\"></i> Run this example</a>\n",
                        example_url
                    );
                }

                println!("  ```nbt");
                for l in example_input.lines() {
                    println!("    {}", l);
                }
                for l in example_output.lines() {
                    println!("    {}", l);
                }
                println!("  ```");
            } else {
                eprintln!(
                        "Warning: Example \"{example_code}\" of function {fn_name} did not run successfully."
                    );
            }
        }
        println!("</details>");
        println!();
    }
}

// Replace $..$ with \\( .. \\) for mdbook.
fn replace_equation_delimiters(text_in: String) -> String {
    let mut text_out = String::new();
    for (i, part) in text_in.split('$').enumerate() {
        if i % 2 == 0 {
            text_out.push_str(part);
        } else {
            text_out.push_str("\\\\( ");
            text_out.push_str(part);
            text_out.push_str(" \\\\)");
        }
    }
    return text_out;
}

fn prepare_context() -> Context {
    let module_path = Path::new(&std::env::var_os("CARGO_MANIFEST_DIR").unwrap()).join("modules");
    let mut importer = FileSystemImporter::default();
    importer.add_path(module_path);
    return Context::new(importer);
}

fn main() {
    let mut ctx = prepare_context();
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
