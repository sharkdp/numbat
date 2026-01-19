use compact_str::{CompactString, format_compact};
use itertools::Itertools;
use numbat::InterpreterSettings;
use numbat::markup::plain_text_format;
use numbat::module_importer::FileSystemImporter;
use numbat::resolver::CodeSource;
use numbat::{Context, FunctionInfo};
use std::path::Path;
use std::process::exit;

const AUTO_GENERATED_HINT: &str = "<!-- NOTE! This file is auto-generated -->";

fn inspect_units(ctx: &Context, prelude_ctx: &Context) {
    println!("{AUTO_GENERATED_HINT}

# List of supported units

See also: [Unit notation](../basics/unit-notation.md).

All SI-accepted units support [metric prefixes](https://en.wikipedia.org/wiki/Metric_prefix) (`mm`, `cm`, `km`, ... or `millimeter`, `centimeter`, `kilometer`, ...)
and — where sensible — units allow for [binary prefixes](https://en.wikipedia.org/wiki/Binary_prefix) (`MiB`, `GiB`, ... or `mebibyte`, `gibibyte`, ...).
");

    println!("| Dimension | Unit name | Identifier(s) |");
    println!("| --- | --- | --- |");
    for (ref unit_name, (_base_representation, unit_metadata)) in ctx
        .unit_representations()
        .sorted_by_key(|(u, (_, m))| (m.readable_type.to_string(), u.to_lowercase()))
    {
        let mut names = unit_metadata.aliases.clone();
        names.sort_by_key(|(n, _)| n.to_lowercase());
        let names = names.iter().map(|(n, _)| n).join("`, `");

        let url = unit_metadata.url.clone();
        let name = unit_metadata.name.clone().unwrap_or(unit_name.clone());

        let name_with_url = if let Some(url) = url {
            format_compact!("[{name}]({url})")
        } else {
            name.clone()
        };

        // Check if this unit requires an explicit import
        let code_source = ctx.resolver().get_code_source(unit_metadata.code_source_id);
        let import_note = if let CodeSource::Module(ref module_path, _) = code_source {
            let module_str = module_path.to_string();
            if module_str == "units::currencies" {
                // Don't show import hint for currencies
                String::new()
            } else if !prelude_ctx
                .resolver()
                .imported_modules
                .contains(module_path)
            {
                format!(" <br/> (`use {module_path}`)")
            } else {
                String::new()
            }
        } else {
            String::new()
        };

        let readable_type = unit_metadata.readable_type.clone();

        println!("| `{readable_type}` | {name_with_url} | `{names}`{import_note} |");
    }
}

fn inspect_functions_in_module(ctx: &Context, prelude_ctx: &Context, module: String) {
    for FunctionInfo {
        fn_name,
        name,
        signature_str: signature,
        description,
        url,
        examples,
        code_source,
    } in ctx.functions()
    {
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
            let description = replace_equation_delimiters(description_raw.trim());

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

        for (example_code, example_description) in examples {
            let mut example_ctx = prelude_ctx.clone();
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

            // Use no-op print function to suppress any print output (e.g. from `inspect`)
            let mut settings = InterpreterSettings {
                print_fn: Box::new(|_| {}),
            };
            if let Ok((statements, results)) = example_ctx.interpret_with_settings(
                &mut settings,
                &example_code,
                CodeSource::Internal,
            ) {
                let example_input = extra_import + &example_code;

                // Encode the example url
                let example_url = format!(
                    "https://numbat.dev/?q={}",
                    percent_encoding::utf8_percent_encode(
                        &example_input,
                        percent_encoding::NON_ALPHANUMERIC
                    )
                );

                // Assemble the example output
                let result_markup = results.to_markup(
                    statements.last(),
                    example_ctx.dimension_registry(),
                    true,
                    true,
                );
                let example_output = &plain_text_format(&result_markup, false);

                // Print the example as an admonition
                let title = example_description
                    .map(|d| replace_equation_delimiters(&d).to_string())
                    .unwrap_or_else(|| "Example".to_string());
                println!("!!! example \"{}\"", title);
                println!("    ```nbt");
                for l in example_input.lines() {
                    println!("    {}", l);
                }
                println!();
                for l in example_output.lines() {
                    println!("    {}", l);
                }
                println!("    ```");
                println!(
                    "    [:material-play-circle: Run this example]({}){{ .md-button }}",
                    example_url
                );
                println!();
            } else {
                eprintln!(
                    "Error: Example \"{example_code}\" of function {fn_name} did not run successfully."
                );
                exit(1);
            }
        }
    }
}

// Replace $..$ with \( .. \) for MathJax/arithmatex.
fn replace_equation_delimiters(text_in: &str) -> CompactString {
    let mut text_out = CompactString::with_capacity(text_in.len());
    // TODO: handle \$ in math
    for (i, part) in text_in.split('$').enumerate() {
        if i % 2 == 0 {
            text_out.push_str(part);
        } else {
            text_out.push_str("\\( ");
            text_out.push_str(part);
            text_out.push_str(" \\)");
        }
    }
    text_out
}

fn prepare_context() -> Context {
    let module_path = Path::new(&std::env::var_os("CARGO_MANIFEST_DIR").unwrap()).join("modules");
    let mut importer = FileSystemImporter::default();
    importer.add_path(module_path);
    Context::new(importer)
}

fn main() {
    let mut ctx = prepare_context();
    let _result = ctx.interpret("use all::*", CodeSource::Internal).unwrap();

    let mut example_ctx = prepare_context();
    let _result = example_ctx
        .interpret("use prelude::*", CodeSource::Internal)
        .unwrap();

    let mut args = std::env::args();
    args.next();
    if let Some(arg) = args.next() {
        match arg.as_str() {
            "units" => inspect_units(&ctx, &example_ctx),
            "functions" => {
                let module = args.next().unwrap();
                inspect_functions_in_module(&ctx, &example_ctx, module)
            }
            _ => eprintln!("USAGE: inspect [units|functions <module>]"),
        }
    }
}
