/// Print a help, linking the documentation, and live-running some examples
/// in an isolated context.
use crate::markup as m;
use crate::module_importer::BuiltinModuleImporter;
use crate::resolver::CodeSource;
use crate::Context;
use crate::InterpreterSettings;

use std::sync::{Arc, Mutex};

fn evaluate_example(context: &mut Context, input: &str) -> m::Markup {
    let statement_output: Arc<Mutex<Vec<m::Markup>>> = Arc::new(Mutex::new(vec![]));
    let statement_output_c = statement_output.clone();
    let mut settings = InterpreterSettings {
        print_fn: Box::new(move |s: &m::Markup| {
            statement_output_c.lock().unwrap().push(s.clone());
        }),
    };

    let (statements, interpreter_result) = context
        .interpret_with_settings(&mut settings, input, CodeSource::Internal)
        .expect("No error in 'help' examples");

    let markup =
        statement_output
            .lock()
            .unwrap()
            .iter()
            .fold(m::empty(), |accumulated_mk, single_line| {
                accumulated_mk + m::nl() + m::whitespace("  ") + single_line.clone() + m::nl()
            })
            + interpreter_result.to_markup(
                statements.last(),
                context.dimension_registry(),
                true,
                true,
            );

    markup
}

pub fn help_markup() -> m::Markup {
    let mut output = m::nl()
        + m::text("Numbat is a statically typed programming language for scientific computations")
        + m::nl()
        + m::text("with first class support for physical dimensions and units. Please refer to")
        + m::nl()
        + m::text("the full documentation online at ")
        + m::string("https://numbat.dev/doc/")
        + m::text(" or try one of these ")
        + m::nl()
        + m::text("examples:")
        + m::nl()
        + m::nl();

    let examples = [
        "8 km / (1 h + 25 min)",
        "atan2(30 cm, 1 m) -> deg",
        "let ω = 2 π c / 660 nm",
        r#"print("Energy of red photons: {ℏ ω -> eV}")"#,
    ];
    let mut example_context = Context::new(BuiltinModuleImporter::default());
    let _use_prelude_output = evaluate_example(&mut example_context, "use prelude");
    for example in examples.iter() {
        output += m::text(">>> ") + m::text(*example) + m::nl();
        output += evaluate_example(&mut example_context, example) + m::nl();
    }
    output
}
