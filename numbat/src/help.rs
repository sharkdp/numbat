/**
 * Print a help, linking the documentation and live-running some examples
 * in an isolated context
 */
use crate::markup as m;
use crate::module_importer::BuiltinModuleImporter;
use crate::pretty_print::PrettyPrint;
use crate::resolver::CodeSource;
use crate::{Context, InterpreterResult, NumbatError};
use crate::{InterpreterSettings, NameResolutionError, Type};

use std::sync::{Arc, Mutex};

/**
 * evaluate an example code in the input context
 *
 * Current limitations
 * - We need the interpreted statement to get the pretty-printed statement,
 *   but that only comes after interpretation. Thus we cannot show an example
 *   where the statement itself prints (i.e. the `print` function) because it
 *   will be printed _before_ the pretty-printed statement.
 */
fn evaluate_example(
    context: &mut Context,
    input: &str,
    to_be_printed: Arc<Mutex<Vec<m::Markup>>>,
    pretty_print: bool,
) -> () {
    let to_be_printed_c = to_be_printed.clone();
    let mut settings = InterpreterSettings {
        print_fn: Box::new(move |s: &m::Markup| {
            to_be_printed_c
                .lock()
                .unwrap()
                .push(m::nl() + m::whitespace("  ") + s.clone() + m::nl());
        }),
    };

    let (result, registry) = {
        let registry = context.dimension_registry().clone(); // TODO: get rid of this clone
        (
            context.interpret_with_settings(&mut settings, input, CodeSource::Internal),
            registry,
        )
    };

    match result {
        Ok((statements, interpreter_result)) => {
            if pretty_print {
                let statements_markup =
                    statements
                        .iter()
                        .fold(m::empty(), |accumulated_mk, statement| {
                            accumulated_mk
                                + m::nl()
                                + m::whitespace("  ")
                                + statement.pretty_print()
                                + m::nl()
                        });
                to_be_printed.lock().unwrap().push(statements_markup);
            }

            match interpreter_result {
                InterpreterResult::Value(value) => {
                    let type_ = statements.last().map_or(m::empty(), |s| {
                        if let crate::Statement::Expression(e) = s {
                            let type_ = e.get_type();

                            if type_ == Type::scalar() {
                                m::empty()
                            } else {
                                m::dimmed("    [")
                                    + e.get_type().to_readable_type(&registry)
                                    + m::dimmed("]")
                            }
                        } else {
                            m::empty()
                        }
                    });

                    let q_markup = m::nl()
                        + m::whitespace("    ")
                        + m::operator("=")
                        + m::space()
                        + value.pretty_print()
                        + type_
                        + m::nl();
                    to_be_printed.lock().unwrap().push(q_markup);
                }
                InterpreterResult::Continue => (),
                InterpreterResult::Exit(_exit_status) => {
                    println!("Interpretation Error.");
                }
            }
        }
        Err(NumbatError::ResolverError(e)) => {
            context.print_diagnostic(e.clone());
        }
        Err(NumbatError::NameResolutionError(
            e @ (NameResolutionError::IdentifierClash { .. }
            | NameResolutionError::ReservedIdentifier(_)),
        )) => {
            context.print_diagnostic(e);
        }
        Err(NumbatError::TypeCheckError(e)) => {
            context.print_diagnostic(e);
        }
        Err(NumbatError::RuntimeError(e)) => {
            context.print_diagnostic(e);
        }
    }
}

pub fn help_markup(pretty_print: bool) -> m::Markup {
    let output = Arc::new(Mutex::new(vec![
        m::nl()
            + m::keyword("numbat")
            + m::space()
            + m::text(env!("CARGO_PKG_DESCRIPTION"))
            + m::nl()
            + m::text("You can start by trying one of the examples:")
            + m::nl(),
    ]));

    let examples = vec![
        "8 km / (1 h + 25 min)",
        "atan2(30 cm, 1 m) -> deg",
        "let ω = 2 π c / 660 cm",
        "# Energy of red photons",
        "ℏ ω -> eV",
    ];
    let mut example_context = Context::new(BuiltinModuleImporter::default());
    evaluate_example(&mut example_context, "use prelude", output.clone(), false);
    for example in examples.iter() {
        output
            .lock()
            .unwrap()
            .push(m::text(">>> ") + m::text(example) + m::nl());
        evaluate_example(&mut example_context, example, output.clone(), pretty_print);
        output.lock().unwrap().push(m::nl());
    }
    output.lock().unwrap().push(
        m::text("Full documentation:")
            + m::space()
            + m::keyword("https://numbat.dev/doc/")
            + m::nl()
            + m::nl(),
    );
    let markup = output.lock().unwrap().to_vec().into_iter().sum();
    markup
}
