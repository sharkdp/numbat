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

fn evaluate_example(context: &mut Context, input: &str, pretty_print: bool) -> m::Markup {
    let statement_output: Arc<Mutex<Vec<m::Markup>>> = Arc::new(Mutex::new(vec![]));
    let statement_output_c = statement_output.clone();
    let mut settings = InterpreterSettings {
        print_fn: Box::new(move |s: &m::Markup| {
            statement_output_c.lock().unwrap().push(s.clone());
        }),
    };

    let (result, registry) = {
        let registry = context.dimension_registry().clone(); // TODO: get rid of this clone
        (
            context.interpret_with_settings(&mut settings, input, CodeSource::Internal),
            registry,
        )
    };

    let mut full_output = m::empty();

    match result {
        Ok((statements, interpreter_result)) => {
            if pretty_print {
                full_output += statements
                    .iter()
                    .fold(m::empty(), |accumulated_mk, statement| {
                        accumulated_mk
                            + m::nl()
                            + m::whitespace("  ")
                            + statement.pretty_print()
                            + m::nl()
                    });
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

                    full_output += statement_output.lock().unwrap().iter().fold(
                        m::empty(),
                        |accumulated_mk, single_line| {
                            accumulated_mk
                                + m::nl()
                                + m::whitespace("  ")
                                + single_line.clone()
                                + m::nl()
                        },
                    ) + m::nl()
                        + m::whitespace("    ")
                        + m::operator("=")
                        + m::space()
                        + value.pretty_print()
                        + type_
                        + m::nl();
                }
                InterpreterResult::Continue => {
                    full_output += statement_output.lock().unwrap().iter().fold(
                        m::empty(),
                        |accumulated_mk, single_line| {
                            accumulated_mk
                                + m::nl()
                                + m::whitespace("  ")
                                + single_line.clone()
                                + m::nl()
                        },
                    );
                }
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

    full_output
}

pub fn help_markup(pretty_print: bool) -> m::Markup {
    let mut output = m::nl()
        + m::keyword("numbat")
        + m::space()
        + m::text(env!("CARGO_PKG_DESCRIPTION"))
        + m::nl()
        + m::text("You can start by trying one of the examples:")
        + m::nl();

    let examples = vec![
        "8 km / (1 h + 25 min)",
        "atan2(30 cm, 1 m) -> deg",
        "let ω = 2 π c / 660 cm",
        r#"print("Energy of red photons: {ℏ ω -> eV}")"#,
    ];
    let mut example_context = Context::new(BuiltinModuleImporter::default());
    let _use_prelude_output = evaluate_example(&mut example_context, "use prelude", false);
    for example in examples.iter() {
        output += m::text(">>> ") + m::text(example) + m::nl();
        output += evaluate_example(&mut example_context, example, pretty_print);
        output += m::nl();
    }
    output += m::text("Full documentation:")
        + m::space()
        + m::keyword("https://numbat.dev/doc/")
        + m::nl();
    output
}
