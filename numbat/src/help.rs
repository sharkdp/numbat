use crate::command::HelpKind;
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

fn basic_help_markup() -> m::Markup {
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

    output += m::text("Numbat supports a number of commands. Use ")
        + m::string("help commands")
        + m::text(" for more info.")
        + m::nl();

    output
}

fn all_commands_markup() -> m::Markup {
    fn m_cmd(cmd: &'static str) -> m::Markup {
        m::keyword(cmd)
    }

    fn m_arg(arg: &'static str) -> m::Markup {
        m::value(arg)
    }

    fn cmd(
        cmd: &'static str,
        args: impl AsRef<[&'static str]>,
        aliases: impl AsRef<[&'static str]>,
        help: &'static str,
    ) -> m::Markup {
        cmd_fmt(cmd, args, aliases, m::text(help))
    }

    fn cmd_fmt(
        cmd: &'static str,
        args: impl AsRef<[&'static str]>,
        aliases: impl AsRef<[&'static str]>,
        help: m::Markup,
    ) -> m::Markup {
        let indent = m::text("  ");
        let mut output = indent.clone();
        output += m_cmd(cmd);
        for arg in args.as_ref() {
            output += m::space();
            output += m_arg(arg)
        }

        output += m::text(": ");
        output += help;

        let aliases = aliases.as_ref();
        let mut aliases_iter = aliases.iter();
        if let Some(first_alias) = aliases_iter.next() {
            if aliases.len() == 1 {
                output += m::text(" (alias: ");
            } else {
                output += m::text(" (aliases: ");
            }
            output += m_cmd(first_alias);

            for alias in aliases_iter {
                output += m::text(", ");
                output += m_cmd(alias);
            }
            output += m::text(")");
        }

        output += m::nl();

        output
    }

    m::nl()
        + m::text("Numbat supports the following commands:")
        + m::nl()
        + cmd("help", [], ["?"], "show a basic introduction to Numbat")
        + cmd(
            "help",
            ["commands"],
            ["?"],
            "show the list of Numbat commands and information about them",
        )
        + cmd(
            "info",
            ["<identifier>"],
            [],
            "get more info about a particular item, such as a function, variable, or unit",
        )
        + cmd("list", [], [], "show all currently defined items")
        + cmd_fmt(
            "list",
            ["<what>"],
            [],
            m::text("show all currently defined items of the specified type, where ")
                + m_arg("<what>")
                + m::text(" is one of ")
                + m_arg("functions")
                + m::text(", ")
                + m_arg("definitions")
                + m::text(", ")
                + m_arg("variables")
                + m::text(", or ")
                + m_arg("units"),
        )
        + cmd("clear", [], [], "clear the current screen contents")
        + cmd_fmt(
            "save",
            [],
            [],
            m::text("save the current session history to ")
                + m_arg("history.nbt")
                + m::text(" in the current directory"),
        )
        + cmd_fmt(
            "save",
            ["<dst>"],
            [],
            m::text("save the current session history to file ")
                + m_arg("<dst>")
                + m::text("; the recommended file extension is ")
                + m::string(".nbt"),
        )
        + cmd("exit", [], ["quit"], "exit Numbat")
}

pub fn help_markup(help_kind: HelpKind) -> m::Markup {
    match help_kind {
        HelpKind::BasicHelp => basic_help_markup(),
        HelpKind::AllCommands => all_commands_markup(),
    }
}
