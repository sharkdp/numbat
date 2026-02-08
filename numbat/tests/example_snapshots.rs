mod common;

use std::sync::{Arc, Mutex};

use common::get_test_context;
use numbat::InterpreterResult;
use numbat::InterpreterSettings;
use numbat::markup::{Formatter, PlainTextFormatter};
use numbat::resolver::CodeSource;

fn run_example(code: &str) -> String {
    let mut ctx = get_test_context();

    let output: Arc<Mutex<Vec<String>>> = Arc::new(Mutex::new(vec![]));
    let output_clone = output.clone();

    let mut settings = InterpreterSettings {
        print_fn: Box::new(move |markup| {
            let fmt = PlainTextFormatter {};
            let text = fmt.format(markup, false);
            output_clone.lock().unwrap().push(text.to_string());
        }),
    };

    let result = ctx.interpret_with_settings(&mut settings, code, CodeSource::Internal);

    let mut lines = output.lock().unwrap().clone();

    match result {
        Ok((_, InterpreterResult::Value(val))) => {
            let fmt = PlainTextFormatter {};
            let result_text = fmt.format(&val.pretty_print(), false);
            lines.push(format!("    = {}", result_text.trim()));
        }
        Ok((_, InterpreterResult::Continue)) => {
            // No final value to display
        }
        Err(e) => {
            lines.push(format!("Error: {e}"));
        }
    }

    lines.join("\n")
}

#[test]
fn example_snapshots() {
    insta::glob!("../../examples", "*.nbt", |path| {
        let code = std::fs::read_to_string(path).unwrap();
        let output = run_example(&code);

        let file_name = path.file_stem().unwrap().to_str().unwrap();
        insta::assert_snapshot!(file_name, output);
    });
}
