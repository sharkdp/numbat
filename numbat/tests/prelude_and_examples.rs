mod common;

use common::get_test_context;

use codespan_reporting::term::{self, Config, termcolor::NoColor};
use numbat::diagnostic::{ErrorDiagnostic, ResolverDiagnostic};
use numbat::resolver::CodeSource;
use numbat::{InterpreterResult, NumbatError};

use std::ffi::OsStr;
use std::fs;

use crate::common::get_test_context_without_prelude;

fn assert_runs(code: &str) {
    let result = get_test_context().interpret(code, CodeSource::Internal);
    assert!(result.is_ok(), "Failed with: {result:#?}");
    assert!(matches!(
        result.unwrap().1,
        InterpreterResult::Value(_) | InterpreterResult::Continue
    ));
}

fn assert_runs_without_prelude(code: &str) {
    let result = get_test_context_without_prelude().interpret(code, CodeSource::Internal);
    assert!(result.is_ok(), "Failed with: {}", result.unwrap_err());
    assert!(matches!(
        result.unwrap().1,
        InterpreterResult::Value(_) | InterpreterResult::Continue
    ));
}

fn get_diagnostic_output(code: &str) -> String {
    let mut ctx = get_test_context();
    // Normalize CRLF to LF so that byte offsets in spans are consistent across platforms
    let code = code.replace("\r\n", "\n");
    if let Err(e) = ctx.interpret(&code, CodeSource::Internal) {
        let mut output = NoColor::new(Vec::new());
        let config = Config::default();

        let diagnostics: Vec<_> = match &*e {
            NumbatError::ResolverError(e) => e.diagnostics(),
            NumbatError::NameResolutionError(e) => e.diagnostics(),
            NumbatError::TypeCheckError(e) => e.diagnostics(),
            NumbatError::RuntimeError(e) => {
                let rd = ResolverDiagnostic {
                    resolver: ctx.resolver(),
                    error: e,
                };
                rd.diagnostics()
            }
        };

        for diagnostic in diagnostics {
            term::emit(&mut output, &config, &ctx.resolver().files, &diagnostic).unwrap();
        }
        String::from_utf8(output.into_inner()).unwrap()
    } else {
        panic!("Expected error but code succeeded")
    }
}

fn run_for_each_file(glob_pattern: &str, f: impl Fn(&str)) {
    for entry in glob::glob(glob_pattern).unwrap() {
        let path = entry.unwrap();
        if path.extension() != Some(OsStr::new("nbt")) {
            continue;
        }

        println!("Testing example {path:?}");
        let example_code = fs::read_to_string(path).unwrap();

        f(&example_code);
    }
}

#[test]
fn modules_are_self_consistent() {
    run_for_each_file("modules/**/*.nbt", assert_runs_without_prelude);
}

#[test]
fn numbat_tests_are_executed_successfully() {
    run_for_each_file("../examples/tests/*.nbt", assert_runs);
}

// Insta filter to normalize absolute module paths across platforms.
// Matches paths like /home/.../modules/ or D:\...\modules\ and replaces
// everything up to and including "modules/" or "modules\" with [MODULES_PATH]/.
const MODULE_PATH_FILTER: (&str, &str) = (r"(?m)\S+[/\\]modules[/\\]\S+\.nbt", "[PRELUDE_FILE]");

#[test]
fn parse_error_snapshots() {
    insta::glob!("../../examples/parse_error", "*.nbt", |path| {
        let code = std::fs::read_to_string(path).unwrap();
        let output = get_diagnostic_output(&code);
        insta::with_settings!({filters => vec![MODULE_PATH_FILTER]}, {
            insta::assert_snapshot!(output);
        });
    });
}

#[test]
fn name_resolution_error_snapshots() {
    insta::glob!("../../examples/name_resolution_error", "*.nbt", |path| {
        let code = std::fs::read_to_string(path).unwrap();
        let output = get_diagnostic_output(&code);
        insta::with_settings!({filters => vec![MODULE_PATH_FILTER]}, {
            insta::assert_snapshot!(output);
        });
    });
}

#[test]
fn typecheck_error_snapshots() {
    insta::glob!("../../examples/typecheck_error", "*.nbt", |path| {
        let code = std::fs::read_to_string(path).unwrap();
        let output = get_diagnostic_output(&code);
        insta::with_settings!({filters => vec![MODULE_PATH_FILTER]}, {
            insta::assert_snapshot!(output);
        });
    });
}

#[test]
fn runtime_error_snapshots() {
    insta::glob!("../../examples/runtime_error", "*.nbt", |path| {
        let code = std::fs::read_to_string(path).unwrap();
        let output = get_diagnostic_output(&code);
        insta::with_settings!({filters => vec![MODULE_PATH_FILTER]}, {
            insta::assert_snapshot!(output);
        });
    });
}
