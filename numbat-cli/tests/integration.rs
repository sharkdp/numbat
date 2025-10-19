use std::path::Path;

use assert_cmd::Command;
use predicates::boolean::PredicateBooleanExt;

fn numbat() -> Command {
    let module_path = Path::new(&std::env::var_os("CARGO_MANIFEST_DIR").unwrap())
        .parent()
        .unwrap()
        .join("numbat")
        .join("modules");
    unsafe {
        std::env::set_var("NUMBAT_MODULES_PATH", module_path);
    }

    let mut cmd = Command::cargo_bin("numbat").unwrap();
    cmd.arg("--no-init");
    cmd.arg("--no-config");
    cmd
}

#[test]
fn pass_expression_on_command_line() {
    numbat()
        .arg("--expression")
        .arg("2 meter + 3 meter")
        .assert()
        .success()
        .stdout(predicates::str::contains("5 m"));

    numbat()
        .arg("-e")
        .arg("let x = 2")
        .arg("-e")
        .arg("x^3")
        .assert()
        .success()
        .stdout(predicates::str::contains("8"));

    numbat()
        .arg("--expression")
        .arg("2 +/ 3")
        .assert()
        .stderr(predicates::str::contains("while parsing"));

    numbat()
        .arg("--expression")
        .arg("2 meter + 3 second")
        .assert()
        .failure()
        .stderr(predicates::str::contains("while type checking"));

    numbat()
        .arg("--expression")
        .arg("1/0")
        .assert()
        .failure()
        .stderr(predicates::str::contains("runtime error"));

    numbat()
        .arg("--expression")
        .arg("type(2 m/s)")
        .assert()
        .success()
        .stdout(predicates::str::contains("Length / Time"));
}

#[test]
fn read_code_from_file() {
    numbat()
        .arg("tests/examples/pendulum.nbt")
        .assert()
        .success();

    numbat()
        .arg("tests/examples/parser_error.nbt")
        .assert()
        .failure()
        .stderr(predicates::str::contains("while parsing"));
}

#[test]
fn process_code_from_file_and_cli_expression() {
    numbat()
        .arg("tests/examples/pendulum.nbt")
        .arg("--expression")
        .arg("oscillation_time(20cm)")
        .assert()
        .success();

    numbat()
        .arg("tests/examples/pendulum.nbt")
        .arg("--expression")
        .arg("oscillation_time(20kg)")
        .assert()
        .failure()
        .stderr(predicates::str::contains("while type checking"));
}

#[test]
fn print_calls() {
    numbat()
        .arg("tests/examples/print.nbt")
        .assert()
        .success()
        .stdout(predicates::str::contains(
            "1\n2 m\nhello world\npi = 3.14159\n1 + 2 = 3\n",
        ));
}

#[test]
fn without_prelude() {
    numbat()
        .arg("--no-prelude")
        .arg("--expression")
        .arg("2.1 + 3.1")
        .assert()
        .success()
        .stdout(predicates::str::contains("5.2"));

    numbat()
        .arg("--no-prelude")
        .arg("--expression")
        .arg("1 meter")
        .assert()
        .failure()
        .stderr(predicates::str::contains("unknown identifier"));
}

#[test]
fn pretty_printing() {
    numbat()
        .arg("--pretty-print=always")
        .arg("--expression")
        .arg("let v=30km/h")
        .assert()
        .success()
        .stdout(predicates::str::contains(
            "let v: Velocity = 30 kilometre / hour",
        ));
}

#[test]
fn help_text() {
    numbat()
        .write_stdin("help")
        .assert()
        .success()
        .stdout(predicates::str::contains(
            "Energy of red photons: 1.87855 eV",
        ));
}

#[test]
fn info_text() {
    numbat().write_stdin("info g0").assert().success().stdout(
        predicates::str::contains("Standard acceleration of gravity on earth")
            .and(predicates::str::contains("9.80665 m/s²")),
    );

    numbat().write_stdin("info C").assert().success().stdout(
        predicates::str::contains("Coulomb").and(predicates::str::contains("1 coulomb = ")),
    );

    numbat()
        .write_stdin("info round")
        .assert()
        .success()
        .stdout(
            predicates::str::contains("Round")
                .and(predicates::str::contains("Round to the nearest integer.")),
        );
}
