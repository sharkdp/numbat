use std::path::Path;

use assert_cmd::Command;

fn numbat() -> Command {
    let module_path = Path::new(&std::env::var_os("CARGO_MANIFEST_DIR").unwrap()).join("modules");
    std::env::set_var("NUMBAT_MODULES_PATH", module_path);

    let mut cmd = Command::cargo_bin("numbat").unwrap();
    cmd.arg("--no-init");
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
fn print_calls() {
    numbat()
        .arg("tests/examples/print.nbt")
        .assert()
        .success()
        .stdout(predicates::str::contains(
            "1 \n2 m\nhello world\npi = 3.14159\n1 + 2 = 3\n",
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
