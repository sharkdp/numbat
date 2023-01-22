use assert_cmd::Command;

fn insect() -> Command {
    Command::cargo_bin("insect").unwrap()
}

#[test]
fn pass_expression_on_command_line() {
    insect()
        .arg("--expression")
        .arg("2 meter + 3 meter")
        .assert()
        .success()
        .stdout(predicates::str::contains("5.000000 meter"));

    insect()
        .arg("--expression")
        .arg("2 ++ 3")
        .assert()
        .failure()
        .stderr(predicates::str::contains("Parse error"));

    insect()
        .arg("--expression")
        .arg("2 meter + 3 second")
        .assert()
        .failure()
        .stderr(predicates::str::contains("Type check error"));

    insect()
        .arg("--expression")
        .arg("1/0")
        .assert()
        .failure()
        .stderr(predicates::str::contains("Runtime error"));
}

#[test]
fn read_code_from_file() {
    insect()
        .arg("../examples/pendulum.ins")
        .assert()
        .success()
        .stdout(predicates::str::contains("1.098956 second"));

    insect()
        .arg("../examples/parse_error/trailing_characters.ins")
        .assert()
        .failure()
        .stderr(predicates::str::contains("Parse error"));
}

#[test]
fn print_calls() {
    insect()
        .arg("../examples/print.ins")
        .assert()
        .success()
        .stdout(predicates::str::contains("1.000000 \n2.000000 meter^1"));
}

#[test]
fn assert_eq_procedure() {
    insect()
        .arg("../examples/assert_eq_success.ins")
        .assert()
        .success();

    insect()
        .arg("../examples/assert_eq_failure.ins")
        .assert()
        .failure()
        .stderr(predicates::str::contains(
            "the following two quantities are not the same",
        ));
}

#[test]
fn without_prelude() {
    insect()
        .arg("--no-prelude")
        .arg("--expression")
        .arg("2 + 3")
        .assert()
        .success()
        .stdout(predicates::str::contains("5.000000"));
}
