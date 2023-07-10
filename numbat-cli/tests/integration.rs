use assert_cmd::Command;

fn numbat() -> Command {
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
        .arg("--expression")
        .arg("2 ++ 3")
        .assert()
        .failure()
        .stderr(predicates::str::contains("while parsing"));

    numbat()
        .arg("--expression")
        .arg("2 meter + 3 second")
        .assert()
        .failure()
        .stderr(predicates::str::contains("Type check error"));

    numbat()
        .arg("--expression")
        .arg("1/0")
        .assert()
        .failure()
        .stderr(predicates::str::contains("runtime error"));
}

#[test]
fn read_code_from_file() {
    numbat().arg("../examples/pendulum.nbt").assert().success();

    numbat()
        .arg("../examples/parse_error/trailing_characters.nbt")
        .assert()
        .failure()
        .stderr(predicates::str::contains("while parsing"));
}

#[test]
fn print_calls() {
    numbat()
        .arg("../examples/print.nbt")
        .assert()
        .success()
        .stdout(predicates::str::contains("1 \n2 m"));
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
