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
        // .failure()   TODO
        .stderr(predicates::str::contains("Parse error"));
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
        // .failure()    TODO
        .stderr(predicates::str::contains("Parse error"));
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
