mod common;

use common::get_test_context;

use numbat::markup::{Formatter, PlainTextFormatter};
use numbat::resolver::CodeSource;
use numbat::{pretty_print::PrettyPrint, Context, InterpreterResult};

fn expect_output_with_context(ctx: &mut Context, code: &str, expected_output: &str) {
    if let InterpreterResult::Quantity(q) = ctx.interpret(code, CodeSource::Text).unwrap().1 {
        let fmt = PlainTextFormatter {};

        let actual_output = fmt.format(&q.pretty_print(), false);
        assert_eq!(actual_output.trim(), expected_output);
    } else {
        panic!();
    }
}

fn expect_output(code: &str, expected_output: &str) {
    let mut ctx = get_test_context();
    expect_output_with_context(&mut ctx, code, expected_output)
}

fn expect_failure(code: &str, msg_part: &str) {
    let mut ctx = get_test_context();
    if let Err(e) = ctx.interpret(code, CodeSource::Text) {
        let error_message = e.to_string();
        println!("{}", error_message);
        assert!(error_message.contains(msg_part));
    } else {
        panic!();
    }
}

#[test]
fn test_exponentiation() {
    expect_output("3²*2", "18");
    expect_output("3² 2", "18");
    expect_output("3²·2", "18");
    expect_output("3³*2", "54");
    expect_output("3³(2)", "54");
    expect_output("(1+2)²", "9");
    expect_output("2²pi", "12.566371"); // TODO: when we support significant digits, this should be 12.5664
    expect_output("2² pi", "12.566371");
    expect_output("2²·pi", "12.566371");
    expect_output("5m² to cm·m", "500 cm·m");
    expect_output("2⁵", "32");
    expect_output("-4¹", "-4");
    expect_output("2⁻¹", "0.5");
    expect_output("2⁻²", "0.25");
    expect_output("10⁻⁵", "0.00001");
}

#[test]
fn test_conversions() {
    expect_output("2in to cm", "5.08 cm");
    expect_output("5m^2 -> m*cm", "500 m·cm");
    expect_output("5m^2 -> cm*m", "500 cm·m");
    expect_output("1 kB / 10 ms -> MB/s", "0.1 MB/s");
}

#[test]
fn test_implicit_conversion() {
    let mut ctx = get_test_context();

    let _ = ctx.interpret("let x = 5 m", CodeSource::Text).unwrap();

    expect_output_with_context(&mut ctx, "x", "5 m");
    expect_output_with_context(&mut ctx, "2x", "10 m");
    expect_output_with_context(&mut ctx, "2 x", "10 m");
    expect_output_with_context(&mut ctx, "x x", "25 m²");
    expect_output_with_context(&mut ctx, "x²", "25 m²");

    expect_failure("x2", "Unknown identifier 'x2'");
}

#[test]
fn test_function_inverses() {
    expect_output("sin(asin(0.1234))", "0.1234");
    expect_output("cos(acos(0.1234))", "0.1234");
    expect_output("tan(atan(0.1234))", "0.1234");
    expect_output("sinh(asinh(0.1234))", "0.1234");
    expect_output("cosh(acosh(1.1234))", "1.1234");
    expect_output("tanh(atanh(0.1234))", "0.1234");
    expect_output("log(exp(0.1234))", "0.1234");
    expect_output("log10(10^0.1234)", "0.1234");
    expect_output("log2(2^0.1234)", "0.1234");
    expect_output("sqr(sqrt(0.1234))", "0.1234");

    expect_output("asin(sin(0.1234))", "0.1234");
    expect_output("acos(cos(0.1234))", "0.1234");
    expect_output("atan(tan(0.1234))", "0.1234");
    expect_output("asinh(sinh(0.1234))", "0.1234");
    expect_output("acosh(cosh(1.1234))", "1.1234");
    expect_output("atanh(tanh(0.1234))", "0.1234");
    expect_output("exp(log(0.1234))", "0.1234");
    expect_output("10^(log10(0.1234))", "0.1234");
    expect_output("2^(log2(0.1234))", "0.1234");
    expect_output("sqrt(sqr(0.1234))", "0.1234");
}

#[test]
fn test_math() {
    expect_output("sin(90°)", "1");
    expect_output("sin(30°)", "0.5");
    expect_output("sin(pi/2)", "1");

    expect_output("atan2(10, 0) / (pi / 2)", "1");
    expect_output("atan2(100 cm, 1 m) / (pi / 4)", "1");
    expect_failure(
        "atan2(100 cm, 1 m²)",
        "parameter type: Length\n argument type: Length²",
    );

    expect_output("5 % 3", "2");
    expect_output("-1 % 4", "3");
    expect_output("8 cm % 5 cm", "3 cm");
    expect_output("235 cm % 1 m", "35 cm");
    expect_output("2 m % 7 cm", "0.04 m");
    expect_failure("8 m % 5 s", "parameter type: Length\n argument type: Time")
}

#[test]
fn test_temperature_conversions() {
    expect_output("from_celsius(11.5)", "284.65 K");
    expect_output("from_fahrenheit(89.3)", "304.983333 K");
    expect_output("to_celsius(0 K)", "-273.15");
    expect_output("to_fahrenheit(30 K)", "-405.67");
    expect_output("to_celsius(from_celsius(100))", "100");
    expect_output("to_fahrenheit(from_fahrenheit(100))", "100.0");
    expect_output("from_celsius(to_celsius(123 K))", "123 K");
    expect_output("from_fahrenheit(to_fahrenheit(123 K))", "123.0 K");

    expect_output("-40 // from_fahrenheit // to_celsius", "-40.0");
}

#[test]
fn test_other_functions() {
    expect_output("sqrt(4)", "2");
    expect_output("log10(100000)", "5");
    expect_output("log(e^15)", "15");
    expect_output("ln(e^15)", "15");
    expect_output("ceil(3.1)", "4");
    expect_output("floor(3.9)", "3");
    expect_output("round(3.9)", "4");
    expect_output("round(3.1)", "3");
}

#[test]
fn test_last_result_identifier() {
    let mut ctx = get_test_context();

    let _ = ctx.interpret("2 + 3", CodeSource::Text).unwrap();
    expect_output_with_context(&mut ctx, "ans", "5");

    let _ = ctx.interpret("1 + 2", CodeSource::Text).unwrap();
    expect_output_with_context(&mut ctx, "_", "3");
}

#[test]
fn test_misc_examples() {
    expect_output("1920/16*9", "1080");
    expect_output("2^32", "4294967296");
    expect_output("sqrt(1.4^2 + 1.5^2) * cos(pi/3)^2", "0.512957");

    expect_output("2min + 30s", "2.5 min");
    expect_output("2min + 30s -> sec", "150 s");
    expect_output("4/3 * pi * (6000km)³", "904778684233.860352 km³"); // TODO: insect prints this as 904779000000 km³ (sign. digits)
    expect_output("40kg * 9.8m/s^2 * 150cm", "588 m²·kg/s²");
    expect_output("sin(30°)", "0.5");

    expect_output("60mph -> m/s", "26.8224 m/s");
    expect_output("240km/day -> km/h", "10.0 km/h");
    expect_output("1mrad -> °", "0.057296°");
    expect_output("52weeks -> days", "364 day");
    expect_output("5in + 2ft -> cm", "73.66 cm");
    expect_output("atan(30cm / 2m) -> deg", "8.530766°");
    expect_output("6Mbit/s * 1.5h -> GB", "4.05 GB");
    expect_output("6Mbit/s * 1.5h -> GiB", "3.771856 GiB");

    expect_output("3m/4m", "0.75");
    expect_output("4/2*2", "4");
    expect_output("1/2 Hz -> s", "0.5 s");
}

#[test]
fn test_full_simplify() {
    expect_output("5 cm/m", "0.05");
    expect_output("hour/second", "3600");

    expect_output("5 to cm/m", "500 cm/m");
    expect_output(
        "fn f(x: Scalar) -> Scalar = x to cm/m
             f(5)",
        "500 cm/m",
    );
}

#[test]
fn test_prefixes() {
    expect_output("hertz second", "1");
    expect_output("kilohertz millisecond", "1");
    expect_output("megahertz microsecond", "1");
    expect_output("gigahertz nanosecond", "1");
    expect_output("terahertz picosecond", "1");
    expect_output("petahertz femtosecond", "1");
    expect_output("exahertz attosecond", "1");
    expect_output("zettahertz zeptosecond", "1.0"); // TODO: this might be "resolved" when we use a high-precision float. type
    expect_output("yottahertz yoctosecond", "1");
    expect_output("ronnahertz rontosecond", "1");
    expect_output("quettahertz quectosecond", "1.0"); // TODO: this might be "resolved" when we use a high-precision float. type
}

#[test]
fn test_error_messages() {
    expect_failure(
        "3kg+",
        "Expected one of: number, identifier, parenthesized expression",
    );
    expect_failure("let kg=2", "Identifier is already in use: 'kg'");
    expect_failure("let pi=2", "Identifier is already in use: 'pi'");
    expect_failure("let sin=2", "Identifier is already in use: 'sin'");
    expect_failure("let print=2", "Expected identifier after 'let' keyword");
    expect_failure("fn kg(x: Scalar) = 1", "Identifier is already in use: 'kg'");
    expect_failure("fn pi(x: Scalar) = 1", "Identifier is already in use: 'pi'");
    expect_failure(
        "fn sin(x: Scalar) = 1",
        "Identifier is already in use: 'sin'",
    );
    expect_failure(
        "fn print(x: Scalar) = 1",
        "Expected identifier after 'fn' keyword",
    );
    expect_failure("foo", "Unknown identifier 'foo'");
    expect_failure("1/0", "Division by zero");
}
