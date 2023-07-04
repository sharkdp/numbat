use numbat::markup::{Formatter, PlainTextFormatter};
use numbat::{pretty_print::PrettyPrint, Context, InterpreterResult};

fn expect_output_with_context(ctx: &mut Context, code: &str, expected_output: &str) {
    if let InterpreterResult::Quantity(q) = ctx.interpret(code).unwrap().1 {
        let fmt = PlainTextFormatter {};

        let actual_output = fmt.format(&q.pretty_print(), false);
        assert_eq!(actual_output.trim(), expected_output);
    } else {
        panic!();
    }
}

fn expect_output(code: &str, expected_output: &str) {
    let mut ctx = Context::new(false);
    expect_output_with_context(&mut ctx, code, expected_output)
}

fn expect_failure(code: &str) {
    let mut ctx = Context::new(false);
    assert!(ctx.interpret(code).is_err());
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
    let mut ctx = Context::new(false);

    let _ = ctx.interpret("let x = 5 m").unwrap();

    expect_output_with_context(&mut ctx, "x", "5 m");
    expect_output_with_context(&mut ctx, "2x", "10 m");
    expect_output_with_context(&mut ctx, "2 x", "10 m");
    expect_output_with_context(&mut ctx, "x x", "25 m²");
    expect_output_with_context(&mut ctx, "x²", "25 m²");

    expect_failure("x2");
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

    expect_output("asin(sin(0.1234))", "0.1234");
    expect_output("acos(cos(0.1234))", "0.1234");
    expect_output("atan(tan(0.1234))", "0.1234");
    expect_output("asinh(sinh(0.1234))", "0.1234");
    expect_output("acosh(cosh(1.1234))", "1.1234");
    expect_output("atanh(tanh(0.1234))", "0.1234");
    expect_output("exp(log(0.1234))", "0.1234");
}

#[test]
fn test_temperature_conversions() {
    expect_output("fromCelsius(11.5)", "284.65 K");
    expect_output("fromFahrenheit(89.3)", "304.983333 K");
    expect_output("toCelsius(0 K)", "-273.15");
    expect_output("toFahrenheit(30 K)", "-405.67");
    expect_output("toCelsius(fromCelsius(100))", "100");
    expect_output("toFahrenheit(fromFahrenheit(100))", "100.0");
    expect_output("fromCelsius(toCelsius(123 K))", "123 K");
    expect_output("fromFahrenheit(toFahrenheit(123 K))", "123.0 K");

    expect_output("-40 // fromFahrenheit // toCelsius", "-40.0");
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
    let mut ctx = Context::new(false);

    let _ = ctx.interpret("2 + 3").unwrap();
    expect_output_with_context(&mut ctx, "ans", "5");

    let _ = ctx.interpret("1 + 2").unwrap();
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
    expect_output("1mrad -> °", "0.057296 deg"); // TODO: do we want that to be "x.y°"?
    expect_output("52weeks -> days", "364 day");
    expect_output("5in + 2ft -> cm", "73.66 cm");
    expect_output("atan(30cm / 2m) -> °", "8.530766 deg");
    expect_output("6Mbit/s * 1.5h -> GB", "4.05 GB");
    expect_output("6Mbit/s * 1.5h -> GiB", "3.771856 GiB");

    expect_output("3m/4m", "0.75");
    expect_output("4/2*2", "4");
    expect_output("1/2 Hz -> s", "0.5 s");
}
