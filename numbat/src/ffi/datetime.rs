use jiff::Span;
use jiff::Timestamp;
use jiff::Zoned;
use num_traits::ToPrimitive;

use super::macros::*;
use super::Args;
use super::Result;
use crate::datetime;
use crate::quantity::Quantity;
use crate::value::FunctionReference;
use crate::value::Value;
use crate::RuntimeError;

use std::fmt::Write;

pub fn now(_args: Args) -> Result<Value> {
    return_datetime!(Zoned::now())
}

pub fn datetime(mut args: Args) -> Result<Value> {
    let input = string_arg!(args);

    let output = datetime::parse_datetime(&input)
        .map_err(|e| RuntimeError::DateParsingError(e.to_string()))?;

    return_datetime!(output)
}

pub fn format_datetime(mut args: Args) -> Result<Value> {
    let format = string_arg!(args);
    let dt = datetime_arg!(args);

    let mut output = String::new();
    write!(output, "{}", dt.strftime(&format)).map_err(|_| RuntimeError::DateFormattingError)?;

    return_string!(output)
}

pub fn get_local_timezone(_args: Args) -> Result<Value> {
    let local_tz = datetime::get_local_timezone_or_utc();
    let tz_name = local_tz.iana_name().unwrap_or("<unknown timezone>");

    return_string!(tz_name)
}

pub fn tz(mut args: Args) -> Result<Value> {
    let tz = string_arg!(args);

    Ok(Value::FunctionReference(FunctionReference::TzConversion(
        tz,
    )))
}

pub fn unixtime(mut args: Args) -> Result<Value> {
    let input = datetime_arg!(args);

    let output = input.timestamp().as_second();

    return_scalar!(output as f64)
}

pub fn from_unixtime(mut args: Args) -> Result<Value> {
    let timestamp = quantity_arg!(args).unsafe_value().to_f64() as i64;

    let dt = Timestamp::from_second(timestamp)
        .map_err(|_| RuntimeError::DateTimeOutOfRange)?
        .to_zoned(datetime::get_local_timezone_or_utc());

    return_datetime!(dt)
}

fn calendar_add(
    mut args: Args,
    unit_name: &str,
    to_span: fn(i64) -> std::result::Result<Span, jiff::Error>,
) -> Result<Value> {
    let dt = datetime_arg!(args);
    let n = quantity_arg!(args).unsafe_value().to_f64();

    if n.fract() != 0.0 {
        return Err(RuntimeError::UserError(format!(
            "calendar_add: requires an integer number of {unit_name}s"
        )));
    }

    let n_i64 = n.to_i64().ok_or_else(|| {
        RuntimeError::UserError(format!("calendar:add: number of {unit_name}s is too large",))
    })?;

    let output = dt
        .checked_add(to_span(n_i64).map_err(|_| RuntimeError::DurationOutOfRange)?)
        .map_err(|_| RuntimeError::DateTimeOutOfRange)?;

    return_datetime!(output)
}

pub fn _add_days(args: Args) -> Result<Value> {
    calendar_add(args, "day", |n| Span::new().try_days(n))
}

pub fn _add_months(args: Args) -> Result<Value> {
    calendar_add(args, "month", |n| Span::new().try_months(n))
}

pub fn _add_years(args: Args) -> Result<Value> {
    calendar_add(args, "year", |n| Span::new().try_years(n))
}
