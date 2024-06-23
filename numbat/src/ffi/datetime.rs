use super::macros::*;
use super::Result;
use crate::datetime;
use crate::quantity::Quantity;
use crate::value::FunctionReference;
use crate::value::Value;
use crate::RuntimeError;

use std::fmt::Write;

pub fn now(_args: &[Value]) -> Result<Value> {
    let now = chrono::Local::now().fixed_offset();

    return_datetime!(now)
}

pub fn datetime(args: &[Value]) -> Result<Value> {
    let input = string_arg!(args, 0);

    let output = datetime::parse_datetime(input)
        .ok_or(RuntimeError::DateParsingErrorUnknown)?
        .fixed_offset();

    return_datetime!(output)
}

pub fn format_datetime(args: &[Value]) -> Result<Value> {
    let format = string_arg!(args, 0);
    let dt = datetime_arg!(args, 1);

    let mut output = String::new();
    write!(output, "{}", dt.format(format)).map_err(|_| RuntimeError::DateFormattingError)?;

    return_string!(output)
}

pub fn get_local_timezone(_args: &[Value]) -> Result<Value> {
    let local_tz = datetime::get_local_timezone_or_utc().to_string();

    return_string!(local_tz)
}

pub fn tz(args: &[Value]) -> Result<Value> {
    let tz = string_arg!(args, 0);

    Ok(Value::FunctionReference(FunctionReference::TzConversion(
        tz.into(),
    )))
}

pub fn unixtime(args: &[Value]) -> Result<Value> {
    let input = datetime_arg!(args, 0);

    let output = input.timestamp();

    return_scalar!(output as f64)
}

pub fn from_unixtime(args: &[Value]) -> Result<Value> {
    let timestamp = quantity_arg!(args, 0).unsafe_value().to_f64() as i64;

    let dt = chrono::DateTime::from_timestamp(timestamp, 0)
        .ok_or(RuntimeError::DateTimeOutOfRange)?
        .with_timezone(&datetime::get_local_timezone_or_utc())
        .fixed_offset();

    return_datetime!(dt)
}
