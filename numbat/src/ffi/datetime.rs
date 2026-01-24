use compact_str::CompactString;
use jiff::Span;
use jiff::Timestamp;
use jiff::Zoned;
use jiff::fmt::StdFmtWrite;
use jiff::fmt::strtime::BrokenDownTime;
use num_traits::ToPrimitive;

use super::Args;
use super::Result;
use super::macros::*;
use crate::datetime;
use crate::interpreter::RuntimeErrorKind;
use crate::quantity::Quantity;
use crate::typechecker::type_scheme::TypeScheme;
use crate::value::FunctionReference;
use crate::value::Value;
use crate::vm::ExecutionContext;

pub fn now(
    _ctx: &mut ExecutionContext,
    _args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    return_datetime!(Zoned::now())
}

pub fn datetime(
    _ctx: &mut ExecutionContext,
    mut args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    let input = string_arg!(args);

    let output = datetime::parse_datetime(&input)
        .map_err(|e| RuntimeErrorKind::DateParsingError(e.to_string()))?;

    return_datetime!(output)
}

pub fn format_datetime(
    _ctx: &mut ExecutionContext,
    mut args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    let format = string_arg!(args);
    let dt = datetime_arg!(args);

    let mut output = CompactString::with_capacity(format.len());
    BrokenDownTime::from(&dt)
        // jiff::fmt::StdFmtWrite is a wrapper that turns an arbitrary std::fmt::Write
        // into a jiff::fmt::Write, which is necessary to write a formatted datetime
        // into it
        .format(&format, StdFmtWrite(&mut output))
        .map_err(|e| RuntimeErrorKind::DateFormattingError(e.to_string()))?;

    return_string!(owned = output)
}

pub fn get_local_timezone(
    _ctx: &mut ExecutionContext,
    _args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    let local_tz = datetime::get_local_timezone_or_utc();
    let tz_name = local_tz.iana_name().unwrap_or("<unknown timezone>");

    return_string!(borrowed = tz_name)
}

pub fn tz(
    _ctx: &mut ExecutionContext,
    mut args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    let tz = string_arg!(args);

    Ok(Value::FunctionReference(FunctionReference::TzConversion(
        tz,
    )))
}

pub fn unixtime_us(
    _ctx: &mut ExecutionContext,
    mut args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    let input = datetime_arg!(args);

    let us = input.timestamp().as_microsecond();

    return_scalar!(us as f64)
}

pub fn from_unixtime_us(
    _ctx: &mut ExecutionContext,
    mut args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    let us = quantity_arg!(args).unsafe_value().to_f64() as i64;

    let dt = Timestamp::from_microsecond(us)
        .map_err(|_| RuntimeErrorKind::DateTimeOutOfRange)?
        .to_zoned(datetime::get_local_timezone_or_utc());

    return_datetime!(dt)
}

fn calendar_add(
    mut args: Args,
    unit_name: &str,
    to_span: fn(i64) -> std::result::Result<Span, jiff::Error>,
) -> Result<Value, Box<RuntimeErrorKind>> {
    let dt = datetime_arg!(args);
    let n = quantity_arg!(args).unsafe_value().to_f64();

    if n.fract() != 0.0 {
        return Err(Box::new(RuntimeErrorKind::UserError(format!(
            "calendar_add: requires an integer number of {unit_name}s"
        ))));
    }

    let n_i64 = n.to_i64().ok_or_else(|| {
        RuntimeErrorKind::UserError(format!("calendar:add: number of {unit_name}s is too large",))
    })?;

    let output = dt
        .checked_add(to_span(n_i64).map_err(|_| RuntimeErrorKind::DurationOutOfRange)?)
        .map_err(|_| RuntimeErrorKind::DateTimeOutOfRange)?;

    return_datetime!(output)
}

pub fn _add_days(
    _ctx: &mut ExecutionContext,
    args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    calendar_add(args, "day", |n| Span::new().try_days(n))
}

pub fn _add_months(
    _ctx: &mut ExecutionContext,
    args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    calendar_add(args, "month", |n| Span::new().try_months(n))
}

pub fn _add_years(
    _ctx: &mut ExecutionContext,
    args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    calendar_add(args, "year", |n| Span::new().try_years(n))
}
