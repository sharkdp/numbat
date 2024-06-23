/// Some macros for writing FFI functions

macro_rules! quantity_arg {
    ($args:ident, $index:expr) => {
        $args[$index].unsafe_as_quantity()
    };
}
pub(crate) use quantity_arg;

macro_rules! scalar_arg {
    ($args:ident, $index:expr) => {
        quantity_arg!($args, $index).as_scalar().unwrap()
    };
}
pub(crate) use scalar_arg;

macro_rules! string_arg {
    ($args:ident, $index:expr) => {
        $args[$index].unsafe_as_string()
    };
}
pub(crate) use string_arg;

macro_rules! datetime_arg {
    ($args:ident, $index:expr) => {
        $args[$index].unsafe_as_datetime()
    };
}
pub(crate) use datetime_arg;

macro_rules! return_scalar {
    ( $value:expr) => {
        Ok(Value::Quantity(Quantity::from_scalar($value)))
    };
}
pub(crate) use return_scalar;

macro_rules! return_quantity {
    ($value:expr, $unit:expr) => {
        Ok(Value::Quantity(Quantity::new_f64($value, $unit)))
    };
}
pub(crate) use return_quantity;

macro_rules! return_boolean {
    ($value:expr) => {
        Ok(Value::Boolean($value))
    };
}
pub(crate) use return_boolean;

macro_rules! return_list {
    ($value:expr) => {
        Ok(Value::List($value.into()))
    };
}
pub(crate) use return_list;

macro_rules! return_string {
    ($value:expr) => {
        Ok(Value::String($value.into()))
    };
}
pub(crate) use return_string;

macro_rules! return_datetime {
    ($value:expr) => {
        Ok(Value::DateTime($value))
    };
}
pub(crate) use return_datetime;
