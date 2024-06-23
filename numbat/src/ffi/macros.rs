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
