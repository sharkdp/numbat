use super::macros::*;
use super::Args;
use super::Result;
use crate::value::Value;

pub fn _create_line_plot(mut args: Args) -> Result<Value> {
    let mut fields = arg!(args).unsafe_as_struct_fields();
    let ys = fields.pop().unwrap();
    let xs = fields.pop().unwrap();
    let y_label = fields.pop().unwrap();
    let x_label = fields.pop().unwrap();

    let x_label = x_label.unsafe_as_string();
    let y_label = y_label.unsafe_as_string();
    let xs = xs
        .unsafe_as_list()
        .into_iter()
        .map(|e| e.unsafe_as_quantity().unsafe_value().to_f64())
        .collect::<Vec<_>>();
    let ys = ys
        .unsafe_as_list()
        .into_iter()
        .map(|e| e.unsafe_as_quantity().unsafe_value().to_f64())
        .collect::<Vec<_>>();

    crate::plot::line_plot(xs, ys, &x_label, &y_label);

    return_string!("Plot will be opened in the browser")
}
