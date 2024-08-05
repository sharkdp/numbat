use super::macros::*;
use super::Args;
use super::Result;
use crate::value::Value;

pub fn _create_line_plot(mut args: Args) -> Result<Value> {
    let mut fields = arg!(args).unsafe_as_struct_fields();
    let ys = fields.pop().unwrap();
    let xs = fields.pop().unwrap();
    let y_unit = fields.pop().unwrap().unsafe_as_string();
    let y_label = fields.pop().unwrap().unsafe_as_string();
    let x_unit = fields.pop().unwrap().unsafe_as_string();
    let x_label = fields.pop().unwrap().unsafe_as_string();

    let x_label = format!(
        "{x_label}{x_unit}",
        x_unit = if x_unit.is_empty() {
            "".into()
        } else {
            format!(" [{}]", x_unit)
        }
    );
    let y_label = format!(
        "{y_label}{y_unit}",
        y_unit = if y_unit.is_empty() {
            "".into()
        } else {
            format!(" [{}]", y_unit)
        }
    );

    let xs = xs
        .unsafe_as_list()
        .iter()
        .cloned()
        .map(|e| e.unsafe_as_quantity().unsafe_value().to_f64())
        .collect::<Vec<_>>();
    let ys = ys
        .unsafe_as_list()
        .iter()
        .cloned()
        .map(|e| e.unsafe_as_quantity().unsafe_value().to_f64())
        .collect::<Vec<_>>();

    crate::plot::line_plot(xs, ys, &x_label, &y_label);

    return_string!("Plot will be opened in the browser")
}
