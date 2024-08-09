use super::macros::*;
use super::Args;
use super::Result;
use crate::value::Value;
use crate::RuntimeError;

fn line_plot(mut args: Args) {
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

    crate::plot::line_plot(xs, ys, &x_label, &y_label)
}

fn bar_chat(mut args: Args) {
    let mut fields = arg!(args).unsafe_as_struct_fields();
    let x_labels = fields.pop().unwrap();
    let values = fields.pop().unwrap();
    let value_unit = fields.pop().unwrap().unsafe_as_string();
    let value_label = fields.pop().unwrap().unsafe_as_string();

    let x_labels = x_labels
        .unsafe_as_list()
        .iter()
        .cloned()
        .map(|e| e.unsafe_as_string())
        .collect::<Vec<_>>();
    let values = values
        .unsafe_as_list()
        .iter()
        .cloned()
        .map(|e| e.unsafe_as_quantity().unsafe_value().to_f64())
        .collect::<Vec<_>>();

    let value_label = format!(
        "{value_label}{value_unit}",
        value_unit = if value_unit.is_empty() {
            "".into()
        } else {
            format!(" [{}]", value_unit)
        }
    );

    crate::plot::bar_chart(values, x_labels, &value_label)
}

#[cfg(target_family = "wasm")]
pub fn show(args: Args) -> String {
    "Plotting is currently not supported on this platform.".into()
}

#[cfg(not(target_family = "wasm"))]
pub fn show(args: Args) -> Result<Value> {
    // Dynamic dispatch hack since we don't have bounded polymorphism.
    // And no real support for generics in the FFI.
    let Value::StructInstance(info, _) = args.front().unwrap() else {
        return Err(RuntimeError::UserError(format!(
            "Unsupported argument to 'show'.",
        )));
    };

    if info.name == "LinePlot" {
        line_plot(args)
    } else if info.name == "BarChart" {
        bar_chat(args)
    } else {
        return Err(RuntimeError::UserError(format!(
            "Unsupported plot type: {}",
            info.name
        )));
    };

    return_string!("Done.")
}
