use plotly::Plot;

use super::macros::*;
use super::Args;
use super::Result;
use crate::value::Value;
use crate::RuntimeError;
use compact_str::CompactString;

fn line_plot(mut args: Args) -> Plot {
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

fn bar_chart(mut args: Args) -> Plot {
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

#[cfg(not(target_family = "wasm"))]
fn show_plot(plot: Plot) -> CompactString {
    plot.show();

    CompactString::const_new("Plot will be opened in the browser")
}

#[cfg(target_family = "wasm")]
fn show_plot(_plot: Plot) -> CompactString {
    // The way we could implement this would be to return plot.to_inline_html(..).
    // This would have to be retrieved on the JS side and then rendered using plotly.js.

    CompactString::const_new("Plotting is currently not supported on this platform.")
}

pub fn show(args: Args) -> Result<Value> {
    // Dynamic dispatch hack since we don't have bounded polymorphism.
    // And no real support for generics in the FFI.
    let Value::StructInstance(info, _) = args.front().unwrap() else {
        return Err(RuntimeError::UserError(
            "Unsupported argument to 'show'.".into(),
        ));
    };

    let plot = if info.name == "LinePlot" {
        line_plot(args)
    } else if info.name == "BarChart" {
        bar_chart(args)
    } else {
        return Err(RuntimeError::UserError(format!(
            "Unsupported plot type: {}",
            info.name
        )));
    };

    return_string!(owned = show_plot(plot))
}
