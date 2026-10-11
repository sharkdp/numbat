#[cfg(feature = "plotting")]
use plotly::Plot;

use crate::interpreter::RuntimeErrorKind;
use crate::typechecker::type_scheme::TypeScheme;
#[allow(unused_imports)]
use crate::vm::ExecutionContext;

#[cfg(feature = "plotting")]
use super::macros::*;

#[cfg(feature = "plotting")]
use compact_str::CompactString;

use super::Args;
use super::FfiContext;
use super::Result;
use crate::value::Value;

#[cfg(feature = "plotting")]
fn line_plot(mut args: Args) -> Result<Plot, Box<RuntimeErrorKind>> {
    let mut fields = arg!(args).unsafe_as_struct_fields();
    let ys = fields.pop().unwrap();
    let xs = fields.pop().unwrap();
    let y_label = fields.pop().unwrap().unsafe_as_string();
    let x_label = fields.pop().unwrap().unsafe_as_string();

    let xs = xs.unsafe_as_list();
    let ys = ys.unsafe_as_list();

    let get_unit = |list: &crate::list::NumbatList<Value>| {
        list.iter()
            .next()
            .map(|v| v.clone().unsafe_as_quantity().unit().clone())
            .ok_or_else(|| Box::new(RuntimeErrorKind::UserError("Cannot plot empty data".into())))
    };
    let x_unit = get_unit(&xs)?;
    let y_unit = get_unit(&ys)?;

    let x_label = format!(
        "{x_label}{x_unit}",
        x_unit = if x_unit.is_scalar() {
            "".into()
        } else {
            format!(" [{}]", x_unit)
        }
    );
    let y_label = format!(
        "{y_label}{y_unit}",
        y_unit = if y_unit.is_scalar() {
            "".into()
        } else {
            format!(" [{}]", y_unit)
        }
    );

    let xs = xs
        .iter()
        .cloned()
        .map(|e| {
            e.unsafe_as_quantity()
                .unsafe_value()
                .try_as_real()
                .ok_or_else(|| {
                    Box::new(RuntimeErrorKind::ExpectedRealNumberInFunction("plot".into()))
                })
        })
        .collect::<std::result::Result<Vec<_>, _>>()?;
    let ys = ys
        .iter()
        .cloned()
        .map(|e| {
            e.unsafe_as_quantity()
                .unsafe_value()
                .try_as_real()
                .ok_or_else(|| {
                    Box::new(RuntimeErrorKind::ExpectedRealNumberInFunction("plot".into()))
                })
        })
        .collect::<std::result::Result<Vec<_>, _>>()?;

    Ok(crate::plot::line_plot(xs, ys, &x_label, &y_label))
}

#[cfg(feature = "plotting")]
fn bar_chart(mut args: Args) -> Result<Plot, Box<RuntimeErrorKind>> {
    let mut fields = arg!(args).unsafe_as_struct_fields();
    let x_labels = fields.pop().unwrap();
    let values = fields.pop().unwrap();
    let value_label = fields.pop().unwrap().unsafe_as_string();

    let x_labels = x_labels
        .unsafe_as_list()
        .iter()
        .cloned()
        .map(|e| e.unsafe_as_string())
        .collect::<Vec<_>>();

    let values = values.unsafe_as_list();
    let value_unit = values
        .iter()
        .next()
        .map(|v| v.clone().unsafe_as_quantity().unit().clone())
        .ok_or_else(|| Box::new(RuntimeErrorKind::UserError("Cannot plot empty data".into())))?;

    let values = values
        .iter()
        .cloned()
        .map(|e| {
            e.unsafe_as_quantity()
                .unsafe_value()
                .try_as_real()
                .ok_or_else(|| {
                    Box::new(RuntimeErrorKind::ExpectedRealNumberInFunction("plot".into()))
                })
        })
        .collect::<std::result::Result<Vec<_>, _>>()?;

    let value_label = format!(
        "{value_label}{value_unit}",
        value_unit = if value_unit.is_scalar() {
            "".into()
        } else {
            format!(" [{}]", value_unit)
        }
    );

    Ok(crate::plot::bar_chart(values, x_labels, &value_label))
}

#[cfg(feature = "plotting")]
fn show_plot(plot: Plot) -> CompactString {
    plot.show();

    CompactString::const_new("Plot will be opened in the browser")
}

#[cfg(feature = "plotting")]
pub fn show(
    _ctx: &mut FfiContext,
    args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    // Dynamic dispatch hack since we don't have bounded polymorphism.
    // And no real support for generics in the FFI.

    use crate::interpreter::RuntimeErrorKind;
    let Value::StructInstance(info, _) = &args.front().unwrap().value else {
        return Err(Box::new(RuntimeErrorKind::UserError(
            "Unsupported argument to 'show'.".into(),
        )));
    };

    let plot = if info.name == "LinePlot" {
        line_plot(args)?
    } else if info.name == "BarChart" {
        bar_chart(args)?
    } else {
        return Err(Box::new(RuntimeErrorKind::UserError(format!(
            "Unsupported plot type: {}",
            info.name
        ))));
    };

    return_string!(owned = show_plot(plot))
}

#[cfg(not(feature = "plotting"))]
pub fn show(
    _ctx: &mut FfiContext,
    _args: Args,
    _return_type: &TypeScheme,
) -> Result<Value, Box<RuntimeErrorKind>> {
    return Err(Box::new(RuntimeErrorKind::UserError(
        "Plotting is currently not supported on this platform.".into(),
    )));
}
