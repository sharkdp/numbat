use compact_str::CompactString;

#[cfg(feature = "plotting")]
use plotly::{
    Bar, Configuration, Layout, Plot, Scatter,
    color::Rgb,
    common::{Font, Line},
    layout::Axis,
};

#[cfg(feature = "plotting")]
pub fn line_plot(xs: Vec<f64>, ys: Vec<f64>, x_label: &str, y_label: &str) -> Plot {
    let x_min = xs[0];
    let x_max = xs[xs.len() - 1];
    let y_min = ys.iter().fold(f64::INFINITY, |a, &b| a.min(b));
    let y_max = ys.iter().fold(f64::NEG_INFINITY, |a, &b| a.max(b));

    let y_max = y_max + 0.1 * (y_max - y_min);
    let y_min = y_min - 0.1 * (y_max - y_min);

    let mut plot = Plot::new();
    let trace = Scatter::new(xs, ys).line(Line::new().color(Rgb::new(0x00, 0x77, 0xff)));
    plot.add_trace(trace);

    let layout = Layout::new()
        .x_axis(Axis::new().title(x_label).range(vec![x_min, x_max]))
        .y_axis(Axis::new().title(y_label).range(vec![y_min, y_max]))
        .font(Font::new().size(16).family("Lato, Roboto, sans-serif"))
        .width(1200);
    plot.set_layout(layout);

    plot.set_configuration(Configuration::new());

    plot
}

#[cfg(feature = "plotting")]
pub fn bar_chart(values: Vec<f64>, x_labels: Vec<CompactString>, value_label: &str) -> Plot {
    let mut plot = Plot::new();

    let trace = Bar::new(x_labels, values);
    plot.add_trace(trace);

    let layout = Layout::new()
        .y_axis(Axis::new().title(value_label))
        .font(Font::new().size(16).family("Lato, Roboto, sans-serif"))
        .width(1200);
    plot.set_layout(layout);

    plot.set_configuration(Configuration::new());

    plot
}
