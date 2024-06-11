use plotters::prelude::*;

pub fn line_plot(xs: Vec<f64>, ys: Vec<f64>, x_label: &str, y_label: &str) -> String {
    let filename = "/tmp/numbat-plot.png".to_string();

    let root = BitMapBackend::new(filename.as_str(), (800, 600)).into_drawing_area();

    root.fill(&WHITE).unwrap();

    let x_min = xs[0];
    let x_max = xs[xs.len() - 1];
    let y_min = ys.iter().fold(f64::INFINITY, |a, &b| a.min(b));
    let y_max = ys.iter().fold(f64::NEG_INFINITY, |a, &b| a.max(b));

    let y_max = y_max + 0.1 * (y_max - y_min);
    let y_min = y_min - 0.1 * (y_max - y_min);

    let data = xs.iter().zip(ys.iter()).map(|(x, y)| (*x, *y));

    let mut chart = ChartBuilder::on(&root)
        .margin(10)
        .set_label_area_size(LabelAreaPosition::Left, 60)
        .set_label_area_size(LabelAreaPosition::Right, 60)
        .set_label_area_size(LabelAreaPosition::Bottom, 60)
        .set_label_area_size(LabelAreaPosition::Top, 60)
        .build_cartesian_2d(x_min..x_max, y_min..y_max)
        .unwrap();

    chart
        .configure_mesh()
        .x_label_style(("sans-serif", 18))
        .y_label_style(("sans-serif", 18))
        .x_labels(10)
        .max_light_lines(4)
        .x_desc(x_label)
        .y_desc(y_label)
        .draw()
        .unwrap();

    let linestyle = ShapeStyle {
        color: RGBColor(0x00, 0x77, 0xff).into(),
        filled: false,
        stroke_width: 1,
    };

    chart.draw_series(LineSeries::new(data, linestyle)).unwrap();

    // To avoid the IO failure being ignored silently, we manually call the present function
    root.present().expect("Unable to write result to file");

    filename.clone()
}
