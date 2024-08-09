use egui::{emath::Numeric, Color32, Stroke};
use egui_plot::{Bar, BarChart, Line, Orientation, Plot, PlotPoints};

pub fn line_plot(xs: Vec<f64>, ys: Vec<f64>, x_label: &str, y_label: &str) {
    let options = eframe::NativeOptions::default();
    let x_label = x_label.to_string();
    let y_label = y_label.to_string();

    eframe::run_simple_native("Numbat Plot", options, move |ctx, _frame| {
        egui::TopBottomPanel::top("top_panel").show(ctx, |ui| {
            egui::widgets::global_dark_light_mode_buttons(ui);
        });
        egui::CentralPanel::default().show(ctx, |ui| {
            let plot_points: PlotPoints = xs.iter().zip(&ys).map(|(x, y)| [*x, *y]).collect();
            let line = Line::new(plot_points);

            Plot::new("plot")
                .x_axis_label(&x_label)
                .y_axis_label(&y_label)
                .show(ui, |plot_ui| plot_ui.line(line));
        });
    })
    .unwrap();
}

pub fn bar_chart(values: Vec<f64>, x_labels: Vec<String>, value_label: &str) {
    let options = eframe::NativeOptions::default();
    let value_label = value_label.to_string();

    eframe::run_simple_native("Numbat Plot", options, move |ctx, _frame| {
        egui::TopBottomPanel::top("top_panel").show(ctx, |ui| {
            egui::widgets::global_dark_light_mode_buttons(ui);
        });
        egui::CentralPanel::default().show(ctx, |ui| {
            let bars = x_labels
                .iter()
                .zip(&values)
                .enumerate()
                .map(|(i, (x_label, value))| Bar {
                    name: x_label.clone(),
                    orientation: Orientation::Vertical,
                    argument: i.to_f64(),
                    value: *value,
                    base_offset: None,
                    bar_width: 0.5,
                    stroke: Stroke::new(1.0, Color32::TRANSPARENT),
                    fill: Color32::TRANSPARENT,
                })
                .collect();
            let chart = BarChart::new(bars);

            Plot::new("plot")
                .y_axis_label(&value_label)
                .show(ui, |plot_ui| plot_ui.bar_chart(chart));
        });
    })
    .unwrap();
}
