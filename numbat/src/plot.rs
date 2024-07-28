use egui_plot::{Line, Plot, PlotPoints};

pub fn line_plot(xs: Vec<f64>, ys: Vec<f64>, x_label: &str, y_label: &str) {
    let options = eframe::NativeOptions::default();
    let x_label = x_label.to_string();
    let y_label = y_label.to_string();

    eframe::run_simple_native("Plot", options, move |ctx, _frame| {
        egui::TopBottomPanel::top("Top panel").show(ctx, |ui| {
            egui::widgets::global_dark_light_mode_buttons(ui);
        });
        egui::CentralPanel::default().show(ctx, |ui| {
            let sin: PlotPoints = xs.iter().zip(&ys).map(|(x, y)| [*x, *y]).collect();
            let line = Line::new(sin);
            Plot::new("my_plot")
                .x_axis_label(&x_label)
                .y_axis_label(&y_label)
                .show(ui, |plot_ui| plot_ui.line(line));
        });
    })
    .unwrap();
}
