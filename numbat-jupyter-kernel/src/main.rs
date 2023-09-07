use clap::Parser;
use clap_derive::{Parser, Subcommand};
use jupyter::{
    async_trait, ExecutionReply, ExecutionRequest, ExecutionResult, InstallAction,
    JupyterKernelProtocol, JupyterKernelSockets, JupyterResult, LanguageInfo, OpenAction,
    StartAction, UnboundedSender, UninstallAction,
};
use numbat::{
    markup::{FormattedString, Formatter},
    module_importer::BuiltinModuleImporter,
    pretty_print::PrettyPrint,
    resolver::CodeSource,
    Context, InterpreterResult,
};
use plotters::{prelude::*, style::WHITE};
use std::path::PathBuf;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
pub struct JupyterApplication {
    /// Sets a custom config file
    #[arg(short, long, value_name = "FILE")]
    config: Option<PathBuf>,
    /// Turn debugging information on
    #[arg(short, long, action = clap::ArgAction::Count)]
    debug: u8,
    #[command(subcommand)]
    command: JupyterCommands,
}

pub struct HtmlFormatter;

impl Formatter for HtmlFormatter {
    fn format_part(&self, FormattedString(_, format_type, text): &FormattedString) -> String {
        use numbat::markup::FormatType::*;
        match format_type {
            Dimmed => format!("<code>{}</code>", text),
            Text => format!("<code>{}</code>", text),
            String => format!("<code>{}</code>", text),
            Whitespace => format!("<code>{}</code>", text),
            Keyword => format!("<b>{}</b>", text),
            Value => format!("<span style=\"color: #43a17e\">{}</span>", text),
            Unit => format!("<span style=\"color: #0a0afe\">{}</span>", text),
            Identifier => format!("<b>{}</b>", text),
            TypeIdentifier => format!("{}", text),
            Operator => format!("<b>{}</b>", text),
            Decorator => format!("{}", text),
        }
    }
}

pub struct NumbatContext {
    sockets: JupyterKernelSockets,
    numbat: Context,
}

impl NumbatContext {
    fn plot_function(
        &mut self,
        fn_name: &str,
        arg_name: &str,
        arg_unit_name: &str,
        x_min: f64,
        x_max: f64,
    ) -> bool {
        let num_points = 400;

        let mut data: Vec<(f64, f64)> = vec![];

        let mut unit = String::new();

        for i in 0..num_points {
            let x = (i as f64) * (x_max - x_min) / ((num_points - 1) as f64) + x_min;

            // TODO: this is so uncool
            let numbat_code = format!("{fn_name}({x} {arg_unit_name})");
            if let Ok(result) = self.numbat.interpret(&numbat_code, CodeSource::Internal) {
                let InterpreterResult::Quantity(y_q) = result.1 else { return false; };

                if unit.is_empty() {
                    unit = format!(" [{}]", y_q.unit());
                }

                data.push((x, y_q.unsafe_value().0));
            } else {
                return false;
            }
        }

        let root = BitMapBackend::new("/tmp/numbat-plot.png", (800, 600)).into_drawing_area();

        root.fill(&WHITE).unwrap();

        let y_min = data
            .iter()
            .map(|(_, y)| y)
            .min_by(|a, b| a.partial_cmp(b).unwrap())
            .unwrap();
        let y_max = data
            .iter()
            .map(|(_, y)| y)
            .max_by(|a, b| a.partial_cmp(b).unwrap())
            .unwrap();

        let y_range = y_max - y_min;
        let y_min = y_min - 0.1 * y_range;
        let y_max = y_max + 0.1 * y_range;

        let mut chart = ChartBuilder::on(&root)
            .margin(10)
            //.caption(fn_name, ("sans-serif", 40))
            .set_label_area_size(LabelAreaPosition::Left, 60)
            .set_label_area_size(LabelAreaPosition::Right, 60)
            .set_label_area_size(LabelAreaPosition::Bottom, 40)
            .build_cartesian_2d(x_min..x_max, y_min..y_max)
            .unwrap();

        chart
            .configure_mesh()
            .x_label_style(("sans-serif", 18))
            .y_label_style(("sans-serif", 18))
            .x_labels(20)
            .max_light_lines(4)
            .y_desc(format!("{}{}", fn_name, unit))
            .x_desc(format!("{arg_name} [{arg_unit_name}]"))
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

        true
    }
}

#[async_trait]
impl JupyterKernelProtocol for NumbatContext {
    fn language_info(&self) -> LanguageInfo {
        let mut info = LanguageInfo::new("numbat", "Numbat")
            .with_file_extensions("*.nbt", "application/x-numbat")
            .with_version(env!("CARGO_PKG_VERSION"))
            .with_syntax("python", "python");
        //info.png_32 = include_bytes!("../../assets/numbat-32.png");
        //info.png_64 = include_bytes!("../../assets/numbat-64.png");
        info
    }

    async fn running(&mut self, code: ExecutionRequest) -> ExecutionReply {
        if code.code.starts_with("plot ") {
            let args = code.code.split(" ").collect::<Vec<_>>();
            let fn_name = args[1];
            let arg_name = args[2];
            let x_min = args[3];
            let x_max = args[4];
            let unit_name = args[5];

            if self.plot_function(
                fn_name,
                arg_name,
                unit_name,
                x_min.parse().unwrap(),
                x_max.parse().unwrap(),
            ) {
                use image::io::Reader as ImageReader;
                let image = ImageReader::open("/tmp/numbat-plot.png")
                    .unwrap()
                    .decode()
                    .unwrap();

                self.sockets.send_executed(image).await;
            } else {
                self.sockets
                    .send_executed(format!(
                        "error while plotting '{fn_name}({arg_name})' with input-unit '{unit_name}'."
                    ))
                    .await;
            }
        } else {
            let result = self.numbat.interpret(&code.code, CodeSource::Text);

            match result {
                Ok((_statements, interpreter_result)) => match interpreter_result {
                    InterpreterResult::Value(v) => {
                        let v_pretty = v.pretty_print();
                        let output = HtmlFormatter {}.format(&v_pretty, true);

                        self.sockets
                            .send_executed(jupyter::value_type::HtmlText::new(output))
                            .await;
                    }
                    InterpreterResult::Continue => {}
                    InterpreterResult::Exit(_) => {}
                },
                Err(e) => {
                    self.sockets.send_executed(format!("{}", e)).await;
                }
            }
        }
        ExecutionReply::new(true, code.execution_count)
    }
    async fn bind_execution_socket(&self, sender: UnboundedSender<ExecutionResult>) {
        self.sockets.bind_execution_socket(sender).await
    }
}

#[derive(Subcommand)]
enum JupyterCommands {
    Open(Box<OpenAction>),
    Start(Box<StartAction>),
    Install(Box<InstallAction>),
    Uninstall(Box<UninstallAction>),
}

impl JupyterApplication {
    pub fn run(&self) -> JupyterResult<()> {
        let mut importer = BuiltinModuleImporter::default();
        let mut numbat = Context::new(importer);
        let _ = numbat.interpret("use prelude", CodeSource::Internal);

        let config = NumbatContext {
            sockets: JupyterKernelSockets::default(),
            numbat,
        };
        match &self.command {
            JupyterCommands::Open(v) => v.run(),
            JupyterCommands::Start(v) => v.run(config),
            JupyterCommands::Install(v) => v.run(config),
            JupyterCommands::Uninstall(v) => v.run(config),
        }
    }
}

fn main() -> JupyterResult<()> {
    let app = JupyterApplication::parse();
    app.run()
}
