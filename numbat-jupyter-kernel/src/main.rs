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
    async fn plot_function(&mut self, fn_name: &str) {
        use image::io::Reader as ImageReader;
        let img = ImageReader::open("/home/ped1st/software/numbat/assets/numbat-800.png")
            .unwrap()
            .decode()
            .unwrap();

        self.sockets.send_executed(img).await;
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

            self.plot_function(fn_name).await;
        }

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
