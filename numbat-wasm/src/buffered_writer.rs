use termcolor::WriteColor;

pub trait BufferedWriter: WriteColor {
    fn to_string(&self) -> String;
}
