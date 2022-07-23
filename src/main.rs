mod ast;
mod parser;
mod pretty_print;
mod tokenizer;

use parser::Parser;
use tokenizer::tokenize;
use pretty_print::PrettyPrint;

use anyhow::Result;

fn main() -> Result<()> {
    let mut args = std::env::args();
    args.next();
    if args.next().as_deref() == Some("-r") {
        loop {
            let mut input_string = String::new();
            std::io::stdin().read_line(&mut input_string).unwrap();

            let result = tokenize(&input_string);
            println!("{:?}", result);
        }
    } else {
        let tokens = tokenize("1 * 2 + 3 -> 4")?;
        dbg!(&tokens);
        let mut parser = Parser::new(&tokens);
        let ast = parser.expression();
        println!("{}", ast.pretty_print());
    }

    Ok(())
}
