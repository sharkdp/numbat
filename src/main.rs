mod tokenizer;

use tokenizer::tokenize;

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
        let tokens = tokenize("12+34->foo")?;
        println!("tokens = {:?}", tokens);
    }

    Ok(())
}
