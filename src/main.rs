mod tokenizer;

use tokenizer::Tokenizer;

use anyhow::Result;

fn main() -> Result<()> {
    let mut tokenizer = Tokenizer::new("12+34");
    let tokens = tokenizer.scan()?;
    println!("tokens = {:?}", tokens);

    Ok(())
}
