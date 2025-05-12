use std::{
    fs,
    io::{BufRead, BufReader, Write},
    path::PathBuf,
};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Single colon should be replaced with double colon once crate MSRV hits 1.77
    println!("cargo:rerun-if-changed=src/modules/units/currencies.nbt");

    let currencies_file = BufReader::new(fs::File::open("modules/units/currencies.nbt")?);

    let mut currency_names = Vec::new();

    for line in currencies_file.lines() {
        let line = line?;
        let line = line.trim();

        if line.starts_with("@aliases(") {
            // assumes that @aliases(...) is on a single line
            let alias_str = line.split('(').nth(1).unwrap().split(')').next().unwrap();

            currency_names.extend(
                alias_str
                    .split(',')
                    .map(|s| s.trim().split(':').next().unwrap().to_owned()),
            );
        } else if line.starts_with("unit ") {
            // assumes that `unit` and unit name are on the same line
            let name = line.split(' ').nth(1).unwrap().split(':').next().unwrap();

            currency_names.push(name.trim().to_owned());
        }
    }

    // may be useful for performance — can binary search instead of linear
    currency_names.sort();

    let out_file = PathBuf::from(std::env::var("OUT_DIR")?).join("currencies.rs");
    let mut out_file = fs::File::create(&out_file)?;

    write!(out_file, "{currency_names:?}")?;

    Ok(())
}
