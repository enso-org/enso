use std::fs::File;
use std::io::prelude::*;
use definition::parser::Parser;


fn main() -> std::io::Result<()> {
    println!("cargo:rerun-if-changed=../definition/src/parser.rs");

    let mut file    = File::create("src/parser-generated.rs")?;
    let specialized = Parser::new().specialize();
    file.write_all(specialized.as_bytes())?;
    Ok(())
}