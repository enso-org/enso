use std::fs::File;
use std::io::prelude::*;
use lexer_definition::lexer::Lexer;
use flexer::FlexerTemp;



/// Generates the lexer engine and saves the result into the file `src/lexer-engine.rs`.
///
/// The content of the generated file can be used with the `include!` macro.
fn generate_engine() -> std::io::Result<()> {
    let mut file = File::create("src/lexer-engine.rs")?;
    let engine   = Lexer::new().generate_specialized_code();
    file.write_all(engine.as_bytes())?;
    Ok(())
}

fn main() -> std::io::Result<()> {
    generate_engine()
}
