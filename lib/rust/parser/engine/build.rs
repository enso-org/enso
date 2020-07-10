use std::fs::File;
use std::io::prelude::*;
use parser_definition::lexer::Lexer;
use flexer::Flexer;


/// Generates lexer engine and saves the result into a file `src/lexer-engine.rs`.
/// The content of the generated file can be used trough the `include!` macro.
fn generate_engine() -> std::io::Result<()> {
    let mut file = File::create("src/lexer-engine.rs")?;
    let engine   = Lexer::new().specialize();
    file.write_all(engine.as_bytes())?;
    Ok(())
}

fn main() -> std::io::Result<()> {
    generate_engine()
}
