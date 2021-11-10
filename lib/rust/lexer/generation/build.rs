use enso_flexer::Definition;
use enso_flexer::State;
use lexer_definition::lexer::EnsoLexer;
use std::fs::File;
use std::io::prelude::*;



/// Generates the lexer engine and saves the result into the file `src/engine.rs`.
///
/// The content of the generated file can be used with the `include!` macro.
fn generate_engine() -> std::io::Result<()> {
    let definition_path = "../definition/src/lexer.rs";
    let output_directory = "src/generated";
    let _ = std::fs::create_dir(output_directory);
    let output_path = "src/generated/engine.rs";
    let definition_error = format!("The lexer definition should exist at {}.", definition_path);
    let output_error = format!("Cannot open output file at {}.", output_path);
    let mut lexer_def = File::open(definition_path).expect(&definition_error);
    let mut contents = String::new();
    let mut file = File::create(output_path).expect(&output_error);
    let lexer = EnsoLexer::define();
    let engine = lexer.specialize().unwrap();
    lexer_def.read_to_string(&mut contents).expect("Unable to read lexer definition.");
    file.write_all(contents.as_bytes()).expect("Unable to write lexer definition.");
    file.write_all("\n".as_bytes())?;
    file.write_all(engine.as_bytes()).expect("Unable to write lexer specialization.");
    Ok(())
}

fn main() -> std::io::Result<()> {
    generate_engine()
}
