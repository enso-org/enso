use std::io::prelude::*;

use enso_flexer::Definition;
use enso_flexer::State;
use flexer_test_definition::TestLexer;
use std::fs::File;



/// Generates the lexer engine and saves the result into the file `src/engine.rs`.
///
/// The content of the generated file can be used with the `include!` macro.
fn generate_engine() {
    let definition_path = "../definition/src/lib.rs";
    let output_directory = "src/generated";
    let _ = std::fs::create_dir(output_directory);
    let output_path = "src/generated/engine.rs";
    let mut lexer_def = File::open(definition_path)
        .unwrap_or_else(|_| panic!("The lexer definition should exist at {}.", definition_path));
    let mut contents = String::new();
    let mut file = File::create(output_path)
        .unwrap_or_else(|_| panic!("Cannot open output file at {}.", output_path));
    let lexer = TestLexer::define();
    let engine = lexer.specialize().unwrap();
    lexer_def.read_to_string(&mut contents).expect("Unable to read lexer definition.");
    file.write_all(contents.as_bytes()).expect("Unable to write lexer definition.");
    file.write_all(engine.as_bytes()).expect("Unable to write lexer specialization.");
}

fn main() {
    generate_engine()
}
