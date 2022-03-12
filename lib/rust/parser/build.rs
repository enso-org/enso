use enso_automata::dfa;
use std::fs;
use std::path::Path;


fn main() {
    fs::create_dir("dist").ok();
    let dest_path = Path::new("dist/lexer.rs");

    fs::write(&dest_path, dfa::example_parser()).unwrap();
}
