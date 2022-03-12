use enso_automata::dfa;
use std::fs;
use std::path::Path;


fn main() {
    fs::create_dir("dist").ok();
    let dest_path = Path::new("dist/lexer.rs");

    fs::write(&dest_path, dfa::example_parser()).unwrap();
    // // Tell Cargo that if the given file changes, to rerun this build script.
    // println!("cargo:rerun-if-changed=src/hello.c");
    // // Use the `cc` crate to build a C file and statically link it.
    // cc::Build::new()
    //     .file("src/hello.c")
    //     .compile("hello");
}
