// === Standard Linter Configuration ===
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]

// === Non-Standard Linter Configuration ===



// === Non-Standard Linter Configuration ===



// === Non-Standard Linter Configuration ===



// === Non-Standard Linter Configuration ===



// === Non-Standard Linter Configuration ===



//
// === Non-Standard Linter Configuration ===



// === Non-Standard Linter Configuration ===



// === Non-standard linter configuration ===



// === Standard linter configuration ===
#![warn(missing_copy_implementations)]#![warn(missing_debug_implementations)]#![warn(missing_docs)]#![warn(trivial_casts)]#![warn(trivial_numeric_casts)]#![warn(unsafe_code)]#![warn(unused_import_braces)]#![warn(unused_qualifications)]
// === Non-standard linter configuration ===

use ast_new::generation::ScalaGenerator;
use std::fs::File;
use std::io::Write;



pub fn main() -> std::io::Result<()> {
    let matches = clap::App::new("Enso AST")
        .version("1.0")
        .author("Enso Team <enso-dev@enso.org>")
        .about("Enso AST generator.")
        .args_from_usage("--generate-scala-ast [FILE] 'Generates a scala ast in specified file.'")
        .get_matches();

    if let Some(file) = matches.value_of("generate-scala-ast") {
        File::create(file)?.write_all(ScalaGenerator::ast()?.as_bytes())?;
        println!("Generated scala ast at path: {}", file);
    }
    Ok(())
}
