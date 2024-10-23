//! Generate a schema representing `enso-parser`'s AST types. This schema can be used to generate
//! AST representations and deserialization in other languages, such as TypeScript.
//!
//! The JSON schema data will be emitted to standard output.

use std::env;
use std::fs;
use std::path::Path;

// =========================
// === Schema Generation ===
// =========================

fn main() -> std::io::Result<()> {
    let args: Vec<_> = env::args().collect();
    let schema = enso_parser_schema::schema();
    if args.len() == 2 {
        let path = Path::new(&args[1]);
        if let Some(directory) = path.parent() {
            fs::create_dir_all(directory)?;
        }
        let file = fs::File::create(path)?;
        serde_json::to_writer_pretty(file, &schema)?;
    } else {
        serde_json::to_writer_pretty(std::io::stdout(), &schema)?;
    }
    Ok(())
}
