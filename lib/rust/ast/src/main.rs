use ast::generation::ScalaGenerator;

use std::env;
use std::fs::File;
use std::io::Write;
use itertools::Itertools;


pub fn main() -> std::io::Result<()> {
    let args = env::args().collect_vec();

    if Some("--generate-scala-ast") == args.get(1).map(|s| s.as_str()) {
        let file = args.get(2).expect("Output file wasn't given.");

        File::create(file)?.write_all(ScalaGenerator::ast()?.as_bytes())?;
    }
    Ok(())
}
