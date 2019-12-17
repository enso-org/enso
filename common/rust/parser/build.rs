use std::fs::{File, create_dir_all, canonicalize};
use std::io::BufReader;
use std::io::prelude::*;
use std::env;


fn prepend(input: File, mut output: File, text: &str) -> std::io::Result<()> {
    let buffered = BufReader::new(input);
    writeln!(output, "{}", text)?;
    for line in buffered.lines() {
        writeln!(output, "{}", line?)?;
    }
    Ok(())
}

fn move_js_artifacts() -> std::io::Result<()> {
    let root = canonicalize("../../..")
        .expect("Couldn't get root of workspace.");
    let pkg_path = &root
        .join("common").join("rust").join("parser").join("pkg");
    let parser_path_fix = &pkg_path.join("scala-parser.js");
    let parser_path     = &root.join("target").join("scala-parser.js");

    create_dir_all(&pkg_path).expect(&format!(
        "Could not create file {}", pkg_path.to_str().unwrap()
    ));

    let original = File::open(&parser_path).expect(&format!(
        "Could not find file {} {}",
        parser_path.to_str().unwrap(),
        "(Hint: You can generate it by running `sbt syntaxJS/fullOptJS`.)"
    ));
    let fixed = File::create(&parser_path_fix).expect(&format!(
        "Could not create file {}", parser_path_fix.to_str().unwrap()
    ));

    // fix for a bug in scalajs https://github.com/scala-js/scala-js/issues/3677/
    prepend(original, fixed, "var __ScalaJSEnv = { global: window };")
}

fn main() -> std::io::Result<()>  {
    let target = env::var("TARGET").unwrap();
    if target.contains("wasm32") {
        move_js_artifacts()?;
    }
    Ok(())
}