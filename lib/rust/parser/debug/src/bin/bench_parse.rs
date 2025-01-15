//! Parses Enso sources, measuring time spent in the parser.

// === Features ===
#![feature(test)]
// === Non-Standard Linter Configuration ===
#![allow(clippy::option_map_unit_fn)]
#![allow(clippy::precedence)]
#![allow(dead_code)]
#![deny(unconditional_recursion)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(unused_qualifications)]



// ===========
// === CLI ===
// ===========

fn main() {
    let args = std::env::args().skip(1);
    let parser = enso_parser::Parser::new();
    let parse_time: std::time::Duration = args
        .map(|path| {
            let code = read_source(path).unwrap();
            let start = std::time::Instant::now();
            std::hint::black_box(parser.parse_module(&code));
            start.elapsed()
        })
        .sum();
    println!("Parse time: {} ms", parse_time.as_millis());
}

fn read_source(path: impl AsRef<Path>) -> io::Result<String> {
    let code = fs::read_to_string(path)?;
    Ok(if let Some((_meta, code)) = enso_parser::metadata::parse(&code) {
        code.to_owned()
    } else {
        code
    })
}



// ===============================
// === `cargo bench` interface ===
// ===============================

extern crate test;

use std::fs::DirEntry;
use std::fs::{self};
use std::io;
use std::path::Path;

fn visit_files<F: FnMut(&DirEntry)>(dir: &Path, f: &mut F) -> io::Result<()> {
    if dir.is_dir() {
        for entry in fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();
            if path.is_dir() {
                visit_files(&path, f)?;
            } else {
                f(&entry);
            }
        }
    }
    Ok(())
}

#[bench]
fn bench_std_lib(b: &mut test::Bencher) {
    let mut sources = vec![];
    visit_files(Path::new("../../../../distribution/lib"), &mut |dir_ent| {
        let path = dir_ent.path();
        if let Some(ext) = path.extension() {
            if ext == "enso" {
                sources.push(read_source(path).unwrap())
            }
        }
    })
    .unwrap();
    sources.sort_unstable();
    let parser = enso_parser::Parser::new();
    b.bytes = sources.iter().map(|s| s.len() as u64).sum();
    b.iter(|| {
        for source in &sources {
            test::black_box(parser.parse_module(source));
        }
    });
}
