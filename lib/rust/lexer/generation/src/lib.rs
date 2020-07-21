//! This library exposes the specialized version of the Enso lexer.
//!
//! Its sole purpose is to avoid the lexer definition getting out of sync with its implementation
//! (the generated engine), which requires the engine to live in a separate crate.
//!
//! This separation enables generation of the enso lexer source code with `build.rs` during
//! compilation. Its output is then stored in a new file `lexer-engine.rs`and exported by `lexer.rs`.

pub mod lexer;
