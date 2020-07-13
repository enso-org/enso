//! This library exposes the specialized version of enso lexer.
//! Its sole purpose is to avoid getting lexer definition out of sync with its implementation
//! (the generated engine), which requires the engine to live in separate crate.
//!
//! This separation enables a call to `lexer.specialize` (function that generates the lexer engine)
//! inside `build.rs` during compilation. Its output is then stored in a new file `lexer-engine.rs`
//! and exported by `lexer.rs`.

pub mod lexer;
