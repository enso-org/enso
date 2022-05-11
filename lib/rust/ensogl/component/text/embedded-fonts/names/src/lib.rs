//! Font families containing the names of the fonts embedded in the app.

// === Features ===
#![feature(const_trait_impl)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
// === Non-Standard Linter Configuration ===
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]



// ==================
// === FontFamily ===
// ==================

/// Trait with methods returning names of fonts in a font family.
#[allow(missing_docs)]
pub trait FontFamily {
    fn regular() -> &'static str;
    fn bold() -> &'static str;
    fn mono() -> &'static str;
    fn mono_bold() -> &'static str;
}



// ==================
// === DejaVuSans ===
// ==================

/// A type with methods returning names of fonts in the DejaVuSans font family.
#[derive(Copy, Clone, Debug)]
pub struct DejaVuSans {}

impl const FontFamily for DejaVuSans {
    fn regular() -> &'static str {
        "DejaVuSans"
    }
    fn bold() -> &'static str {
        "DejaVuSans-Bold"
    }
    fn mono() -> &'static str {
        "DejaVuSansMono"
    }
    fn mono_bold() -> &'static str {
        "DejaVuSansMono-Bold"
    }
}
