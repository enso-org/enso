//! Names of fonts embedded in the app.

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



pub trait FontDefinition {
    fn regular(&self) -> &'static str;
    fn bold(&self) -> &'static str;
    fn mono(&self) -> &'static str;
    fn mono_bold(&self) -> &'static str;
}

#[derive(Copy, Clone, Debug)]
pub struct DejaVuSans {}

pub const DEJA_VU_SANS: DejaVuSans = DejaVuSans {};

impl const FontDefinition for DejaVuSans {
    fn regular(&self) -> &'static str {
        "DejaVuSans"
    }
    fn bold(&self) -> &'static str {
        "DejaVuSans-Bold"
    }
    fn mono(&self) -> &'static str {
        "DejaVuSansMono"
    }
    fn mono_bold(&self) -> &'static str {
        "DejaVuSansMono-Bold"
    }
}
