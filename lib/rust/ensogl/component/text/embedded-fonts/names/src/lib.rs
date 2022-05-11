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



// =================
// === Constants ===
// =================

/// An object of the DejaVuSans type. Creating an object here makes it possible to avoid creating
/// temporary objects in the places where the methods need to be referenced.
pub const DEJA_VU_SANS: DejaVuSans = Default::default();



// ==================
// === FontFamily ===
// ==================

/// Trait with methods returning names of fonts in a given font family.
#[allow(missing_docs)]
pub trait FontFamily {
    fn regular(&self) -> &'static str;
    fn bold(&self) -> &'static str;
    fn mono(&self) -> &'static str;
    fn mono_bold(&self) -> &'static str;
}



// ==================
// === DejaVuSans ===
// ==================

/// A type with methods returning names of fonts in the DejaVuSans font family.
#[derive(Copy, Clone, Debug)]
pub struct DejaVuSans {}

impl const FontFamily for DejaVuSans {
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

impl const Default for DejaVuSans {
    fn default() -> Self {
        Self {}
    }
}
