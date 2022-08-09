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



pub use owned_ttf_parser::Style;
pub use owned_ttf_parser::Weight;
pub use owned_ttf_parser::Width;


/// Combination of all information allowing mapping the font face to a font file. The combination
/// reflects how the `@font-face` rule is defined in the CSS. See the following link to learn more:
/// https://www.w3schools.com/cssref/css3_pr_font-face_rule.asp
#[derive(Clone, Copy, Default, Debug, PartialEq, Eq)]
pub struct FaceHeader {
    pub width:  Width,
    pub weight: Weight,
    pub style:  Style,
}

#[allow(missing_docs)]
pub trait Family {
    fn name(&self) -> &str;
    fn is_variable(&self) -> bool;
    fn is_monospace(&self) -> bool;
    fn file_name(&self, header: FaceHeader) -> Option<&str>;
}



// ==================
// === DejaVuSans ===
// ==================

/// A type with methods returning names of fonts in the DejaVuSans font family.
#[derive(Copy, Clone, Debug)]
pub struct DejaVuSans;

impl Family for DejaVuSans {
    fn name(&self) -> &str {
        "DejaVuSans"
    }

    fn is_variable(&self) -> bool {
        false
    }

    fn is_monospace(&self) -> bool {
        false
    }

    fn file_name(&self, header: FaceHeader) -> Option<&str> {
        match (header.width, header.weight, header.style) {
            (Width::Normal, Weight::Normal, Style::Normal) => Some("DejaVuSans.ttf"),
            (Width::Normal, Weight::Bold, Style::Normal) => Some("DejaVuSans-Bold.ttf"),
            _ => None,
        }
    }
}



// ======================
// === DejaVuSansMono ===
// ======================

/// A type with methods returning names of fonts in the DejaVuSans font family.
#[derive(Copy, Clone, Debug)]
pub struct DejaVuSansMono;

impl Family for DejaVuSansMono {
    fn name(&self) -> &str {
        "DejaVuSansMono"
    }

    fn is_variable(&self) -> bool {
        false
    }

    fn is_monospace(&self) -> bool {
        true
    }

    fn file_name(&self, header: FaceHeader) -> Option<&str> {
        match (header.width, header.weight, header.style) {
            (Width::Normal, Weight::Normal, Style::Normal) => Some("DejaVuSansMono.ttf"),
            (Width::Normal, Weight::Bold, Style::Normal) => Some("DejaVuSansMono-Bold.ttf"),
            _ => None,
        }
    }
}
