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


use std::collections::HashMap;
use std::rc::Rc;

pub use owned_ttf_parser::Style;
pub use owned_ttf_parser::Weight;
pub use owned_ttf_parser::Width;



/// Combination of all information allowing mapping the font face to a font file for non-variable
/// fonts. For variable fonts, there is just one definition for any combination of the parameters.
/// The combination reflects how the `@font-face` rule is defined in the CSS. See the following link
/// to learn more: https://www.w3schools.com/cssref/css3_pr_font-face_rule.asp
#[derive(Clone, Copy, Default, Debug, PartialEq, Eq, Hash)]
pub struct NonVariableFontFaceHeader {
    pub width:  Width,
    pub weight: Weight,
    pub style:  Style,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NonVariableFontFamilyDefinition {
    pub map: HashMap<NonVariableFontFaceHeader, String>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VariableFontFamilyDefinition {
    pub file: String,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FontFamilyDefinition {
    Variable(VariableFontFamilyDefinition),
    NonVariable(NonVariableFontFamilyDefinition),
}

impl NonVariableFontFaceHeader {
    pub fn new(width: Width, weight: Weight, style: Style) -> Self {
        Self { width, weight, style }
    }
}

impl NonVariableFontFamilyDefinition {
    pub fn new(map: HashMap<NonVariableFontFaceHeader, String>) -> Self {
        Self { map }
    }

    pub fn from_iter(iter: impl IntoIterator<Item = (NonVariableFontFaceHeader, String)>) -> Self {
        Self::new(iter.into_iter().collect())
    }
}

impl VariableFontFamilyDefinition {
    pub fn new(file: impl Into<String>) -> Self {
        let file = file.into();
        Self { file }
    }
}



pub fn font_family_files_map() -> HashMap<String, FontFamilyDefinition> {
    let mut map = HashMap::new();
    map.insert(
        "mplus1".into(),
        FontFamilyDefinition::Variable(VariableFontFamilyDefinition::new("MPLUS1[wght].ttf")),
    );
    map.insert(
        "dejavusans".into(),
        FontFamilyDefinition::NonVariable(NonVariableFontFamilyDefinition::from_iter([
            (
                NonVariableFontFaceHeader::new(Width::Normal, Weight::Normal, Style::Normal),
                "DejaVuSans.ttf".to_string(),
            ),
            (
                NonVariableFontFaceHeader::new(Width::Normal, Weight::Bold, Style::Normal),
                "DejaVuSans-Bold.ttf".to_string(),
            ),
        ])),
    );
    map
}



//
// // ==================
// // === DejaVuSans ===
// // ==================
//
// /// A type with methods returning names of fonts in the DejaVuSans font family.
// #[derive(Copy, Clone, Debug)]
// pub struct DejaVuSans;
//
// impl Family for DejaVuSans {
//     fn name(&self) -> &str {
//         "DejaVuSans"
//     }
//
//     fn is_variable(&self) -> bool {
//         false
//     }
//
//     fn is_monospace(&self) -> bool {
//         false
//     }
//
//     fn file_name(&self, header: NonVariableFaceHeader) -> Option<&str> {
//         match (header.width, header.weight, header.style) {
//             (Width::Normal, Weight::Normal, Style::Normal) => Some("DejaVuSans.ttf"),
//             (Width::Normal, Weight::Bold, Style::Normal) => Some("DejaVuSans-Bold.ttf"),
//             _ => None,
//         }
//     }
// }
//
//
//
// // ======================
// // === DejaVuSansMono ===
// // ======================
//
// /// A type with methods returning names of fonts in the DejaVuSans font family.
// #[derive(Copy, Clone, Debug)]
// pub struct DejaVuSansMono;
//
// impl Family for DejaVuSansMono {
//     fn name(&self) -> &str {
//         "DejaVuSansMono"
//     }
//
//     fn is_variable(&self) -> bool {
//         false
//     }
//
//     fn is_monospace(&self) -> bool {
//         true
//     }
//
//     fn file_name(&self, header: NonVariableFaceHeader) -> Option<&str> {
//         match (header.width, header.weight, header.style) {
//             (Width::Normal, Weight::Normal, Style::Normal) => Some("DejaVuSansMono.ttf"),
//             (Width::Normal, Weight::Bold, Style::Normal) => Some("DejaVuSansMono-Bold.ttf"),
//             _ => None,
//         }
//     }
// }
