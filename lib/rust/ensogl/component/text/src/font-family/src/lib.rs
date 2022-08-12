//! Definition of a font family, a set of related font faces.
//!
//! # One font face per file
//! The implementation of this library has an important limitation that should not cause any
//! problems, however, it is important to be aware of it. In case of non-variable fonts, only one
//! font face is supported per file. A font face is identified by (width, weight, style) triple (see
//! [`NonVariableFaceHeader`]) to learn more. If you want to use font faces defined in the same
//! file (e.g. ".ttf" file), you have to split them into multiple files first. All major browsers
//! have the same limitation. For example, you are unable to define in CSS a new font family by
//! loading different faces from the same file with the `@font-face` rule
//! (https://developer.mozilla.org/en-US/docs/Web/CSS/@font-face).

// === Features ===
#![allow(incomplete_features)]
#![feature(negative_impls)]
#![feature(associated_type_defaults)]
#![feature(bool_to_option)]
#![feature(cell_update)]
#![feature(const_type_id)]
#![feature(drain_filter)]
#![feature(entry_insert)]
#![feature(fn_traits)]
#![feature(marker_trait_attr)]
#![feature(specialization)]
#![feature(trait_alias)]
#![feature(type_alias_impl_trait)]
#![feature(unboxed_closures)]
#![feature(trace_macros)]
#![feature(const_trait_impl)]
#![feature(slice_as_chunks)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
// === Non-Standard Linter Configuration ===
#![allow(clippy::option_map_unit_fn)]
#![allow(clippy::precedence)]
#![allow(dead_code)]
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]


use std::collections::HashMap;

pub use owned_ttf_parser::Style;
pub use owned_ttf_parser::Weight;
pub use owned_ttf_parser::Width;



// ============
// === Name ===
// ============

/// A name of a font. The name is normalized during construction to eliminate accidental mistakes.
/// The normalization is done by removing all spaces, dashes, and underscores, and replacing all
/// uppercase letters with lowercase ones.
#[allow(missing_docs)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Name {
    pub normalized: String,
}

impl From<&str> for Name {
    fn from(name: &str) -> Self {
        let normalized = name.to_lowercase().replace(' ', "").replace('-', "").replace('_', "");
        Name { normalized }
    }
}

impl From<&String> for Name {
    fn from(name: &String) -> Self {
        let str: &str = &*name;
        str.into()
    }
}

impl From<String> for Name {
    fn from(name: String) -> Self {
        (&name).into()
    }
}



// ========================
// === FamilyDefinition ===
// ========================

/// Definition of a font family. Font family consist of one font face in case of variable fonts or
/// multiple font faces in case of non-variable ones.
#[allow(missing_docs)]
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FamilyDefinition {
    Variable(VariableFamilyDefinition),
    NonVariable(NonVariableFamilyDefinition),
}



// ================================
// === VariableFamilyDefinition ===
// ================================

/// Definition of a variable font family. See the following link to learn more about variable fonts:
/// https://docs.microsoft.com/en-us/windows/win32/directwrite/opentype-variable-fonts
#[allow(missing_docs)]
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VariableFamilyDefinition {
    pub file_name: String,
}

impl VariableFamilyDefinition {
    /// Constructor.
    pub fn new(file_name: impl Into<String>) -> Self {
        let file_name = file_name.into();
        Self { file_name }
    }
}



// ===================================
// === NonVariableFamilyDefinition ===
// ===================================

/// Definition of a non-variable font family. Contains mapping between (width, weight, style) triple
/// (see [`NonVariableFaceHeader`]) to learn more) and file names.
#[allow(missing_docs)]
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NonVariableFamilyDefinition {
    pub map: HashMap<NonVariableFaceHeader, String>,
}

impl NonVariableFamilyDefinition {
    /// Constructor.
    pub fn new(map: HashMap<NonVariableFaceHeader, String>) -> Self {
        Self { map }
    }

    /// All weights defined in this font family.
    pub fn possible_weights(&self) -> Vec<Weight> {
        self.map.keys().map(|header| header.weight).collect()
    }
}

impl FromIterator<(NonVariableFaceHeader, String)> for NonVariableFamilyDefinition {
    fn from_iter<T>(iter: T) -> Self
    where T: IntoIterator<Item = (NonVariableFaceHeader, String)> {
        Self::new(iter.into_iter().collect())
    }
}



// =============================
// === NonVariableFaceHeader ===
// =============================

/// Combination of all information allowing mapping the font face to a font file for non-variable
/// fonts. For variable fonts, there is just one definition for any combination of the parameters.
#[allow(missing_docs)]
#[derive(Clone, Copy, Default, Debug, PartialEq, Eq, Hash)]
pub struct NonVariableFaceHeader {
    pub width:  Width,
    pub weight: Weight,
    pub style:  Style,
}

impl NonVariableFaceHeader {
    /// Constructor.
    pub fn new(width: Width, weight: Weight, style: Style) -> Self {
        Self { width, weight, style }
    }
}



// ======================
// === Embedded Fonts ===
// ======================

/// List of embedded fonts. For now this list is hardcoded, but it should be generated from the
/// build.rs script in the future.
pub fn font_family_files_map() -> HashMap<Name, FamilyDefinition> {
    let mut map = HashMap::new();
    map.insert(
        "mplus1".into(),
        FamilyDefinition::Variable(VariableFamilyDefinition::new("MPLUS1[wght].ttf")),
    );
    map.insert(
        "dejavusans".into(),
        FamilyDefinition::NonVariable(NonVariableFamilyDefinition::from_iter([
            (
                NonVariableFaceHeader::new(Width::Normal, Weight::Normal, Style::Normal),
                "DejaVuSans.ttf".to_string(),
            ),
            (
                NonVariableFaceHeader::new(Width::Normal, Weight::Bold, Style::Normal),
                "DejaVuSans-Bold.ttf".to_string(),
            ),
        ])),
    );
    map.insert(
        "dejavusansmono".into(),
        FamilyDefinition::NonVariable(NonVariableFamilyDefinition::from_iter([
            (
                NonVariableFaceHeader::new(Width::Normal, Weight::Normal, Style::Normal),
                "DejaVuSansMono.ttf".to_string(),
            ),
            (
                NonVariableFaceHeader::new(Width::Normal, Weight::Bold, Style::Normal),
                "DejaVuSansMono-Bold.ttf".to_string(),
            ),
        ])),
    );
    map
}
