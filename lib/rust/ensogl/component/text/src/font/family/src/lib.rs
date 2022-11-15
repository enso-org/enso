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
#![feature(associated_type_defaults)]
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
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
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

use derive_more::Deref;
use derive_more::Display;
use std::collections::HashMap;


// ==============
// === Export ===
// ==============

pub use owned_ttf_parser::Style;
pub use owned_ttf_parser::Weight;
pub use owned_ttf_parser::Width;



// ============
// === Name ===
// ============

/// A name of a font. The name is normalized to case-insensitive form during construction to
/// eliminate accidental mistakes, the same way as it's done in CSS:
/// https://stackoverflow.com/questions/17967371/are-property-values-in-css-case-sensitive
#[allow(missing_docs)]
#[derive(Clone, Debug, Deref, Display, Hash, PartialEq, Eq)]
pub struct Name {
    pub normalized: String,
}

impl From<&Name> for Name {
    fn from(name: &Name) -> Self {
        name.clone()
    }
}

impl From<&str> for Name {
    fn from(name: &str) -> Self {
        let normalized = name.to_lowercase();
        Name { normalized }
    }
}

impl From<&String> for Name {
    fn from(name: &String) -> Self {
        name.as_str().into()
    }
}

impl From<String> for Name {
    fn from(name: String) -> Self {
        (&name).into()
    }
}



// ==================
// === Definition ===
// ==================

/// Definition of a font family. Font family consist of one font face in case of variable fonts or
/// multiple font faces in case of non-variable ones.
#[allow(missing_docs)]
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Definition {
    Variable(VariableDefinition),
    NonVariable(NonVariableDefinition),
}



// ==========================
// === VariableDefinition ===
// ==========================

/// Definition of a variable font family. See the following link to learn more about variable fonts:
/// https://docs.microsoft.com/en-us/windows/win32/directwrite/opentype-variable-fonts
#[allow(missing_docs)]
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VariableDefinition {
    /// Name of the file that the font data was read from. It contains the file extension, for
    /// example `MPLUS1[wght].ttf`.
    pub file_name: String,
}

impl VariableDefinition {
    /// Constructor.
    pub fn new(file_name: impl Into<String>) -> Self {
        let file_name = file_name.into();
        Self { file_name }
    }
}



// =============================
// === NonVariableDefinition ===
// =============================

/// Definition of a non-variable font family. Contains mapping between (width, weight, style) triple
/// (see [`NonVariableFaceHeader`]) to learn more) and file names.
#[allow(missing_docs)]
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NonVariableDefinition {
    pub map: HashMap<NonVariableFaceHeader, String>,
}

impl NonVariableDefinition {
    /// Constructor.
    pub fn new(map: HashMap<NonVariableFaceHeader, String>) -> Self {
        Self { map }
    }

    /// All weights defined in this font family.
    pub fn possible_weights(&self) -> Vec<Weight> {
        self.map.keys().map(|header| header.weight).collect()
    }
}

impl FromIterator<(NonVariableFaceHeader, String)> for NonVariableDefinition {
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

    /// Distance between two font variations. It is used to find the closest variations if the
    /// provided is not available.
    pub fn similarity_distance(&self, other: NonVariableFaceHeader) -> usize {
        let width_weight = 10;
        let weight_weight = 100;
        let style_weight = 1;

        let self_width = self.width.to_number() as usize;
        let self_weight = self.weight.to_number() as usize;
        let self_style: usize = match self.style {
            Style::Normal => 0,
            Style::Italic => 1,
            Style::Oblique => 2,
        };

        let other_width = other.width.to_number() as usize;
        let other_weight = other.weight.to_number() as usize;
        let other_style: usize = match other.style {
            Style::Normal => 0,
            Style::Italic => 1,
            Style::Oblique => 2,
        };

        let width = self_width.abs_diff(other_width) * width_weight;
        let weight = self_weight.abs_diff(other_weight) * weight_weight;
        let style = self_style.abs_diff(other_style) * style_weight;
        width + weight + style
    }
}



// ==================================
// === NonVariableFaceHeaderMatch ===
// ==================================

/// Indicates whether the provided variation was an exact match or a closest match was found.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[allow(missing_docs)]
pub enum NonVariableFaceHeaderMatchType {
    Exact,
    Closest,
}

/// Result of finding a closest font variation for a non-variable font family.
#[derive(Debug, Copy, Clone)]
#[allow(missing_docs)]
pub struct NonVariableFaceHeaderMatch {
    pub variations: NonVariableFaceHeader,
    pub match_type: NonVariableFaceHeaderMatchType,
}

impl NonVariableFaceHeaderMatch {
    /// Constructor.
    pub fn exact(variations: NonVariableFaceHeader) -> Self {
        Self { variations, match_type: NonVariableFaceHeaderMatchType::Exact }
    }

    /// Constructor.
    pub fn closest(variations: NonVariableFaceHeader) -> Self {
        Self { variations, match_type: NonVariableFaceHeaderMatchType::Closest }
    }

    /// Checks whether the match was exact.
    pub fn was_exact(&self) -> bool {
        self.match_type == NonVariableFaceHeaderMatchType::Exact
    }

    /// Checks whether the match was closest.
    pub fn was_closest(&self) -> bool {
        self.match_type == NonVariableFaceHeaderMatchType::Closest
    }
}
