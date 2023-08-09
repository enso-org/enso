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

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
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



// ===================
// === Font Family ===
// ===================

/// Definition of a font family. Font family consist of one font face in case of variable fonts or
/// multiple font faces in case of non-variable ones.
#[allow(missing_docs)]
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FontFamily {
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
    variations: std::collections::HashMap<NonVariableFaceHeader, String>,
}

impl NonVariableDefinition {
    /// All weights defined in this font family.
    pub fn possible_weights(&self) -> Vec<Weight> {
        let mut weights: Vec<_> = self.variations().map(|var| var.header.weight).collect();
        weights.sort_unstable_by_key(|w| w.to_number());
        weights.dedup();
        weights
    }

    /// Return an iterator over the filenames associated with fonts in this family.
    pub fn files(&self) -> impl Iterator<Item = &str> {
        self.variations().map(|v| v.file)
    }

    /// Return an iterator over the fonts in this family.
    pub fn variations(&self) -> impl Iterator<Item = NonVariableVariation> {
        self.variations
            .iter()
            .map(|(header, file)| NonVariableVariation { header: *header, file: file.as_str() })
    }

    /// Return the font in this family that exactly matches the given parameters, if any.
    pub fn get(&self, header: NonVariableFaceHeader) -> Option<NonVariableVariation> {
        self.variations.get(&header).map(|file| NonVariableVariation { header, file })
    }
}

impl FromIterator<(NonVariableFaceHeader, String)> for NonVariableDefinition {
    fn from_iter<T>(iter: T) -> Self
    where T: IntoIterator<Item = (NonVariableFaceHeader, String)> {
        let map = iter.into_iter().collect();
        Self { variations: map }
    }
}

impl<'a> FromIterator<NonVariableVariation<'a>> for NonVariableDefinition {
    fn from_iter<T>(iter: T) -> Self
    where T: IntoIterator<Item = NonVariableVariation<'a>> {
        let map = iter.into_iter().map(|v| (v.header, v.file.to_owned())).collect();
        Self { variations: map }
    }
}



// ============================
// === NonVariableVariation ===
// ============================

/// The parameters of a font, and the filename where it should be found.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NonVariableVariation<'a> {
    /// The parameters of the font.
    pub header: NonVariableFaceHeader,
    /// The filename for the font.
    pub file:   &'a str,
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
    pub const fn new(width: Width, weight: Weight, style: Style) -> Self {
        Self { width, weight, style }
    }
}
