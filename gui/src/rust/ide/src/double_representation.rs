//! A module with all functions used to synchronize different representations of our language
//! module.

pub mod alias_analysis;
pub mod connection;
pub mod definition;
pub mod graph;
pub mod module;
pub mod node;
pub mod text;

#[cfg(test)]
pub mod test_utils;

use crate::prelude::*;



// ==============
// === Consts ===
// ==============

/// Indentation value from language specification:
///
/// Indentation: Indentation is four spaces, and all tabs are converted to 4 spaces. This is not
/// configurable on purpose.
///
/// Link: https://github.com/luna/enso/blob/main/doc/syntax/encoding.md
pub const INDENT : usize = 4;



// ====================
// === ReferentName ===
// ====================

// === Errors ===

/// Happens if a given string does not fulfill requirements of the referent name;
#[derive(Clone,Debug,Fail)]
#[fail(display="The `{}` is not a valid referent name.",_0)]
pub struct NotReferentName(String);


// === Definition ===

#[derive(Clone,Debug,Display,Shrinkwrap,PartialEq,Eq,Hash)]
/// The name segment is a string that starts with an upper-cased character.
///
/// It is used for naming modules, module path segments and projects.
///
/// This value corresponds to contents of the `Cons` AST shape.
pub struct ReferentName(String);

impl ReferentName {
    /// Check if the given text would be a valid referent name;
    pub fn validate(name:impl AsRef<str>) -> Result<(), NotReferentName> {
        let name = name.as_ref();
        // TODO [mwu]
        //  We should be able to call parser or sth to verify that other requirements for the
        //  referent form identifiers are fulfilled.
        //  This is expected to become properly possible when the Rust rewrite of parser is done.
        //  See: https://github.com/enso-org/enso/issues/435
        let first_char = name.chars().next();
        match first_char {
            Some(c) if c.is_uppercase() => Ok(()),
            _                           => Err(NotReferentName(name.into())),
        }
    }

    /// Try interpreting given string as a referent name.
    ///
    /// Referent name is an identifier starting with an upper-cased letter, like `Maybe`.
    ///
    /// Fails if the given string is not a valid referent name (e.g. an empty string or lower-cased
    /// string).
    pub fn new(name:impl Str) -> Result<ReferentName, NotReferentName> {
        Self::validate(name.as_ref()).map(|_| ReferentName(name.into()))
    }
}


// === Implementations ===

impl AsRef<str> for ReferentName {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}
