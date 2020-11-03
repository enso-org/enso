//! Macro definitions in Enso.

use crate::prelude::*;



// ==================
// === Definition ===
// ==================

/// A macro definition.
///
/// A macro definition consists of a name, which identifies the macro to users, and a list of
/// [sections](`Section`). The sections are the most important portion of the macro definition, as
/// they define the literal portions of the token stream on which the macro will match.
#[derive(Clone,Debug,Default,Eq,PartialEq)]
pub struct Definition {
    /// The name of the macro definition.
    ///
    /// This is used both in error messages and for the purposes of nesting a macro within another
    /// macro.
    name : String,
    /// The sections that make up the macro.
    sections : Vec<Section>
}

impl Definition {
    /// Constructor.
    pub fn new(name:impl Str, sections:Vec<Section>) -> Self {
        let name = name.into();
        Self{name,sections}
    }

    /// Get the name of the macro.
    pub fn name(&self) -> &str {
        self.name.as_str()
    }

    /// Get the sections that make up this macro definition.
    pub fn sections(&self) -> &[Section] {
        self.sections.as_slice()
    }
}



// ===============
// === Section ===
// ===============

/// A section in a macro, representing both a literal section header to match against, and the
/// tokens that the section contains.
///
/// The literal is the _most_ important portion of a section, as they are constants that allow the
/// macro resolver to divide up the input token stream based on these constants.
#[derive(Clone,Debug,Default,Eq,PartialEq)]
pub struct Section {
    /// The literal that begins the section.
    literal : String,
    // TODO [AA] Pattern
}

impl Section {
    /// Constructor.
    pub fn new(literal:impl Str) -> Self {
        let literal = literal.into();
        Self{literal}
    }

    /// Get the name of the section.
    pub fn name(&self) -> &str {
        self.literal()
    }

    /// Get a reference to the literal that heads the section.
    pub fn literal(&self) -> &str {
        self.literal.as_str()
    }
}


// === Macros ===

// TODO [AA] Macro for section definition.
