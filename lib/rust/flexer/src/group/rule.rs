//! An API for declaring rust-code callbacks to be executed when a given pattern is matched.
//!
//! A flexer rule is a [`crate::automata::pattern`] associated with rust code to be executed as a
//! callback.

use crate::automata::pattern::Pattern;



// ==========
// == Rule ==
// ==========

/// A flexer rule.
#[derive(Clone,Debug)]
pub struct Rule {
    /// The pattern that triggers the callback.
    pub pattern: Pattern,

    /// The code to execute when [`Rule::pattern`] matches, containing rust code as a
    /// [`std::string::String`].
    ///
    /// This code will be called directly from a method defined on your Lexer (the one that contains
    /// a [`crate::Flexer`] instance. To this end, the code you provide as a string must be valid in
    /// that context.
    pub callback: String,
}

impl Rule {
    /// Creates a new rule.
    pub fn new(pattern:Pattern, callback:impl Into<String>) -> Self {
        Rule{pattern,callback:callback.into()}
    }
}
