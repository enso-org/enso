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
    pub callback: String,
}

/// A builder that allows us to add a [`Rule`] to [`crate::group::Group`] in an elegant way.
#[derive(Clone,Debug)]
pub struct Builder<Callback> {
    /// The pattern that triggers the callback.
    pub pattern: Pattern,

    /// The callback containing a closure
    pub callback: Callback,
}

impl<F:FnMut(Rule)> Builder<F> {
    /// Feeds the input that triggered the [`Builder::pattern`] to the [`Builder::callback`].
    pub fn run(&mut self, program:String){
        let rule = Rule {pattern:self.pattern.clone(),callback:program};
        (self.callback)(rule);
    }
}
