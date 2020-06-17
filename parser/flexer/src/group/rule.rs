//! An API for declaring Rust callbacks for encountered regex patterns.
//!
use crate::automata::pattern::Pattern;

// ==========
// == Rule ==
// ==========

/// A rule is a pair of regex pattern and callback.
/// The intention is to run the callback after encountering given pattern.
#[derive(Clone, Debug)]
pub struct Rule {
    /// Pattern that triggers the callback.
    pub pattern:Pattern,
    /// Callback containing stringified Rust code.
    pub callback:String,
}

/// Builder that allows us to add `Rule` to `Group` in a nice way.
/// It is possible this structure won't be useful in rust, since borrow checker
/// will likely influence the final API of rule construction.
#[derive(Clone, Debug)]
pub struct Builder<Callback> {
    /// Pattern that triggers the callback.
    pub pattern:Pattern,
    /// Callback containing a closure.
    pub callback:Callback,
}

impl<F:FnMut(Rule)> Builder<F> {
    /// Feeds the input that triggered regex pattern to callback.
    pub fn run(&mut self, program:String) {
        let rule = Rule {
            pattern:self.pattern.clone(),
            callback:program,
        };
        (self.callback)(rule);
    }
}
