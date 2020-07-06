//! Exports an alphabet for an arbitrary finite state automaton.

use crate::automata::symbol::Symbol;

use std::collections::BTreeSet;
use std::ops::RangeInclusive;
use enso_prelude::default;


// ================
// === Alphabet ===
// ================

/// An alphabet is a description of the set of valid input symbols for a given finite state
/// automaton.
///
/// These alphabets are represented over an interval. This means that if both `a` and `b` are in
/// the alphabet, then any symbol `a..=b` is also in the alphabet. For more information, please see
/// the wiki page on [DFAs](https://en.wikipedia.org/wiki/Deterministic_finite_automaton).
// TODO [AA] Clarify the above, as it doesn't quite make sense to me. It makes it sound like I can't
//  encode a language `a` | `c`
#[derive(Clone,Debug,PartialEq,Eq)]
pub struct Alphabet {
    /// The intervals of all valid input symbols.
    ///
    /// The interval is further divided into sub-intervals (e.g. `[a,z,A,Z]` should be understood to
    /// mean `[a..=z, z..=A, A..=Z]`). This is necessary to allow efficient encoding of state
    /// transitions that trigger on not just _one_, but potentially _many_ symbols.
    pub symbols: BTreeSet<Symbol>
}

impl Alphabet {
    /// Inserts a range of symbols into the alphabet.
    pub fn insert(&mut self, range:RangeInclusive<Symbol>) {
        // The symbol range is associated with a transition in the automaton. Therefore we:
        // Mark the symbol with the new transition.
        self.symbols.insert(Symbol{val:range.start().val});
        // Mark the symbol without the new transition.
        self.symbols.insert(Symbol{val:range.end().val + 1});
        // This way each symbol in alphabet corresponds to a unique set of transitions.
    }
}


// === Trait Impls ===

impl Default for Alphabet {
    fn default() -> Self {
        Alphabet {symbols:[default()].iter().cloned().collect()}
    }
}

impl From<Vec<u32>> for Alphabet {
    fn from(vec:Vec<u32>) -> Self {
        let mut dict = Self::default();
        for val in vec {
            dict.symbols.insert(Symbol{val});
        }
        dict
    }
}
