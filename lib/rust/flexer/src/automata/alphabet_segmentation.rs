//! Exports an alphabet for an arbitrary finite state automaton.

use crate::automata::symbol::Symbol;

use std::collections::BTreeSet;
use std::ops::RangeInclusive;
use enso_prelude::default;


// ================
// === Alphabet ===
// ================

/// A representation of the distinct intervals over the input alphabet for a given finite state
/// automaton.
///
/// These intervals are defined by a set of _divisions_ of the input alphabet, where each division
/// is represented as a point in that alphabet. This is necessary to allow for efficient encoding of
/// state transitions that trigger not just on _one_, but potentially on _many_ of the input
/// symbols in the automaton's alphabet.
///
/// This is best explained by way of example. Consider the original unbounded alphabet:
///
/// ```text
///  ... a b c d e f g h ... z ...
/// ```
///
/// We want to add a rule that matches on the interval `[b, d]`. This results in there being three
/// intervals on the alphabet, as there are two divisions (annotated below):
///
/// ```text
///  ... a | b c d | e f g h ... z ...
/// div:   1       2
/// seg: 1   2       3
/// ```
///
/// If we then add a rule that matches on the interval `[d, f]`, we end up with five intervals on
/// the alphabet, with four divisions (annotated below):
///
/// ```text
///  ... a | b c | d | e f | g h ... z ...
/// div:   1     2   3     4
/// seg: 1   2     3  4      5
/// ```
///
/// This type tracks these divisions explicitly for an input alphabet defined for all automata in
/// this library as `0u32..=u32::max_value()`.
#[derive(Clone,Debug,PartialEq,Eq)]
pub struct AlphabetSegmentation {
    /// The divisions over the set of input symbols.
    pub divisions: BTreeSet<Symbol>
}

impl AlphabetSegmentation {
    /// Inserts a range of symbols into the alphabet.
    pub fn insert(&mut self, range:RangeInclusive<Symbol>) {
        // The symbol range is associated with a transition in the automaton. Therefore we:
        // Mark the symbol with the new transition.
        self.divisions.insert(Symbol{val:range.start().val});
        // Mark the symbol without the new transition.
        self.divisions.insert(Symbol{val:range.end().val + 1});
        // This way each symbol in alphabet corresponds to a unique set of transitions.
    }

    /// Creates an [`AlphabetSegmentation`] from an input set of divisions.
    pub fn from_divisions(divisions:Vec<u32>) -> Self {
        let mut dict = Self::default();
        for val in divisions {
            dict.divisions.insert(Symbol::from(val));
        }
        dict
    }
}


// === Trait Impls ===

impl Default for AlphabetSegmentation {
    fn default() -> Self {
        AlphabetSegmentation { divisions:[default()].iter().cloned().collect()}
    }
}
