//! Exports an alphabet for an arbitrary finite state automaton.

use crate::automata::symbol::Symbol;

use crate::prelude::*;
use std::collections::BTreeSet;
use std::ops::RangeInclusive;



// ====================
// === Segmentation ===
// ====================

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
#[allow(missing_docs)]
pub struct Segmentation {
    pub divisions: BTreeSet<Symbol>
}

impl Segmentation {
    /// Inserts a range of symbols into the alphabet.
    pub fn insert(&mut self, range:RangeInclusive<Symbol>) {
        self.divisions.insert(Symbol::from(range.start()));
        if range.end().val != Symbol::EOF_CODE.val {
            self.divisions.insert(Symbol{val:range.end().val + 1});
        }
    }

    /// Creates a [`Segmentation`] from an input set of divisions.
    pub fn from_divisions(divisions:&[u32]) -> Self {
        let mut dict = Self::default();
        for val in divisions {
            dict.divisions.insert(Symbol::from(*val));
        }
        dict
    }
}


// === Trait Impls ===

impl Default for Segmentation {
    fn default() -> Self {
        let mut divisions: BTreeSet<Symbol> = default();
        // The existence of the default (0) member in the set is assumed by the implementation of
        // the NFA -> DFA conversion.
        divisions.insert(default());
        Segmentation { divisions }
    }
}
