//! Exports an alphabet for an arbitrary finite state automaton.

use crate::symbol::Symbol;

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
/// this library as `0u64..=u64::max_value()`.
#[derive(Clone,Debug,PartialEq,Eq)]
#[allow(missing_docs)]
pub struct Segmentation {
    pub divisions : BTreeSet<Symbol>
}

impl Segmentation {
    /// Inserts a range of symbols into the alphabet.
    pub fn insert(&mut self,range:RangeInclusive<Symbol>) {
        self.divisions.insert(range.start().clone());
        let end = range.end().clone();
        end.next().for_each(|t| self.divisions.insert(t));
    }

    /// Creates an [`AlphabetSegmentation`] from an input set of divisions.
    pub fn from_divisions(divisions:&[u64]) -> Self {
        let mut dict = Self::default();
        for val in divisions {
            dict.divisions.insert(Symbol::from(*val));
        }
        dict
    }

    /// Seal the segmentation.
    pub fn seal(&self) -> SealedSegmentation {
        self.into()
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



// ==========================
// === SealedSegmentation ===
// ==========================

/// An immutable version of `Segmentation` which consists cached information allowing for fast
/// segmentation analysis.
#[derive(Clone,Debug,Default,Eq,PartialEq)]
#[allow(missing_docs)]
pub struct SealedSegmentation {
    pub division_map : BTreeMap<Symbol,usize>
}

impl SealedSegmentation {
    /// The index of the provided symbol. Please note that the index always exists, as the alphabet
    /// spans across all possible symbols.
    pub fn index_of_symbol(&self, symbol:&Symbol) -> usize {
        self.range(symbol..).next().map(|(k,v)|{
            if k == symbol { *v } else { v - 1 }
        }).unwrap_or_else(|| self.len() - 1)
    }
}

impl Deref for SealedSegmentation {
    type Target = BTreeMap<Symbol,usize>;
    fn deref(&self) -> &Self::Target {
        &self.division_map
    }
}

impl From<&Segmentation> for SealedSegmentation {
    fn from(s:&Segmentation) -> Self {
        let division_map = s.divisions.iter().cloned().enumerate().map(|(ix,s)|(s,ix)).collect();
        Self {division_map}
    }
}
