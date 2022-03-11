//! This module exports StateId implementation for Nondeterministic Finite Automata.

use crate::prelude::*;

use crate::alphabet;
use crate::symbol::Symbol;

use crate::nfa::Nfa; // FIXME



// ===========
// == StateId ==
// ===========

/// A state identifier for an arbitrary finite automaton.
#[derive(Derivative)]
#[derivative(Clone(bound = ""))]
#[derivative(Copy(bound = ""))]
#[derivative(Eq(bound = ""))]
#[derivative(Hash(bound = ""))]
#[derivative(Ord(bound = ""))]
#[derivative(PartialEq(bound = ""))]
#[derivative(PartialOrd(bound = ""))]
#[allow(missing_docs)]
pub struct StateId<T> {
    tp: PhantomData<T>,
    id: usize,
}

impl<T> StateId<T> {
    /// An identifier representing the invalid state.
    ///
    /// When in an invalid state, a finite automaton will reject the sequence of input symbols.
    pub const INVALID: StateId<T> = Self::new(usize::max_value());
}

impl<T> StateId<T> {
    /// Constructor. Not exposed to public as it should never be possible to construct a state
    /// from a number.
    pub(crate) const fn new(id: usize) -> Self {
        let tp = PhantomData;
        Self { tp, id }
    }

    /// Identifier of this state expressed as `usize`.
    pub fn id(self) -> usize {
        self.id
    }

    /// Checks whether this state is valid.
    pub fn is_invalid(self) -> bool {
        self == Self::INVALID
    }
}

// === Trait Impls ===

impl<T> Default for StateId<T> {
    /// Returns state::INVALID. This is because every finite automata has an invalid state
    /// and because all transitions in automata transition matrix lead to invalid state by default.
    fn default() -> Self {
        StateId::INVALID
    }
}

impl<T> Debug for StateId<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = if *self == Self::INVALID { "INVALID".into() } else { format!("{:?}", self.id) };
        write!(f, "StateId({})", name)
    }
}



// =============
// === Tests ===
// =============

// #[cfg(test)]
// mod tests {
//     use super::super::alphabet;
//     use super::*;
//
//     // === Trait Impls ====
//
//     impl From<Vec<Transition>> for Data {
//         fn from(links: Vec<Transition>) -> Self {
//             let epsilon_links = vec![];
//             let export = false;
//             Data { epsilon_links, links, export }
//         }
//     }
//
//
//     // === The Tests ===
//
//     #[test]
//     fn state_default() {
//         assert_eq!(StateId::<Nfa>::default(), StateId::<Nfa>::INVALID);
//     }
//
//     #[test]
//     fn state_data_default() {
//         let state = Data::default();
//         assert!(state.epsilon_links().is_empty());
//         assert!(state.links().is_empty());
//         assert!(!state.export)
//     }
//
//     #[test]
//     fn state_targets() {
//         let alphabet = alphabet::Segmentation::from_divisions(&[0, 5, 10, 15, 25, 50]);
//         let state = Data::from(vec![
//             Transition::new(Symbol::from(0u64)..=Symbol::from(10u64), StateId::<Nfa>::new(1)),
//             Transition::new(Symbol::from(5u64)..=Symbol::from(15u64), StateId::<Nfa>::new(2)),
//         ]);
//         assert_eq!(state.links().len(), 2);
//         let targets = state.targets(&alphabet);
//         let expected_targets: Vec<StateId<Nfa>> = vec![
//             StateId::<Nfa>::new(1),
//             StateId::<Nfa>::new(1),
//             StateId::<Nfa>::new(1),
//             StateId::<Nfa>::new(2),
//             StateId::<Nfa>::INVALID,
//             StateId::<Nfa>::INVALID,
//         ];
//         assert_eq!(expected_targets, targets);
//     }
// }
