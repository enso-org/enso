//! This module exports State implementation for Nondeterministic Finite
//! Automata.

use crate::automata::alphabet::Alphabet;
use crate::automata::state;

use std::ops::RangeInclusive;

// =======================
// == State Of Automata ==
// =======================

/// Flag for invalid state.
/// When finite automata gets into invalid state the input sequence of symbols
/// is rejected.
pub const INVALID:Id = Id {
    id:usize::max_value(),
};

/// Newtype wrapper for finite automata input symbol.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol {
    #[allow(missing_docs)]
    pub val:u32,
}

/// Newtype wrapper for finite automata state ID.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Id {
    #[allow(missing_docs)]
    pub id:usize,
}

impl Default for Id {
    /// Returns state::INVALID. This is because every finite automata has an
    /// invalid state and because all transitions in automata transition
    /// matrix lead to invalid state by default.
    fn default() -> Self { state::INVALID }
}

/// Named NFA state with a set of transitions (links).
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct State {
    /// Set of transitions that don't require any symbol to trigger.
    /// I.E. If there is an epsilon link from state A to state B, then whenever
    /// we are in state A, we can freely move to state B.
    pub epsilon_links:Vec<Id>,
    /// Set of transitions that trigger with specific symbol on input.
    /// When triggered, the automata will transition to the `link.target`.
    pub links:Vec<Link>,
    /// Name of the state.
    /// We use it to autogenerate a call to Rust method with same name.
    pub name:Option<String>,
}

/// A transition to new automata state
/// that requires specific symbol on automata input to trigger.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Link {
    /// Any symbol from the range will trigger this link.
    pub symbols:RangeInclusive<Symbol>,
    /// A state that is visited, after the link is triggered.
    pub target:Id,
}

impl State {
    /// Updater for field `name`. Returns updated state.
    pub fn named(mut self, name:&str) -> Self {
        self.name = Some(name.to_owned());
        self
    }

    /// Returns transition (next state) for each symbol in alphabet.
    pub fn targets(&self, alphabet:&Alphabet) -> Vec<Id> {
        let mut targets = vec![];
        let mut index = 0;
        let mut links = self.links.clone();
        links.sort_by_key(|link| *link.symbols.start());
        for &symbol in &alphabet.symbols {
            while links.len() > index && *links[index].symbols.end() < symbol {
                index += 1;
            }
            if links.len() <= index || *links[index].symbols.start() > symbol {
                targets.push(state::INVALID);
            } else {
                targets.push(links[index].target);
            }
        }
        targets
    }
}

impl From<Vec<usize>> for State {
    /// Creates a state with epsilon links.
    fn from(vec:Vec<usize>) -> Self {
        let epsilon_links = vec.iter().cloned().map(|id| Id { id }).collect();
        State {
            epsilon_links,
            ..Default::default()
        }
    }
}

impl From<Vec<(RangeInclusive<u32>, usize)>> for State {
    /// Creates a state with ordinary links.
    fn from(vec:Vec<(RangeInclusive<u32>, usize)>) -> Self {
        let link = |(range, id):(RangeInclusive<u32>, usize)| {
            let start = Symbol { val:*range.start() };
            let end = Symbol { val:*range.end() };
            Link {
                symbols:start..=end,
                target:Id { id },
            }
        };
        let links = vec.iter().cloned().map(link).collect();
        State {
            links,
            ..Default::default()
        }
    }
}
