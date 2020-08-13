//! This module exports State implementation for Nondeterministic Finite Automata.

use crate::automata::alphabet;
use crate::automata::symbol::Symbol;

use crate::prelude::*;



// ===========
// == State ==
// ===========

/// A named state for a [`super::nfa::NFA`].
#[derive(Clone,Debug,Default,PartialEq,Eq)]
pub struct State {
    /// A set of transitions that can trigger without consuming a symbol (ε-transitions).
    pub epsilon_links:Vec<Identifier>,
    /// The set of transitions that trigger while consuming a specific symbol.
    ///
    /// When triggered, the automaton will transition to the [`Transition::target_state`].
    pub links:Vec<Transition>,
    /// The name of the state.
    ///
    /// This is used to auto-generate a call to the rust method of the same name.
    pub name:Option<String>,
    /// The function to call when evaluating the state.
    pub callback:String
}

impl State {
    /// Updater for field `name`. Returns updated state.
    pub fn named(mut self, name:&str) -> Self {
        self.name = Some(name.to_owned());
        self
    }

    /// Returns transition (next state) for each symbol in alphabet.
    pub fn targets(&self, alphabet:&alphabet::Segmentation) -> Vec<Identifier> {
        let mut targets = vec![];
        let mut index   = 0;
        let mut links   = self.links.clone();
        links.sort_by_key(|link| *link.symbols.start());
        for &symbol in &alphabet.divisions {
            while links.len() > index && *links[index].symbols.end() < symbol {
                index += 1;
            }
            if links.len() <= index || *links[index].symbols.start() > symbol {
                targets.push(Identifier::INVALID);
            } else {
                targets.push(links[index].target_state);
            }
        }
        targets
    }
}


// === Trait Impls ====

impl From<Vec<usize>> for State {
    /// Creates a state with epsilon links.
    fn from(vec:Vec<usize>) -> Self {
        let epsilon_links = vec.iter().cloned().map(|id| Identifier{id}).collect();
        State{epsilon_links,..Default::default()}
    }
}

impl From<Vec<(RangeInclusive<u32>, usize)>> for State {
    /// Creates a state with ordinary links.
    fn from(vec:Vec<(RangeInclusive<u32>, usize)>) -> Self {
        let link = |(range, id): (RangeInclusive<u32>, usize)| {
            let start = Symbol{value:*range.start()};
            let end   = Symbol{value:*range.end()};
            Transition{symbols:start..=end,target_state:Identifier{id}}
        };
        let links = vec.iter().cloned().map(link).collect();
        State{links,..Default::default()}
    }
}



// ================
// == Identifier ==
// ================

/// A state identifier for an arbitrary finite automaton.
#[derive(Clone,Copy,Debug,PartialEq,Eq,PartialOrd,Ord,Hash)]
#[allow(missing_docs)]
pub struct Identifier {
    pub id: usize
}

impl Identifier {
    /// An identifier representing the invalid state.
    ///
    /// When in an invalid state, a finite automaton will reject the sequence of input symbols.
    pub const INVALID:Identifier = Identifier{id:usize::max_value()};

    /// Constructs a new state identifier.
    pub fn new(id:usize) -> Identifier {
        Identifier{id}
    }
}

// === Trait Impls ===

impl Default for Identifier {
    /// Returns state::INVALID. This is because every finite automata has an invalid state
    /// and because all transitions in automata transition matrix lead to invalid state by default.
    fn default() -> Self {
        Identifier::INVALID
    }
}

impl From<usize> for Identifier {
    fn from(id: usize) -> Self {
        Identifier{id}
    }
}



// ============
// === Link ===
// ============

/// A transition between states in a finite automaton that must consume a symbol to trigger.
#[derive(Clone,Debug,PartialEq,Eq)]
pub struct Transition {
    /// The range of symbols on which this transition will trigger.
    pub symbols:RangeInclusive<Symbol>,
    /// The state that is entered after the transition has triggered.
    pub target_state:Identifier,
}
