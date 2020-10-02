//! This module exports State implementation for Nondeterministic Finite Automata.

use crate::alphabet;
use crate::symbol::Symbol;

use crate::prelude::*;

use crate::nfa::Nfa; // FIXME

// ===========
// == State ==
// ===========

/// A state identifier for an arbitrary finite automaton.
#[derive(Derivative)]
#[derivative(Clone(bound=""))]
#[derivative(Copy(bound=""))]
#[derivative(Eq(bound=""))]
#[derivative(Hash(bound=""))]
#[derivative(Ord(bound=""))]
#[derivative(PartialEq(bound=""))]
#[derivative(PartialOrd(bound=""))]
#[allow(missing_docs)]
pub struct State<T> {
    tp : PhantomData<T>,
    id : usize
}

impl<T> State<T> {
    /// An identifier representing the invalid state.
    ///
    /// When in an invalid state, a finite automaton will reject the sequence of input symbols.
    pub const INVALID : State<T> = Self::new(usize::max_value());
}

impl<T> State<T> {
    /// Constructor. Not exposed to public as it should never be possible to construct a state
    /// from a number.
    pub(crate) const fn new(id:usize) -> Self {
        let tp = PhantomData;
        Self {tp,id}
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

impl<T> Default for State<T> {
    /// Returns state::INVALID. This is because every finite automata has an invalid state
    /// and because all transitions in automata transition matrix lead to invalid state by default.
    fn default() -> Self {
        State::INVALID
    }
}

impl<T> Debug for State<T> {
    fn fmt(&self, f:&mut fmt::Formatter<'_>) -> fmt::Result {
        let name = if *self == Self::INVALID { "INVALID".into() } else { format!("{:?}",self.id) };
        write!(f,"State({})",name)
    }
}



// ==========
// == Data ==
// ==========

/// A named state for a [`super::nfa::NFA`].
#[derive(Clone,Debug,Default,PartialEq,Eq)]
pub struct Data {
    /// A set of transitions that can trigger without consuming a symbol (ε-transitions).
    pub epsilon_links: Vec<State<Nfa>>,
    /// The set of transitions that trigger while consuming a specific symbol.
    ///
    /// When triggered, the automaton will transition to the [`Transition::target_state`].
    pub links: Vec<Transition>,
    /// Information whether the state should be exported and marked as a "source" state in the DFA
    /// representation. Non exported states are considered "transitive" states and are used as
    /// helpers to design the NFA network. All user defined states are marked to be exported.
    pub export : bool,
}

impl Data {
    /// Returns transition (next state) for each symbol in alphabet.
    pub fn targets(&self, alphabet:&alphabet::Segmentation) -> Vec<State<Nfa>> {
        let mut targets = vec![];
        let mut index   = 0;
        let mut links   = self.links.clone();
        links.sort_by_key(|link| link.symbols.start().clone());
        for symbol in &alphabet.divisions {
            while links.len() > index && links[index].symbols.end() < symbol {
                index += 1;
            }
            if links.len() <= index || links[index].symbols.start() > symbol {
                targets.push(State::INVALID);
            } else {
                targets.push(links[index].target);
            }
        }
        targets
    }
}



// ==================
// === Transition ===
// ==================

/// A transition between states in a finite automaton that must consume a symbol to trigger.
#[derive(Clone,Debug,PartialEq,Eq)]
pub struct Transition {
    /// The range of symbols on which this transition will trigger.
    pub symbols: RangeInclusive<Symbol>,
    /// The state that is entered after the transition has triggered.
    pub target: State<Nfa>,
}

impl Transition {
    /// Constructor.
    pub fn new(symbols:RangeInclusive<Symbol>, target:State<Nfa>) -> Self {
        Self {symbols,target}
    }

    /// Display the symbols range of this tansition.
    pub fn display_symbols(&self) -> String {
        if self.symbols.start() == self.symbols.end() {
            format!("{}",self.symbols.start())
        } else {
            format!("{} .. {}",self.symbols.start(),self.symbols.end())
        }
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    // === Trait Impls ====

    impl From<Vec<usize>> for Data {
        /// Creates a state with epsilon links.
        fn from(vec:Vec<usize>) -> Self {
            let epsilon_links = vec.iter().cloned().map(State::new).collect();
            Data {epsilon_links,..Default::default()}
        }
    }

    impl From<Vec<(RangeInclusive<u64>, usize)>> for Data {
        /// Creates a state with ordinary links.
        fn from(vec:Vec<(RangeInclusive<u64>, usize)>) -> Self {
            let link = |(range, id): (RangeInclusive<u64>, usize)| {
                let start = Symbol::new(*range.start());
                let end   = Symbol::new(*range.end());
                Transition::new(start..=end, State::new(id))
            };
            let links = vec.iter().cloned().map(link).collect();
            Data {links,..Default::default()}
        }
    }
}
