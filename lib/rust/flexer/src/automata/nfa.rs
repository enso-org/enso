//! The structure for defining non-deterministic finite automata.

use crate::automata::alphabet;
use crate::automata::dfa::DFA;
use crate::automata::dfa::RuleExecutable;
use crate::automata::pattern::Pattern;
use crate::automata::state::State;
use crate::automata::state::Transition;
use crate::automata::state;
use crate::automata::symbol::Symbol;
use crate::data::matrix::Matrix;

use itertools::Itertools;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::ops::RangeInclusive;

use crate::prelude::*;



// =========================================
// === Non-Deterministic Finite Automata ===
// =========================================

/// A state identifier based on a set of states.
///
/// This is used during the NFA -> DFA transformation, where multiple states can merge together due
/// to the collapsing of epsilon transitions.
type StateSetId = BTreeSet<state::Identifier>;

/// The definition of a [NFA](https://en.wikipedia.org/wiki/Nondeterministic_finite_automaton) for a
/// given set of symbols, states, and transitions (specifically a NFA with ε-moves).
///
/// A NFA is a finite state automaton that accepts or rejects a given sequence of symbols. In
/// contrast with a DFA, the NFA may transition between states _without_ reading any new symbol
/// through use of
/// [epsilon links](https://en.wikipedia.org/wiki/Nondeterministic_finite_automaton#NFA_with_%CE%B5-moves).
///
/// ```text
///  ┌───┐  'N'  ┌───┐    ┌───┐  'F'  ┌───┐    ┌───┐  'A'  ┌───┐
///  │ 0 │ ----> │ 1 │ -> │ 2 │ ----> │ 3 │ -> │ 3 │ ----> │ 3 │
///  └───┘       └───┘ ε  └───┘       └───┘ ε  └───┘       └───┘
/// ```
#[derive(Clone,Debug,Default,PartialEq,Eq)]
pub struct NFA {
    /// A set of disjoint intervals over the input alphabet.
    pub alphabet_segmentation:alphabet::Segmentation,
    /// A set of named NFA states, with (epsilon) transitions.
    pub states:Vec<State>,
}

impl NFA {
    /// Adds a new state to the NFA and returns its identifier.
    pub fn new_state(&mut self) -> state::Identifier {
        let id = self.states.len();
        self.states.push(State::default());
        state::Identifier{id}
    }

    /// Creates an epsilon transition between two states.
    ///
    /// Whenever the automaton happens to be in `source` state it can immediately transition to the
    /// `target` state. It is, however, not _required_ to do so.
    pub fn connect(&mut self, source:state::Identifier, target:state::Identifier) {
        self.states[source.id].epsilon_links.push(target);
    }

    /// Creates an ordinary transition for a range of symbols.
    ///
    /// If any symbol from such range happens to be the input when the automaton is in the `source`
    /// state, it will immediately transition to the `target` state.
    pub fn connect_via
    ( &mut self
    , source       : state::Identifier
    , target_state : state::Identifier
    , symbols      : &RangeInclusive<Symbol>
    ) {
        self.alphabet_segmentation.insert(symbols.clone());
        self.states[source.id].links.push(Transition{symbols:symbols.clone(),target_state});
    }

    /// Transforms a pattern to an NFA using the algorithm described
    /// [here](https://www.youtube.com/watch?v=RYNN-tb9WxI).
    /// The asymptotic complexity is linear in number of symbols.
    pub fn new_pattern(&mut self, source:state::Identifier, pattern:&Pattern) -> state::Identifier {
        let current = self.new_state();
        self.connect(source,current);
        match pattern {
            Pattern::Range(range) => {
                let state = self.new_state();
                self.connect_via(current,state,range);
                state
            },
            Pattern::Many(body) => {
                let s1 = self.new_state();
                let s2 = self.new_pattern(s1,body);
                let s3 = self.new_state();
                self.connect(current,s1);
                self.connect(current,s3);
                self.connect(s2,s3);
                self.connect(s3,s1);
                s3
            },
            Pattern::Seq(patterns) => {
                patterns.iter().fold(current,|s,pat| self.new_pattern(s,pat))
            },
            Pattern::Or(patterns) => {
                let states = patterns.iter().map(|pat| self.new_pattern(current,pat)).collect_vec();
                let end    = self.new_state();
                for state in states {
                    self.connect(state,end);
                }
                end
            },
            Pattern::Always => current,
        }
    }

    /// Merges states that are connected by epsilon links, using an algorithm based on the one shown
    /// [here](https://www.youtube.com/watch?v=taClnxU-nao).
    fn eps_matrix(&self) -> Vec<StateSetId> {
        fn fill_eps_matrix
        ( nfa      : &NFA
        , states   : &mut Vec<StateSetId>
        , visited  : &mut Vec<bool>
        , state    : state::Identifier
        ) {
            let mut state_set = StateSetId::new();
            visited[state.id] = true;
            state_set.insert(state);
            for &target in &nfa.states[state.id].epsilon_links {
                if !visited[target.id] {
                    fill_eps_matrix(nfa,states,visited,target);
                }
                state_set.insert(target);
                state_set.extend(states[target.id].iter());
            }
            states[state.id] = state_set;
        }

        let mut states = vec![StateSetId::new(); self.states.len()];
        for id in 0..self.states.len() {
            let mut visited = vec![false; states.len()];
            fill_eps_matrix(self,&mut states,&mut visited,state::Identifier{id});
        }
        states
    }

    /// Computes a transition matrix `(state, symbol) => state` for the NFA, ignoring epsilon links.
    fn nfa_matrix(&self) -> Matrix<state::Identifier> {
        let mut matrix = Matrix::new(self.states.len(),self.alphabet_segmentation.divisions.len());

        for (state_ix, source) in self.states.iter().enumerate() {
            let targets = source.targets(&self.alphabet_segmentation);
            for (voc_ix, &target) in targets.iter().enumerate() {
                matrix[(state_ix,voc_ix)] = target;
            }
        }
        matrix
    }
}


// === Trait Impls ===

impl From<&NFA> for DFA {

    /// Transforms an NFA into a DFA, based on the algorithm described
    /// [here](https://www.youtube.com/watch?v=taClnxU-nao).
    /// The asymptotic complexity is quadratic in number of states.
    fn from(nfa:&NFA) -> Self {
        let     nfa_mat     = nfa.nfa_matrix();
        let     eps_mat     = nfa.eps_matrix();
        let mut dfa_mat     = Matrix::new(0,nfa.alphabet_segmentation.divisions.len());
        let mut dfa_eps_ixs = Vec::<StateSetId>::new();
        let mut dfa_eps_map = HashMap::<StateSetId,state::Identifier>::new();

        dfa_eps_ixs.push(eps_mat[0].clone());
        dfa_eps_map.insert(eps_mat[0].clone(),state::Identifier::from(0));

        let mut i = 0;
        while i < dfa_eps_ixs.len()  {
            dfa_mat.new_row();
            for voc_ix in 0..nfa.alphabet_segmentation.divisions.len() {
                let mut eps_set = StateSetId::new();
                for &eps_ix in &dfa_eps_ixs[i] {
                    let tgt = nfa_mat[(eps_ix.id,voc_ix)];
                    if tgt != state::Identifier::INVALID {
                        eps_set.extend(eps_mat[tgt.id].iter());
                    }
                }
                if !eps_set.is_empty() {
                    dfa_mat[(i,voc_ix)] = match dfa_eps_map.get(&eps_set) {
                        Some(&id) => id,
                        None => {
                            let id = state::Identifier::new(dfa_eps_ixs.len());
                            dfa_eps_ixs.push(eps_set.clone());
                            dfa_eps_map.insert(eps_set,id);
                            id
                        },
                    };
                }
            }
            i += 1;
        }

        let mut callbacks = vec![None; dfa_eps_ixs.len()];
        let     priority  = dfa_eps_ixs.len();
        for (dfa_ix, epss) in dfa_eps_ixs.into_iter().enumerate() {
            let has_name = |&key:&state::Identifier| nfa.states[key.id].name.is_some();
            if let Some(eps) = epss.into_iter().find(has_name) {
                let code          = nfa.states[eps.id].name.as_ref().cloned().unwrap();
                callbacks[dfa_ix] = Some(RuleExecutable {code,priority});
            }
        }

        let alphabet_segmentation = nfa.alphabet_segmentation.clone();
        let links = dfa_mat;

        DFA{alphabet_segmentation,links,callbacks}
    }
}



// ===========
// == Tests ==
// ===========

#[cfg(test)]
pub mod tests {
    extern crate test;

    use crate::automata::dfa;

    use super::*;
    use test::Bencher;

    /// NFA that accepts a newline '\n'.
    pub fn newline() -> NFA {
        NFA {
            states:vec![
                State::from(vec![1]),
                State::from(vec![(10..=10,2)]),
                State::from(vec![3]).named("group_0_rule_0"),
                State::default(),
            ],
            alphabet_segmentation:alphabet::Segmentation::from_divisions(vec![10, 11].as_slice()),
        }
    }

    /// NFA that accepts any letter in the range a..=z.
    pub fn letter() -> NFA {
        NFA {
            states:vec![
                State::from(vec![1]),
                State::from(vec![(97..=122,2)]),
                State::from(vec![3]).named("group_0_rule_0"),
                State::default(),
            ],
            alphabet_segmentation:alphabet::Segmentation::from_divisions(vec![97, 123].as_slice()),
        }
    }

    /// NFA that accepts any number of spaces ' '.
    pub fn spaces() -> NFA {
        NFA {
            states:vec![
                State::from(vec![1]),
                State::from(vec![2]),
                State::from(vec![(32..=32,3)]),
                State::from(vec![4]),
                State::from(vec![5,8]),
                State::from(vec![6]),
                State::from(vec![(32..=32,7)]),
                State::from(vec![8]),
                State::from(vec![5,9]).named("group_0_rule_0"),
                State::default(),
            ],
            alphabet_segmentation:alphabet::Segmentation::from_divisions(vec![0, 32, 33].as_slice()),
        }
    }

    /// NFA that accepts one letter a..=z or many spaces ' '.
    pub fn letter_and_spaces() -> NFA {
        NFA {
            states:vec![
                State::from(vec![1,3]),
                State::from(vec![(97..=122,2)]),
                State::from(vec![11]).named("group_0_rule_0"),
                State::from(vec![4]),
                State::from(vec![(32..=32,5)]),
                State::from(vec![6]),
                State::from(vec![7,10]),
                State::from(vec![8]),
                State::from(vec![(32..=32,9)]),
                State::from(vec![10]),
                State::from(vec![7,11]).named("group_0_rule_1"),
                State::default(),
            ],
            alphabet_segmentation:alphabet::Segmentation::from_divisions(vec![32, 33, 97, 123].as_slice()),
        }
    }

    #[test]
    fn test_to_dfa_newline() {
        assert_eq!(DFA::from(&newline()),dfa::tests::newline());
    }

    #[test]
    fn test_to_dfa_letter() {
        assert_eq!(DFA::from(&letter()),dfa::tests::letter());
    }

    #[test]
    fn test_to_dfa_spaces() {
        assert_eq!(DFA::from(&spaces()),dfa::tests::spaces());
    }

    #[test]
    fn test_to_dfa_letter_and_spaces() {
        assert_eq!(DFA::from(&letter_and_spaces()),dfa::tests::letter_and_spaces());
    }

    #[bench]
    fn bench_to_dfa_newline(bencher:&mut Bencher) {
        bencher.iter(|| DFA::from(&newline()))
    }

    #[bench]
    fn bench_to_dfa_letter(bencher:&mut Bencher) {
        bencher.iter(|| DFA::from(&letter()))
    }

    #[bench]
    fn bench_to_dfa_spaces(bencher:&mut Bencher) {
        bencher.iter(|| DFA::from(&spaces()))
    }

    #[bench]
    fn bench_to_dfa_letter_and_spaces(bencher:&mut Bencher) {
        bencher.iter(|| DFA::from(&letter_and_spaces()))
    }
}
