//! Implementation of Nondeterministic Finite Automata and it's conversion to DFA.

use crate::automata::alphabet::Alphabet;
use crate::automata::dfa::DFA;
use crate::automata::dfa::Callback;
use crate::automata::state::Link;
use crate::automata::state::Symbol;
use crate::automata::state::State;
use crate::automata::state;
use crate::data::matrix::Matrix;

use std::collections::HashMap;
use std::collections::BTreeSet;
use std::ops::RangeInclusive;
use crate::automata::pattern::Pattern;
use itertools::Itertools;


// ========================================
// === Nondeterministic Finite Automata ===
// ========================================

/// Type alias for a state Id based on set of states.
/// It is used during NFA -> DFA transformation where multiple states can merge together,
/// thanks to epsilon links.
type StateSetId = BTreeSet<state::Id>;

/// NFA automata with a set of symbols, states and transitions.
/// Nondeterministic Finite Automata is a finite-state machine that accepts or rejects a given
/// sequence of symbols.
/// Compared to `DFA`, NFA can transition into multiple new states without reading any symbol
/// (so called epsilon link / transition),
///   ___              ___         ___              ___              ___
///  | 0 | -- 'N' --> | 1 | ----> | 2 | -- 'F' --> | 3 | -- 'A' --> | 4 |
///   ‾‾‾              ‾‾‾         ‾‾‾              ‾‾‾              ‾‾‾
/// More information at: https://en.wikipedia.org/wiki/Deterministic_finite_automaton
#[derive(Clone,Debug,Default,PartialEq,Eq)]
pub struct NFA {
    /// Finite set of all valid input symbols.
    pub alphabet: Alphabet,
    /// Set of named NFA states with (epsilon) transitions.
    pub states: Vec<State>,
}

impl NFA {
    /// Adds a new state to NFA and returns it's Id.
    pub fn new_state(&mut self) -> state::Id {
        let id = self.states.len();
        self.states.push(State::default());
        state::Id {id}
    }

    /// Creates an epsilon transition between two states.
    /// Whenever the automata happens to be in `source` state  it can immediatelly move to
    /// `target` state (but does not have to).
    pub fn connect(&mut self, source:state::Id, target:state::Id) {
        self.states[source.id].epsilon_links.push(target);
    }

    /// Creates an ordinary transition (for a range of symbols) between two states.
    /// If any symbol from such range happens to be on input when the automata  is in `source`
    /// state, it will immediatelly move to `target` state.
    pub fn connect_by
    (&mut self, source:state::Id, target:state::Id, symbols:&RangeInclusive<Symbol>) {
        self.alphabet.insert(symbols.clone());
        self.states[source.id].links.push(Link{symbols:symbols.clone(), target});
    }

    /// Transforms pattern to NFA.
    /// The algorithm is based on: https://www.youtube.com/watch?v=RYNN-tb9WxI
    pub fn new_pattern(&mut self, source:state::Id, pattern:&Pattern) -> state::Id {
        let current = self.new_state();
        self.connect(source,current);
        match pattern {
            Pattern::Range(range) => {
                let state = self.new_state();
                self.connect_by(current,state,range);
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
            Pattern::And(patterns) => {
                patterns.iter().fold(current,|s,pat| self.new_pattern(s,pat))
            },
            Pattern::Or(patterns) => {
                let states = patterns.iter().map(|pat| self.new_pattern(current,pat)).collect_vec();
                let end    = self.new_state();
                for state in states {
                    self.connect(state,end);
                }
                end
            }
        }
    }


    // === NFA -> DFA ===

    /// Merges states that are connected by epsilon links.
    /// The algorithm is based on: https://www.youtube.com/watch?v=taClnxU-nao
    fn eps_matrix(&self) -> Vec<StateSetId> {
        fn fill_eps_matrix
        ( nfa      : &NFA
        , states   : &mut Vec<StateSetId>
        , computed : &mut Vec<bool>
        , visited  : &mut Vec<bool>
        , state    : state::Id
        ) {
            let mut state_set = StateSetId::new();
            let mut circular  = false;
            visited[state.id] = true;
            state_set.insert(state);
            for &target in &nfa.states[state.id].epsilon_links {
                if !visited[target.id] {
                    fill_eps_matrix(nfa,states,computed,visited,target);
                }
                state_set.insert(target);
                state_set.extend(states[target.id].iter());
                if !computed[target.id] {
                    circular = true
                }
            }
            if !circular {
                computed[state.id] = true
            }
            states[state.id] = state_set;
        }

        let mut states   = vec![StateSetId::new(); self.states.len()];
        let mut computed = vec![false; self.states.len()];
        for id in 0..self.states.len() {
            let mut visited = vec![false; states.len()];
            fill_eps_matrix(self,&mut states,&mut computed,&mut visited,state::Id{id});
        }
        states
    }

    /// Computes a transition matrix (state X symbol => state) for NFA.
    /// Ignores epsilon links.
    fn nfa_matrix(&self) -> Matrix<state::Id> {
        let mut matrix = Matrix::new(self.states.len(),self.alphabet.symbols.len());

        for (state_ix, source) in self.states.iter().enumerate() {
            let targets = source.targets(&self.alphabet);
            for (voc_ix, &target) in targets.iter().enumerate() {
                matrix[(state_ix,voc_ix)] = target;
            }
        }
        matrix
    }
}

impl From<&NFA> for DFA {
    /// Transforms NFA into DFA.
    /// The algorithm is based on: https://www.youtube.com/watch?v=taClnxU-nao
    fn from(nfa:&NFA) -> Self {
        let     nfa_mat     = nfa.nfa_matrix();
        let     eps_mat     = nfa.eps_matrix();
        let mut dfa_mat     = Matrix::new(0,nfa.alphabet.symbols.len());
        let mut dfa_eps_ixs = Vec::<StateSetId>::new();
        let mut dfa_eps_map = HashMap::<StateSetId,state::Id>::new();

        dfa_eps_ixs.push(eps_mat[0].clone());
        dfa_eps_map.insert(eps_mat[0].clone(), state::Id{id:0});

        let mut i = 0;
        while i < dfa_eps_ixs.len()  {
            dfa_mat.new_row();
            for voc_ix in 0..nfa.alphabet.symbols.len() {
                let mut eps_set = StateSetId::new();
                for &eps_ix in &dfa_eps_ixs[i] {
                    let tgt = nfa_mat[(eps_ix.id,voc_ix)];
                    if tgt != state::INVALID {
                        eps_set.extend(eps_mat[tgt.id].iter());
                    }
                }
                if !eps_set.is_empty() {
                    dfa_mat[(i,voc_ix)] = match dfa_eps_map.get(&eps_set) {
                        Some(&id) => id,
                        None => {
                            let id = state::Id {id:dfa_eps_ixs.len()};
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
            let has_name = |&key:&state::Id| nfa.states[key.id].name.is_some();
            if let Some(eps) = epss.into_iter().find(has_name) {
                let rule  = nfa.states[eps.id].name.as_ref().cloned().unwrap();
                callbacks[dfa_ix] = Some(Callback {name:rule,priority});
            }
        }

        DFA {alphabet:nfa.alphabet.clone(),links:dfa_mat,callbacks}
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

    /// NFA automata that accepts newline '\n'.
    pub fn newline() -> NFA {
        NFA {
            states: vec![
                State::from(vec![1]),
                State::from(vec![(10..=10,2)]),
                State::from(vec![3]).named("group0_rule0"),
                State::default(),
            ],
            alphabet: Alphabet::from(vec![10,11]),
        }
    }

    /// NFA automata that accepts any letter a..=z.
    pub fn letter() -> NFA {
        NFA {
            states: vec![
                State::from(vec![1]),
                State::from(vec![(97..=122,2)]),
                State::from(vec![3]).named("group0_rule0"),
                State::default(),
            ],
            alphabet: Alphabet::from(vec![97,123]),
        }
    }

    /// NFA automata that accepts any number of spaces ' '.
    pub fn spaces() -> NFA {
        NFA {
            states: vec![
                State::from(vec![1]),
                State::from(vec![2]),
                State::from(vec![(32..=32,3)]),
                State::from(vec![4]),
                State::from(vec![5,8]),
                State::from(vec![6]),
                State::from(vec![(32..=32,7)]),
                State::from(vec![8]),
                State::from(vec![5,9]).named("group0_rule0"),
                State::default(),
            ],
            alphabet: Alphabet::from(vec![0,32,33]),
        }
    }

    /// NFA automata that accepts one letter a..=z or many spaces ' '.
    pub fn letter_and_spaces() -> NFA {
        NFA {
            states: vec![
                State::from(vec![1,3]),
                State::from(vec![(97..=122,2)]),
                State::from(vec![11]).named("group0_rule0"),
                State::from(vec![4]),
                State::from(vec![(32..=32,5)]),
                State::from(vec![6]),
                State::from(vec![7,10]),
                State::from(vec![8]),
                State::from(vec![(32..=32,9)]),
                State::from(vec![10]),
                State::from(vec![7,11]).named("group0_rule1"),
                State::default(),
            ],
            alphabet: Alphabet::from(vec![32,33,97,123]),
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
