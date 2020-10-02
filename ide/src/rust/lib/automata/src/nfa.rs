//! The structure for defining non-deterministic finite automata.

use crate::prelude::*;

use crate::alphabet;
use crate::pattern::Pattern;
use crate::state::Transition;
use crate::state;
use crate::symbol::Symbol;
use crate::data::matrix::Matrix;

use std::collections::BTreeSet;
use std::ops::RangeInclusive;



// =============
// === Types ===
// =============

/// Specialized NFA state type.
pub type State = state::State<Nfa>;



// =========================================
// === Non-Deterministic Finite Automata ===
// =========================================

/// A state identifier based on a set of states.
///
/// This is used during the NFA -> Dfa transformation, where multiple states can merge together due
/// to the collapsing of epsilon transitions.
pub type StateSetId = BTreeSet<State>;

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
#[derive(Clone,Debug,PartialEq,Eq)]
#[allow(missing_docs)]
pub struct Nfa {
    pub        start    : State,
    pub(crate) alphabet : alphabet::Segmentation,
    pub(crate) states   : Vec<state::Data>,
}

impl Nfa {
    /// Constructor.
    pub fn new() -> Self {
        let start    = default();
        let alphabet = default();
        let states   = default();
        Self {start,alphabet,states}.init_start_state()
    }

    /// Convert the automata to a GraphViz Dot code for the deubgging purposes.
    pub fn as_graphviz_code(&self) -> String {
        let mut out = String::new();
        for (ix,state) in self.states.iter().enumerate() {
            let opts = if state.export { "" } else { "[fillcolor=\"#EEEEEE\" fontcolor=\"#888888\"]" };
            out += &format!("node_{}[label=\"{}\"]{}\n",ix,ix,opts);
            for link in &state.links {
                out += &format!("node_{} -> node_{}[label=\"{}\"]\n",ix,link.target.id(),link.display_symbols());
            }
            for link in &state.epsilon_links {
                out += &format!("node_{} -> node_{}[style=dashed]\n",ix,link.id());
            }
        }
        let opts = "node [shape=circle style=filled fillcolor=\"#4385f5\" fontcolor=\"#FFFFFF\" color=white penwidth=5.0 margin=0.1 width=0.5 height=0.5 fixedsize=true]";
        format!("digraph G {{\n{}\n{}\n}}\n",opts,out)
    }

    fn init_start_state(mut self) -> Self {
        let start = self.new_state();
        self[start].export = true;
        self.start = start;
        self
    }

    /// Adds a new state to the NFA and returns its identifier.
    pub fn new_state(&mut self) -> State {
        let id = self.states.len();
        self.states.push(default());
        State::new(id)
    }

    /// Adds a new state to the NFA, marks it as an exported state, and returns its identifier.
    pub fn new_state_exported(&mut self) -> State {
        let state = self.new_state();
        self[state].export = true;
        state
    }

    /// Creates an epsilon transition between two states.
    ///
    /// Whenever the automaton happens to be in `source` state it can immediately transition to the
    /// `target` state. It is, however, not _required_ to do so.
    pub fn connect(&mut self, source:State, target:State) {
        self[source].epsilon_links.push(target);
    }

    /// Creates an ordinary transition for a range of symbols.
    ///
    /// If any symbol from such range happens to be the input when the automaton is in the `source`
    /// state, it will immediately transition to the `target` state.
    pub fn connect_via(&mut self, source:State, target:State, symbols:&RangeInclusive<Symbol>) {
        self.alphabet.insert(symbols.clone());
        self[source].links.push(Transition::new(symbols.clone(),target));
    }

    // FIXME[WD]: It seems that it should be possible to simplify this function. This would
    // drastically save memory (50-70%):
    // 1. We are always adding epsilon connection on the beginning. This should not be needed, but
    //    if we did it this way, it means there is a corner case probably. To be checked.
    // 2. In other places we have similar things. For example, in `Or` pattern we use epsilon
    //    connections to merge results, but we could theoretically first create the output, and
    //    then expand sub-patterns with the provided output.
    /// Transforms a pattern to connected NFA states by using the algorithm described
    /// [here](https://www.youtube.com/watch?v=RYNN-tb9WxI).
    /// The asymptotic complexity is linear in number of symbols.
    pub fn new_pattern(&mut self, current:State, pattern:impl AsRef<Pattern>) -> State {
        let pattern = pattern.as_ref();
        //let current = self.new_state();
        //self.connect(source,current);
        let state = match pattern {
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
            Pattern::Never  => self.new_state(),
        };
        self[state].export = true;
        state
    }

    /// Transforms a pattern to connected NFA states by using the algorithm described
    /// [here](https://www.youtube.com/watch?v=RYNN-tb9WxI). This function is similar to
    /// `new_pattern`, but it consumes an explicit target state.
    /// The asymptotic complexity is linear in number of symbols.
    pub fn new_pattern_to(&mut self, source:State, target:State, pattern:impl AsRef<Pattern>) {
        let pattern = pattern.as_ref();
        match pattern {
            Pattern::Range(range) => {
                self.connect_via(source,target,range);
            },
            Pattern::Many(body) => {
                let s1 = self.new_state();
                let s2 = self.new_pattern(s1,body);
                let target = self.new_state();
                self.connect(source,s1);
                self.connect(source,target);
                self.connect(s2,target);
                self.connect(target,s1);
            },
            Pattern::Seq(patterns) => {
                let out = patterns.iter().fold(source,|s,pat| self.new_pattern(s,pat));
                self.connect(out,target)
            },
            Pattern::Or(patterns) => {
                let states = patterns.iter().map(|pat| self.new_pattern(source,pat)).collect_vec();
                for state in states {
                    self.connect(state,target);
                }
            },
            Pattern::Always => {
                self.connect(source,target)
            },
            Pattern::Never => {},
        };
        self[target].export = true;
    }

    /// Merges states that are connected by epsilon links, using an algorithm based on the one shown
    /// [here](https://www.youtube.com/watch?v=taClnxU-nao).
    pub fn eps_matrix(&self) -> Vec<StateSetId> {
        fn fill_eps_matrix
        ( nfa     : &Nfa
        , states  : &mut Vec<StateSetId>
        , visited : &mut Vec<bool>
        , state   : State
        ) {
            let mut state_set = StateSetId::new();
            visited[state.id()] = true;
            state_set.insert(state);
            for &target in &nfa[state].epsilon_links {
                if !visited[target.id()] {
                    fill_eps_matrix(nfa,states,visited,target);
                }
                state_set.insert(target);
                state_set.extend(states[target.id()].iter());
            }
            states[state.id()] = state_set;
        }

        let mut states = vec![StateSetId::new(); self.states.len()];
        for id in 0..self.states.len() {
            let mut visited = vec![false; states.len()];
            fill_eps_matrix(self,&mut states,&mut visited,State::new(id));
        }
        states
    }

    /// Computes a transition matrix `(state, symbol) => state` for the Nfa, ignoring epsilon links.
    pub fn nfa_matrix(&self) -> Matrix<State> {
        let mut matrix = Matrix::new(self.states.len(),self.alphabet.divisions.len());

        for (state_ix, source) in self.states.iter().enumerate() {
            let targets = source.targets(&self.alphabet);
            for (voc_ix, &target) in targets.iter().enumerate() {
                matrix[(state_ix,voc_ix)] = target;
            }
        }
        matrix
    }
}

impl Default for Nfa {
    fn default() -> Self {
        Self::new()
    }
}

impl Index<State> for Nfa {
    type Output = state::Data;
    fn index(&self, state:State) -> &Self::Output {
        &self.states[state.id()]
    }
}

impl IndexMut<State> for Nfa {
    fn index_mut(&mut self, state:State) -> &mut Self::Output {
        &mut self.states[state.id()]
    }
}



// // ===========
// // == Tests ==
// // ===========
//
// #[cfg(test)]
// pub mod tests {
//     extern crate test;
//
//     use crate::dfa;
//
//     use super::*;
//     use test::Bencher;
//
//     /// Nfa that accepts a newline '\n'.
//     pub fn newline() -> Nfa {
//         Nfa {
//             start: State::new(0),
//             states: vec![
//                 state::Data::from(vec![1]),
//                 state::Data::from(vec![(10..=10,2)]),
//                 state::Data::from(vec![3]).named("group0_rule0"),
//                 state::Data::default(),
//             ],
//             alphabet: alphabet::Segmentation::from_divisions(vec![10, 11].as_slice()),
//         }
//     }
//
//     /// Nfa that accepts any letter in the range a..=z.
//     pub fn letter() -> Nfa {
//         Nfa {
//             start: State::new(0),
//             states: vec![
//                 state::Data::from(vec![1]),
//                 state::Data::from(vec![(97..=122,2)]),
//                 state::Data::from(vec![3]).named("group0_rule0"),
//                 State::default(),
//             ],
//             alphabet: alphabet::Segmentation::from_divisions(vec![97, 123].as_slice()),
//         }
//     }
//
//     /// Nfa that accepts any number of spaces ' '.
//     pub fn spaces() -> Nfa {
//         Nfa {
//             start: State::new(0),
//             states: vec![
//                 state::Data::from(vec![1]),
//                 state::Data::from(vec![2]),
//                 state::Data::from(vec![(32..=32,3)]),
//                 state::Data::from(vec![4]),
//                 state::Data::from(vec![5,8]),
//                 state::Data::from(vec![6]),
//                 state::Data::from(vec![(32..=32,7)]),
//                 state::Data::from(vec![8]),
//                 state::Data::from(vec![5,9]).named("group0_rule0"),
//                 State::default(),
//             ],
//             alphabet: alphabet::Segmentation::from_divisions(vec![0, 32, 33].as_slice()),
//         }
//     }
//
//     /// Nfa that accepts one letter a..=z or many spaces ' '.
//     pub fn letter_and_spaces() -> Nfa {
//         Nfa {
//             start: State::new(0),
//             states: vec![
//                 state::Data::from(vec![1,3]),
//                 state::Data::from(vec![(97..=122,2)]),
//                 state::Data::from(vec![11]).named("group0_rule0"),
//                 state::Data::from(vec![4]),
//                 state::Data::from(vec![(32..=32,5)]),
//                 state::Data::from(vec![6]),
//                 state::Data::from(vec![7,10]),
//                 state::Data::from(vec![8]),
//                 state::Data::from(vec![(32..=32,9)]),
//                 state::Data::from(vec![10]),
//                 state::Data::from(vec![7,11]).named("group0_rule1"),
//                 State::default(),
//             ],
//             alphabet: alphabet::Segmentation::from_divisions(vec![32, 33, 97, 123].as_slice()),
//         }
//     }
//
//     #[test]
//     fn test_to_dfa_newline() {
//         assert_eq!(Dfa::from(&newline()),dfa::tests::newline());
//     }
//
//     #[test]
//     fn test_to_dfa_letter() {
//         assert_eq!(Dfa::from(&letter()),dfa::tests::letter());
//     }
//
//     #[test]
//     fn test_to_dfa_spaces() {
//         assert_eq!(Dfa::from(&spaces()),dfa::tests::spaces());
//     }
//
//     #[test]
//     fn test_to_dfa_letter_and_spaces() {
//         assert_eq!(Dfa::from(&letter_and_spaces()),dfa::tests::letter_and_spaces());
//     }
//
//     #[bench]
//     fn bench_to_dfa_newline(bencher:&mut Bencher) {
//         bencher.iter(|| Dfa::from(&newline()))
//     }
//
//     #[bench]
//     fn bench_to_dfa_letter(bencher:&mut Bencher) {
//         bencher.iter(|| Dfa::from(&letter()))
//     }
//
//     #[bench]
//     fn bench_to_dfa_spaces(bencher:&mut Bencher) {
//         bencher.iter(|| Dfa::from(&spaces()))
//     }
//
//     #[bench]
//     fn bench_to_dfa_letter_and_spaces(bencher:&mut Bencher) {
//         bencher.iter(|| Dfa::from(&letter_and_spaces()))
//     }
// }
