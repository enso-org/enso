//! The structure for defining deterministic finite automata.

use crate::prelude::*;

use crate::symbol::Symbol;
use crate::alphabet;
use crate::state;
use crate::data::matrix::Matrix;
use crate::nfa;
use crate::nfa::Nfa;



// =============
// === Types ===
// =============

/// Specialized DFA state type.
pub type State = state::State<Dfa>;



// =====================================
// === Deterministic Finite Automata ===
// =====================================

/// The definition of a [DFA](https://en.wikipedia.org/wiki/Deterministic_finite_automaton) for a
/// given set of symbols, states, and transitions.
///
/// A DFA is a finite state automaton that accepts or rejects a given sequence of symbols by
/// executing on a sequence of states _uniquely_ determined by the sequence of input symbols.
///
/// ```text
///  ┌───┐  'D'  ┌───┐  'F'  ┌───┐  'A'  ┌───┐
///  │ 0 │ ----> │ 1 │ ----> │ 2 │ ----> │ 3 │
///  └───┘       └───┘       └───┘       └───┘
/// ```
#[derive(Clone,Debug,Default,Eq,PartialEq)]
pub struct Dfa {
    /// A set of disjoint intervals over the allowable input alphabet.
    pub alphabet : alphabet::SealedSegmentation,
    /// The transition matrix for the Dfa.
    ///
    /// It represents a function of type `(state, symbol) -> state`, returning the identifier for
    /// the new state.
    ///
    /// For example, the transition matrix for an automaton that accepts the language
    /// `{"A" | "B"}*"` would appear as follows, with `-` denoting the invalid state. The leftmost
    /// column encodes the input state, while the topmost row encodes the input symbols.
    ///
    /// |   | A | B |
    /// |:-:|:-:|:-:|
    /// | 0 | 1 | - |
    /// | 1 | - | 0 |
    ///
    pub links : Matrix<State>,
    /// For each DFA state contains a list of NFA states it was constructed from.
    pub sources : Vec<Vec<nfa::State>>,
}

impl Dfa {
    /// The start state of the automata.
    pub const START_STATE : State = State::new(0);
}

impl Dfa {
    /// Simulate the DFA transition with the provided input symbol.
    pub fn next_state(&self, current_state:State, symbol:&Symbol) -> State {
        let ix = self.alphabet.index_of_symbol(&symbol);
        self.links.safe_index(current_state.id(),ix).unwrap_or_default()
    }

    /// Convert the automata to GraphViz Dot code for the deubgging purposes.
    pub fn as_graphviz_code(&self) -> String {
        let mut out = String::new();
        for row in 0 .. self.links.rows {
            out += &format!("node_{}[label=\"{}\"]\n",row,row);
            for column in 0 .. self.links.columns {
                let state = self.links[(row,column)];
                if !state.is_invalid() {
                    out += &format!("node_{} -> node_{}\n",row,state.id());
                }
            }
        }
        let opts = "node [shape=circle style=filled fillcolor=\"#4385f5\" fontcolor=\"#FFFFFF\" \
                    color=white penwidth=5.0 margin=0.1 width=0.5 height=0.5 fixedsize=true]";
        format!("digraph G {{\n{}\n{}\n}}\n",opts,out)
    }
}


// === Trait Impls ===

impl From<Vec<Vec<usize>>> for Matrix<State> {
    fn from(input:Vec<Vec<usize>>) -> Self {
        let rows       = input.len();
        let columns    = if rows == 0 {0} else {input[0].len()};
        let mut matrix = Self::new(rows,columns);
        for row in 0..rows {
            for column in 0..columns {
                matrix[(row,column)] = State::new(input[row][column]);
            }
        }
        matrix
    }
}



// ===================
// === Conversions ===
// ===================

impl From<&Nfa> for Dfa {
    /// Transforms an Nfa into a Dfa, based on the algorithm described
    /// [here](https://www.youtube.com/watch?v=taClnxU-nao).
    /// The asymptotic complexity is quadratic in number of states.
    fn from(nfa:&Nfa) -> Self {
        let     nfa_mat     = nfa.nfa_matrix();
        let     eps_mat     = nfa.eps_matrix();
        let mut dfa_mat     = Matrix::new(0,nfa.alphabet.divisions.len());
        let mut dfa_eps_ixs = Vec::<nfa::StateSetId>::new();
        let mut dfa_eps_map = HashMap::<nfa::StateSetId,State>::new();

        dfa_eps_ixs.push(eps_mat[0].clone());
        dfa_eps_map.insert(eps_mat[0].clone(),Dfa::START_STATE);

        let mut i = 0;
        while i < dfa_eps_ixs.len()  {
            dfa_mat.new_row();
            for voc_ix in 0..nfa.alphabet.divisions.len() {
                let mut eps_set = nfa::StateSetId::new();
                for &eps_ix in &dfa_eps_ixs[i] {
                    let tgt = nfa_mat[(eps_ix.id(),voc_ix)];
                    if tgt != nfa::State::INVALID {
                        eps_set.extend(eps_mat[tgt.id()].iter());
                    }
                }
                if !eps_set.is_empty() {
                    dfa_mat[(i,voc_ix)] = match dfa_eps_map.get(&eps_set) {
                        Some(&id) => id,
                        None => {
                            let id = State::new(dfa_eps_ixs.len());
                            dfa_eps_ixs.push(eps_set.clone());
                            dfa_eps_map.insert(eps_set,id);
                            id
                        },
                    };
                }
            }
            i += 1;
        }

        let mut sources = vec![];
        for epss in dfa_eps_ixs.into_iter() {
            sources.push(epss.into_iter().filter(|state| nfa[*state].export).collect_vec());
        }

        let alphabet = (&nfa.alphabet).into();
        let links    = dfa_mat;
        Dfa {alphabet,links,sources}
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
pub mod tests {
    extern crate test;
    use super::*;
    use crate::nfa;
    use test::Bencher;
    use crate::nfa::tests::NfaTest;


    // === Utilities ===

    fn invalid() -> usize {
        State::INVALID.id()
    }

    fn assert_same_alphabet(dfa:&Dfa, nfa:&Nfa) {
        assert_eq!(dfa.alphabet,nfa.alphabet.seal());
    }

    fn assert_same_matrix(dfa:&Dfa, expected:&Matrix<State>) {
        assert_eq!(dfa.links,*expected);
    }

    fn get_name<'a>(nfa:&'a NfaTest, dfa:&Dfa, state:State) -> Option<&'a String> {
        let sources = &dfa.sources[state.id()];
        let mut result = None;
        for source in sources.iter() {
            let name = nfa.name(*source);
            if name.is_some() {
                result = name;
                break;
            }
        }
        result
    }

    fn make_state(ix:usize) -> State {
        State::new(ix)
    }


    // === The Tests ===

    #[test]
    fn dfa_pattern_range() {
        let nfa = nfa::tests::pattern_range();
        let dfa = Dfa::from(&nfa.nfa);
        assert_same_alphabet(&dfa,&nfa);
        let expected = Matrix::from(
            vec![
                vec![invalid() , 1         , invalid()],
                vec![invalid() , invalid() , invalid()],
            ]
        );
        assert_same_matrix(&dfa,&expected);
    }

    #[test]
    fn dfa_pattern_or() {
        let nfa = nfa::tests::pattern_or();
        let dfa = Dfa::from(&nfa.nfa);
        assert_same_alphabet(&dfa,&nfa);
        let expected = Matrix::from(
            vec![
                vec![invalid() , 1         , invalid() , 2         , invalid()],
                vec![invalid() , invalid() , invalid() , invalid() , invalid()],
                vec![invalid() , invalid() , invalid() , invalid() , invalid()],
            ]
        );
        assert_same_matrix(&dfa,&expected);
    }

    #[test]
    fn dfa_pattern_seq() {
        let nfa = nfa::tests::pattern_seq();
        let dfa = Dfa::from(&nfa.nfa);
        assert_same_alphabet(&dfa,&nfa);
        let expected = Matrix::from(
            vec![
                vec![invalid() , 1         , invalid() , invalid() , invalid()],
                vec![invalid() , invalid() , invalid() , 2         , invalid()],
                vec![invalid() , invalid() , invalid() , invalid() , invalid()],
            ]
        );
        assert_same_matrix(&dfa,&expected);
    }

    #[test]
    fn dfa_pattern_many() {
        let nfa = nfa::tests::pattern_many();
        let dfa = Dfa::from(&nfa.nfa);
        assert_same_alphabet(&dfa,&nfa);
        let expected = Matrix::from(
            vec![
                vec![invalid() , 1 , invalid()],
                vec![invalid() , 1 , invalid()],
            ]
        );
        assert_same_matrix(&dfa,&expected);
    }

    #[test]
    fn dfa_pattern_always() {
        let nfa = nfa::tests::pattern_always();
        let dfa = Dfa::from(&nfa.nfa);
        assert_same_alphabet(&dfa,&nfa);
        let expected = Matrix::from(
            vec![
                vec![invalid()]
            ]
        );
        assert_same_matrix(&dfa,&expected);
    }

    #[test]
    fn dfa_pattern_never() {
        let nfa = nfa::tests::pattern_never();
        let dfa = Dfa::from(&nfa.nfa);
        assert_same_alphabet(&dfa,&nfa);
        let expected = Matrix::from(
            vec![
                vec![invalid()]
            ]
        );
        assert_same_matrix(&dfa,&expected);
    }

    #[test]
    fn dfa_simple_rules() {
        let nfa = nfa::tests::simple_rules();
        let dfa = Dfa::from(&nfa.nfa);
        assert_same_alphabet(&dfa,&nfa);
        let expected = Matrix::from(
            vec![
                vec![invalid() , 1         , invalid() , invalid()],
                vec![invalid() , invalid() , 2         , invalid()],
                vec![invalid() , invalid() , invalid() , invalid()],
            ]
        );
        assert_same_matrix(&dfa,&expected);
    }

    #[test]
    fn dfa_complex_rules() {
        let nfa = nfa::tests::complex_rules();
        let dfa = Dfa::from(&nfa.nfa);
        assert_same_alphabet(&dfa,&nfa);
        let expected = Matrix::from(
            vec![
                vec![1         , 2         , 1         , 1         , 1         , 1         , 3        ],
                vec![invalid() , invalid() , invalid() , invalid() , invalid() , invalid() , invalid()],
                vec![invalid() , invalid() , invalid() , 4         , 5         , invalid() , invalid()],
                vec![invalid() , invalid() , invalid() , invalid() , invalid() , invalid() , invalid()],
                vec![invalid() , invalid() , invalid() , 6         , invalid() , invalid() , invalid()],
                vec![invalid() , invalid() , invalid() , invalid() , 7         , invalid() , invalid()],
                vec![invalid() , invalid() , invalid() , 6         , invalid() , invalid() , invalid()],
                vec![invalid() , invalid() , invalid() , invalid() , 7         , invalid() , invalid()],
            ]
        );
        assert_same_matrix(&dfa,&expected);
    }

    #[test]
    fn dfa_named_rules() {
        let nfa = nfa::tests::named_rules();
        let dfa = Dfa::from(&nfa.nfa);
        assert_same_alphabet(&dfa, &nfa);
        assert_eq!(dfa.sources.len(),5);
        assert_eq!(get_name(&nfa,&dfa,make_state(0)),None);
        assert_eq!(get_name(&nfa,&dfa,make_state(1)),Some(&String::from("rule_1")));
        assert_eq!(get_name(&nfa,&dfa,make_state(2)),Some(&String::from("rule_2")));
        assert_eq!(get_name(&nfa,&dfa,make_state(3)),Some(&String::from("rule_1")));
        assert_eq!(get_name(&nfa,&dfa,make_state(4)),Some(&String::from("rule_2")));
    }

    // === The Benchmarks ===

    #[bench]
    fn bench_to_dfa_pattern_range(bencher:&mut Bencher) {
        bencher.iter(|| Dfa::from(&nfa::tests::pattern_range().nfa))
    }

    #[bench]
    fn bench_to_dfa_pattern_or(bencher:&mut Bencher) {
        bencher.iter(|| Dfa::from(&nfa::tests::pattern_or().nfa))
    }

    #[bench]
    fn bench_to_dfa_pattern_seq(bencher:&mut Bencher) {
        bencher.iter(|| Dfa::from(&nfa::tests::pattern_seq().nfa))
    }

    #[bench]
    fn bench_to_dfa_pattern_many(bencher:&mut Bencher) {
        bencher.iter(|| Dfa::from(&nfa::tests::pattern_many().nfa))
    }

    #[bench]
    fn bench_to_dfa_pattern_always(bencher:&mut Bencher) {
        bencher.iter(|| Dfa::from(&nfa::tests::pattern_always().nfa))
    }

    #[bench]
    fn bench_to_dfa_pattern_never(bencher:&mut Bencher) {
        bencher.iter(|| Dfa::from(&nfa::tests::pattern_never().nfa))
    }

    #[bench]
    fn bench_to_dfa_simple_rules(bencher:&mut Bencher) {
        bencher.iter(|| Dfa::from(&nfa::tests::simple_rules().nfa))
    }

    #[bench]
    fn bench_to_dfa_complex_rules(bencher:&mut Bencher) {
        bencher.iter(|| Dfa::from(&nfa::tests::complex_rules().nfa))
    }
}
