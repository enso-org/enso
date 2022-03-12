//! The structure for defining deterministic finite automata.

use crate::prelude::*;

use crate::alphabet;
use crate::data::matrix::Matrix;
use crate::nfa;
use crate::nfa::Nfa;
use crate::state;
use crate::symbol::Symbol;



// =============
// === Types ===
// =============

/// Specialized DFA state type.
pub type State = state::StateId<Dfa>;



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
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct Dfa {
    /// A set of disjoint intervals over the allowable input alphabet.
    pub alphabet:         alphabet::SealedSegmentation,
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
    pub links:            Matrix<State>,
    pub sources:          Vec<Vec<nfa::StateId>>,
    /// For each DFA state contains a list of NFA states it was constructed from.
    pub exported_sources: Vec<Vec<nfa::StateId>>,
}

impl Dfa {
    /// The start state of the automata.
    pub const START_STATE: State = State::new(0);
}

impl Dfa {
    /// Simulate the DFA transition with the provided input symbol.
    pub fn next_state(&self, current_state: State, symbol: &Symbol) -> State {
        let ix = self.alphabet.index_of_symbol(symbol);
        self.links.safe_index(current_state.id(), ix).unwrap_or_default()
    }

    /// Convert the automata to GraphViz Dot code for the deubgging purposes.
    pub fn as_graphviz_code(&self) -> String {
        let mut out = String::new();
        let alphabet_ranges = self.alphabet.ranges();
        for row in 0..self.links.rows {
            let sources = &self.sources[row];
            out += &format!("node_{}[label=\"{} ({:?})\"]\n", row, row, sources);
            for column in 0..self.links.columns {
                let range = alphabet_ranges[column];
                let state = self.links[(row, column)];
                if !state.is_invalid() {
                    out += &format!("node_{} -> node_{}[label=\"{}\"]\n", row, state.id(), range);
                }
            }
        }
        let opts = "node [shape=circle style=filled fillcolor=\"#4385f5\" fontcolor=\"#FFFFFF\" \
                    color=white penwidth=5.0 margin=0.1]";
        format!("digraph G {{\n{}\n{}\n}}\n", opts, out)
    }
}


// === Trait Impls ===

impl From<Vec<Vec<usize>>> for Matrix<State> {
    fn from(input: Vec<Vec<usize>>) -> Self {
        let rows = input.len();
        let columns = if rows == 0 { 0 } else { input[0].len() };
        let mut matrix = Self::new(rows, columns);
        for row in 0..rows {
            for column in 0..columns {
                matrix[(row, column)] = State::new(input[row][column]);
            }
        }
        matrix
    }
}



// ===================
// === Conversions ===
// ===================

/// Merges states that are connected by epsilon links, using an algorithm based on the one shown
/// [here](https://www.youtube.com/watch?v=taClnxU-nao).
pub fn eps_matrix(nfa: &Nfa) -> Vec<nfa::StateSetId> {
    fn fill_eps_matrix(
        nfa: &Nfa,
        states: &mut Vec<nfa::StateSetId>,
        visited: &mut Vec<bool>,
        state: nfa::StateId,
    ) {
        let mut state_set = nfa::StateSetId::new();
        visited[state.id()] = true;
        state_set.insert(state);
        for &target in &nfa[state].epsilon_links {
            if !visited[target.id()] {
                fill_eps_matrix(nfa, states, visited, target);
            }
            state_set.insert(target);
            state_set.extend(states[target.id()].iter());
        }
        states[state.id()] = state_set;
    }

    let mut states = vec![nfa::StateSetId::new(); nfa.states.len()];
    for id in 0..nfa.states.len() {
        let mut visited = vec![false; states.len()];
        fill_eps_matrix(nfa, &mut states, &mut visited, nfa::StateId::new(id));
    }
    states
}

/// Computes a transition matrix `(state, symbol) => state` for the Nfa, ignoring epsilon links.
pub fn nfa_matrix(nfa: &Nfa) -> Matrix<nfa::StateId> {
    let mut matrix = Matrix::new(nfa.states.len(), nfa.alphabet.divisions.len());

    for (state_ix, state) in nfa.states.iter().enumerate() {
        let targets = state.targets(&nfa.alphabet);
        for (voc_ix, &target_state_id) in targets.iter().enumerate() {
            matrix[(state_ix, voc_ix)] = target_state_id;
        }
    }
    matrix
}



impl From<&Nfa> for Dfa {
    /// Transforms an Nfa into a Dfa, based on the algorithm described
    /// [here](https://www.youtube.com/watch?v=taClnxU-nao).
    /// The asymptotic complexity is quadratic in number of states.
    fn from(nfa: &Nfa) -> Self {
        let nfa_mat = nfa_matrix(nfa);
        let eps_mat = eps_matrix(nfa);
        let mut dfa_mat = Matrix::new(0, nfa.alphabet.divisions.len());
        let mut dfa_eps_ixs = Vec::<nfa::StateSetId>::new();
        let mut dfa_eps_map = HashMap::<nfa::StateSetId, State>::new();

        dfa_eps_ixs.push(eps_mat[0].clone());
        dfa_eps_map.insert(eps_mat[0].clone(), Dfa::START_STATE);

        let mut i = 0;
        while i < dfa_eps_ixs.len() {
            dfa_mat.new_row();
            for voc_ix in 0..nfa.alphabet.divisions.len() {
                let mut eps_set = nfa::StateSetId::new();
                for &eps_ix in &dfa_eps_ixs[i] {
                    let tgt = nfa_mat[(eps_ix.id(), voc_ix)];
                    if tgt != nfa::StateId::INVALID {
                        eps_set.extend(eps_mat[tgt.id()].iter());
                    }
                }
                if !eps_set.is_empty() {
                    dfa_mat[(i, voc_ix)] = match dfa_eps_map.get(&eps_set) {
                        Some(&id) => id,
                        None => {
                            let id = State::new(dfa_eps_ixs.len());
                            dfa_eps_ixs.push(eps_set.clone());
                            dfa_eps_map.insert(eps_set, id);
                            id
                        }
                    };
                }
            }
            i += 1;
        }

        let mut exported_sources = vec![];
        for epss in dfa_eps_ixs.iter() {
            exported_sources
                .push(epss.into_iter().filter(|state| nfa[**state].export).cloned().collect_vec());
        }

        let mut sources = vec![];
        for epss in dfa_eps_ixs.into_iter() {
            sources.push(epss.into_iter().collect_vec());
        }

        let alphabet = (&nfa.alphabet).into();
        let links = dfa_mat;
        Dfa { alphabet, links, sources, exported_sources }
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
    use crate::nfa::tests::NfaTest;
    use test::Bencher;


    // === Utilities ===

    fn invalid() -> usize {
        State::INVALID.id()
    }

    fn assert_same_alphabet(dfa: &Dfa, nfa: &Nfa) {
        assert_eq!(dfa.alphabet, nfa.alphabet.seal());
    }

    fn assert_same_matrix(dfa: &Dfa, expected: &Matrix<State>) {
        assert_eq!(dfa.links, *expected);
    }

    fn get_name<'a>(nfa: &'a NfaTest, dfa: &Dfa, state: State) -> Option<&'a String> {
        let sources = &dfa.exported_sources[state.id()];
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

    fn make_state(ix: usize) -> State {
        State::new(ix)
    }


    // === The Tests ===

    /// Input NFA:
    ///
    ///  ╭──EPS───╮╭───a────╮
    ///  ▼        │▼        │
    /// s0 ──a──▶ s1 ──b──▶ s2
    ///  │                  ▲
    ///  ╰───c──▶ s3 ──c────┤
    ///            ╰──EPS───╯
    #[test]
    fn test1() {
        use crate::pattern::Pattern;
        use crate::state::INVALID_IX as X;

        let mut nfa = Nfa::default();
        let start_state_id = nfa.start;

        let s0 = nfa.start;
        let s1 = nfa.new_state();
        let s2 = nfa.new_state();
        let s3 = nfa.new_state();
        nfa.connect(s0, s1, 'a');
        nfa.connect(s0, s3, 'c');
        nfa.connect_eps(s1, s0);
        nfa.connect(s1, s2, 'b');
        nfa.connect(s2, s1, 'a');
        nfa.connect(s3, s2, 'c');
        nfa.connect_eps(s3, s2);

        let m1 = nfa_matrix(&nfa);
        #[rustfmt::skip]
        let m1_expected = Matrix::<nfa::StateId>::new_from_slice(4, 5, &[
            X, 1, X, 3, X,
            X, X, 2, X, X,
            X, 1, X, X, X,
            X, X, X, 2, X,
        ]);
        assert_eq!(m1, m1_expected);

        let m2 = eps_matrix(&nfa);
        println!("{:?}", m2);


        // let pattern = Pattern::never().many();

        // let pattern = Pattern::or(Pattern::range('x'..='x'), Pattern::range('y'..='y'));

        // let pattern = Pattern::Seq(vec![Pattern::range('y'..='y'), Pattern::range('z'..='z')]);

        // nfa.new_pattern(start_state_id, pattern);
        let dfa = Dfa::from(&nfa);

        // println!("{:?}", dfa.sources);

        println!("{}", dfa.as_graphviz_code());

        println!("{:#?}", dfa.alphabet);
    }

    #[test]
    fn dfa_pattern_range() {
        let nfa = nfa::tests::pattern_range();
        let dfa = Dfa::from(&nfa.nfa);
        assert_same_alphabet(&dfa, &nfa);
        let expected = Matrix::from(vec![vec![invalid(), 1, invalid()], vec![
            invalid(),
            invalid(),
            invalid(),
        ]]);
        assert_same_matrix(&dfa, &expected);
    }

    #[test]
    fn dfa_pattern_or() {
        let nfa = nfa::tests::pattern_or();
        let dfa = Dfa::from(&nfa.nfa);
        assert_same_alphabet(&dfa, &nfa);
        let expected = Matrix::from(vec![
            vec![invalid(), 1, invalid(), 2, invalid()],
            vec![invalid(), invalid(), invalid(), invalid(), invalid()],
            vec![invalid(), invalid(), invalid(), invalid(), invalid()],
        ]);
        assert_same_matrix(&dfa, &expected);
    }

    #[test]
    fn dfa_pattern_seq() {
        let nfa = nfa::tests::pattern_seq();
        let dfa = Dfa::from(&nfa.nfa);
        assert_same_alphabet(&dfa, &nfa);
        let expected = Matrix::from(vec![
            vec![invalid(), 1, invalid(), invalid(), invalid()],
            vec![invalid(), invalid(), invalid(), 2, invalid()],
            vec![invalid(), invalid(), invalid(), invalid(), invalid()],
        ]);
        assert_same_matrix(&dfa, &expected);
    }

    #[test]
    fn dfa_pattern_many() {
        let nfa = nfa::tests::pattern_many();
        let dfa = Dfa::from(&nfa.nfa);
        assert_same_alphabet(&dfa, &nfa);
        let expected =
            Matrix::from(vec![vec![invalid(), 1, invalid()], vec![invalid(), 1, invalid()]]);
        assert_same_matrix(&dfa, &expected);
    }

    #[test]
    fn dfa_pattern_always() {
        let nfa = nfa::tests::pattern_always();
        let dfa = Dfa::from(&nfa.nfa);
        assert_same_alphabet(&dfa, &nfa);
        let expected = Matrix::from(vec![vec![invalid()]]);
        assert_same_matrix(&dfa, &expected);
    }

    #[test]
    fn dfa_pattern_never() {
        let nfa = nfa::tests::pattern_never();
        let dfa = Dfa::from(&nfa.nfa);
        assert_same_alphabet(&dfa, &nfa);
        let expected = Matrix::from(vec![vec![invalid()]]);
        assert_same_matrix(&dfa, &expected);
    }

    #[test]
    fn dfa_simple_rules() {
        let nfa = nfa::tests::simple_rules();
        let dfa = Dfa::from(&nfa.nfa);
        assert_same_alphabet(&dfa, &nfa);
        let expected = Matrix::from(vec![
            vec![invalid(), 1, invalid(), invalid()],
            vec![invalid(), invalid(), 2, invalid()],
            vec![invalid(), invalid(), invalid(), invalid()],
        ]);
        assert_same_matrix(&dfa, &expected);
    }

    #[test]
    fn dfa_complex_rules() {
        let nfa = nfa::tests::complex_rules();
        let dfa = Dfa::from(&nfa.nfa);
        assert_same_alphabet(&dfa, &nfa);
        let expected = Matrix::from(vec![
            vec![1, 2, 1, 1, 1, 1, 3],
            vec![invalid(), invalid(), invalid(), invalid(), invalid(), invalid(), invalid()],
            vec![invalid(), invalid(), invalid(), 4, 5, invalid(), invalid()],
            vec![invalid(), invalid(), invalid(), invalid(), invalid(), invalid(), invalid()],
            vec![invalid(), invalid(), invalid(), 6, invalid(), invalid(), invalid()],
            vec![invalid(), invalid(), invalid(), invalid(), 7, invalid(), invalid()],
            vec![invalid(), invalid(), invalid(), 6, invalid(), invalid(), invalid()],
            vec![invalid(), invalid(), invalid(), invalid(), 7, invalid(), invalid()],
        ]);
        assert_same_matrix(&dfa, &expected);
    }

    #[test]
    fn dfa_named_rules() {
        let nfa = nfa::tests::named_rules();
        let dfa = Dfa::from(&nfa.nfa);
        assert_same_alphabet(&dfa, &nfa);
        assert_eq!(dfa.exported_sources.len(), 5);
        assert_eq!(get_name(&nfa, &dfa, make_state(0)), None);
        assert_eq!(get_name(&nfa, &dfa, make_state(1)), Some(&String::from("rule_1")));
        assert_eq!(get_name(&nfa, &dfa, make_state(2)), Some(&String::from("rule_2")));
        assert_eq!(get_name(&nfa, &dfa, make_state(3)), Some(&String::from("rule_1")));
        assert_eq!(get_name(&nfa, &dfa, make_state(4)), Some(&String::from("rule_2")));
    }

    // === The Benchmarks ===

    #[bench]
    fn bench_to_dfa_pattern_range(bencher: &mut Bencher) {
        bencher.iter(|| Dfa::from(&nfa::tests::pattern_range().nfa))
    }

    #[bench]
    fn bench_to_dfa_pattern_or(bencher: &mut Bencher) {
        bencher.iter(|| Dfa::from(&nfa::tests::pattern_or().nfa))
    }

    #[bench]
    fn bench_to_dfa_pattern_seq(bencher: &mut Bencher) {
        bencher.iter(|| Dfa::from(&nfa::tests::pattern_seq().nfa))
    }

    #[bench]
    fn bench_to_dfa_pattern_many(bencher: &mut Bencher) {
        bencher.iter(|| Dfa::from(&nfa::tests::pattern_many().nfa))
    }

    #[bench]
    fn bench_to_dfa_pattern_always(bencher: &mut Bencher) {
        bencher.iter(|| Dfa::from(&nfa::tests::pattern_always().nfa))
    }

    #[bench]
    fn bench_to_dfa_pattern_never(bencher: &mut Bencher) {
        bencher.iter(|| Dfa::from(&nfa::tests::pattern_never().nfa))
    }

    #[bench]
    fn bench_to_dfa_simple_rules(bencher: &mut Bencher) {
        bencher.iter(|| Dfa::from(&nfa::tests::simple_rules().nfa))
    }

    #[bench]
    fn bench_to_dfa_complex_rules(bencher: &mut Bencher) {
        bencher.iter(|| Dfa::from(&nfa::tests::complex_rules().nfa))
    }
}
