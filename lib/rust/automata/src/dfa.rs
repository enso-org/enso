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
pub type StateId = state::StateId<Dfa>;



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
    pub links:            Matrix<StateId>,
    pub sources:          Vec<Vec<nfa::StateId>>,
    /// For each DFA state contains a list of NFA states it was constructed from.
    pub exported_sources: Vec<Vec<nfa::StateId>>,
}

impl Dfa {
    /// The start state of the automata.
    pub const START_STATE: StateId = StateId::new(0);
}

impl Dfa {
    /// Simulate the DFA transition with the provided input symbol.
    pub fn next_state(&self, current_state: StateId, symbol: &Symbol) -> StateId {
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

fn gen_parser_steps_code(
    p_name: String,
    dfa: &Dfa,
    name_map: &HashMap<nfa::StateId, String>,
) -> String {
    let mut out = String::new();
    let alphabet_ranges = dfa.alphabet.ranges();
    let mut fn_names = Vec::new();
    for row in 0..dfa.links.rows {
        let fn_name = format!("step_state{}", row);
        out.push_str(&format!("fn {}<'s>(parser: &mut {}<'s>) {{\n", fn_name, p_name));
        out.push_str(&format!("    println!(\"Calling '{}'\");\n", fn_name));
        let mut first_condition = true;
        for column in 0..dfa.links.columns {
            let glue = if first_condition { "" } else { "else " };
            first_condition = false;
            let range = alphabet_ranges[column];
            out.push_str(&format!(
                "    {}if parser.runner.current_input >= {} && parser.runner.current_input <= {} {{\n",
                glue, range.start.index, range.end.index,
            ));
            let target_state = &dfa.links[(row, column)];
            if target_state.is_invalid() {
                out.push_str("        // invalid\n");
                out.push_str("        println!(\"invalid\");\n");
            } else {
                let exported_states = &dfa.exported_sources[target_state.id()];

                out.push_str(&format!(
                    "        parser.runner.dfa_state = {}.into();\n",
                    target_state.id()
                ));
                for state in exported_states {
                    if let Some(name) = name_map.get(state) {
                        out.push_str(&format!("        println!(\"CALL: {}\");\n", name));
                        out.push_str(&format!("        parser.{}();\n", name));
                    }
                }
            }
            out.push_str("    }\n");
        }
        fn_names.push(fn_name);
        out.push_str("}\n\n")
    }
    out.push_str("\n");

    out.push_str(&format!("const STEPS_LOOKUP_TABLE: &[fn(&mut {})] = &[\n", p_name));
    for fn_name in fn_names {
        out.push_str(&format!("    {},\n", fn_name));
    }
    out.push_str("];\n\n");

    out.push_str(&format!("fn step<'s>(parser: &mut {}<'s>) {{\n", p_name));
    out.push_str("    parser.runner.next_input_char();\n");
    out.push_str("    STEPS_LOOKUP_TABLE[parser.runner.dfa_state.id()](parser);\n");
    out.push_str("}\n");

    out
}


pub struct ParserRunner<'s> {
    pub input_iter:    std::str::Chars<'s>,
    pub current_input: Symbol,
    pub dfa_state:     StateId,
}

impl<'s> ParserRunner<'s> {
    pub fn new(input: &'s str) -> Self {
        let input_iter = input.chars();
        let current_input = default();
        let dfa_state = Dfa::START_STATE;
        Self { input_iter, current_input, dfa_state }
    }

    pub fn next_input_char(&mut self) {
        self.current_input = self.input_iter.next().map(|t| t.into()).unwrap_or(Symbol::eof());
    }
}

pub trait Parser {
    fn bindings() -> Vec<Binding>;
    fn runner(&self) -> &ParserRunner;
}

#[derive(Clone, Debug)]
pub struct Binding {
    pub pattern: Pattern,
    pub name:    String,
}

impl Binding {
    pub fn new(pattern: Pattern, name: String) -> Self {
        Self { pattern, name }
    }
}

use crate::Pattern;

pub struct P1<'s> {
    pub runner: ParserRunner<'s>,
}



macro_rules! def_parser {
    ([$($bound:tt)*] $name:ident [$($arg:tt)*] $($ts:tt)*) => {
        impl<$($bound)*> P1<$($arg)*> {
            $crate::def_parser_body!{[] [$($bound)*] $name [$($arg)*] $($ts)*}
        }
    };
}

#[macro_export]
macro_rules! def_parser_body {
    ([$([$pat:literal $fn_name:ident])*] [$($bound:tt)*] $name:ident [$($arg:tt)*]) => {
        fn _gen_bindings_() -> Vec<Binding> {
            $(
                let pattern = Pattern::from_regex($pat);
                let fn_name = stringify!($fn_name).into();
                let $fn_name = Binding::new(pattern,fn_name);
            )*
            vec![$($fn_name),*]
        }
    };
    ([$($all_fns:tt)*] [$($bound:tt)*] $name:ident [$($arg:tt)*]
        pat fn $pat:literal $fn_name:ident $fn_args:tt $fn_body:tt $($ts:tt)*
    ) => {
        pub fn $fn_name $fn_args $fn_body
        $crate::def_parser_body!{ [$($all_fns)* [$pat $fn_name]] [$($bound)*] $name [$($arg)*] $($ts)* }
    };
}

#[macro_export]
def_parser! {['s] P1['s]
    pat fn "aa*" aa_star(&mut self) {
        println!("TEST aa*");
    }

    // pat fn "ax" ax(&mut self) {
    //     println!("TEST ax!");
    // }
    //
    // pat fn "axy" axy(&mut self) {
    //     println!("TEST axy!");
    // }
    //
    // pat fn "a?x." ax_dot(&mut self) {
    //     println!("TEST ax.!");
    // }
}


impl<'s> Parser for P1<'s> {
    fn bindings() -> Vec<Binding> {
        Self::_gen_bindings_()
    }

    fn runner(&self) -> &ParserRunner {
        &self.runner
    }
}

pub fn gen_parser<P: Parser>() -> String {
    let bindings = P::bindings();
    let mut nfa = Nfa::default();
    let mut map: HashMap<nfa::StateId, String> = default();
    for binding in bindings {
        let id = nfa.new_pattern(nfa.start, &binding.pattern);
        println!("-----");
        println!("{}", nfa.as_graphviz_code());
        map.insert(id, binding.name.clone());
    }
    let dfa = Dfa::from(&nfa);
    println!("=====");
    println!("{}", dfa.as_graphviz_code());
    gen_parser_steps_code("P1".into(), &dfa, &map)
}

pub fn example_parser() -> String {
    gen_parser::<P1>()
}

// pub fn example_parser2() -> String {
//     use crate::pattern::Pattern;
//     use crate::state::INVALID_IX as X;
//
//     let mut nfa = Nfa::default();
//     let start_state_id = nfa.start;
//
//     let s0 = nfa.start;
//     let s1 = nfa.new_state();
//     let s2 = nfa.new_state();
//     let s3 = nfa.new_state();
//     nfa.connect(s0, s1, 'a');
//     nfa.connect(s0, s3, 'c');
//     nfa.connect_eps(s1, s0);
//     nfa.connect(s1, s2, 'b');
//     nfa.connect(s2, s1, 'a');
//     nfa.connect(s3, s2, 'c');
//     nfa.connect_eps(s3, s2);
//
//     let m1 = nfa_matrix(&nfa);
//     #[rustfmt::skip]
//         let m1_expected = Matrix::<nfa::StateId>::new_from_slice(4, 5, &[
//         X, 1, X, 3, X,
//         X, X, 2, X, X,
//         X, 1, X, X, X,
//         X, X, X, 2, X,
//     ]);
//     assert_eq!(m1, m1_expected);
//
//     let m2 = eps_matrix(&nfa);
//     // println!("{:?}", m2);
//
//
//     // let pattern = Pattern::never().many();
//
//     // let pattern = Pattern::or(Pattern::range('x'..='x'), Pattern::range('y'..='y'));
//
//     // let pattern = Pattern::Seq(vec![Pattern::range('y'..='y'), Pattern::range('z'..='z')]);
//
//     // nfa.new_pattern(start_state_id, pattern);
//     let dfa = Dfa::from(&nfa);
//
//     // println!("{:?}", dfa.sources);
//     //
//     // println!("{}", dfa.as_graphviz_code());
//     //
//     // println!("{}", gen_parser_steps_code(&dfa));
//     gen_parser_steps_code(&dfa)
// }


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

pub fn nfa_matrix(nfa: &Nfa) -> Matrix<Vec<nfa::StateId>> {
    let mut matrix =
        Matrix::<Vec<nfa::StateId>>::new(nfa.states.len(), nfa.alphabet.divisions.len());

    for (state_ix, state) in nfa.states.iter().enumerate() {
        let targets = state.targets(&nfa.alphabet);
        for (voc_ix, target_state_id) in targets.iter().enumerate() {
            matrix[(state_ix, voc_ix)].extend(target_state_id);
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
        let mut dfa_eps_map = HashMap::<nfa::StateSetId, StateId>::new();

        dfa_eps_ixs.push(eps_mat[0].clone());
        dfa_eps_map.insert(eps_mat[0].clone(), Dfa::START_STATE);

        let mut i = 0;
        while i < dfa_eps_ixs.len() {
            dfa_mat.new_row();
            for voc_ix in 0..nfa.alphabet.divisions.len() {
                let mut eps_set = nfa::StateSetId::new();
                for &eps_ix in &dfa_eps_ixs[i] {
                    let tgts = &nfa_mat[(eps_ix.id(), voc_ix)];
                    for tgt in tgts {
                        eps_set.extend(eps_mat[tgt.id()].iter());
                    }
                }
                if !eps_set.is_empty() {
                    dfa_mat[(i, voc_ix)] = match dfa_eps_map.get(&eps_set) {
                        Some(&id) => id,
                        None => {
                            let id = StateId::new(dfa_eps_ixs.len());
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
        StateId::INVALID.id()
    }

    fn assert_same_alphabet(dfa: &Dfa, nfa: &Nfa) {
        assert_eq!(dfa.alphabet, nfa.alphabet.seal());
    }

    fn assert_same_matrix(dfa: &Dfa, expected: &Matrix<StateId>) {
        assert_eq!(dfa.links, *expected);
    }

    fn get_name<'a>(nfa: &'a NfaTest, dfa: &Dfa, state: StateId) -> Option<&'a String> {
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

    fn make_state(ix: usize) -> StateId {
        StateId::new(ix)
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
        // assert_eq!(m1, m1_expected);

        let m2 = eps_matrix(&nfa);
        println!("{:?}", m2);


        // let pattern = Pattern::never().many();

        // let pattern = Pattern::or(Pattern::range('x'..='x'), Pattern::range('y'..='y'));

        // let pattern = Pattern::Seq(vec![Pattern::range('y'..='y'), Pattern::range('z'..='z')]);

        // nfa.new_pattern(start_state_id, pattern);
        let dfa = Dfa::from(&nfa);

        // println!("{:?}", dfa.sources);

        println!("{}", dfa.as_graphviz_code());

        // println!("{}", gen_parser_steps_code(&dfa));

        use regex_syntax;

        println!("!!!");
        println!("{:#?}", regex_syntax::Parser::new().parse("ab|c").unwrap());
    }

    #[test]
    fn test3() {
        use crate::pattern::Pattern;
        use crate::state::INVALID_IX as X;

        let mut nfa = Nfa::default();
        let start_state_id = nfa.start;

        let pat = (Pattern::from('a') >> Pattern::from('b')).many() >> Pattern::any();
        nfa.new_pattern(nfa.start, pat);

        println!("{}", nfa.as_graphviz_code());

        let m1 = nfa_matrix(&nfa);
        println!("{:?}", m1);

        println!("\n\n");

        let m2 = eps_matrix(&nfa);

        let dfa = Dfa::from(&nfa);

        println!("{}", dfa.as_graphviz_code());
    }

    #[test]
    fn test2() {
        use crate::pattern::Pattern;
        use crate::state::INVALID_IX as X;

        let mut nfa = Nfa::default();
        let start_state_id = nfa.start;

        let s0 = nfa.start;
        let s1 = nfa.new_state();
        let s2 = nfa.new_state_exported();
        let s3 = nfa.new_state();
        let s4 = nfa.new_state();
        let s5 = nfa.new_state_exported();
        nfa.connect(s0, s1, 'a');
        nfa.connect(s1, s2, 'b');
        nfa.connect(s0, s3, 'a');
        nfa.connect(s3, s4, 'b');
        nfa.connect(s4, s5, 'c');

        println!("{}", nfa.as_graphviz_code());

        let m1 = nfa_matrix(&nfa);
        println!("{:?}", m1);

        println!("\n\n");



        // #[rustfmt::skip]
        //     let m1_expected = Matrix::<nfa::StateId>::new_from_slice(4, 5, &[
        //     X, 1, X, 3, X,
        //     X, X, 2, X, X,
        //     X, 1, X, X, X,
        //     X, X, X, 2, X,
        // ]);
        // assert_eq!(m1, m1_expected);

        let m2 = eps_matrix(&nfa);
        // println!("{:?}", m2);


        // let pattern = Pattern::never().many();

        // let pattern = Pattern::or(Pattern::range('x'..='x'), Pattern::range('y'..='y'));

        // let pattern = Pattern::Seq(vec![Pattern::range('y'..='y'), Pattern::range('z'..='z')]);

        // nfa.new_pattern(start_state_id, pattern);
        let dfa = Dfa::from(&nfa);

        // println!("{:?}", dfa.sources);

        println!("{}", dfa.as_graphviz_code());

        // println!("{}", gen_parser_steps_code(&dfa));
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
