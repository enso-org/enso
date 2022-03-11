//! The structure for defining non-deterministic finite automata.

use crate::prelude::*;

use crate::alphabet;
use crate::data::matrix::Matrix;
use crate::pattern::Pattern;
use crate::state;
use crate::symbol::Symbol;

use std::collections::BTreeSet;
use std::ops::RangeInclusive;



// =============
// === Types ===
// =============

/// Specialized NFA state type.
pub type StateId = state::StateId<Nfa>;

/// A state identifier based on a set of states.
///
/// This is used during the NFA -> Dfa transformation, where multiple states can merge together due
/// to the collapsing of epsilon transitions.
pub type StateSetId = BTreeSet<StateId>;



// ==================
// === Transition ===
// ==================

/// A transition between two NFA states. The transition between the states requires a symbol to be
/// consumed.
#[derive(Clone, Debug, PartialEq, Eq)]
#[allow(missing_docs)]
pub struct Transition {
    pub symbols: RangeInclusive<Symbol>,
    pub target:  StateId,
}

impl Transition {
    /// Constructor.
    pub fn new(symbols: RangeInclusive<Symbol>, target: StateId) -> Self {
        Self { symbols, target }
    }

    /// Display the symbols range of this transition.
    pub fn display_symbols(&self) -> String {
        if self.symbols.start() == self.symbols.end() {
            format!("{}", self.symbols.start())
        } else {
            format!("{} .. {}", self.symbols.start(), self.symbols.end())
        }
    }
}



// ===========
// == State ==
// ===========

/// A named state for a [`super::nfa::Nfa`].
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct State {
    /// A set of transitions that can trigger without consuming a symbol (ε-transitions).
    pub epsilon_links: Vec<StateId>,
    /// The set of transitions that trigger while consuming a specific symbol.
    ///
    /// When triggered, the automaton will transition to the [`Transition::target`].
    pub links:         Vec<Transition>,
    /// Information whether the state should be exported and marked as a "source" state in the DFA
    /// representation. Non exported states are considered "transitive" states and are used as
    /// helpers to design the NFA network. All user defined states are marked to be exported.
    pub export:        bool,
}

impl State {
    /// Get a reference to the links in this state.
    pub fn links(&self) -> &Vec<Transition> {
        &self.links
    }

    /// Get a reference to the epsilon links in this state.
    pub fn epsilon_links(&self) -> &Vec<StateId> {
        &self.epsilon_links
    }

    /// Returns the transition (next state) for each symbol in the alphabet.
    pub fn targets(&self, alphabet: &alphabet::Segmentation) -> Vec<StateId> {
        let mut targets = vec![];
        let mut index = 0;
        let mut links = self.links.clone();
        links.sort_by_key(|link| link.symbols.start().clone());
        for symbol in &alphabet.divisions {
            while links.len() > index && links[index].symbols.end() < symbol {
                index += 1;
            }
            if links.len() <= index || links[index].symbols.start() > symbol {
                targets.push(StateId::INVALID);
            } else {
                targets.push(links[index].target);
            }
        }
        targets
    }
}


// ===========
// === NFA ===
// ===========

/// The definition of a [NFA](https://en.wikipedia.org/wiki/Nondeterministic_finite_automaton) for a
/// given set of symbols, states, and transitions (specifically a NFA with ε-moves).
///
/// A NFA is a finite state automaton that accepts or rejects a given sequence of symbols. In
/// contrast with a DFA, the NFA may transition between states _without_ reading any new symbol
/// through use of
/// [epsilon links](https://en.wikipedia.org/wiki/Nondeterministic_finite_automaton#NFA_with_%CE%B5-moves).
///
/// For a good introduction to NDA, DFA, and their conversions, see the following videos:
/// - [Regular Expression to NFA](https://www.youtube.com/watch?v=RYNN-tb9WxI)
/// - [Converting NFA to DFA](https://www.youtube.com/watch?v=taClnxU-nao)
#[derive(Clone, Debug, PartialEq, Eq)]
#[allow(missing_docs)]
pub struct Nfa {
    pub start:           StateId,
    pub(crate) alphabet: alphabet::Segmentation,
    pub(crate) states:   Vec<State>,
}

impl Nfa {
    /// Constructor.
    pub fn new() -> Self {
        let start = default();
        let alphabet = default();
        let states = default();
        Self { start, alphabet, states }.init_start_state()
    }

    /// Initialize the start state of the automaton.
    fn init_start_state(mut self) -> Self {
        let start = self.new_state();
        self[start].export = true;
        self.start = start;
        self
    }

    /// Adds a new state to the NFA and returns its identifier.
    pub fn new_state(&mut self) -> StateId {
        let id = self.states.len();
        self.states.push(default());
        StateId::new(id)
    }

    /// Adds a new state to the NFA, marks it as an exported state, and returns its identifier.
    pub fn new_state_exported(&mut self) -> StateId {
        let state = self.new_state();
        self[state].export = true;
        state
    }

    /// Get a reference to the states for this automaton.
    pub fn states(&self) -> &Vec<State> {
        &self.states
    }

    /// Get a reference to the alphabet for this automaton.
    pub fn alphabet(&self) -> &alphabet::Segmentation {
        &self.alphabet
    }

    /// Create a new connection between [`source`] and [`target`] that can be traversed by consuming
    /// [`symbols`]. If [`symbols`] are not provided, an epsilon transition is created, which allows
    /// freely transitioning between the states without consuming any input.
    pub fn connect(
        &mut self,
        source: StateId,
        target: StateId,
        symbols: Option<&RangeInclusive<Symbol>>,
    ) {
        match symbols {
            None => self[source].epsilon_links.push(target),
            Some(symbols) => {
                self.alphabet.insert(symbols.clone());
                self[source].links.push(Transition::new(symbols.clone(), target));
            }
        }
    }

    /// Transforms a pattern to connected NFA states by using the algorithm described
    /// [here](https://www.youtube.com/watch?v=RYNN-tb9WxI). Please note that the video contains
    /// error in the explanation of [`Pattern::Many`]. The correct solution is presented and
    /// implemented below.
    pub fn new_pattern(&mut self, source: StateId, pattern: impl AsRef<Pattern>) -> StateId {
        self.new_pattern_to(source, pattern, None)
    }

    /// Just like [`new_pattern`], but the target [`StateId`] can be provided as an argument.
    pub fn new_pattern_to(
        &mut self,
        source: StateId,
        pattern: impl AsRef<Pattern>,
        target: Option<StateId>,
    ) -> StateId {
        let output_state = match pattern.as_ref() {
            // source ──<range>──▶ target
            Pattern::Range(range) => {
                let target = target.unwrap_or_else(|| self.new_state());
                self.connect(source, target, Some(range));
                target
            }

            //                       ╭──EPSILON──╮
            //                       ▼           │
            // source ──EPSILON──▶ s1 ──body──▶ s2 ──EPSILON──▶ target
            //       │                                           ▲
            //       ╰──────────────────EPSILON───────────────────╯
            Pattern::Many(body) => {
                let s1 = self.new_state();
                let s2 = self.new_state();
                self.new_pattern_to(s1, body, Some(s2));
                let target = target.unwrap_or_else(|| self.new_state());
                self.connect(source, s1, None);
                self.connect(source, target, None);
                self.connect(s2, target, None);
                self.connect(s2, s1, None);
                target
            }

            // source ──first──▶ middle ──second──▶ target
            Pattern::Seq(first, second) => {
                let target = target.unwrap_or_else(|| self.new_state());
                let middle = self.new_pattern_to(source, first, None);
                self.new_pattern_to(middle, second, Some(target));
                target
            }

            // source ──┬──first───┬─▶ target
            //          ╰──second──╯
            Pattern::Or(first, second) => {
                let target = target.unwrap_or_else(|| self.new_state());
                self.new_pattern_to(source, first, Some(target));
                self.new_pattern_to(source, second, Some(target));
                target
            }

            // source ──▶ target
            Pattern::Always => match target {
                None => source,
                Some(target) => {
                    self.connect(source, target, None);
                    target
                }
            },

            // current   <no connection>   target
            Pattern::Never => match target {
                None => self.new_state(),
                Some(target) => target,
            },
        };
        self[output_state].export = true;
        output_state
    }



    /// Convert the automata to a GraphViz Dot code for the deubgging purposes.
    pub fn as_graphviz_code(&self) -> String {
        let mut out = String::new();
        for (ix, state) in self.states.iter().enumerate() {
            let opts =
                if state.export { "" } else { "[fillcolor=\"#EEEEEE\" fontcolor=\"#888888\"]" };
            out += &format!("node_{}[label=\"{}\"]{}\n", ix, ix, opts);
            for link in &state.links {
                out += &format!(
                    "node_{} -> node_{}[label=\"{}\"]\n",
                    ix,
                    link.target.id(),
                    link.display_symbols()
                );
            }
            for link in &state.epsilon_links {
                out += &format!("node_{} -> node_{}[style=dashed]\n", ix, link.id());
            }
        }
        let opts = "node [shape=circle style=filled fillcolor=\"#4385f5\" fontcolor=\"#FFFFFF\" \
        color=white penwidth=5.0 margin=0.1 width=0.5 height=0.5 fixedsize=true]";
        format!("digraph G {{\n{}\n{}\n}}\n", opts, out)
    }
}

impl Default for Nfa {
    fn default() -> Self {
        Self::new()
    }
}

impl Index<StateId> for Nfa {
    type Output = State;
    fn index(&self, state: StateId) -> &Self::Output {
        &self.states[state.id()]
    }
}

impl IndexMut<StateId> for Nfa {
    fn index_mut(&mut self, state: StateId) -> &mut Self::Output {
        &mut self.states[state.id()]
    }
}



// ===========
// == Tests ==
// ===========

#[cfg(test)]
pub mod tests {
    use super::*;

    // === Test Utilities ===

    #[allow(missing_docs)]
    #[derive(Clone, Debug, Default, PartialEq)]
    pub struct NfaTest {
        pub nfa:               Nfa,
        pub start_state_id:    StateId,
        pub pattern_state_ids: Vec<StateId>,
        pub end_state_id:      StateId,
        pub callbacks:         HashMap<StateId, String>,
        pub names:             HashMap<StateId, String>,
    }
    #[allow(missing_docs)]
    impl NfaTest {
        pub fn make(patterns: Vec<Pattern>) -> Self {
            let mut nfa = Nfa::default();
            let start_state_id = nfa.start;
            let mut pattern_state_ids = vec![];
            let end_state_id = nfa.new_state_exported();
            for pattern in patterns {
                let id = nfa.new_pattern(start_state_id, &pattern);
                pattern_state_ids.push(id);
                nfa.connect(id, end_state_id, None);
            }
            let callbacks = default();
            let names = default();
            Self { nfa, start_state_id, pattern_state_ids, end_state_id, callbacks, names }
        }

        pub fn make_rules(rules: Vec<Rule>) -> Self {
            let mut nfa = Nfa::default();
            let start_state_id = nfa.start;
            let mut pattern_state_ids = vec![];
            let end_state_id = nfa.new_state_exported();
            let mut callbacks: HashMap<_, _> = default();
            let mut names: HashMap<_, _> = default();
            for rule in rules {
                let id = nfa.new_pattern(start_state_id, &rule.pattern);
                callbacks.insert(id, rule.callback.clone());
                names.insert(id, rule.name.clone());
                pattern_state_ids.push(id);
                nfa.connect(id, end_state_id, None);
            }
            Self { nfa, start_state_id, pattern_state_ids, end_state_id, callbacks, names }
        }

        pub fn callback(&self, state: StateId) -> Option<&String> {
            self.callbacks.get(&state)
        }

        pub fn name(&self, state: StateId) -> Option<&String> {
            self.names.get(&state)
        }

        pub fn id(id: usize) -> StateId {
            StateId::new(id)
        }

        pub fn has_transition(&self, trigger: RangeInclusive<Symbol>, target: StateId) -> bool {
            self.states.iter().any(|r| {
                r.links().iter().any(|transition| {
                    (transition.symbols == trigger) && transition.target == target
                })
            })
        }

        pub fn has_epsilon(&self, from: StateId, to: StateId) -> bool {
            self.states.iter().enumerate().fold(false, |l, (ix, r)| {
                let state_has =
                    ix == from.id() && r.epsilon_links().iter().any(|ident| *ident == to);
                l || state_has
            })
        }
    }
    impl Deref for NfaTest {
        type Target = Nfa;

        fn deref(&self) -> &Self::Target {
            &self.nfa
        }
    }

    #[allow(missing_docs)]
    #[derive(Clone, Debug, PartialEq)]
    pub struct Rule {
        pattern:  Pattern,
        callback: String,
        name:     String,
    }
    #[allow(missing_docs)]
    impl Rule {
        pub fn new(pattern: &Pattern, callback: impl Str, name: impl Str) -> Rule {
            let pattern = pattern.clone();
            let callback = callback.into();
            let name = name.into();
            Rule { pattern, callback, name }
        }
    }


    // === The Automata ===

    pub fn pattern_range() -> NfaTest {
        let pattern = Pattern::range('a'..='z');
        NfaTest::make(vec![pattern])
    }

    pub fn pattern_or() -> NfaTest {
        let pattern = Pattern::char('a') | Pattern::char('d');
        NfaTest::make(vec![pattern])
    }

    pub fn pattern_seq() -> NfaTest {
        let pattern = Pattern::char('a') >> Pattern::char('d');
        NfaTest::make(vec![pattern])
    }

    pub fn pattern_many() -> NfaTest {
        let pattern = Pattern::char('a').many();
        NfaTest::make(vec![pattern])
    }

    pub fn pattern_always() -> NfaTest {
        let pattern = Pattern::always();
        NfaTest::make(vec![pattern])
    }

    pub fn pattern_never() -> NfaTest {
        let pattern = Pattern::never();
        NfaTest::make(vec![pattern])
    }

    pub fn simple_rules() -> NfaTest {
        let a = Pattern::char('a');
        let b = Pattern::char('b');
        let ab = &a >> &b;
        NfaTest::make(vec![a, ab])
    }

    pub fn complex_rules() -> NfaTest {
        let a_word = Pattern::char('a').many1();
        let b_word = Pattern::char('b').many1();
        let space = Pattern::char(' ');
        let spaced_a_word = &space >> &a_word;
        let spaced_b_word = &space >> &b_word;
        let any = Pattern::any();
        let end = Pattern::eof();
        NfaTest::make(vec![spaced_a_word, spaced_b_word, end, any])
    }

    pub fn named_rules() -> NfaTest {
        let a_word = Pattern::char('a').many1();
        let b_word = Pattern::char('b').many1();
        let rules = vec![
            Rule::new(&a_word, "self.on_a_word(reader)", "rule_1"),
            Rule::new(&b_word, "self.on_b_word(reader)", "rule_2"),
        ];
        NfaTest::make_rules(rules)
    }


    // === The Tests ===

    #[test]
    fn nfa_pattern_range() {
        let nfa = pattern_range();

        println!("{}", nfa.as_graphviz_code());

        assert!(nfa.alphabet.divisions().contains(&Symbol::from(0u64)));
        assert!(nfa.alphabet.divisions().contains(&Symbol::from(97u64)));
        assert!(nfa.alphabet.divisions().contains(&Symbol::from(123u64)));
        assert_eq!(nfa.states.len(), 4);
        assert!(nfa.has_epsilon(nfa.start_state_id, NfaTest::id(2)));
        assert!(nfa.has_epsilon(nfa.pattern_state_ids[0], nfa.end_state_id));
        assert!(nfa.has_transition(Symbol::from('a')..=Symbol::from('z'), nfa.pattern_state_ids[0]));
        assert!(nfa[nfa.start_state_id].export);
        assert!(nfa[nfa.pattern_state_ids[0]].export);
        assert!(nfa[nfa.end_state_id].export);
    }

    #[test]
    fn nfa_pattern_or() {
        let nfa = pattern_or();

        assert!(nfa.alphabet.divisions().contains(&Symbol::from(0u64)));
        assert!(nfa.alphabet.divisions().contains(&Symbol::from(97u64)));
        assert!(nfa.alphabet.divisions().contains(&Symbol::from(98u64)));
        assert!(nfa.alphabet.divisions().contains(&Symbol::from(100u64)));
        assert!(nfa.alphabet.divisions().contains(&Symbol::from(101u64)));
        assert_eq!(nfa.states.len(), 8);
        assert!(nfa.has_epsilon(nfa.start_state_id, NfaTest::id(2)));
        assert!(nfa.has_epsilon(NfaTest::id(2), NfaTest::id(3)));
        assert!(nfa.has_epsilon(NfaTest::id(2), NfaTest::id(5)));
        assert!(nfa.has_epsilon(NfaTest::id(6), nfa.pattern_state_ids[0]));
        assert!(nfa.has_epsilon(NfaTest::id(4), nfa.pattern_state_ids[0]));
        assert!(nfa.has_epsilon(nfa.pattern_state_ids[0], nfa.end_state_id));
        assert!(nfa.has_transition(Symbol::from('a')..=Symbol::from('a'), NfaTest::id(4)));
        assert!(nfa.has_transition(Symbol::from('d')..=Symbol::from('d'), NfaTest::id(6)));
        assert!(nfa[nfa.start_state_id].export);
        assert!(nfa[nfa.pattern_state_ids[0]].export);
        assert!(nfa[nfa.end_state_id].export);
    }

    #[test]
    fn nfa_pattern_seq() {
        let nfa = pattern_seq();

        assert!(nfa.alphabet.divisions().contains(&Symbol::from(0u64)));
        assert!(nfa.alphabet.divisions().contains(&Symbol::from(97u64)));
        assert!(nfa.alphabet.divisions().contains(&Symbol::from(98u64)));
        assert!(nfa.alphabet.divisions().contains(&Symbol::from(100u64)));
        assert!(nfa.alphabet.divisions().contains(&Symbol::from(101u64)));
        assert_eq!(nfa.states.len(), 7);
        assert!(nfa.has_epsilon(nfa.start_state_id, NfaTest::id(2)));
        assert!(nfa.has_epsilon(NfaTest::id(2), NfaTest::id(3)));
        assert!(nfa.has_epsilon(NfaTest::id(4), NfaTest::id(5)));
        assert!(nfa.has_epsilon(nfa.pattern_state_ids[0], nfa.end_state_id));
        assert!(nfa.has_transition(Symbol::from('a')..=Symbol::from('a'), NfaTest::id(4)));
        assert!(nfa.has_transition(Symbol::from('d')..=Symbol::from('d'), NfaTest::id(6)));
        assert!(nfa[nfa.start_state_id].export);
        assert!(nfa[nfa.pattern_state_ids[0]].export);
        assert!(nfa[nfa.end_state_id].export);
    }

    #[test]
    fn nfa_pattern_many() {
        let nfa = pattern_many();

        assert!(nfa.alphabet.divisions().contains(&Symbol::from(0u64)));
        assert!(nfa.alphabet.divisions().contains(&Symbol::from(97u64)));
        assert!(nfa.alphabet.divisions().contains(&Symbol::from(98u64)));
        assert_eq!(nfa.states.len(), 7);
        assert!(nfa.has_epsilon(nfa.start_state_id, NfaTest::id(2)));
        assert!(nfa.has_epsilon(NfaTest::id(2), NfaTest::id(3)));
        assert!(nfa.has_epsilon(NfaTest::id(3), NfaTest::id(4)));
        assert!(nfa.has_epsilon(NfaTest::id(5), nfa.pattern_state_ids[0]));
        assert!(nfa.has_epsilon(nfa.pattern_state_ids[0], NfaTest::id(3)));
        assert!(nfa.has_epsilon(nfa.pattern_state_ids[0], nfa.end_state_id));
        assert!(nfa.has_transition(Symbol::from('a')..=Symbol::from('a'), NfaTest::id(5)));
        assert!(nfa[nfa.start_state_id].export);
        assert!(nfa[nfa.pattern_state_ids[0]].export);
        assert!(nfa[nfa.end_state_id].export);
    }

    #[test]
    fn nfa_pattern_always() {
        let nfa = pattern_always();

        assert!(nfa.alphabet.divisions().contains(&Symbol::from(0u64)));
        assert_eq!(nfa.states.len(), 3);
        assert!(nfa.has_epsilon(nfa.start_state_id, nfa.pattern_state_ids[0]));
        assert!(nfa.has_epsilon(nfa.pattern_state_ids[0], nfa.end_state_id));
        assert!(nfa[nfa.start_state_id].export);
        assert!(nfa[nfa.pattern_state_ids[0]].export);
        assert!(nfa[nfa.end_state_id].export);
    }

    #[test]
    fn nfa_pattern_never() {
        let nfa = pattern_never();

        assert!(nfa.alphabet.divisions().contains(&Symbol::from(0u64)));
        assert_eq!(nfa.states.len(), 4);
        assert!(nfa.has_epsilon(nfa.start_state_id, NfaTest::id(2)));
        assert!(nfa.has_epsilon(NfaTest::id(3), nfa.end_state_id));
        assert!(nfa[nfa.start_state_id].export);
        assert!(nfa[nfa.pattern_state_ids[0]].export);
        assert!(nfa[nfa.end_state_id].export);
    }

    #[test]
    fn nfa_simple_rules() {
        let nfa = simple_rules();

        assert!(nfa.alphabet.divisions().contains(&Symbol::from(0u64)));
        assert!(nfa.alphabet.divisions().contains(&Symbol::from(97u64)));
        assert!(nfa.alphabet.divisions().contains(&Symbol::from(98u64)));
        assert!(nfa.alphabet.divisions().contains(&Symbol::from(99u64)));
        assert_eq!(nfa.states.len(), 9);
        assert!(nfa.has_epsilon(nfa.start_state_id, NfaTest::id(2)));
        assert!(nfa.has_epsilon(nfa.start_state_id, NfaTest::id(4)));
        assert!(nfa.has_epsilon(nfa.pattern_state_ids[0], nfa.end_state_id));
        assert!(nfa.has_epsilon(NfaTest::id(4), NfaTest::id(5)));
        assert!(nfa.has_epsilon(NfaTest::id(6), NfaTest::id(7)));
        assert!(nfa.has_epsilon(nfa.pattern_state_ids[1], nfa.end_state_id));
        assert!(nfa.has_transition(Symbol::from('a')..=Symbol::from('a'), nfa.pattern_state_ids[0]));
        assert!(nfa.has_transition(Symbol::from('a')..=Symbol::from('a'), NfaTest::id(6)));
        assert!(nfa.has_transition(Symbol::from('b')..=Symbol::from('b'), nfa.pattern_state_ids[1]));
        assert!(nfa[nfa.start_state_id].export);
        assert!(nfa[nfa.pattern_state_ids[0]].export);
        assert!(nfa[nfa.pattern_state_ids[1]].export);
        assert!(nfa[nfa.end_state_id].export);
    }

    #[test]
    fn nfa_complex_rules() {
        let nfa = complex_rules();

        assert!(nfa.alphabet.divisions().contains(&Symbol::from(0u64)));
        assert!(nfa.alphabet.divisions().contains(&Symbol::from(32u64)));
        assert!(nfa.alphabet.divisions().contains(&Symbol::from(33u64)));
        assert!(nfa.alphabet.divisions().contains(&Symbol::from(97u64)));
        assert!(nfa.alphabet.divisions().contains(&Symbol::from(98u64)));
        assert!(nfa.alphabet.divisions().contains(&Symbol::from(99u64)));
        assert!(nfa.alphabet.divisions().contains(&Symbol::eof()));
        assert_eq!(nfa.states.len(), 26);
        assert!(nfa.has_transition(Symbol::from(' ')..=Symbol::from(' '), NfaTest::id(4)));
        assert!(nfa.has_transition(Symbol::from('a')..=Symbol::from('a'), NfaTest::id(6)));
        assert!(nfa.has_transition(Symbol::from('a')..=Symbol::from('a'), NfaTest::id(10)));
        assert!(nfa.has_transition(Symbol::from(' ')..=Symbol::from(' '), NfaTest::id(14)));
        assert!(nfa.has_transition(Symbol::from('b')..=Symbol::from('b'), NfaTest::id(16)));
        assert!(nfa.has_transition(Symbol::from('b')..=Symbol::from('b'), NfaTest::id(20)));
        assert!(nfa.has_transition(Symbol::eof()..=Symbol::eof(), nfa.pattern_state_ids[2]));
        assert!(nfa.has_transition(Symbol::null()..=Symbol::eof(), nfa.pattern_state_ids[3]));
        assert!(nfa[nfa.start_state_id].export);
        assert!(nfa[nfa.pattern_state_ids[0]].export);
        assert!(nfa[nfa.pattern_state_ids[1]].export);
        assert!(nfa[nfa.pattern_state_ids[2]].export);
        assert!(nfa[nfa.pattern_state_ids[3]].export);
        assert!(nfa[nfa.end_state_id].export);
    }

    #[test]
    fn nfa_named_rules() {
        let nfa = named_rules();

        assert_eq!(nfa.states.len(), 18);
        for (ix, _) in nfa.states.iter().enumerate() {
            let state_id = StateId::new(ix);
            if nfa.pattern_state_ids.contains(&state_id) {
                assert!(nfa.name(state_id).is_some());
                assert!(nfa.callback(state_id).is_some());
            } else {
                assert!(nfa.name(state_id).is_none());
                assert!(nfa.callback(state_id).is_none());
            }
        }
        assert_eq!(nfa.name(nfa.pattern_state_ids[0]), Some(&("rule_1".to_string())));
        assert_eq!(
            nfa.callback(nfa.pattern_state_ids[0]),
            Some(&("self.on_a_word(reader)".to_string()))
        );
        assert_eq!(nfa.name(nfa.pattern_state_ids[1]), Some(&("rule_2".to_string())));
        assert_eq!(
            nfa.callback(nfa.pattern_state_ids[1]),
            Some(&("self.on_b_word(reader)".to_string()))
        );
    }
}
