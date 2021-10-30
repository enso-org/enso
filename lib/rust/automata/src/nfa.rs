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

/// A state identifier based on a set of states.
///
/// This is used during the NFA -> Dfa transformation, where multiple states can merge together due
/// to the collapsing of epsilon transitions.
pub type StateSetId = BTreeSet<State>;



// =========================================
// === Non-Deterministic Finite Automata ===
// =========================================

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

    /// Initialize the start state of the automaton.
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

    /// Get a reference to the states for this automaton.
    pub fn states(&self) -> &Vec<state::Data> {
        &self.states
    }

    /// Get a reference to the alphabet for this automaton.
    pub fn alphabet(&self) -> &alphabet::Segmentation {
        &self.alphabet
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
    pub fn new_pattern(&mut self, source:State, pattern:impl AsRef<Pattern>) -> State {
        let pattern = pattern.as_ref();
        let current = self.new_state();
        self.connect(source,current);
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
        let current = self.new_state();
        self.connect(source,current);
        match pattern {
            Pattern::Range(range) => {
                self.connect_via(current,target,range);
            },
            Pattern::Many(body) => {
                let s1 = self.new_state();
                let s2 = self.new_pattern(s1,body);
                let target = self.new_state();
                self.connect(current,s1);
                self.connect(current,target);
                self.connect(s2,target);
                self.connect(target,s1);
            },
            Pattern::Seq(patterns) => {
                let out = patterns.iter().fold(current,|s,pat| self.new_pattern(s,pat));
                self.connect(out,target)
            },
            Pattern::Or(patterns) => {
                let states = patterns.iter().map(|pat| self.new_pattern(current,pat)).collect_vec();
                for state in states {
                    self.connect(state,target);
                }
            },
            Pattern::Always => {
                self.connect(current,target)
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

    /// Convert the automata to a GraphViz Dot code for the deubgging purposes.
    pub fn as_graphviz_code(&self) -> String {
        let mut out = String::new();
        for (ix,state) in self.states.iter().enumerate() {
            let opts = if state.export { "" } else {
                "[fillcolor=\"#EEEEEE\" fontcolor=\"#888888\"]"
            };
            out += &format!("node_{}[label=\"{}\"]{}\n",ix,ix,opts);
            for link in &state.links {
                out += &format!(
                    "node_{} -> node_{}[label=\"{}\"]\n",ix,link.target.id(),link.display_symbols()
                );
            }
            for link in &state.epsilon_links {
                out += &format!("node_{} -> node_{}[style=dashed]\n",ix,link.id());
            }
        }
        let opts = "node [shape=circle style=filled fillcolor=\"#4385f5\" fontcolor=\"#FFFFFF\" \
        color=white penwidth=5.0 margin=0.1 width=0.5 height=0.5 fixedsize=true]";
        format!("digraph G {{\n{}\n{}\n}}\n",opts,out)
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



// ===========
// == Tests ==
// ===========

#[cfg(test)]
pub mod tests {
    use super::*;

    // === Test Utilities ===

    #[allow(missing_docs)]
    #[derive(Clone,Debug,Default,PartialEq)]
    pub struct NfaTest {
        pub nfa               : Nfa,
        pub start_state_id    : State,
        pub pattern_state_ids : Vec<State>,
        pub end_state_id      : State,
        pub callbacks         : HashMap<State,String>,
        pub names             : HashMap<State,String>,
    }
    #[allow(missing_docs)]
    impl NfaTest {
        pub fn make(patterns:Vec<Pattern>) -> Self {
            let mut nfa               = Nfa::default();
            let start_state_id        = nfa.start;
            let mut pattern_state_ids = vec![];
            let end_state_id          = nfa.new_state_exported();
            for pattern in patterns {
                let id = nfa.new_pattern(start_state_id,&pattern);
                pattern_state_ids.push(id);
                nfa.connect(id,end_state_id);
            }
            let callbacks = default();
            let names     = default();
            Self{nfa,start_state_id,pattern_state_ids,end_state_id,callbacks,names}
        }

        pub fn make_rules(rules:Vec<Rule>) -> Self {
            let mut nfa                    = Nfa::default();
            let start_state_id             = nfa.start;
            let mut pattern_state_ids      = vec![];
            let end_state_id               = nfa.new_state_exported();
            let mut callbacks:HashMap<_,_> = default();
            let mut names:HashMap<_,_>     = default();
            for rule in rules {
                let id = nfa.new_pattern(start_state_id,&rule.pattern);
                callbacks.insert(id,rule.callback.clone());
                names.insert(id,rule.name.clone());
                pattern_state_ids.push(id);
                nfa.connect(id,end_state_id);
            }
            Self{nfa,start_state_id,pattern_state_ids,end_state_id,callbacks,names}
        }

        pub fn callback(&self, state:State) -> Option<&String> {
            self.callbacks.get(&state)
        }

        pub fn name(&self, state:State) -> Option<&String> {
            self.names.get(&state)
        }

        pub fn id(id:usize) -> State {
            State::new(id)
        }

        pub fn has_transition(&self, trigger:RangeInclusive<Symbol>, target:State) -> bool {
            self.states.iter().any(|r| r.links().iter().find(|transition | {
                    (transition.symbols == trigger) && transition.target == target
                }).is_some())
        }

        pub fn has_epsilon(&self, from:State, to:State) -> bool {
            self.states.iter().enumerate().fold(false,|l,(ix,r)| {
                let state_has = ix == from.id() && r.epsilon_links().iter().find(|ident| {
                    **ident == to
                }).is_some();
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
    #[derive(Clone,Debug,PartialEq)]
    pub struct Rule {
        pattern : Pattern,
        callback : String,
        name : String
    }
    #[allow(missing_docs)]
    impl Rule {
        pub fn new(pattern:&Pattern, callback:impl Str, name:impl Str) -> Rule {
            let pattern  = pattern.clone();
            let callback = callback.into();
            let name     = name.into();
            Rule{pattern,callback,name}
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
        let a   = Pattern::char('a');
        let b   = Pattern::char('b');
        let ab  = &a >> &b;
        NfaTest::make(vec![a,ab])
    }

    pub fn complex_rules() -> NfaTest {
        let a_word        = Pattern::char('a').many1();
        let b_word        = Pattern::char('b').many1();
        let space         = Pattern::char(' ');
        let spaced_a_word = &space >> &a_word;
        let spaced_b_word = &space >> &b_word;
        let any           = Pattern::any();
        let end           = Pattern::eof();
        NfaTest::make(vec![spaced_a_word,spaced_b_word,end,any])
    }

    pub fn named_rules() -> NfaTest {
        let a_word = Pattern::char('a').many1();
        let b_word = Pattern::char('b').many1();
        let rules = vec![
            Rule::new(&a_word,"self.on_a_word(reader)","rule_1"),
            Rule::new(&b_word,"self.on_b_word(reader)","rule_2"),
        ];
        NfaTest::make_rules(rules)
    }


    // === The Tests ===

    #[test]
    fn nfa_pattern_range() {
        let nfa = pattern_range();

        assert!(nfa.alphabet.divisions().contains(&Symbol::from(0u64)));
        assert!(nfa.alphabet.divisions().contains(&Symbol::from(97u64)));
        assert!(nfa.alphabet.divisions().contains(&Symbol::from(123u64)));
        assert_eq!(nfa.states.len(),4);
        assert!(nfa.has_epsilon(nfa.start_state_id,NfaTest::id(2)));
        assert!(nfa.has_epsilon(nfa.pattern_state_ids[0],nfa.end_state_id));
        assert!(nfa.has_transition(Symbol::from('a')..=Symbol::from('z'),nfa.pattern_state_ids[0]));
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
        assert_eq!(nfa.states.len(),8);
        assert!(nfa.has_epsilon(nfa.start_state_id,NfaTest::id(2)));
        assert!(nfa.has_epsilon(NfaTest::id(2),NfaTest::id(3)));
        assert!(nfa.has_epsilon(NfaTest::id(2),NfaTest::id(5)));
        assert!(nfa.has_epsilon(NfaTest::id(6),nfa.pattern_state_ids[0]));
        assert!(nfa.has_epsilon(NfaTest::id(4),nfa.pattern_state_ids[0]));
        assert!(nfa.has_epsilon(nfa.pattern_state_ids[0],nfa.end_state_id));
        assert!(nfa.has_transition(Symbol::from('a')..=Symbol::from('a'),NfaTest::id(4)));
        assert!(nfa.has_transition(Symbol::from('d')..=Symbol::from('d'),NfaTest::id(6)));
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
        assert_eq!(nfa.states.len(),7);
        assert!(nfa.has_epsilon(nfa.start_state_id,NfaTest::id(2)));
        assert!(nfa.has_epsilon(NfaTest::id(2),NfaTest::id(3)));
        assert!(nfa.has_epsilon(NfaTest::id(4),NfaTest::id(5)));
        assert!(nfa.has_epsilon(nfa.pattern_state_ids[0],nfa.end_state_id));
        assert!(nfa.has_transition(Symbol::from('a')..=Symbol::from('a'),NfaTest::id(4)));
        assert!(nfa.has_transition(Symbol::from('d')..=Symbol::from('d'),NfaTest::id(6)));
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
        assert_eq!(nfa.states.len(),7);
        assert!(nfa.has_epsilon(nfa.start_state_id,NfaTest::id(2)));
        assert!(nfa.has_epsilon(NfaTest::id(2),NfaTest::id(3)));
        assert!(nfa.has_epsilon(NfaTest::id(3),NfaTest::id(4)));
        assert!(nfa.has_epsilon(NfaTest::id(5),nfa.pattern_state_ids[0]));
        assert!(nfa.has_epsilon(nfa.pattern_state_ids[0],NfaTest::id(3)));
        assert!(nfa.has_epsilon(nfa.pattern_state_ids[0],nfa.end_state_id));
        assert!(nfa.has_transition(Symbol::from('a')..=Symbol::from('a'),NfaTest::id(5)));
        assert!(nfa[nfa.start_state_id].export);
        assert!(nfa[nfa.pattern_state_ids[0]].export);
        assert!(nfa[nfa.end_state_id].export);
    }

    #[test]
    fn nfa_pattern_always() {
        let nfa = pattern_always();

        assert!(nfa.alphabet.divisions().contains(&Symbol::from(0u64)));
        assert_eq!(nfa.states.len(),3);
        assert!(nfa.has_epsilon(nfa.start_state_id,nfa.pattern_state_ids[0]));
        assert!(nfa.has_epsilon(nfa.pattern_state_ids[0],nfa.end_state_id));
        assert!(nfa[nfa.start_state_id].export);
        assert!(nfa[nfa.pattern_state_ids[0]].export);
        assert!(nfa[nfa.end_state_id].export);
    }

    #[test]
    fn nfa_pattern_never() {
        let nfa = pattern_never();

        assert!(nfa.alphabet.divisions().contains(&Symbol::from(0u64)));
        assert_eq!(nfa.states.len(),4);
        assert!(nfa.has_epsilon(nfa.start_state_id,NfaTest::id(2)));
        assert!(nfa.has_epsilon(NfaTest::id(3),nfa.end_state_id));
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
        assert_eq!(nfa.states.len(),9);
        assert!(nfa.has_epsilon(nfa.start_state_id,NfaTest::id(2)));
        assert!(nfa.has_epsilon(nfa.start_state_id,NfaTest::id(4)));
        assert!(nfa.has_epsilon(nfa.pattern_state_ids[0],nfa.end_state_id));
        assert!(nfa.has_epsilon(NfaTest::id(4),NfaTest::id(5)));
        assert!(nfa.has_epsilon(NfaTest::id(6),NfaTest::id(7)));
        assert!(nfa.has_epsilon(nfa.pattern_state_ids[1],nfa.end_state_id));
        assert!(nfa.has_transition(Symbol::from('a')..=Symbol::from('a'),nfa.pattern_state_ids[0]));
        assert!(nfa.has_transition(Symbol::from('a')..=Symbol::from('a'),NfaTest::id(6)));
        assert!(nfa.has_transition(Symbol::from('b')..=Symbol::from('b'),nfa.pattern_state_ids[1]));
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
        assert_eq!(nfa.states.len(),26);
        assert!(nfa.has_transition(Symbol::from(' ')..=Symbol::from(' '),NfaTest::id(4)));
        assert!(nfa.has_transition(Symbol::from('a')..=Symbol::from('a'),NfaTest::id(6)));
        assert!(nfa.has_transition(Symbol::from('a')..=Symbol::from('a'),NfaTest::id(10)));
        assert!(nfa.has_transition(Symbol::from(' ')..=Symbol::from(' '),NfaTest::id(14)));
        assert!(nfa.has_transition(Symbol::from('b')..=Symbol::from('b'),NfaTest::id(16)));
        assert!(nfa.has_transition(Symbol::from('b')..=Symbol::from('b'),NfaTest::id(20)));
        assert!(nfa.has_transition(Symbol::eof()..=Symbol::eof(),nfa.pattern_state_ids[2]));
        assert!(nfa.has_transition(Symbol::null()..=Symbol::eof(),nfa.pattern_state_ids[3]));
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

        assert_eq!(nfa.states.len(),18);
        for (ix, _) in nfa.states.iter().enumerate() {
            let state_id = State::new(ix);
            if nfa.pattern_state_ids.contains(&state_id) {
                assert!(nfa.name(state_id).is_some());
                assert!(nfa.callback(state_id).is_some());
            } else {
                assert!(nfa.name(state_id).is_none());
                assert!(nfa.callback(state_id).is_none());
            }
        }
        assert_eq!(nfa.name(nfa.pattern_state_ids[0]),Some(&("rule_1".to_string())));
        assert_eq!(
            nfa.callback(nfa.pattern_state_ids[0]),
            Some(&("self.on_a_word(reader)".to_string()))
        );
        assert_eq!(nfa.name(nfa.pattern_state_ids[1]),Some(&("rule_2".to_string())));
        assert_eq!(
            nfa.callback(nfa.pattern_state_ids[1]),
            Some(&("self.on_b_word(reader)".to_string()))
        );
    }
}
