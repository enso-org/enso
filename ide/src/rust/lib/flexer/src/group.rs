//! This module exports API for grouping multiple rules (Rust callbacks with regex pattern) together.

use crate::automata::pattern::Pattern;
use crate::automata::nfa::NFA;
use crate::group::rule::Rule;

use itertools::Itertools;

pub mod rule;



// ===========
// == Group ==
// ===========

/// Struct that group rules together. It also inherits rules from parent group (if it has one).
/// Groups are the basic building block of flexer:
/// Flexer internally keeps a stack of groups, only one of them active at a time.
/// Each group contains set of regex patterns and callbacks (together called `Rule`).
/// Whenever a rule.pattern from active group is matched with part of input the associated
/// rule.callback is executed, which in turn may exit the current groupor enter a new one.
/// This allows us to nicely model a situation, where certain part of program (like a string literal)
/// should have very different parsing rules than other (for example body of function).
/// Note that the input is first matched with first added rule, then with the second etc.
/// Therefore, if two rules overlap, only the callback of the first added rule will be executed.
#[derive(Clone,Debug,Default)]
pub struct Group {
    /// Unique ID.
    pub id: usize,
    /// Custom name which is used for debugging.
    pub name: String,
    /// Parent which we inherit rules from.
    pub parent: Option<Box<Group>>,
    /// Set of regex patterns with associated callbacks.
    pub rules: Vec<Rule>,
}

impl Group {
    /// Adds new rule (regex pattern with associated callback) to group.
    pub fn add_rule(&mut self, rule:Rule) {
        self.rules.push(rule)
    }

    /// Returns rule builder for given pattern.
    /// TODO[jv] better describe it's purpose once we agree on correct API.
    pub fn rule(&mut self, pattern:Pattern) -> rule::Builder<impl FnMut(Rule) + '_> {
        rule::Builder{pattern,callback:move |rule| self.add_rule(rule)}
    }

    /// All rules including parent rules.
    pub fn rules(&self) -> Vec<&Rule> {
        let mut parent = &self.parent;
        let mut rules  = (&self.rules).iter().collect_vec();
        while let Some(state) = parent {
            rules.extend((&state.rules).iter());
            parent = &state.parent;
        }
        rules
    }

    /// Canonical name of given rule.
    fn callback_name(&self, rule_ix:usize) -> String {
        format!("group{}_rule{}",self.id,rule_ix)
    }
}

impl From<&Group> for NFA {
    /// Transforms Group to NFA.
    /// Algorithm is based on: https://www.youtube.com/watch?v=RYNN-tb9WxI
    fn from(group:&Group) -> Self {
        let mut nfa = NFA::default();
        let start   = nfa.new_state();
        let build   = |rule:&Rule| nfa.new_pattern(start,&rule.pattern);
        let states  = group.rules().into_iter().map(build).collect_vec();
        let end     = nfa.new_state();
        for (ix, state) in states.into_iter().enumerate() {
            nfa.states[state.id].name = Some(group.callback_name(ix));
            nfa.connect(state, end);
        }
        nfa
    }
}


// =============
// === Tests ===
// =============

#[cfg(test)]
pub mod tests {
    extern crate test;

    use crate::automata::dfa::DFA;
    use crate::automata::nfa;
    use crate::automata::nfa::NFA;
    use crate::automata::pattern::Pattern;
    use crate::group::Group;
    use crate::group::rule::Rule;

    use std::default::Default;
    use test::Bencher;

    fn newline() -> Group {
        let     pattern = Pattern::char('\n');
        let mut group   = Group::default();

        group.add_rule(Rule{pattern,callback:"".into()});

        group
    }

    fn letter() -> Group {
        let     pattern = Pattern::range('a'..='z');
        let mut group   = Group::default();

        group.add_rule(Rule{pattern,callback:"".into()});

        group
    }

    fn spaces() -> Group {
        let     pattern = Pattern::char(' ').many1();
        let mut group   = Group::default();

        group.add_rule(Rule{pattern,callback:"".into()});

        group
    }

    fn letter_and_spaces() -> Group {
        let     letter = Pattern::range('a'..='z');
        let     spaces = Pattern::char(' ').many1();
        let mut group  = Group::default();

        group.add_rule(Rule{pattern:letter,callback:"".into()});
        group.add_rule(Rule{pattern:spaces,callback:"".into()});

        group
    }

    fn hundred_rules() -> Group {
        let     pattern = Pattern::all("The quick brown fox jumps over the lazy dog!!");
        let mut group   = Group::default();

        for _ in 0..100 {
            group.add_rule(Rule{pattern:pattern.clone(),callback:"".into()})
        }
        group
    }

    #[test]
    fn test_to_nfa_newline() {
        assert_eq!(NFA::from(&newline()), nfa::tests::newline());
    }

    #[test]
    fn test_to_nfa_letter() {
        assert_eq!(NFA::from(&letter()), nfa::tests::letter());
    }

    #[test]
    fn test_to_nfa_spaces() {
        assert_eq!(NFA::from(&spaces()), nfa::tests::spaces());
    }

    #[test]
    fn test_to_nfa_letter_and_spaces() {
        assert_eq!(NFA::from(&letter_and_spaces()), nfa::tests::letter_and_spaces());
    }

    #[bench]
    fn bench_to_nfa_newline(bencher:&mut Bencher) {
        bencher.iter(|| NFA::from(&newline()))
    }

    #[bench]
    fn bench_to_nfa_letter(bencher:&mut Bencher) {
        bencher.iter(|| NFA::from(&letter()))
    }

    #[bench]
    fn bench_to_nfa_spaces(bencher:&mut Bencher) {
        bencher.iter(|| NFA::from(&spaces()))
    }

    #[bench]
    fn bench_to_nfa_letter_and_spaces(bencher:&mut Bencher) {
        bencher.iter(|| NFA::from(&letter_and_spaces()))
    }

    #[bench]
    fn bench_hundred_rules(bencher:&mut Bencher) {
        bencher.iter(|| DFA::from(&NFA::from(&hundred_rules())));
    }
}
