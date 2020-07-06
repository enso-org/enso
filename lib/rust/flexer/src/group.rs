//! This module provides an API for grouping multiple flexer rules.

use crate::automata::pattern::Pattern;
use crate::automata::nfa::NFA;
use crate::group::rule::Rule;

use itertools::Itertools;

pub mod rule;



// ===========
// == Group ==
// ===========

/// A group is a structure for associating multiple rules with each other, and is the basic building
/// block of the flexer.
///
/// A group consists of the following:
///
/// - A set of [`Rule`s](Rule), each containing a regex pattern and associated callback.
/// - Inherited rules from a parent group, if such a group exists.
///
/// Internally, the flexer maintains a stack of groups, where only one group can be active at any
/// given time. Rules are matched _in order_, and hence overlaps are handled by the order in which
/// the rules are matched, with the first callback being triggered.
///
/// Whenever a [`rule.pattern`](Rule::pattern) from the active group is matched against part of the
/// input, the associated [`rule.callback`](Rule::callback) is executed. This callback may exit the
/// current group or even enter a new one. As a result, groups allow us to elegantly model a
/// situation where certain parts of a program (e.g. within a string literal) have very different
/// lexing rules than other portions of a program (e.g. the body of a function).
#[derive(Clone,Debug,Default)]
pub struct Group {
    /// A unique identifier for the group.
    pub id: usize,
    /// A name for the group (useful in debugging).
    pub name: String,
    /// The parent group from which rules are inherited.
    pub parent: Option<Box<Group>>,
    /// A set of flexer rules.
    pub rules: Vec<Rule>,
}

impl Group {

    /// Adds a new rule to the current group.
    pub fn add_rule(&mut self, rule:Rule) {
        self.rules.push(rule)
    }

    /// Returns a rule builder for the given pattern.
    pub fn rule(&mut self, pattern:Pattern) -> rule::Builder<impl FnMut(Rule) + '_> {
        rule::Builder{pattern, callback:move |rule| self.add_rule(rule)}
    }

    /// The canonical name for a given rule.
    fn callback_name(&self, rule_ix:usize) -> String {
        format!("group{}_rule{}", self.id, rule_ix)
    }
}


// === Getters ===

impl Group {

    /// The full set of rules, including parent rules.
    pub fn rules(&self) -> Vec<&Rule> {
        let mut parent = &self.parent;
        let mut rules  = (&self.rules).iter().collect_vec();
        while let Some(state) = parent {
            rules.extend((&state.rules).iter());
            parent = &state.parent;
        }
        rules
    }
}


// === Trait Impls ===

impl From<&Group> for NFA {
    /// Transforms the input group into an NFA.
    ///
    /// The algorithm is based on this algorithm for
    /// [converting a regular expression to an NFA](https://www.youtube.com/watch?v=RYNN-tb9WxI).
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
