//! This module provides an API for grouping multiple flexer rules.

use crate::automata::nfa::NFA;
use crate::automata::pattern::Pattern;
use crate::group::rule::Rule;

use itertools::Itertools;

pub mod rule;



// =====================
// === GroupRegistry ===
// =====================

/// The Group Registry is a container for [`Group`]s in the flexer implementation.
///
/// It allows groups to contain associations between themselves, and also implements useful
/// conversions for groups.
#[derive(Clone,Debug,Default)]
pub struct GroupRegistry {
    /// The groups defined for the lexer.
    groups: Vec<Group>
}

impl GroupRegistry {
    /// Defines a new group of rules for the lexer with the specified `name` and `parent`.
    ///
    /// It returns the identifier of the newly-created group.
    pub fn define_group(&mut self, name:String, parent_index:Option<usize>) -> usize {
        let id = self.next_id();
        let group = Group::new(id,name,parent_index);
        self.groups.push(group);
        id
    }

    /// Adds an existing `group` to the registry, updating and returning its identifier.
    pub fn add_group(&mut self, mut group:Group) -> usize {
        let new_id = self.next_id();
        group.id = new_id;
        self.groups.push(group);
        new_id
    }

    /// Creates a rule that matches `pattern` for the group identified by `group_id`.
    ///
    /// Panics if `group_id` refers to a nonexistent group.
    pub fn create_rule(&mut self, group_id:usize, pattern:&Pattern, callback:&str) {
        let err = format!("The provided group_id {} is invalid.",group_id);
        let group = self.group_from_id_mut(group_id).expect(&err);
        group.create_rule(pattern,callback);
    }

    /// Associates the provided `rule` with the group identified by `group_id`.
    ///
    /// Panics if `group_id` refers to a nonexistent group.
    pub fn add_rule(&mut self, group_id:usize, rule:Rule) {
        let err = format!("The provided group_id {} is invalid.",group_id);
        let group = self.group_from_id_mut(group_id).expect(&err);
        group.add_rule(rule);
    }

    /// Collates the entire set of rules that are matchable when the lexer has the group identified
    /// by `group_id` as active.
    ///
    /// This set of rules includes the rules inherited from any parent groups.
    pub fn rules_for(&self, group_id:usize) -> Option<Vec<&Rule>> {
        self.group_from_id(group_id).map(|group| {
            let mut parent = group.parent_index.and_then(|ix|self.group_from_id(ix));
            let mut rules = (&group.rules).iter().collect_vec();
            while let Some(parent_group) = parent {
                if parent_group.id == group.id {
                    panic!("There should not be cycles in parent links for lexer groups.")
                }
                rules.extend((&parent_group.rules).iter());
                parent = parent_group.parent_index.and_then(|ix|self.group_from_id(ix));
            }

            rules
        })
    }

    /// Obtains a reference to the group for the given `group_id`.
    pub fn group_from_id(&self, group_id:usize) -> Option<&Group> {
        self.groups.get(group_id)
    }

    /// Obtains a mutable reference to the group for the given `group_id`.
    pub fn group_from_id_mut(&mut self, group_id:usize) -> Option<&mut Group> {
        self.groups.get_mut(group_id)
    }

    /// Converts the group identified by `group_id` into an NFA.
    ///
    /// Returns `None` if the group does not exist, or if the conversion fails.
    pub fn to_nfa_from(&self, group_id:usize) -> Option<NFA> {
        let group = self.group_from_id(group_id);
        group.map(|group| {
            let mut nfa = NFA::default();
            let start   = nfa.new_state();
            let build   = |rule:&Rule| nfa.new_pattern(start,&rule.pattern);
            let rules   = self.rules_for(group_id).expect("Group exists.");
            let states  = rules.into_iter().map(build).collect_vec();
            let end     = nfa.new_state();
            for (ix,state) in states.into_iter().enumerate() {
                nfa.states[state.id].name = Some(group.callback_name(ix));
                nfa.connect(state,end);
            }
            nfa
        })
    }

    /// Generates the next group identifier for this registry.
    fn next_id(&self) -> usize {
        self.groups.len()
    }
}



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
    ///
    /// It is ensured that the group is held mutably.
    pub parent_index: Option<usize>,
    /// A set of flexer rules.
    pub rules: Vec<Rule>,
}

impl Group {

    /// Creates a new group.
    pub fn new(id:usize, name:String, parent_index:Option<usize>) -> Self {
        let rules = Vec::new();
        Group{id,name,parent_index,rules}
    }

    /// Adds a new rule to the current group.
    pub fn add_rule(&mut self, rule:Rule) {
        self.rules.push(rule)
    }

    /// Creates a new rule.
    pub fn create_rule(&mut self, pattern:&Pattern, code:&str) {
        let pattern_clone = pattern.clone();
        let rule = Rule::new(pattern_clone,code.into());
        self.rules.push(rule)
    }

    /// The canonical name for a given rule.
    pub fn callback_name(&self, rule_ix:usize) -> String {
        format!("group{}_rule{}",self.id,rule_ix)
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
pub mod tests {
    extern crate test;

    use crate::automata::nfa;
    use crate::automata::pattern::Pattern;
    use crate::group::{Group, GroupRegistry};
    use crate::group::rule::Rule;

    use std::default::Default;
    use test::Bencher;

    fn newline() -> GroupRegistry {
        let     pattern = Pattern::char('\n');
        let mut group   = Group::default();

        group.add_rule(Rule{pattern,callback:"".into()});

        let mut registry = GroupRegistry::default();
        registry.add_group(group);
        registry
    }

    fn letter() -> GroupRegistry {
        let     pattern = Pattern::range('a'..='z');
        let mut group   = Group::default();

        group.add_rule(Rule{pattern,callback:"".into()});

        let mut registry = GroupRegistry::default();
        registry.add_group(group);
        registry
    }

    fn spaces() -> GroupRegistry {
        let     pattern = Pattern::char(' ').many1();
        let mut group   = Group::default();

        group.add_rule(Rule{pattern,callback:"".into()});

        let mut registry = GroupRegistry::default();
        registry.add_group(group);
        registry
    }

    fn letter_and_spaces() -> GroupRegistry {
        let     letter = Pattern::range('a'..='z');
        let     spaces = Pattern::char(' ').many1();
        let mut group  = Group::default();

        group.add_rule(Rule{pattern:letter,callback:"".into()});
        group.add_rule(Rule{pattern:spaces,callback:"".into()});

        let mut registry = GroupRegistry::default();
        registry.add_group(group);
        registry
    }

    fn complex_rules(count:usize) -> GroupRegistry {
        let mut group   = Group::default();

        for ix in 0..count {
            let string  = ix.to_string();
            let all     = Pattern::all_of(&string);
            let any     = Pattern::any_of(&string);
            let none    = Pattern::none_of(&string);
            let pattern = Pattern::many(all >> any >> none);
            group.add_rule(Rule{pattern:pattern.clone(),callback:"".into()})
        }

        let mut registry = GroupRegistry::default();
        registry.add_group(group);
        registry
    }

    #[test]
    fn test_to_nfa_newline() {
        assert_eq!(newline().to_nfa_from(0).unwrap(),nfa::tests::newline());
    }

    #[test]
    fn test_to_nfa_letter() {
        assert_eq!(letter().to_nfa_from(0).unwrap(),nfa::tests::letter());
    }

    #[test]
    fn test_to_nfa_spaces() {
        assert_eq!(spaces().to_nfa_from(0).unwrap(),nfa::tests::spaces());
    }

    #[test]
    fn test_to_nfa_letter_and_spaces() {
        assert_eq!(letter_and_spaces().to_nfa_from(0).unwrap(),nfa::tests::letter_and_spaces());
    }

    #[bench]
    fn bench_to_nfa_newline(bencher:&mut Bencher) {
        bencher.iter(|| newline().to_nfa_from(0))
    }

    #[bench]
    fn bench_to_nfa_letter(bencher:&mut Bencher) {
        bencher.iter(|| letter().to_nfa_from(0))
    }

    #[bench]
    fn bench_to_nfa_spaces(bencher:&mut Bencher) {
        bencher.iter(|| spaces().to_nfa_from(0))
    }

    #[bench]
    fn bench_to_nfa_letter_and_spaces(bencher:&mut Bencher) {
        bencher.iter(|| letter_and_spaces().to_nfa_from(0))
    }

    #[bench]
    fn bench_ten_rules(bencher:&mut Bencher) {
        bencher.iter(|| complex_rules(10).to_nfa_from(0))
    }

    #[bench]
    fn bench_hundred_rules(bencher:&mut Bencher) {
        bencher.iter(|| complex_rules(100).to_nfa_from(0))
    }

    #[bench]
    fn bench_thousand_rules(bencher:&mut Bencher) {
        bencher.iter(|| complex_rules(1000).to_nfa_from(0))
    }
}
