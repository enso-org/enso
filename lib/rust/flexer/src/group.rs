//! This module provides an API for grouping multiple flexer rules.

use crate::prelude::*;

use crate::automata::nfa::Nfa;
use crate::automata::{nfa, state};
use crate::automata::pattern::Pattern;
use crate::group::rule::Rule;

use itertools::Itertools;
use std::fmt::Display;
use crate::prelude::fmt::Formatter;
use crate::prelude::HashMap;

pub mod rule;



// ================
// === Registry ===
// ================

/// The group Registry is a container for [`Group`]s in the flexer implementation.
///
/// It allows groups to contain associations between themselves, and also implements useful
/// conversions for groups.
#[derive(Clone,Debug,Default)]
pub struct Registry {
    /// The groups defined for the lexer.
    groups:Vec<Group>,
}

impl Registry {
    /// Defines a new group of rules for the lexer with the specified `name` and `parent`.
    ///
    /// It returns the identifier of the newly-created group.
    pub fn define_group
    ( &mut self
    , name         : impl Into<String>
    , parent_index : Option<Identifier>
    ) -> Identifier {
        let id    = self.next_id();
        let group = Group::new(id,name.into(),parent_index);
        self.groups.push(group);
        id
    }

    /// Adds an existing `group` to the registry, updating and returning its identifier.
    pub fn add_group(&mut self, mut group:Group) -> Identifier {
        let new_id = self.next_id();
        group.id   = new_id;
        self.groups.push(group);
        new_id
    }

    /// Creates a rule that matches `pattern` for the group identified by `group_id`.
    ///
    /// Panics if `group_id` refers to a nonexistent group.
    pub fn create_rule(&mut self, group:Identifier, pattern:&Pattern, callback:impl AsRef<str>) {
        let group = self.group_mut(group);
        group.create_rule(pattern,callback.as_ref());
    }

    /// Associates the provided `rule` with the group identified by `group_id`.
    ///
    /// Panics if `group_id` refers to a nonexistent group.
    pub fn add_rule(&mut self, group:Identifier, rule:Rule) {
        let group = self.group_mut(group);
        group.add_rule(rule);
    }

    /// Collates the entire set of rules that are matchable when the lexer has the group identified
    /// by `group_id` as active.
    ///
    /// This set of rules includes the rules inherited from any parent groups.
    pub fn rules_for(&self, group:Identifier) -> Vec<&Rule> {
        let group_handle = self.group(group);
        let mut parent   = group_handle.parent_index.map(|p| self.group(p));
        let mut rules    = (&group_handle.rules).iter().collect_vec();
        while let Some(parent_group) = parent {
            if parent_group.id == group_handle.id {
                panic!("There should not be cycles in parent links for lexer groups.")
            }
            rules.extend((&parent_group.rules).iter());
            parent = parent_group.parent_index.map(|p| self.group(p));
        }
        rules
    }

    /// Obtains a reference to the group for the given `group_id`.
    ///
    /// As group identifiers can only be created by use of this `Registry`, this will always
    /// succeed.
    pub fn group(&self, group:Identifier) -> &Group {
        self.groups.get(group.0).expect("The group must exist.")
    }

    /// Obtains a mutable reference to the group for the given `group_id`.
    ///
    /// As group identifiers can only be created by use of this `Registry`, this will always
    /// succeed.
    pub fn group_mut(&mut self, group:Identifier) -> &mut Group {
        self.groups.get_mut(group.0).expect("The group should exist.")
    }

    /// Converts the group identified by `group_id` into an NFA.
    ///
    /// Returns `None` if the group does not exist, or if the conversion fails.
    pub fn to_nfa_from(&self, group_id:Identifier) -> AutomatonData {
        let group     = self.group(group_id);
        let mut nfa   = AutomatonData::default();
        let start     = nfa.automaton.start;
        nfa.add_public_state(start);
        let build     = |rule:&Rule| nfa.new_pattern(start,&rule.pattern);
        let rules     = self.rules_for(group.id);
        let callbacks = rules.iter().map(|r| r.callback.clone()).collect_vec();
        let states    = rules.into_iter().map(build).collect_vec();
        let end       = nfa.new_state_exported();
        for (ix,state) in states.into_iter().enumerate() {
            nfa.add_public_state(state);
            nfa.set_name(state,group.callback_name(ix));
            nfa.set_code(state,callbacks.get(ix).unwrap().clone());
            nfa.connect(state,end);
        }
        nfa.add_public_state(end);
        nfa
    }

    /// Generates the next group identifier for this registry.
    fn next_id(&self) -> Identifier {
        let val = self.groups.len();
        Identifier(val)
    }

    /// Get an immutable reference to the groups contained within the registry.
    pub fn all(&self) -> &Vec<Group> {
        &self.groups
    }
}


// ====================
// === AutomataData ===
// ====================

/// Storage for the generated automaton and auxiliary data required for code generation.
#[derive(Clone,Debug,Default,PartialEq,Eq)]
pub struct AutomatonData {
    /// The non-deterministic finite automaton implementing the group of rules it was generated
    /// from.
    automaton : Nfa,
    /// The states defined in the automaton.
    states : Vec<nfa::State>,
    /// The names of callbacks, where provided.
    transition_names : HashMap<nfa::State,String>,
    /// The code to execute on a callback, where available.
    callback_code : HashMap<nfa::State,String>,
}

impl AutomatonData {
    /// Set the name for the provided `state_id`.
    pub fn set_name(&mut self, state_id:nfa::State,name:impl Str) {
        self.transition_names.insert(state_id,name.into());
    }

    /// Set the callback code for the provided `state_id`.
    pub fn set_code(&mut self, state_id:nfa::State,code:impl Str) {
        self.callback_code.insert(state_id,code.into());
    }

    /// Add the provided `state` to the state registry.
    pub fn add_public_state(&mut self, state:nfa::State) {
        self.states.push(state);
    }

    /// Get the name for the provided `state_id`, if present.
    pub fn name(&self, state_id:nfa::State) -> Option<&str> {
        self.transition_names.get(&state_id).map(|s| s.as_str())
    }

    /// Get the callback code for the provided `state_id`, if present.
    pub fn code(&self, state_id:nfa::State) -> Option<&str> {
        self.callback_code.get(&state_id).map(|s| s.as_str())
    }

    /// Get a reference to the public states for this automaton.
    ///
    /// A public state is one that was explicitly defined by the user.
    pub fn public_states(&self) -> &Vec<nfa::State> {
        &self.states
    }

    /// Get a reference to the states for this automaton.
    pub fn states(&self) -> &Vec<state::Data> {
        &self.automaton.states()
    }

    /// Get a reference to the state names for this automaton.
    pub fn names(&self) -> &HashMap<nfa::State,String> {
        &self.transition_names
    }

    /// Get a reference to the callbacks for this automaton.
    pub fn callbacks(&self) -> &HashMap<nfa::State,String> {
        &self.callback_code
    }

    /// Get a reference to the automaton itself.
    pub fn automaton(&self) -> &Nfa {
        &self.automaton
    }

    /// Get the rule name for a the provided state.
    pub fn name_for_dfa_state(&self, sources:&[nfa::State]) -> Option<&str> {
        let mut result = None;
        for source in sources.iter() {
            let name = self.name(*source);
            if name.is_some() {
                result = name;
                break;
            }
        }
        result
    }
}

/// Errors that can occur when querying callbacks for a DFA state.
#[derive(Copy,Clone,Debug,Display,Eq,PartialEq)]
pub enum CallbackError {
    /// There are no available callbacks for this state.
    NoCallback,
    /// There is more than one callback available for this state.
    DuplicateCallbacks,
}


// === Trait Impls ===

impl Deref for AutomatonData {
    type Target = Nfa;

    fn deref(&self) -> &Self::Target {
        &self.automaton
    }
}

impl DerefMut for AutomatonData {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.automaton
    }
}



// ==================
// === Identifier ===
// ==================

/// An identifier for a group.
#[allow(missing_docs)]
#[derive(Copy,Clone,Debug,Default,Eq,PartialEq)]
pub struct Identifier(usize);


// === Trait Impls ===

impl From<usize> for Identifier {
    fn from(id:usize) -> Self {
        Identifier(id)
    }
}

impl From<&usize> for Identifier {
    fn from(id:&usize) -> Self {
        Identifier(*id)
    }
}

impl From<Identifier> for usize {
    fn from(value:Identifier) -> Self {
        value.0
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
    pub id:Identifier,
    /// A name for the group (useful in debugging).
    pub name:String,
    /// The parent group from which rules are inherited.
    ///
    /// It is ensured that the group is held mutably.
    pub parent_index:Option<Identifier>,
    /// A set of flexer rules.
    pub rules:Vec<Rule>,
    /// The names for the user-defined states.
    pub state_names:HashMap<usize,String>,
    /// The callback functions for the user-defined states.
    pub state_callbacks:HashMap<usize,String>,
}

impl Group {

    /// Creates a new group.
    pub fn new(id:Identifier, name:impl Into<String>, parent_index:Option<Identifier>) -> Self {
        let rules           = default();
        let state_names     = default();
        let state_callbacks = default();
        Group{id,name:name.into(),parent_index,rules,state_names,state_callbacks}
    }

    /// Adds a new rule to the current group.
    pub fn add_rule(&mut self, rule:Rule) {
        self.rules.push(rule)
    }

    /// Creates a new rule.
    pub fn create_rule(&mut self, pattern:&Pattern, code:&str) {
        let pattern_clone = pattern.clone();
        let rule          = Rule::new(pattern_clone,code);
        self.rules.push(rule)
    }

    /// The canonical name for a given rule.
    pub fn callback_name(&self, rule_ix:usize) -> String {
        format!("group_{}_rule_{}",self.id.0,rule_ix)
    }
}

// === Trait Impls ===

impl From<Group> for Registry {
    fn from(value:Group) -> Self {
        let mut registry = Registry::default();
        registry.add_group(value);
        registry
    }
}

impl Display for Group {
    fn fmt(&self, f:&mut Formatter<'_>) -> std::fmt::Result {
        write!(f,"Group {}",self.name)
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn group_create_rule() {
        let pattern   = Pattern::all_of("abcde");
        let mut group = Group::new(0.into(),"Test Name",None);
        group.create_rule(&pattern,"code");
        let rule = Rule::new(pattern,"code");
        assert!(group.rules.contains(&rule));
        assert_eq!(group.rules[0].callback,"code".to_string());
    }

    #[test]
    fn group_callback_name() {
        let pattern_1 = Pattern::all_of("abcde");
        let pattern_2 = Pattern::all_of("abcde");
        let mut group = Group::new(0.into(),"Test Name",None);
        group.create_rule(&pattern_1,"code");
        group.create_rule(&pattern_2,"code");
        assert_eq!(group.callback_name(0),"group_0_rule_0");
        assert_eq!(group.callback_name(1),"group_0_rule_1");
    }

    #[test]
    fn group_registry_define_group() {
        let mut registry = Registry::default();
        registry.define_group("TEST_GROUP",None);
        assert!(registry.all().iter().any(|g| g.name == *"TEST_GROUP"));
    }

    #[test]
    fn group_registry_create_rule() {
        let pattern      = Pattern::none_of("abcde");
        let mut registry = Registry::default();
        let group_1_id   = registry.define_group("GROUP_1",None);
        let group_2_id   = registry.define_group("GROUP_2",None);

        let group_1      = registry.group_mut(group_1_id);
        group_1.create_rule(&pattern,"rule_1");

        let group_2  = registry.group_mut(group_2_id);
        group_2.create_rule(&pattern,"rule_2");

        let rules_1 = registry.rules_for(group_1_id);
        let rules_2 = registry.rules_for(group_2_id);
        assert!(rules_1.iter().any(|r| **r == Rule::new(pattern.clone(),"rule_1")));
        assert!(rules_2.iter().any(|r| **r == Rule::new(pattern.clone(),"rule_2")));
    }

    #[test]
    fn group_registry_group_parents() {
        let pattern_1 = Pattern::char('a');
        let pattern_2 = Pattern::char('b');
        let pattern_3 = Pattern::char('c');

        let mut registry = Registry::default();
        let group_1_id = registry.define_group("GROUP_1", None);
        let group_2_id = registry.define_group("GROUP_2", Some(group_1_id));
        let group_3_id = registry.define_group("GROUP_3", Some(group_2_id));

        let group_1 = registry.group_mut(group_1_id);
        group_1.create_rule(&pattern_1, "rule_1");

        let group_2 = registry.group_mut(group_2_id);
        group_2.create_rule(&pattern_2, "rule_2");

        let group_3 = registry.group_mut(group_3_id);
        group_3.create_rule(&pattern_3, "rule_3");

        let rules = registry.rules_for(group_3_id);
        assert_eq!(rules.len(), 3);
        assert!(rules.iter().any(|r| **r == Rule::new(pattern_1.clone(),"rule_1")));
        assert!(rules.iter().any(|r| **r == Rule::new(pattern_2.clone(),"rule_2")));
        assert!(rules.iter().any(|r| **r == Rule::new(pattern_3.clone(),"rule_3")));
    }
}
