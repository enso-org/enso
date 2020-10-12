//! The structure for defining deterministic finite automata.

use crate::automata::alphabet;
use crate::automata::state;
use crate::data::matrix::Matrix;



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
pub struct DFA {
    /// A set of disjoint intervals over the allowable input alphabet.
    pub alphabet_segmentation:alphabet::Segmentation,
    /// The transition matrix for the DFA.
    ///
    /// It represents a function of type `(state, symbol) -> state`, returning the identifier for
    /// the new state.
    ///
    /// For example, the transition matrix for an automaton that accepts the language
    /// `{"A" | "B"}*"` would appear as follows, with `-` denoting
    /// [the invalid state](state::Identifier::INVALID). The leftmost column encodes the input
    /// state, while the topmost row encodes the input symbols.
    ///
    /// |   | A | B |
    /// |:-:|:-:|:-:|
    /// | 0 | 1 | - |
    /// | 1 | - | 0 |
    ///
    pub links:Matrix<state::Identifier>,
    /// A collection of callbacks for each state (indexable in order)
    pub callbacks:Vec<Option<RuleExecutable>>,
}

impl DFA {
    /// Check whether the DFA has a rule for the target state.
    ///
    /// This method should only be used in generated code, where its invariants are already checked.
    ///
    /// # Panics
    ///
    /// If no callback exists for `target_state`.
    pub fn has_rule_for(&self, target_state:state::Identifier) -> bool {
        self.callbacks.get(target_state.id).unwrap().is_some()
    }
}


// === Trait Impls ===

impl From<Vec<Vec<usize>>> for Matrix<state::Identifier> {
    fn from(input:Vec<Vec<usize>>) -> Self {
        let rows        = input.len();
        let columns     = if rows == 0 {0} else {input[0].len()};
        let mut matrix  = Self::new(rows,columns);
        for row in 0..rows {
            for column in 0..columns {
                matrix[(row,column)] = state::Identifier::from(input[row][column]);
            }
        }
        matrix
    }
}



// ================
// === Callback ===
// ================

/// The callback associated with an arbitrary state of a finite automaton.
///
/// It contains the rust code that is intended to be executed after encountering a
/// [`pattern`](super::pattern::Pattern) that causes the associated state transition. This pattern
/// is declared in [`Rule.pattern`](crate::group::rule::Rule::pattern).
#[derive(Clone,Debug,PartialEq,Eq)]
pub struct RuleExecutable {
    /// A description of the priority with which the callback is constructed during codegen.
    pub priority:usize,
    /// The rust code that will be executed when running this callback.
    pub code:String,
}

impl RuleExecutable {
    /// Creates a new rule executable with the provided `priority` and `code`.
    pub fn new(priority:usize, code_str:impl Into<String>) -> RuleExecutable {
        let code = code_str.into();
        RuleExecutable{priority,code}
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
pub mod tests {
    use crate::automata::state;

    use super::*;

    const INVALID:usize = state::Identifier::INVALID.id;

    /// DFA automata that accepts newline '\n'.
    pub fn newline() -> DFA {
        DFA {
            alphabet_segmentation:alphabet::Segmentation::from_divisions(&[10,11]),
            links:Matrix::from(vec![vec![INVALID,1,INVALID], vec![INVALID,INVALID,INVALID]]),
            callbacks:vec![
                None,
                Some(RuleExecutable {priority:2, code:"group_0_rule_0".into()}),
            ],
        }
    }

    /// DFA automata that accepts any letter a..=z.
    pub fn letter() -> DFA {
        DFA {
            alphabet_segmentation:alphabet::Segmentation::from_divisions(&[97,123]),
            links:Matrix::from(vec![vec![INVALID,1,INVALID], vec![INVALID,INVALID,INVALID]]),
            callbacks:vec![
                None,
                Some(RuleExecutable {priority:2, code:"group_0_rule_0".into()}),
            ],
        }
    }

    /// DFA automata that accepts any number of spaces ' '.
    pub fn spaces() -> DFA {
        DFA {
            alphabet_segmentation:alphabet::Segmentation::from_divisions(&[0,32,33]),
            links:Matrix::from(vec![
                vec![INVALID,1,INVALID],
                vec![INVALID,2,INVALID],
                vec![INVALID,2,INVALID],
            ]),
            callbacks:vec![
                None,
                Some(RuleExecutable {priority:3, code:"group_0_rule_0".into()}),
                Some(RuleExecutable {priority:3, code:"group_0_rule_0".into()}),
            ],
        }
    }

    /// DFA automata that accepts one letter a..=z or any many spaces.
    pub fn letter_and_spaces() -> DFA {
        DFA {
            alphabet_segmentation:alphabet::Segmentation::from_divisions(&[32,33,97,123]),
            links:Matrix::from(vec![
                vec![INVALID,      1,INVALID,      2,INVALID],
                vec![INVALID,      3,INVALID,INVALID,INVALID],
                vec![INVALID,INVALID,INVALID,INVALID,INVALID],
                vec![INVALID,      3,INVALID,INVALID,INVALID],
            ]),
            callbacks:vec![
                None,
                Some(RuleExecutable {priority:4, code:"group_0_rule_1".into()}),
                Some(RuleExecutable {priority:4, code:"group_0_rule_0".into()}),
                Some(RuleExecutable {priority:4, code:"group_0_rule_1".into()}),
            ],
        }
    }
}
