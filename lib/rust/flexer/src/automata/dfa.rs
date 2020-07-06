//! The structure for defining deterministic finite automata.

use crate::automata::alphabet::Alphabet;
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
///  │ 0 │──────▶│ 1 │──────▶│ 2 │──────▶│ 3 │
///  └───┘       └───┘       └───┘       └───┘
/// ```
#[derive(Clone,Debug,Default,PartialEq,Eq)]
pub struct DFA {
    /// A finite set of all valid input symbols.
    pub alphabet: Alphabet,
    /// The transition matrix for the DFA.
    ///
    /// It represents a function of type `(state, symbol) -> state`, returning the identifier for
    /// the new state.
    ///
    /// For example, the transition matrix for an automaton that accepts the language
    /// `{"A" | "B"}*"` would appear as follows, with `-` denoting
    /// [the invalid state](state::INVALID). The leftmost column encodes the input state, while the
    /// topmost row encodes the input symbols.
    ///
    /// |   | A | B |
    /// |:-:|:-:|:-:|
    /// | 0 | 1 | - |
    /// | 1 | - | 0 |
    ///
    pub links: Matrix<state::Identifier>,
    /// A collection of callbacks for each state (indexable in order)
    pub callbacks: Vec<Option<Callback>>,
}


// === Trait Impls ===

impl From<Vec<Vec<usize>>> for Matrix<state::Identifier> {
    fn from(input:Vec<Vec<usize>>) -> Self {
        let rows        = input.len();
        let columns     = if rows == 0 {0} else {input[0].len()};
        let mut matrix  = Self::new(rows,columns);
        for row in 0..rows {
            for column in 0..columns {
                matrix[(row,column)] = state::Identifier {id:input[row][column]};
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
pub struct Callback {
    // TODO [AA] Document this better after writing rust codegen.
    /// A description of the priority with which the callback is constructed during codegen.
    pub priority: usize,
    /// The rust code that will be executed when running this callback.
    pub name: String,
}



// =============
// === Tests ===
// =============

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::automata::state;

    const I:usize = state::INVALID.id;

    /// DFA automata that accepts newline '\n'.
    pub fn newline() -> DFA {
        DFA {
            alphabet: Alphabet::from(vec![10,11]),
            links: Matrix::from(vec![vec![I,1,I], vec![I,I,I]]),
            callbacks: vec![
                None,
                Some(Callback{priority:2,name:"group0_rule0".into()}),
            ],
        }
    }

    /// DFA automata that accepts any letter a..=z.
    pub fn letter() -> DFA {
        DFA {
            alphabet: Alphabet::from(vec![97,123]),
            links: Matrix::from(vec![vec![I,1,I], vec![I,I,I]]),
            callbacks: vec![
                None,
                Some(Callback{priority:2,name:"group0_rule0".into()}),
            ],
        }
    }

    /// DFA automata that accepts any number of spaces ' '.
    pub fn spaces() -> DFA {
        DFA {
            alphabet: Alphabet::from(vec![0,32,33]),
            links: Matrix::from(vec![
                vec![I,1,I],
                vec![I,2,I],
                vec![I,2,I],
            ]),
            callbacks: vec![
                None,
                Some(Callback{priority:3,name:"group0_rule0".into()}),
                Some(Callback{priority:3,name:"group0_rule0".into()}),
            ],
        }
    }

    /// DFA automata that accepts one letter a..=z or any many spaces.
    pub fn letter_and_spaces() -> DFA {
        DFA {
            alphabet: Alphabet::from(vec![32,33,97,123]),
            links: Matrix::from(vec![
                vec![I,1,I,2,I],
                vec![I,3,I,I,I],
                vec![I,I,I,I,I],
                vec![I,3,I,I,I],
            ]),
            callbacks: vec![
                None,
                Some(Callback{priority:4,name:"group0_rule1".into()}),
                Some(Callback{priority:4,name:"group0_rule0".into()}),
                Some(Callback{priority:4,name:"group0_rule1".into()}),
            ],
        }
    }
}
