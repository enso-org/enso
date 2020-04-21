//! Exports the structure for Deterministic Finite Automata.

use crate::automata::alphabet::Alphabet;
use crate::automata::state;
use crate::data::matrix::Matrix;



// =====================================
// === Deterministic Finite Automata ===
// =====================================

/// Function callback for an arbitrary state of finite automata.
/// It contains name of Rust procedure that is meant to be executed after encountering a pattern
/// (declared in `group::Rule.pattern`).
#[derive(Clone,Debug,PartialEq,Eq)]
pub struct Callback {
    /// TODO[jv] Write better explanation after implementing rust code generation.
    /// Priority is used during rust code generation.
    pub priority: usize,
    /// Name of Rust method that will be called when executing this callback.
    pub name: String,
}

/// DFA automata with a set of symbols, states and transitions.
/// Deterministic Finite Automata is a finite-state machine that accepts or rejects a given sequence
/// of symbols, by running through a state sequence uniquely determined by the input symbol sequence.
///   ___              ___              ___              ___
///  | 0 | -- 'D' --> | 1 | -- 'F' --> | 2 | -- 'A' --> | 3 |
///   ‾‾‾              ‾‾‾              ‾‾‾              ‾‾‾
/// More information at: https://en.wikipedia.org/wiki/Deterministic_finite_automaton

#[derive(Clone,Debug,Default,PartialEq,Eq)]
pub struct DFA {
    /// Finite set of all valid input symbols.
    pub alphabet: Alphabet,
    /// Transition matrix of deterministic finite state automata.
    /// It contains next state for each pair of state and input symbol - (state,symbol) => new state.
    /// For example, a transition matrix for automata that accepts string "ABABAB...." would look
    /// like this:
    ///  states
    /// |       | A | B | <- symbols
    /// | 0     | 1 | - |
    /// | 1     | - | 0 |
    ///  Where `-` denotes `state::INVALID`.
    pub links: Matrix<state::Id>,
    /// Stores callback for each state (if it has one).
    pub callbacks: Vec<Option<Callback>>,
}

impl From<Vec<Vec<usize>>> for Matrix<state::Id> {
    fn from(input:Vec<Vec<usize>>) -> Self {
        let rows        = input.len();
        let columns     = if rows == 0 {0} else {input[0].len()};
        let mut matrix  = Self::new(rows,columns);
        for row in 0..rows {
            for column in 0..columns {
                matrix[(row,column)] = state::Id{id:input[row][column]};
            }
        }
        matrix
    }
}



// ===========
// == Tests ==
// ===========

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
