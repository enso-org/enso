#![deny(unconditional_recursion)]
#![feature(test)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]

//! This module exports the API for defining a simple lexer based on a deterministic finite state
//! automaton.
//!
//! These lexers are capable of lexing any regular grammar, with some extensions to allow working
//! with context sensitive (e.g. indentation-aware) syntax.

// TODO [AA] Logging https://github.com/enso-org/ide/blob/main/src/rust/ide/src/model/execution_context/synchronized.rs#L45

use crate::prelude::*;

use lazy_reader::LazyReader;

pub mod automata;
pub mod data;
pub mod group;

#[allow(missing_docs)]
pub mod prelude {
    pub use enso_prelude::*;
}



// =================
// === Constants ===
// =================

mod constants {
    /// The number of 'frames' to reserve in the state stack, aiming to avoid re-allocation in hot
    /// code paths.
    pub const STATE_STACK_RESERVATION:usize = 1024;
    /// The size of the output buffer (in tokens) to reserve, aiming to avoid re-allocation of the
    /// output buffer for common usage cases.
    pub const OUTPUT_BUFFER_RESERVATION:usize = 1024;
}



// ==============
// === Flexer ===
// ==============

/// The flexer is an engine for generating lexers.
///
/// Akin to flex and other lexer generators, it is given a definition as a series of rules from
/// which it then generates code for a highly optimised lexer implemented on top of a
/// [DFA](https://en.wikipedia.org/wiki/Deterministic_finite_automaton).
///
/// Lexers defined using the flexer work on a stack of _states_, where a state is represented by a
/// [`crate::group::Group`]. Being in a given state (represented below by the top of the
/// `state_stack`) means that the flexer can match a certain set of rules associated with that
/// state. The user may cause the lexer to transition between states by pushing and popping states
/// on the stack, thus allowing a much more flexible lexing engine than pure regular grammars.
#[derive(Clone,Debug)]
pub struct Flexer<Definition,Output,Reader> {
    /// The stack of states that are active during lexer execution.
    pub state_stack: NonEmptyVec<group::Identifier>,
    /// A reader for the input.
    pub reader: Reader,
    /// The result of the current stage of the DFA.
    pub status: StageStatus,
    /// The tokens that have been lexed.
    pub output: Vec<Output>,
    /// The text of the current match of the lexer.
    pub current_match: String,
    /// The definition of the user-provided state for the lexer.
    definition: Definition,
}

impl<Definition,Output,Reader> Flexer<Definition,Output,Reader>
where Definition:State, Reader:LazyReader {
    /// Create a new lexer instance.
    pub fn new(mut reader:Reader) -> Flexer<Definition,Output,Reader> {
        let status           = default();
        let mut output       = Vec::default();
        let definition       = Definition::new(&mut reader);
        let initial_state_id = definition.initial_state();
        let mut state_stack  = NonEmptyVec::singleton(initial_state_id);
        let current_match    = default();

        state_stack.reserve(constants::STATE_STACK_RESERVATION);
        output.reserve(constants::OUTPUT_BUFFER_RESERVATION);
        state_stack.push(initial_state_id);
        Flexer {state_stack,reader,status,output,definition,current_match}
    }
}

/// This block is things that are part of the lexer's interface and functionality.
impl<Definition,Reader,Output> Flexer<Definition,Output,Reader>
where Definition:State, Output:Clone {
    /// Get the lexer result.
    pub fn result(&mut self) -> &Vec<Output> {
        &self.output
    }

    /// Get the lexer's initial state.
    pub fn initial_state(&self) -> group::Identifier {
        self.definition.initial_state()
    }

    /// Get the state that the lexer is currently in.
    pub fn current_state(&self) -> group::Identifier {
        *self.state_stack.last()
    }

    /// Tell the lexer to enter the state described by `state`.
    pub fn push_state(&mut self, state:group::Identifier) {
        self.state_stack.push(state);
    }

    /// End the current state, returning the popped state identifier if one was ended.
    ///
    /// It will never end the initial state of the lexer.
    pub fn pop_state(&mut self) -> Option<group::Identifier> {
        self.state_stack.pop()
    }

    /// End states until the specified `state` is reached, leaving the lexer in `state`.
    ///
    /// If `state` does not exist on the lexer's stack, then the lexer will be left in the root
    /// state.
    pub fn pop_states_until(&mut self, state:group::Identifier) -> Vec<group::Identifier> {
        let non_opt_root_state_position =
            self.state_stack.iter().positions(|elem| *elem == state).last().unwrap_or(0);
        let range = (non_opt_root_state_position + 1)..self.state_stack.len();
        self.state_stack.drain(range).collect()
    }

    /// Check if the lexer is currently in the state described by `state`.
    pub fn is_in_state(&self, state:group::Identifier) -> bool {
        self.current_state() == state
    }
}

// === Trait Impls ===

impl<Definition,Output,Reader> Deref for Flexer<Definition,Output,Reader> {
    type Target = Definition;
    fn deref(&self) -> &Self::Target {
        &self.definition
    }
}

impl<Definition,Output,Reader> DerefMut for Flexer<Definition,Output,Reader> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.definition
    }
}



// ===================
// === StageStatus ===
// ===================

/// The result of executing a single step of the DFA.
#[derive(Clone,Copy,Debug,PartialEq)]
pub enum StageStatus {
    /// The initial state of a lexer stage.
    Initial,
    /// The stage exits successfully, having consumed a complete token.
    ExitSuccess,
    /// The stage exits unsuccessfully.
    ExitFail,
    /// A single step of the DFA has executed successfully.
    ExitFinished,
    /// The lexer should continue, transitioning to the included state.
    ContinueWith(group::Identifier)
}

impl StageStatus {
    /// Check if the lexer stage should continue.
    pub fn should_continue(&self) -> bool {
        self.continue_as().is_some()
    }

    /// Obtain the state to which the lexer should transition, iff the lexer should continue.
    pub fn continue_as(&self) -> Option<group::Identifier> {
        match self {
            StageStatus::Initial           => Some(group::Identifier::new(0)),
            StageStatus::ContinueWith(val) => Some(*val),
            _                              => None
        }
    }
}


// === Trait Impls ===

impl Default for StageStatus {
    fn default() -> Self {
        StageStatus::Initial
    }
}



// ==============
// === Result ===
// ==============

/// The result of executing the lexer on a given input.
#[allow(missing_docs)]
#[derive(Clone,Debug)]
pub struct Result<T> {
    pub kind: ResultKind,
    pub tokens: Vec<T>
}

impl<T> Result<T> {

    /// Create a new lexer result using the provided `kind` and `tokens`.
    pub fn new(kind:ResultKind,tokens:Vec<T>) -> Result<T> {
        Result {kind,tokens}
    }

    /// Create a new success result, with the provided `tokens`.
    pub fn success(tokens:Vec<T>) -> Result<T> {
        Result::new(ResultKind::Success,tokens)
    }

    /// Create a new partial lex result, with the provided `tokens`.
    pub fn partial(tokens:Vec<T>) -> Result<T> {
        Result::new(ResultKind::Partial,tokens)
    }

    /// Create a failure result, with the `tokens` it _did_ manage to consume.
    pub fn failure(tokens:Vec<T>) -> Result<T> {
        Result::new(ResultKind::Failure,tokens)
    }
}

/// The kind of lexer result.
#[derive(Copy,Clone,Debug)]
pub enum ResultKind {
    /// The lexer succeeded, returning the contained token stream.
    Success,
    /// The lexer succeeded on part of the input, returning the contained token stream.
    Partial,
    /// The lexer failed on the input, returning any tokens it _did_ manage to consume.
    Failure
}



// =============
// === State ===
// =============

/// Contains the state needed by any given lexer implementation.
pub trait State {
    /// Create a new instance of the lexer's state.
    fn new<Reader:LazyReader>(reader:&mut Reader) -> Self;
    /// Return the _initial_ lexing state.
    fn initial_state(&self) -> group::Identifier;
    /// Return a reference to the group registry for a given lexer.
    fn groups(&self) -> &group::Registry;
    /// Return a mutable reference to the group registry for a given lexer.
    fn groups_mut(&mut self) -> &mut group::Registry;
}



// ===============
// === Flexer ====
// ===============

// TODO [AA] Remove this once code generation is ready.
#[allow(missing_docs)]
pub trait FlexerTemp {
    /// Creates a new lexer.
    fn new() -> Self;

    /// Returns a code for a highly-optimised lexer implemented on top of a finite-state-automaton.
    fn generate_specialized_code(&mut self) -> String {
        String::from("#[derive(Debug)]\npub struct Lexer {}")
    }
}

