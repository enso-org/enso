#![feature(test)]
#![feature(vec_drain_as_slice)]
#![deny(unconditional_recursion)]
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

use lazy_reader::LazyReader;
use crate::prelude::*;

pub mod automata;
pub mod data;
pub mod group;

// TODO [AA] Remove these
pub mod codegen_testing;
pub mod lexer_def_testing;

#[allow(missing_docs)]
pub mod prelude {
    pub use enso_prelude::*;
}



// ==============
// === Flexer ===
// ==============

/// The flexer is an engine for generating lexers.
///
/// Akin to flex and other lexer generators, it is given a definition as a series of rules from
/// which it then generates code for a highly optimised lexer implemented on top of a
/// [DFA](https://en.wikipedia.org/wiki/Deterministic_finite_automaton).
#[derive(Clone,Debug)]
pub struct Flexer<Definition,Output,Reader> where Reader:LazyReader {
    /// The stack of states that are active during lexer execution.
    state_stack: Vec<usize>,
    /// A reader for the input.
    reader: Reader,
    /// The current match of the lexer.
    current_match: String,
    /// The result of the current stage of the DFA.
    status: FlexerStageStatus,
    /// The tokens that have been lexed.
    tokens: Vec<Output>,
    /// The initial state of the defined lexer.
    initial_state: LexingState,
    /// The definition of the lexer.
    definition: Definition
}

impl<Definition,Output,Reader> Deref for Flexer<Definition,Output,Reader>
where Reader:LazyReader {
    type Target = Definition;
    fn deref(&self) -> &Self::Target {
        &self.definition
    }
}

impl<Definition,Output,Reader> DerefMut for Flexer<Definition,Output,Reader>
where Reader:LazyReader {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.definition
    }
}

impl<Definition,Output,Reader> Flexer<Definition,Output,Reader>
where Definition: FlexerState<Reader>,Reader:LazyReader {
    /// Creates a new lexer instance.
    ///
    /// Please note that the `reader` argument is currently hard-coded for testing purposes. This is
    /// not the intention for the eventual design.
    pub fn new(mut reader:Reader) -> Flexer<Definition,Output,Reader> {
        let mut state_stack = Vec::new();
        state_stack.reserve(1024);
        let current_match = String::from("");
        let status = FlexerStageStatus::Initial;
        let mut tokens = Vec::new();
        tokens.reserve(1024);
        let definition = Definition::new(&mut reader);
        let initial_state = LexingState::new("INITIAL", 0);
        state_stack.push(initial_state.id);

        Flexer {state_stack,reader,current_match,status,tokens,initial_state,definition}
    }
}

/// This block is things that are part of the lexer's interface and functionality.
impl<Definition,Reader,Output> Flexer<Definition,Output,Reader>
where Definition:FlexerState<Reader>,Reader:LazyReader,Output:Clone {
    /// Gets the lexer result.
    fn get_result(&mut self) -> Option<Vec<Output>> {
        Some(self.tokens.clone())
    }

    /// Gets the lexer's root state.
    pub fn root_state(&self) -> &LexingState {
        &self.initial_state
    }

    /// Gets the state that the lexer is currently in.
    pub fn current_state(&self) -> usize {
        *self.state_stack.last().expect("There should always be one state on the stack.")
    }

    /// Tells the lexer to enter the state described by `state`.
    pub fn begin_state(&mut self,state:usize) {
        self.state_stack.push(state);
    }

    /// Ends the current state, returning the popped state identifier if one was ended.
    pub fn end_state(&mut self) -> Option<usize> {
        if self.state_stack.len() > 1 {
            let ix = self.state_stack.pop().expect("There should be an item to pop.");
            Some(ix)
        } else {
            None
        }
    }

    /// Ends states until the specified `state` is reached, leaving the lexer in `state`.
    ///
    /// If `state` does not exist on the lexer's stack, then the lexer will be left in the root
    /// state.
    pub fn end_states_until(&mut self,state:usize) -> Vec<usize> {
        // Never drop the root state
        let position_of_target =
            self.state_stack.iter().positions(|elem| *elem == state).last().unwrap_or(0);
        let range = (position_of_target + 1)..self.state_stack.len();
        let ended_indices: Vec<usize> = self.state_stack.drain(range).collect();
        let mut ended_states = Vec::new();
        for ix in ended_indices {
            ended_states.push(ix);
        }
        ended_states
    }

    /// Checks if the lexer is currently in the state described by `state`.
    pub fn in_state(&mut self,state:usize) -> bool {
        self.current_state() == state
    }
}



// ===================
// === LexingState ===
// ===================

/// A container for state identifiers in the lexer.
#[derive(Clone,Debug,Default)]
pub struct LexingState {
    /// The name of the state, useful for debugging.
    pub name: String,
    /// The identifier of the state.
    id: usize,
}

impl LexingState {
    /// Creates a new state with the specified name and identifier.
    pub fn new(name:&str,id:usize) -> LexingState {
        let name = String::from(name);
        LexingState {name,id}
    }
}



// =========================
// === FlexerStageStatus ===
// =========================

/// The result of executing a single step of the DFA.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum FlexerStageStatus {
    /// The initial state of a lexer stage.
    Initial,
    /// The stage exits successfully, having consumed a complete token.
    ExitSuccess,
    /// The stage exits unsuccessfully.
    ExitFail,
    /// A single step of the DFA has executed successfully.
    ExitFinished,
    /// The lexer should continue, transitioning to the included state.
    ContinueWith(usize)
}

impl FlexerStageStatus {
    /// Checks if the lexer stage should continue.
    pub fn should_continue(&self) -> bool {
        self.continue_as().is_some()
    }

    /// Obtains the state to which the lexer should transition, iff the lexer should continue.
    pub fn continue_as(&self) -> Option<usize> {
        match self {
            FlexerStageStatus::Initial => Some(0),
            FlexerStageStatus::ContinueWith(val) => Some(*val),
            _ => None
        }
    }
}


// === Trait Impls ===

impl PartialEq for LexingState {
    fn eq(&self,other:&Self) -> bool {
        self.id == other.id
    }
}



// ====================
// === FlexerResult ===
// ====================

/// The result of executing the lexer on a given input.
#[derive(Clone,Debug,Eq,PartialEq)]
pub enum FlexerResult<T> {
    /// The lexer succeeded, returning the contained token stream.
    Success(Vec<T>),
    /// The lexer succeeded on part of the input, returning the contained token stream.
    Partial(Vec<T>),
    /// The lexer failed on the input, returning any tokens it _did_ manage to consume.
    Failure(Option<Vec<T>>)
}



// ===================
// === FlexerState ===
// ===================

/// Contains the state needed by any given lexer implementation.
pub trait FlexerState<Reader:LazyReader> {
    /// Creates a new instance of the lexer's state.
    fn new(reader:&mut Reader) -> Self;
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

