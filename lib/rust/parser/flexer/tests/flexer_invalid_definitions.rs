//! This file contains tests for the user-facing error-handling logic in the flexer code generator.
//!
//! This file includes quite a bit of duplicated code, but this is known and intentional as it
//! allows for increased clarity in the testing.

// === Non-Standard Linter Configuration ===
#![allow(missing_docs)]
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::blacklisted_name)] // `foo` is fine here.
#![allow(clippy::new_without_default)] // No need for boilerplate in throwaway test code.

use enso_flexer::*;

use crate::prelude::logger::AnyLogger;
use crate::prelude::logger::Disabled;
use crate::prelude::reader::BookmarkManager;
use crate::prelude::ReaderOps;
use enso_flexer::automata::pattern::Pattern;
use enso_flexer::generate;
use enso_flexer::group;
use enso_flexer::group::Identifier;
use enso_flexer::group::Registry;
use enso_flexer::prelude::*;
use enso_flexer::Flexer;
use enso_flexer::State;



// ====================
// === Type Aliases ===
// ====================

type Logger = Disabled;



// ====================
// === Shared Setup ===
// ====================

/// A token type for these lexers.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Token {
    Foo,
    Bar,
}

/// An output type for these lexers.
#[allow(missing_docs)]
#[derive(Clone, Debug, Default, PartialEq)]
pub struct Output {
    tokens: Vec<Token>,
}

/// A testing lexer state.
pub struct LexerState {
    lexer_states:  group::Registry,
    initial_state: group::Identifier,
}
impl enso_flexer::State for LexerState {
    fn new(_logger: &impl AnyLogger) -> Self {
        let mut lexer_states = group::Registry::default();
        let initial_state = lexer_states.define_group("ROOT", None);
        LexerState { lexer_states, initial_state }
    }

    fn initial_state(&self) -> Identifier {
        self.initial_state
    }

    fn groups(&self) -> &Registry {
        &self.lexer_states
    }

    fn groups_mut(&mut self) -> &mut Registry {
        &mut self.lexer_states
    }

    fn bookmarks(&self) -> &BookmarkManager {
        unimplemented!()
    }

    fn bookmarks_mut(&mut self) -> &mut BookmarkManager {
        unimplemented!()
    }

    fn specialize(&self) -> Result<String, GenError> {
        // Note [Naming "Lexer"]
        generate::specialize(self, "Lexer", "Output")
    }
}

/* Note [Naming "Lexer"]
 * ~~~~~~~~~~~~~~~~~~~~~
 * In general, the name passed to `specialize` should match that of your lexer definition.
 * However here, as we never compile the code, we set it to a generic constant that is a valid
 * rust identifier so as to reduce testing boilerplate.
 */



// ====================
// === Definition 1 ===
// ====================

pub struct Lexer1 {
    lexer: Flexer<LexerState, Output, Logger>,
}

impl Deref for Lexer1 {
    type Target = Flexer<LexerState, Output, Logger>;
    fn deref(&self) -> &Self::Target {
        &self.lexer
    }
}

impl DerefMut for Lexer1 {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.lexer
    }
}

impl Lexer1 {
    pub fn new() -> Lexer1 {
        let logger = Logger::new("Lexer1");
        let lexer = Flexer::new(logger);
        Lexer1 { lexer }
    }

    pub fn my_test_fun<R: ReaderOps>(&mut self, _reader: &mut R) {
        unimplemented!()
    }
}

impl enso_flexer::Definition for Lexer1 {
    fn define() -> Self {
        let mut lexer = Self::new();

        let foo = Pattern::all_of("foo");

        let root_group_id = lexer.initial_state();
        let root_group = lexer.groups_mut().group_mut(root_group_id);
        root_group.create_rule(&foo, "ETERNAL SCREAMING");

        lexer
    }

    fn groups(&self) -> &Registry {
        self.lexer.groups()
    }

    fn set_up(&mut self) {
        unimplemented!()
    }

    fn tear_down(&mut self) {
        unimplemented!()
    }
}

#[test]
fn test_bad_rule_expression() {
    let lexer = Lexer1::define();
    let result = lexer.specialize();
    assert!(result.is_err());
    let message = result.unwrap_err().to_string();
    assert_eq!(message, "`ETERNAL SCREAMING` is not a valid rust expression.");
}


// ====================
// === Definition 2 ===
// ====================

pub struct Lexer2 {
    lexer: Flexer<LexerState, Output, Logger>,
}

impl Deref for Lexer2 {
    type Target = Flexer<LexerState, Output, Logger>;
    fn deref(&self) -> &Self::Target {
        &self.lexer
    }
}

impl DerefMut for Lexer2 {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.lexer
    }
}

impl Lexer2 {
    pub fn new() -> Lexer2 {
        let logger = Logger::new("Lexer2");
        let lexer = Flexer::new(logger);
        Lexer2 { lexer }
    }

    pub fn my_test_fun<R: ReaderOps>(&mut self, _reader: &mut R) {
        unimplemented!()
    }
}

impl enso_flexer::Definition for Lexer2 {
    fn define() -> Self {
        let mut lexer = Self::new();

        let foo = Pattern::all_of("foo");

        let root_group_id = lexer.initial_state();
        let root_group = lexer.groups_mut().group_mut(root_group_id);
        root_group.create_rule(&foo, "self.test_function_no_reader()");

        lexer
    }

    fn groups(&self) -> &Registry {
        self.lexer.groups()
    }

    fn set_up(&mut self) {
        unimplemented!()
    }

    fn tear_down(&mut self) {
        unimplemented!()
    }
}

#[test]
pub fn test_no_reader_arg() {
    let lexer = Lexer2::define();
    let result = lexer.specialize();
    let expected_message =
        "Bad argument to a callback function. It must take a single argument `reader`.";
    assert!(result.is_err());
    let message = result.unwrap_err().to_string();
    assert_eq!(message, expected_message);
}



// ====================
// === Definition 3 ===
// ====================

pub struct Lexer3 {
    lexer: Flexer<LexerState1, Output, Logger>,
}

impl Deref for Lexer3 {
    type Target = Flexer<LexerState1, Output, Logger>;
    fn deref(&self) -> &Self::Target {
        &self.lexer
    }
}

impl DerefMut for Lexer3 {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.lexer
    }
}

impl Lexer3 {
    pub fn new() -> Lexer3 {
        let logger = Logger::new("Lexer3");
        let lexer = Flexer::new(logger);
        Lexer3 { lexer }
    }

    pub fn my_test_fun<R: ReaderOps>(&mut self, _reader: &mut R) {
        unimplemented!()
    }
}

impl enso_flexer::Definition for Lexer3 {
    fn define() -> Self {
        let mut lexer = Self::new();

        let foo = Pattern::all_of("foo");

        let root_group_id = lexer.initial_state();
        let root_group = lexer.groups_mut().group_mut(root_group_id);
        root_group.create_rule(&foo, "self.test_function_reader(reader)");

        lexer
    }

    fn groups(&self) -> &Registry {
        self.lexer.groups()
    }

    fn set_up(&mut self) {
        unimplemented!()
    }

    fn tear_down(&mut self) {
        unimplemented!()
    }
}

pub struct LexerState1 {
    lexer_states:  group::Registry,
    initial_state: group::Identifier,
}
impl enso_flexer::State for LexerState1 {
    fn new(_logger: &impl AnyLogger) -> Self {
        let mut lexer_states = group::Registry::default();
        let initial_state = lexer_states.define_group("ROOT", None);
        LexerState1 { lexer_states, initial_state }
    }

    fn initial_state(&self) -> Identifier {
        self.initial_state
    }

    fn groups(&self) -> &Registry {
        &self.lexer_states
    }

    fn groups_mut(&mut self) -> &mut Registry {
        &mut self.lexer_states
    }

    fn bookmarks(&self) -> &BookmarkManager {
        unimplemented!()
    }

    fn bookmarks_mut(&mut self) -> &mut BookmarkManager {
        unimplemented!()
    }

    fn specialize(&self) -> Result<String, GenError> {
        generate::specialize(self, "Bad Lexer Name", "Output")
    }
}

#[test]
pub fn test_bad_state_name() {
    let lexer = Lexer3::define();
    let result = lexer.specialize();
    assert!(result.is_err());
    let message = result.unwrap_err().to_string();
    assert_eq!(message, "`Bad Lexer Name` is not a valid rust identifier.");
}



// ====================
// === Definition 4 ===
// ====================

pub struct Lexer4 {
    lexer: Flexer<LexerState2, Output, Logger>,
}

impl Deref for Lexer4 {
    type Target = Flexer<LexerState2, Output, Logger>;
    fn deref(&self) -> &Self::Target {
        &self.lexer
    }
}

impl DerefMut for Lexer4 {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.lexer
    }
}

impl Lexer4 {
    pub fn new() -> Lexer4 {
        let logger = Logger::new("Lexer4");
        let lexer = Flexer::new(logger);
        Lexer4 { lexer }
    }

    pub fn my_test_fun<R: ReaderOps>(&mut self, _reader: &mut R) {
        unimplemented!()
    }
}

impl enso_flexer::Definition for Lexer4 {
    fn define() -> Self {
        let mut lexer = Self::new();

        let foo = Pattern::all_of("foo");

        let root_group_id = lexer.initial_state();
        let root_group = lexer.groups_mut().group_mut(root_group_id);
        root_group.create_rule(&foo, "self.test_function_reader(reader)");

        lexer
    }

    fn groups(&self) -> &Registry {
        self.lexer.groups()
    }

    fn set_up(&mut self) {
        unimplemented!()
    }

    fn tear_down(&mut self) {
        unimplemented!()
    }
}

pub struct LexerState2 {
    lexer_states:  group::Registry,
    initial_state: group::Identifier,
}
impl enso_flexer::State for LexerState2 {
    fn new(_logger: &impl AnyLogger) -> Self {
        let mut lexer_states = group::Registry::default();
        let initial_state = lexer_states.define_group("ROOT", None);
        LexerState2 { lexer_states, initial_state }
    }

    fn initial_state(&self) -> Identifier {
        self.initial_state
    }

    fn groups(&self) -> &Registry {
        &self.lexer_states
    }

    fn groups_mut(&mut self) -> &mut Registry {
        &mut self.lexer_states
    }

    fn bookmarks(&self) -> &BookmarkManager {
        unimplemented!()
    }

    fn bookmarks_mut(&mut self) -> &mut BookmarkManager {
        unimplemented!()
    }

    fn specialize(&self) -> Result<String, GenError> {
        generate::specialize(self, "Lexer4", "Bad output name")
    }
}

#[test]
pub fn test_bad_output_name() {
    let lexer = Lexer4::define();
    let result = lexer.specialize();
    assert!(result.is_err());
    let message = result.unwrap_err().to_string();
    assert_eq!(message, "`Bad output name` is not a valid rust path.");
}
