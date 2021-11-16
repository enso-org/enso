#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]

//! This file contains the code defining a lexer for the following small language. Due to the way in
//! which the code-generation from the flexer is used, it has to be defined in a separate crate from
//! the site at which it's used. For the actual tests of this code, please see
//! `flexer-testing/generation`.
//!
//! The language here is being defined as follows:
//!
//! a-word      = 'a'+;
//! b-word      = 'b'+;
//! word        = a-word | b-word;
//! space       = ' ';
//! spaced-word = space, word;
//! language    = word, spaced-word*;
//!
//! Please note that there is a fair amount of duplicated code between this test and the
//! `lexer_generated_api_test` file. This is to present the full view of what each portion of the
//! process looks like.

use enso_flexer::prelude::*;
use enso_flexer::*;

use enso_flexer::automata::pattern::Pattern;
use enso_flexer::group::Registry;
use enso_flexer::prelude::logger::Disabled;
use enso_flexer::prelude::reader::BookmarkManager;



// ====================
// === Type Aliases ===
// ====================

type Logger = Disabled;



// ===========
// === AST ===
// ===========

/// A very simple AST, sufficient for the simple language being defined.
#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    /// A word from the input, consisting of a sequence of all `a` or all `b`.
    Word(String),
    /// A token that the lexer is unable to recognise.
    Unrecognized(String),
}
impl Token {
    /// Construct a new word token.
    pub fn word(name: impl Into<String>) -> Token {
        Token::Word(name.into())
    }

    /// Construct a new unrecognized token.
    pub fn unrecognized(name: impl Into<String>) -> Token {
        Token::Unrecognized(name.into())
    }
}

/// A representation of a stream of tokens.
#[allow(missing_docs)]
#[derive(Clone, Debug, Default, PartialEq)]
pub struct TokenStream {
    tokens: Vec<Token>,
}

impl TokenStream {
    /// Append the provided token to the token stream.
    pub fn push(&mut self, token: Token) {
        self.tokens.push(token);
    }
}


// === Trait Impls ===

impl From<Vec<Token>> for TokenStream {
    fn from(tokens: Vec<Token>) -> Self {
        TokenStream { tokens }
    }
}



// ==================
// === Test Lexer ===
// ==================

/// The definition of a test lexer for the above-described language.
#[derive(Debug)]
pub struct TestLexer {
    lexer: Flexer<TestState, TokenStream, Logger>,
}

impl Deref for TestLexer {
    type Target = Flexer<TestState, TokenStream, Logger>;
    fn deref(&self) -> &Self::Target {
        &self.lexer
    }
}

impl DerefMut for TestLexer {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.lexer
    }
}

impl TestLexer {
    /// Creates a new instance of this lexer.
    pub fn new() -> Self {
        let logger = Logger::new("TestLexer");
        let lexer = Flexer::new(logger);
        TestLexer { lexer }
    }
}

/// Rules for the root state.
#[allow(dead_code, missing_docs)]
impl TestLexer {
    fn on_first_word<R: ReaderOps>(&mut self, _reader: &mut R) {
        let str = self.current_match.clone();
        let ast = Token::Word(str);
        self.output.push(ast);
        let id = self.seen_first_word_state;
        self.push_state(id);
    }

    fn on_err_suffix_first_word<R: ReaderOps>(&mut self, _reader: &mut R) {
        let ast = Token::Unrecognized(self.current_match.clone());
        self.output.push(ast);
    }

    fn on_no_err_suffix_first_word<R: ReaderOps>(&mut self, _reader: &mut R) {}

    fn rules_in_root(lexer: &mut TestLexer) {
        let a_word = Pattern::char('a').many1();
        let b_word = Pattern::char('b').many1();
        let any = Pattern::any();
        let end = Pattern::eof();

        let root_group_id = lexer.initial_state;
        let root_group = lexer.groups_mut().group_mut(root_group_id);

        root_group.create_rule(&a_word, "self.on_first_word(reader)");
        root_group.create_rule(&b_word, "self.on_first_word(reader)");
        root_group.create_rule(&end, "self.on_no_err_suffix_first_word(reader)");
        root_group.create_rule(&any, "self.on_err_suffix_first_word(reader)");
    }
}

/// Rules for the "seen first word" state.
#[allow(dead_code, missing_docs)]
impl TestLexer {
    fn on_spaced_word<R: ReaderOps>(&mut self, _reader: &mut R, _test_arg: bool) {
        let str = self.current_match.clone();
        let ast = Token::Word(String::from(str.trim()));
        self.output.push(ast);
    }

    fn on_err_suffix<R: ReaderOps>(&mut self, reader: &mut R) {
        self.on_err_suffix_first_word(reader);
        self.pop_state();
    }

    fn on_no_err_suffix<R: ReaderOps>(&mut self, reader: &mut R) {
        self.on_no_err_suffix_first_word(reader);
        self.pop_state();
    }

    fn rules_in_seen_first_word(lexer: &mut TestLexer) {
        let a_word = Pattern::char('a').many1();
        let b_word = Pattern::char('b').many1();
        let space = Pattern::char(' ');
        let spaced_a_word = &space >> &a_word;
        let spaced_b_word = &space >> &b_word;
        let any = Pattern::any();
        let end = Pattern::eof();

        let seen_first_word_group_id = lexer.seen_first_word_state;
        let seen_first_word_group = lexer.groups_mut().group_mut(seen_first_word_group_id);

        seen_first_word_group.create_rule(&spaced_a_word, "self.on_spaced_word(reader,true)");
        seen_first_word_group.create_rule(&spaced_b_word, "self.on_spaced_word(reader,false)");
        seen_first_word_group.create_rule(&end, "self.on_no_err_suffix(reader)");
        seen_first_word_group.create_rule(&any, "self.on_err_suffix(reader)");
    }
}


// === Trait Impls ===

impl enso_flexer::Definition for TestLexer {
    fn define() -> Self {
        let mut lexer = TestLexer::new();

        TestLexer::rules_in_seen_first_word(&mut lexer);
        TestLexer::rules_in_root(&mut lexer);

        lexer
    }

    fn groups(&self) -> &Registry {
        self.lexer.groups()
    }

    fn set_up(&mut self) {}

    fn tear_down(&mut self) {}
}

impl Default for TestLexer {
    fn default() -> Self {
        TestLexer::new()
    }
}



// ===================
// === Lexer State ===
// ===================

/// The stateful components of the test lexer.
#[derive(Debug)]
pub struct TestState {
    /// The registry for groups in the lexer.
    lexer_states:          group::Registry,
    /// The initial state of the lexer.
    initial_state:         group::Identifier,
    /// The state entered when the first word has been seen.
    seen_first_word_state: group::Identifier,
    /// The bookmarks for this lexer.
    bookmarks:             BookmarkManager,
}


// === Trait Impls ===

impl enso_flexer::State for TestState {
    fn new(_logger: &impl AnyLogger) -> Self {
        let mut lexer_states = group::Registry::default();
        let initial_state = lexer_states.define_group("ROOT", None);
        let seen_first_word_state = lexer_states.define_group("SEEN FIRST WORD", None);
        let bookmarks = BookmarkManager::new();
        Self { lexer_states, initial_state, seen_first_word_state, bookmarks }
    }

    fn initial_state(&self) -> group::Identifier {
        self.initial_state
    }

    fn groups(&self) -> &group::Registry {
        &self.lexer_states
    }

    fn groups_mut(&mut self) -> &mut group::Registry {
        &mut self.lexer_states
    }

    fn bookmarks(&self) -> &BookmarkManager {
        &self.bookmarks
    }

    fn bookmarks_mut(&mut self) -> &mut BookmarkManager {
        &mut self.bookmarks
    }

    fn specialize(&self) -> Result<String, GenError> {
        generate::specialize(self, "TestLexer", "TokenStream")
    }
}
