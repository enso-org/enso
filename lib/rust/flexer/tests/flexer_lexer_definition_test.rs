//! This file contains tests for the intended definition code of a lexer using the flexer, based on
//! the following small language.
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

use flexer::prelude::*;
use flexer::group;
use lazy_reader::{BookmarkId, LazyReader, Reader};
use flexer::{State, Flexer};
use lazy_reader::decoder::DecoderUTF8;
use flexer::automata::pattern::Pattern;


// ===========
// === AST ===
// ===========

/// A very simple AST, sufficient for the simple lexer being defined.
#[derive(Clone,Debug,PartialEq)]
pub enum AST {
    /// A word from the input, consisting of a sequence of all `a` or all `b`.
    Word(String),
    /// A token that the lexer is unable to recognise.
    Unrecognised(String)
}



// ==================
// === Test Lexer ===
// ==================

/// The definition of a test lexer for the above-described language.
#[derive(Debug)]
pub struct TestLexer<Reader:LazyReader> {
    lexer: Flexer<TestState,AST,Reader>
}

impl<Reader:LazyReader> Deref for TestLexer<Reader> {
    type Target = Flexer<TestState,AST,Reader>;
    fn deref(&self) -> &Self::Target {
        &self.lexer
    }
}

impl<Reader:LazyReader> DerefMut for TestLexer<Reader> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.lexer
    }
}

impl<Reader:LazyReader> TestLexer<Reader> {
    /// Creates a new instance of this lexer.
    pub fn new(reader:Reader) -> Self {
        let lexer = Flexer::new(reader);
        TestLexer{lexer}
    }
}

/// Implementations of functionality used by the lexer.
///
/// These functions are provided by the user, by hand.
#[allow(missing_docs)]
impl<Reader:LazyReader> TestLexer<Reader> {
    pub fn on_first_word(&mut self) {
        let str = self.current_match.clone();
        let ast = AST::Word(str);
        self.output.push(ast);
        let id = self.seen_first_word_state;
        self.push_state(id);
    }

    pub fn on_spaced_word(&mut self) {
        let str = self.current_match.clone();
        let ast = AST::Word(String::from(str.trim()));
        self.output.push(ast);
    }

    pub fn on_err_suffix_first_word(&mut self) {
        let ast = AST::Unrecognised(self.current_match.clone());
        self.output.push(ast);
    }

    pub fn on_err_suffix(&mut self) {
        self.on_err_suffix_first_word();
        self.pop_state();
    }

    pub fn on_no_err_suffix_first_word(&mut self) {}

    pub fn on_no_err_suffix(&mut self) {
        self.on_no_err_suffix_first_word();
        self.pop_state();
    }
}



// ===================
// === Lexer State ===
// ===================

/// The stateful components of the test lexer.
#[derive(Debug)]
pub struct TestState {
    /// The registry for groups in the lexer.
    lexer_states: group::Registry,
    /// The initial state of the lexer.
    initial_state: group::Identifier,
    /// The state entered when the first word has been seen.
    seen_first_word_state: group::Identifier,
    /// A bookmark that is set when a match occurs, allowing for rewinding if necessary.
    matched_bookmark: BookmarkId,
}


// === Trait Impls ===

impl flexer::State for TestState {
    fn new<Reader:LazyReader>(reader:&mut Reader) -> Self {
        let mut lexer_states      = group::Registry::default();
        let initial_state         = lexer_states.define_group("ROOT",None);
        let seen_first_word_state = lexer_states.define_group("SEEN FIRST WORD",None);
        let matched_bookmark      = reader.add_bookmark();
        Self{lexer_states,initial_state,seen_first_word_state,matched_bookmark}
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
}



// =============
// === Tests ===
// =============

#[test]
fn test_lexer_definition() {
    // FIXME [AA] Work out how to best-define the lexer.
    // TODO [AA] Needing a dummy reader to define the lexer is awkward.
    let str       = "aaaaa".as_bytes();
    let reader    = Reader::new(str,DecoderUTF8());
    let mut lexer = TestLexer::new(reader);

    let a_word        = Pattern::char('a').many1();
    let b_word        = Pattern::char('b').many1();
    let space         = Pattern::char(' ');
    let spaced_a_word = space.clone() >> a_word.clone();
    let spaced_b_word = space.clone() >> b_word.clone();
    let any           = Pattern::any();
    let end           = Pattern::eof();

    let root_group_id = lexer.initial_state;
    let root_group    = lexer.groups_mut().group_mut(root_group_id).unwrap();
    root_group.create_rule(&a_word,"self.on_first_word()");
    root_group.create_rule(&b_word,"self.on_first_word()");
    root_group.create_rule(&end,   "self.on_no_err_suffix_first_word()");
    root_group.create_rule(&any,   "self.on_err_suffix_first_word()");

    let seen_first_word_group_id = lexer.seen_first_word_state;
    let seen_first_word_group =
        lexer.groups_mut().group_mut(seen_first_word_group_id).unwrap();
    seen_first_word_group.create_rule(&spaced_a_word,"self.on_spaced_word()");
    seen_first_word_group.create_rule(&spaced_b_word,"self.on_spaced_word()");
    seen_first_word_group.create_rule(&end,          "self.on_no_err_suffix()");
    seen_first_word_group.create_rule(&any,          "self.on_err_suffix()");
}
