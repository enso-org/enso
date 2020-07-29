//! This file contains tests for the intended defnition code of a lexer using the flexer, based on
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

use flexer::prelude::*;
use flexer::group::{Group, GroupRegistry};
use lazy_reader::{BookmarkId, LazyReader, Reader};
use flexer::{FlexerState, Flexer};
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



// =============
// === Lexer ===
// =============

#[allow(missing_docs)]
#[derive(Clone,Debug)]
pub struct Lexer {
    groups: Vec<Group>
}

#[allow(missing_docs)]
impl Lexer {
    pub fn new() -> Self {
        let groups = Vec::new();
        Lexer{groups}
    }

    // TODO [AA] Parent groups.
    pub fn define_group(&mut self,name:&str) -> &mut Group {
        let id    = self.groups.len();
        let group = Group::new(id,String::from(name),None);
        self.groups.push(group);
        self.groups.get_mut(id).expect("Has just been pushed so should always exist.")
    }
}

#[derive(Debug)]
#[allow(missing_docs)]
pub struct TestLexer<Reader:LazyReader> {
    lexer: Flexer<TestState,AST,Reader>
}

/// The definition?
#[allow(missing_docs)]
impl<Reader:LazyReader> TestLexer<Reader> {
    pub fn new(reader:Reader) -> Self {
        let lexer = Flexer::new(reader);
        TestLexer{lexer}
    }
}

/// Implementation helpers.
#[allow(missing_docs)]
impl<Reader:LazyReader> TestLexer<Reader> {
    pub fn on_first_word(&mut self) {
        let str = self.current_match.clone();
        let ast = AST::Word(str);
        self.tokens.push(ast);
        let id = self.seen_first_word_state;
        self.begin_state(id);
    }

    pub fn on_spaced_word(&mut self) {
        let str = self.current_match.clone();
        let ast = AST::Word(String::from(str.trim()));
        self.tokens.push(ast);
    }

    pub fn on_err_suffix_first_word(&mut self) {
        let ast = AST::Unrecognised(self.current_match.clone());
        self.tokens.push(ast);
    }

    pub fn on_err_suffix(&mut self) {
        self.on_err_suffix_first_word();
        self.end_state();
    }

    pub fn on_no_err_suffix_first_word(&mut self) {}

    pub fn on_no_err_suffix(&mut self) {
        self.on_no_err_suffix_first_word();
        self.end_state();
    }
}

// === Trait Impls ===

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

#[allow(missing_docs)]
#[derive(Debug)]
pub struct TestState {
    lexer_states: GroupRegistry,
    initial_state: usize,
    seen_first_word_state: usize,
    matched_bookmark: BookmarkId
}

impl FlexerState for TestState {
    fn new<Reader:LazyReader>(reader: &mut Reader) -> Self {
        let mut lexer_states      = GroupRegistry::default();
        let initial_state         = lexer_states.define_group("ROOT".into(), None);
        let seen_first_word_state = lexer_states.define_group("SEEN FIRST WORD".into(), None);
        let matched_bookmark      = reader.add_bookmark();
        TestState{lexer_states,initial_state,seen_first_word_state,matched_bookmark}
    }

    fn initial_state(&self) -> usize {
        self.initial_state
    }

    fn groups(&self) -> &GroupRegistry {
        &self.lexer_states
    }

    fn groups_mut(&mut self) -> &mut GroupRegistry {
        &mut self.lexer_states
    }
}



// =============
// === Tests ===
// =============

#[test]
fn test_lexer_definition() {
    // TODO [AA] Needing a dummy reader to define the lexer is awkward.
    let str = "aaaaa".as_bytes();
    let reader = Reader::new(str,DecoderUTF8());
    let mut lexer = TestLexer::new(reader);

    let a_word        = Pattern::char('a').many1();
    let b_word        = Pattern::char('b').many1();
    let space         = Pattern::char(' ');
    let spaced_a_word = space.clone() >> a_word.clone();
    let spaced_b_word = space.clone() >> b_word.clone();
    let any           = Pattern::any();
    let end           = Pattern::eof();

    let root_group_id = lexer.initial_state;
    let root_group    = lexer.groups_mut().group_from_id_mut(root_group_id).unwrap();
    root_group.create_rule(&a_word,"self.on_first_word()");
    root_group.create_rule(&b_word,"self.on_first_word()");
    root_group.create_rule(&end,   "self.on_no_err_suffix_first_word()");
    root_group.create_rule(&any,   "self.on_err_suffix_first_word()");

    let seen_first_word_group_id = lexer.seen_first_word_state;
    let seen_first_word_group =
        lexer.groups_mut().group_from_id_mut(seen_first_word_group_id).unwrap();
    seen_first_word_group.create_rule(&spaced_a_word,"self.on_spaced_word()");
    seen_first_word_group.create_rule(&spaced_b_word,"self.on_spaced_word()");
    seen_first_word_group.create_rule(&end,          "self.on_no_err_suffix()");
    seen_first_word_group.create_rule(&any,          "self.on_err_suffix()");
}
