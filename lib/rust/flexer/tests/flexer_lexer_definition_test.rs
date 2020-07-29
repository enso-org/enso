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
use flexer::automata::dfa::DFA;
use flexer::automata::nfa::NFA;
use flexer::group::Group;
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

    pub fn specialize(&self) -> String {
        let group_nfa:Vec<NFA>  = self.groups.iter().map(|group|group.into()).collect();
        let _group_dfa:Vec<DFA> = group_nfa.iter().map(|nfa|nfa.into()).collect();
        let str = String::new();
        str
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
    pub fn def_on_first_word_str(&mut self) {
        let str = self.current_match.clone();
        let ast = AST::Word(str);
        self.def_on_first_word(ast);
    }

    pub fn def_on_first_word(&mut self,ast:AST) {
        self.tokens.push(ast);
        let id = self.seen_first_word_group.id;
        self.begin_state(id);
    }

    pub fn def_on_spaced_word_str(&mut self) {
        let str = self.current_match.clone();
        let ast = AST::Word(String::from(str.trim()));
        self.def_on_spaced_word(ast);
    }

    pub fn def_on_spaced_word(&mut self,ast:AST) {
        self.tokens.push(ast);
    }

    pub fn def_on_err_suffix_first_word(&mut self) {
        let ast = AST::Unrecognised(self.current_match.clone());
        self.tokens.push(ast);
    }

    pub fn def_on_err_suffix(&mut self) {
        self.def_on_err_suffix_first_word();
        self.end_state();
    }

    pub fn def_on_no_err_suffix_first_word(&mut self) {}

    pub fn def_on_no_err_suffix(&mut self) {
        self.def_on_no_err_suffix_first_word();
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

// TODO [AA] _Where_ do I want to write the definition?
#[allow(missing_docs)]
#[derive(Debug)]
pub struct TestState {
    root_group: Rc<Group>,
    seen_first_word_group: Rc<Group>,
    matched_bookmark: BookmarkId
}

impl<Reader:LazyReader> FlexerState<Reader> for TestState {
    fn new(reader: &mut Reader) -> Self {
        // TODO [AA] Parent groups need to be mutable as held.
        let root_group = Rc::new(Group::new(0,String::from("ROOT"),None));
        // TODO [AA] The parent here is just for compilation testing. It doesn't lex properly.
        let seen_first_word_group = Rc::new(Group::new(1,String::from("SEEN FIRST WORD"),Some(root_group.clone())));
        let matched_bookmark = reader.add_bookmark();
        TestState{root_group,seen_first_word_group,matched_bookmark}
    }

    fn initial_state(&self) -> &Group {
        &self.root_group
    }

    fn groups(&self) -> Vec<&Group> {
        vec![&self.root_group,&self.seen_first_word_group]
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

    // TODO [AA] Can't use this, because it requires no additional refs be held.
    // TODO [AA] Needs to be RefCell.
    // let root_group = Rc::get_mut(&mut lexer.root_group).unwrap();
    // TODO [AA] The functions used in callbacks must take no arguments other than self.
    // root_group.create_rule(&a_word,"lexer.on_first_word_str()")
}

// #[test]
// fn try_generate_code() {
//     let mut lexer = Lexer::new();
//
//     let a_word        = Pattern::char('a').many1();
//     let b_word        = Pattern::char('b').many1();
//     let space         = Pattern::char(' ');
//     let spaced_a_word = space.clone() >> a_word.clone();
//     let spaced_b_word = space.clone() >> b_word.clone();
//     let any           = Pattern::any();
//     let end           = Pattern::eof();
//
//     // The ordering here is necessary.
//     let root_group = lexer.define_group("ROOT");
//     root_group.create_rule(&a_word,"1 + 1");
//     root_group.create_rule(&b_word,"2 + 2");
//     root_group.create_rule(&end,"3 + 3");
//     root_group.create_rule(&any,"4 + 4");
//
//     let seen_first_word_group = lexer.define_group("SEEN_FIRST_WORD");
//     seen_first_word_group.create_rule(&spaced_a_word,"5 + 5");
//     seen_first_word_group.create_rule(&spaced_b_word,"6 + 6");
//     seen_first_word_group.create_rule(&end,"7 + 7");
//     seen_first_word_group.create_rule(&any,"8 + 8");
//
//     let result   = lexer.specialize();
//     let expected = String::from("");
//
//     assert_eq!(result,expected)
// }
