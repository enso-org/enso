//! This file contains experimentation with the form of the codegen for the flexer, based on a small
//! language.
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
//! PLEASE NOTE THE FOLLOWING:
//!
//! - Logging is being ignored for now.
//! - Docs are intentionally missing for now in many places.
//!
//! RUNNING: cargo test -p flexer --lib

use crate::prelude::*;
use lazy_reader::{Reader, BookmarkId};
use lazy_reader::decoder::DecoderUTF8;



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
// === State ===
// =============

/// A container for state identifiers in the lexer.
#[derive(Clone,Debug,Default)]
pub struct State {
    /// The name of the state, useful for debugging.
    pub name: String,
    /// The identifier of the state.
    id: usize,
}

impl State {
    /// Creates a new state with the specified name and identifier.
    pub fn new(name:&str,id:usize) -> State {
        let name = String::from(name);
        State{name,id}
    }
}


// === Trait Impls ===

impl PartialEq for State {
    fn eq(&self,other:&Self) -> bool {
        self.id == other.id
    }
}



// =============
// === Lexer ===
// =============

/// The lexer implementation.
///
/// Please note that every method and member prefixed with `def_` are user-defined, and those
/// prefixed with `gen_` are generated from the lexer definition.
#[derive(Clone, Debug)]
pub struct Lexer<'a,T> {
    /// The stack of states that are active during lexer execution.
    state_stack: Vec<usize>,
    /// A reader for the input.
    reader: Reader<DecoderUTF8,&'a [u8]>,
    /// The current match of the lexer.
    current_match: String,
    /// The result of the current stage of the DFA.
    status: LexerStageStatus,
    /// The tokens that have been lexed.
    tokens: Vec<T>,
    /// The initial state of the defined lexer.
    def_initial_state: State,
    /// The state entered when the first word has been seen.
    def_seen_first_word_state: State,
    /// A bookmark that is set when a match occurs, allowing for rewinding if necessary.
    def_matched_bookmark: BookmarkId,
}

impl <T> Lexer<'_,T> {
    /// Creates a new lexer instance.
    ///
    /// Please note that the `reader` argument is currently hard-coded for testing purposes. This is
    /// not the intention for the eventual design.
    pub fn new(mut reader:Reader<DecoderUTF8,&[u8]>) -> Lexer<T> {
        let mut state_stack = Vec::new();
        state_stack.reserve(1024);
        let current_match = String::from("");
        let status = LexerStageStatus::Initial;
        let mut tokens = Vec::new();
        tokens.reserve(1024);
        let def_initial_state = State::new("INITIAL",0);
        let def_seen_first_word_state = State::new("SEEN FIRST WORD",1);
        state_stack.push(def_initial_state.id);
        let def_matched_bookmark = reader.add_bookmark();

        Lexer{
             state_stack
            ,reader
            ,current_match
            ,status
            ,tokens
            ,def_initial_state
            ,def_seen_first_word_state
            ,def_matched_bookmark}
    }
}

/// This block is things that are part of the lexer's interface and functionality.
impl Lexer<'_,AST> {
    /// Executes the lexer on the input provided by the reader, resulting in a
    /// series of tokens.
    pub fn run(&mut self) -> LexerResult<AST> {
        self.reader.advance_char();

        while self.run_current_state() == LexerStageStatus::ExitSuccess {}

        match self.get_result() {
            Some(res) => match self.status {
                LexerStageStatus::ExitFinished => LexerResult::Success(res),
                LexerStageStatus::ExitFail => LexerResult::Failure(Some(res)),
                _ => LexerResult::Partial(res)
            }
            None => LexerResult::Failure(None)
        }
    }

    /// Gets the lexer result.
    fn get_result(&mut self) -> Option<Vec<AST>> {
        Some(self.tokens.clone())
    }

    /// Gets the lexer's root state.
    pub fn root_state(&self) -> &State {
        &self.def_initial_state
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

    /// Executes the lexer in the current state.
    fn run_current_state(&mut self) -> LexerStageStatus {
        self.status = LexerStageStatus::Initial;

        // Runs until reaching a state that no longer says to continue.
        while let Some(next_state) = self.status.continue_as() {
            self.status = self.gen_step(next_state);

            if self.reader.finished() {
                self.status = LexerStageStatus::ExitFinished
            }

            if self.status.should_continue() {
                if let Ok(char) = self.reader.character.char {
                    self.reader.result.push(char);
                }
                self.reader.advance_char();
            }
        }

        self.status
    }
}

/// This impl block contains functionality that should be generated.
#[allow(missing_docs)]
impl Lexer<'_,AST> {

    // === Defined Actions ===

    pub fn def_on_first_word_str(&mut self,str:String) {
        let ast = AST::Word(str);
        self.def_on_first_word(ast);
    }

    pub fn def_on_first_word(&mut self,ast:AST) {
        self.tokens.push(ast);
        self.begin_state(self.def_seen_first_word_state.id);
    }

    pub fn def_on_spaced_word_str(&mut self,str:String) {
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


    // === DFA Steps ===

    fn gen_step(&mut self,next_state:usize) -> LexerStageStatus {
        let current_state = self.current_state();

        // This match should be generated
        match current_state {
            0 => self.gen_dispatch_in_state_0(next_state),
            1 => self.gen_dispatch_in_state_1(next_state),
            _ => unreachable_panic!("Unreachable state reached in lexer.")
        }
    }

    fn gen_dispatch_in_state_0(&mut self,new_state_index:usize) -> LexerStageStatus {
        match new_state_index {
            0 => self.gen_state_0_to_0(),
            1 => self.gen_state_0_to_1(),
            2 => self.gen_state_0_to_2(),
            3 => self.gen_state_0_to_3(),
            4 => self.gen_state_0_to_4(),
            5 => self.gen_state_0_to_5(),
            6 => self.gen_state_0_to_6(),
            _ => unreachable_panic!("Unreachable state reached in lexer.")
        }
    }

    fn gen_state_0_to_0(&mut self) -> LexerStageStatus {
        match u32::from(self.reader.character) {
            97 => LexerStageStatus::ContinueWith(3),
            98 => LexerStageStatus::ContinueWith(4),
            _  => LexerStageStatus::ContinueWith(2)
        }
    }

    fn gen_state_0_to_1(&mut self) -> LexerStageStatus {
        // Code similar to this is duplicated _a lot_. This is intentional, as I can't find a way to
        // remove the duplication that doesn't incur dynamic dispatch overhead.
        self.current_match = self.reader.pop_result();
        self.gen_group_0_rule_2();
        self.reader.bookmark(self.def_matched_bookmark);
        LexerStageStatus::ExitSuccess
    }

    fn gen_state_0_to_2(&mut self) -> LexerStageStatus {
        self.current_match = self.reader.pop_result();
        self.gen_group_0_rule_3();
        self.reader.bookmark(self.def_matched_bookmark);
        LexerStageStatus::ExitSuccess
    }

    fn gen_state_0_to_3(&mut self) -> LexerStageStatus {
        match u32::from(self.reader.character) {
            97 => LexerStageStatus::ContinueWith(5),
            _  => {
                self.current_match = self.reader.pop_result();
                self.gen_group_0_rule_0();
                self.reader.bookmark(self.def_matched_bookmark);
                LexerStageStatus::ExitSuccess
            }
        }
    }

    fn gen_state_0_to_4(&mut self) -> LexerStageStatus {
        match u32::from(self.reader.character) {
            98 => LexerStageStatus::ContinueWith(6),
            _  => {
                self.current_match = self.reader.pop_result();
                self.gen_group_0_rule_1();
                self.reader.bookmark(self.def_matched_bookmark);
                LexerStageStatus::ExitSuccess
            }
        }
    }

    fn gen_state_0_to_5(&mut self) -> LexerStageStatus {
        match u32::from(self.reader.character) {
            97 => LexerStageStatus::ContinueWith(5),
            _ => {
                self.current_match = self.reader.pop_result();
                self.gen_group_0_rule_0();
                self.reader.bookmark(self.def_matched_bookmark);
                LexerStageStatus::ExitSuccess
            }
        }
    }

    fn gen_state_0_to_6(&mut self) -> LexerStageStatus {
        match u32::from(self.reader.character) {
            98 => LexerStageStatus::ContinueWith(6),
            _ => {
                self.current_match = self.reader.pop_result();
                self.gen_group_0_rule_1();
                self.reader.bookmark(self.def_matched_bookmark);
                LexerStageStatus::ExitSuccess
            }
        }
    }

    fn gen_group_0_rule_0(&mut self) -> () {
        self.def_on_first_word_str(self.current_match.clone())
    }

    fn gen_group_0_rule_1(&mut self) -> () {
        self.def_on_first_word_str(self.current_match.clone())
    }

    fn gen_group_0_rule_2(&mut self) -> () {
        self.def_on_err_suffix_first_word()
    }

    fn gen_group_0_rule_3(&mut self) {
        self.def_on_err_suffix_first_word()
    }

    fn gen_dispatch_in_state_1(&mut self, new_state_index:usize) -> LexerStageStatus {
        match new_state_index {
            0 => self.gen_state_1_to_0(),
            1 => self.gen_state_1_to_1(),
            2 => self.gen_state_1_to_2(),
            3 => self.gen_state_1_to_3(),
            4 => self.gen_state_1_to_4(),
            5 => self.gen_state_1_to_5(),
            6 => self.gen_state_1_to_6(),
            7 => self.gen_state_1_to_7(),
            _ => unreachable_panic!("Unreachable state reached in lexer.")
        }
    }

    fn gen_state_1_to_0(&mut self) -> LexerStageStatus {
        match u32::from(self.reader.character) {
            32 => LexerStageStatus::ContinueWith(3),
            _  => LexerStageStatus::ContinueWith(2)
        }
    }

    fn gen_state_1_to_1(&mut self) -> LexerStageStatus {
        self.current_match = self.reader.pop_result();
        self.gen_group_1_rule_2();
        self.reader.bookmark(self.def_matched_bookmark);
        LexerStageStatus::ExitSuccess
    }

    fn gen_state_1_to_2(&mut self) -> LexerStageStatus {
        self.current_match = self.reader.pop_result();
        self.gen_group_1_rule_3();
        self.reader.bookmark(self.def_matched_bookmark);
        LexerStageStatus::ExitSuccess
    }

    fn gen_state_1_to_3(&mut self) -> LexerStageStatus {
        match u32::from(self.reader.character) {
            97 => LexerStageStatus::ContinueWith(4),
            98 => LexerStageStatus::ContinueWith(5),
            _  => {
                self.current_match = self.reader.pop_result();
                self.gen_group_1_rule_3();
                self.reader.bookmark(self.def_matched_bookmark);
                LexerStageStatus::ExitSuccess
            }
        }
    }

    fn gen_state_1_to_4(&mut self) -> LexerStageStatus {
        match u32::from(self.reader.character) {
            97 => LexerStageStatus::ContinueWith(6),
            _  => {
                self.current_match = self.reader.pop_result();
                self.gen_group_1_rule_0();
                self.reader.bookmark(self.def_matched_bookmark);
                LexerStageStatus::ExitSuccess
            }
        }
    }

    fn gen_state_1_to_5(&mut self) -> LexerStageStatus {
        match u32::from(self.reader.character) {
            98 => LexerStageStatus::ContinueWith(7),
            _  => {
                self.current_match = self.reader.pop_result();
                self.gen_group_1_rule_1();
                self.reader.bookmark(self.def_matched_bookmark);
                LexerStageStatus::ExitSuccess
            }
        }
    }

    fn gen_state_1_to_6(&mut self) -> LexerStageStatus {
        match u32::from(self.reader.character) {
            97 => LexerStageStatus::ContinueWith(6),
            _  => {
                self.current_match = self.reader.pop_result();
                self.gen_group_1_rule_0();
                self.reader.bookmark(self.def_matched_bookmark);
                LexerStageStatus::ExitSuccess
            }
        }
    }

    fn gen_state_1_to_7(&mut self) -> LexerStageStatus {
        match u32::from(self.reader.character) {
            98 => LexerStageStatus::ContinueWith(7),
            _  => {
                self.current_match = self.reader.pop_result();
                self.gen_group_1_rule_1();
                self.reader.bookmark(self.def_matched_bookmark);
                LexerStageStatus::ExitSuccess
            }
        }
    }

    fn gen_group_1_rule_0(&mut self) {
        self.def_on_spaced_word_str(self.current_match.clone());
    }

    fn gen_group_1_rule_1(&mut self) -> () {
        self.def_on_spaced_word_str(self.current_match.clone());
    }

    fn gen_group_1_rule_2(&mut self) {
        self.def_on_no_err_suffix();
    }

    fn gen_group_1_rule_3(&mut self) {
        self.def_on_err_suffix()
    }
}



// ========================
// === LexerStageStatus ===
// ========================

/// The result of executing a single step of the DFA.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum LexerStageStatus {
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

impl LexerStageStatus {
    /// Checks if the lexer stage should continue.
    pub fn should_continue(&self) -> bool {
        self.continue_as().is_some()
    }

    /// Obtains the state to which the lexer should transition, iff the lexer should continue.
    pub fn continue_as(&self) -> Option<usize> {
        match self {
            LexerStageStatus::Initial => Some(0),
            LexerStageStatus::ContinueWith(val) => Some(*val),
            _ => None
        }
    }
}



// ===================
// === LexerResult ===
// ===================

/// The result of executing the lexer on a given input.
#[derive(Clone,Debug,Eq,PartialEq)]
pub enum LexerResult<T> {
    /// The lexer succeeded, returning the contained token stream.
    Success(Vec<T>),
    /// The lexer succeeded on part of the input, returning the contained token stream.
    Partial(Vec<T>),
    /// The lexer failed on the input, returning any tokens it _did_ manage to consume.
    Failure(Option<Vec<T>>)
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod test {
    extern crate test;

    use super::*;
    use lazy_reader::Reader;
    use lazy_reader::decoder::DecoderUTF8;

    /// Executes the test on the provided input string slice.
    fn run_test_on(str:&str) -> Vec<AST> {
        // Hardcoded for ease of use here.
        let reader = Reader::new(str.as_bytes(),DecoderUTF8());
        let mut lexer:Lexer<AST> = Lexer::new(reader);

        match lexer.run() {
            LexerResult::Success(tokens) => tokens,
            _ => vec![]
        }
    }

    #[test]
    fn test_single_a_word() {
        let input = "aaaaa";
        let expected_output = vec![AST::Word(String::from(input))];
        let result = run_test_on(input);
        assert_eq!(result,expected_output);
    }

    #[test]
    fn test_single_b_word() {
        let input = "bbbbb";
        let expected_output = vec![AST::Word(String::from(input))];
        let result = run_test_on(input);
        assert_eq!(result,expected_output);
    }

    #[test]
    fn test_two_word() {
        let input = "aaaaa bbbbb";
        let expected_output =
            vec![AST::Word(String::from("aaaaa")),AST::Word(String::from("bbbbb"))];
        let result = run_test_on(input);
        assert_eq!(result,expected_output);
    }

    #[test]
    fn test_multi_word() {
        let input = "bbb aa a b bbbbb aa";
        let expected_output = vec![
            AST::Word(String::from("bbb")),
            AST::Word(String::from("aa")),
            AST::Word(String::from("a")),
            AST::Word(String::from("b")),
            AST::Word(String::from("bbbbb")),
            AST::Word(String::from("aa"))
        ];
        let result = run_test_on(input);
        assert_eq!(result,expected_output);
    }

    #[test]
    fn test_invalid_single_word() {
        let input = "c";
        let expected_output = vec![AST::Unrecognised(String::from(input))];
        let result = run_test_on(input);
        assert_eq!(result,expected_output);
    }

    #[test]
    fn test_multi_word_invalid() {
        let input = "aaaaaa c bbbbbb";
        let expected_output = vec![
            AST::Word(String::from("aaaaaa")),
            AST::Unrecognised(String::from(" ")),
            AST::Unrecognised(String::from("c")),
            AST::Unrecognised(String::from(" ")),
            AST::Word(String::from("bbbbbb")),
        ];
        let result = run_test_on(input);
        assert_eq!(result,expected_output);
    }

    #[test]
    fn test_end_invalid() {
        let input = "bbbbbb c";
        let expected_output = vec![
            AST::Word(String::from("bbbbbb")),
            AST::Unrecognised(String::from(" ")),
            AST::Unrecognised(String::from("c")),
        ];
        let result = run_test_on(input);
        assert_eq!(result,expected_output);
    }
}
