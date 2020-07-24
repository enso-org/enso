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
use lazy_reader::Error::EOF;

#[allow(missing_docs)]
#[derive(Clone, Debug, PartialEq)]
pub enum AST {
    Word(String),
    Unrecognised(String)
}

#[allow(missing_docs)]
#[derive(Clone, Debug, Default)]
pub struct State {
    pub name: String,
    id: usize,
}

#[allow(missing_docs)]
impl State {
    pub fn new(name: &str, index: usize) -> State {
        let name = String::from(name);
        State{name, id: index }
    }
}

#[allow(missing_docs)]
impl PartialEq for State {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

#[allow(missing_docs)]
#[derive(Clone, Debug)]
pub struct Lexer<'a, T> {
    state_stack: Vec<usize>,
    reader: Reader<DecoderUTF8,&'a [u8]>,
    current_match: String,
    status:LexerStageStatus,
    tokens:Vec<T>,
    def_initial_state: State,
    def_seen_first_word_state: State,
    def_matched_bookmark: BookmarkId,
    def_rule_bookmark: BookmarkId
}

#[allow(missing_docs)]
impl <T> Lexer<'_,T> {
    pub fn new(mut reader: Reader<DecoderUTF8, &[u8]>) -> Lexer<T> {
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
        let def_rule_bookmark = reader.add_bookmark();

        Lexer{
             state_stack
            ,reader
            ,current_match
            ,status
            ,tokens
            ,def_initial_state
            ,def_seen_first_word_state
            ,def_matched_bookmark
            ,def_rule_bookmark}
    }
}

#[allow(missing_docs)]
impl Lexer<'_,AST> {

    pub fn run(&mut self) -> LexerResult<Vec<AST>> {
        self.reader.bookmark(self.def_matched_bookmark);
        self.reader.next_char();

        while self.run_current_state() == LexerStageStatus::ExitSuccess {}

        match self.def_get_result() {
            Some(res) => match self.status {
                LexerStageStatus::ExitFinished => LexerResult::Success(res),
                LexerStageStatus::ExitFail     => LexerResult::Failure(Some(res)),
                _                              => LexerResult::Partial(res)
            }
            None => LexerResult::Failure(None)
        }
    }

    fn def_get_result(&mut self) -> Option<Vec<AST>> {
        Some(self.tokens.clone())
    }

    pub fn root_state(&self) -> &State {
        &self.def_initial_state
    }

    pub fn begin_state(&mut self,state:usize) {
        self.state_stack.push(state);
    }

    pub fn current_state(&self) -> usize {
        *self.state_stack.last().expect("There should always be one state on the stack.")
    }

    pub fn end_state(&mut self) -> Option<usize> {
        if self.state_stack.len() > 1 {
            let ix = self.state_stack.pop().expect("There should be an item to pop.");
            Some(ix)
        } else {
            None
        }
    }

    pub fn end_states_until(&mut self, state: usize) -> Vec<usize> {
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

    pub fn in_state(&mut self, state: usize) -> bool {
        self.current_state() == state
    }

    fn run_current_state(&mut self) -> LexerStageStatus {
        self.status = LexerStageStatus::Initial;

        while let Some(next_state) = self.status.continue_as() {
            self.status = self.def_step(next_state);

            if self.reader.finished() {
                self.status = LexerStageStatus::ExitFinished
            }

            if self.status.should_continue() {
                if let Ok(char) = self.reader.character.char {
                    self.reader.result.push(char);
                }
                self.reader.next_char();
            }
        }

        self.status
    }

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

    pub fn def_on_no_err_suffix_first_word(&mut self) {
        self.def_submit();
    }

    pub fn def_on_no_err_suffix(&mut self) {
        self.def_on_no_err_suffix_first_word();
        self.end_state();
    }

    pub fn def_submit(&mut self) {}

    fn def_step(&mut self, next_state:usize) -> LexerStageStatus {
        let current_state = self.current_state();

        // This match should be generated
        match current_state {
            0 => self.def_dispatch_in_state_0(next_state),
            1 => self.def_dispatch_in_state_1(next_state),
            _ => unreachable_panic!("Unreachable state reached in lexer.")
        }
    }

    fn def_dispatch_in_state_0(&mut self, new_state_index:usize) -> LexerStageStatus {
        match new_state_index {
            0 => self.def_state_0_to_0(),
            1 => self.def_state_0_to_1(),
            2 => self.def_state_0_to_2(),
            3 => self.def_state_0_to_3(),
            4 => self.def_state_0_to_4(),
            5 => self.def_state_0_to_5(),
            6 => self.def_state_0_to_6(),
            _ => unreachable_panic!("Unreachable state reached in lexer.")
        }
    }

    fn def_state_0_to_0(&mut self) -> LexerStageStatus {
        match u32::from(self.reader.character) {
            97 => LexerStageStatus::ContinueWith(3),
            98 => LexerStageStatus::ContinueWith(4),
            _  => LexerStageStatus::ContinueWith(2)
        }
    }

    fn def_state_0_to_1(&mut self) -> LexerStageStatus {
        self.current_match = self.reader.pop_result();
        self.def_group_0_rule_2();
        self.reader.bookmark(self.def_matched_bookmark);
        LexerStageStatus::ExitSuccess
    }

    fn def_state_0_to_2(&mut self) -> LexerStageStatus {
        self.current_match = self.reader.pop_result();
        self.def_group_0_rule_3();
        self.reader.bookmark(self.def_matched_bookmark);
        LexerStageStatus::ExitSuccess
    }

    fn def_state_0_to_3(&mut self) -> LexerStageStatus {
        match u32::from(self.reader.character) {
            97 => LexerStageStatus::ContinueWith(5),
            _  => {
                self.current_match = self.reader.pop_result();
                self.def_group_0_rule_0();
                self.reader.bookmark(self.def_matched_bookmark);
                LexerStageStatus::ExitSuccess
            }
        }
    }

    fn def_state_0_to_4(&mut self) -> LexerStageStatus {
        match u32::from(self.reader.character) {
            98 => LexerStageStatus::ContinueWith(6),
            _  => {
                self.current_match = self.reader.pop_result();
                self.def_group_0_rule_1();
                self.reader.bookmark(self.def_matched_bookmark);
                LexerStageStatus::ExitSuccess
            }
        }
    }

    fn def_state_0_to_5(&mut self) -> LexerStageStatus {
        match u32::from(self.reader.character) {
            97 => LexerStageStatus::ContinueWith(5),
            _ => {
                self.current_match = self.reader.pop_result();
                self.def_group_0_rule_0();
                self.reader.bookmark(self.def_matched_bookmark);
                LexerStageStatus::ExitSuccess
            }
        }
    }

    fn def_state_0_to_6(&mut self) -> LexerStageStatus {
        match u32::from(self.reader.character) {
            98 => LexerStageStatus::ContinueWith(6),
            _ => {
                self.current_match = self.reader.pop_result();
                self.def_group_0_rule_1();
                self.reader.bookmark(self.def_matched_bookmark);
                LexerStageStatus::ExitSuccess
            }
        }
    }

    fn def_group_0_rule_0(&mut self) -> () {
        self.def_on_first_word_str(self.current_match.clone())
    }

    fn def_group_0_rule_1(&mut self) -> () {
        self.def_on_first_word_str(self.current_match.clone())
    }

    fn def_group_0_rule_2(&mut self) -> () {
        self.def_on_err_suffix_first_word()
    }

    fn def_group_0_rule_3(&mut self) {
        self.def_on_err_suffix_first_word()
    }

    fn def_dispatch_in_state_1(&mut self, new_state_index:usize) -> LexerStageStatus {
        match new_state_index {
            0 => self.def_state_1_to_0(),
            1 => self.def_state_1_to_1(),
            2 => self.def_state_1_to_2(),
            3 => self.def_state_1_to_3(),
            4 => self.def_state_1_to_4(),
            5 => self.def_state_1_to_5(),
            6 => self.def_state_1_to_6(),
            7 => self.def_state_1_to_7(),
            _ => unreachable_panic!("Unreachable state reached in lexer.")
        }
    }

    fn def_state_1_to_0(&mut self) -> LexerStageStatus {
        match u32::from(self.reader.character) {
            32 => LexerStageStatus::ContinueWith(3),
            _  => LexerStageStatus::ContinueWith(2)
        }
    }

    fn def_state_1_to_1(&mut self) -> LexerStageStatus {
        self.current_match = self.reader.pop_result();
        self.def_group_1_rule_2();
        self.reader.bookmark(self.def_matched_bookmark);
        LexerStageStatus::ExitSuccess
    }

    fn def_state_1_to_2(&mut self) -> LexerStageStatus {
        self.current_match = self.reader.pop_result();
        self.def_group_1_rule_3();
        self.reader.bookmark(self.def_matched_bookmark);
        LexerStageStatus::ExitSuccess
    }

    fn def_state_1_to_3(&mut self) -> LexerStageStatus {
        match u32::from(self.reader.character) {
            97 => LexerStageStatus::ContinueWith(4),
            98 => LexerStageStatus::ContinueWith(5),
            _  => {
                self.current_match = self.reader.pop_result();
                self.def_group_1_rule_3();
                self.reader.bookmark(self.def_matched_bookmark);
                LexerStageStatus::ExitSuccess
            }
        }
    }

    fn def_state_1_to_4(&mut self) -> LexerStageStatus {
        match u32::from(self.reader.character) {
            97 => LexerStageStatus::ContinueWith(6),
            _  => {
                self.current_match = self.reader.pop_result();
                self.def_group_1_rule_0();
                self.reader.bookmark(self.def_matched_bookmark);
                LexerStageStatus::ExitSuccess
            }
        }
    }

    fn def_state_1_to_5(&mut self) -> LexerStageStatus {
        match u32::from(self.reader.character) {
            98 => LexerStageStatus::ContinueWith(7),
            _  => {
                self.current_match = self.reader.pop_result();
                self.def_group_1_rule_1();
                self.reader.bookmark(self.def_matched_bookmark);
                LexerStageStatus::ExitSuccess
            }
        }
    }

    fn def_state_1_to_6(&mut self) -> LexerStageStatus {
        match u32::from(self.reader.character) {
            97 => LexerStageStatus::ContinueWith(6),
            _  => {
                self.current_match = self.reader.pop_result();
                self.def_group_1_rule_0();
                self.reader.bookmark(self.def_matched_bookmark);
                LexerStageStatus::ExitSuccess
            }
        }
    }

    fn def_state_1_to_7(&mut self) -> LexerStageStatus {
        match u32::from(self.reader.character) {
            98 => LexerStageStatus::ContinueWith(7),
            _  => {
                self.current_match = self.reader.pop_result();
                self.def_group_1_rule_1();
                self.reader.bookmark(self.def_matched_bookmark);
                LexerStageStatus::ExitSuccess
            }
        }
    }

    fn def_group_1_rule_0(&mut self) {
        self.def_on_spaced_word_str(self.current_match.clone());
    }

    fn def_group_1_rule_1(&mut self) -> () {
        self.def_on_spaced_word_str(self.current_match.clone());
    }

    fn def_group_1_rule_2(&mut self) {
        self.def_on_no_err_suffix();
    }

    fn def_group_1_rule_3(&mut self) {
        self.def_on_err_suffix()
    }
}

#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum LexerStageStatus {
    Initial,
    ExitSuccess,
    ExitFail,
    ExitFinished,
    ContinueWith(usize)
}

#[allow(missing_docs)]
impl LexerStageStatus {
    pub fn should_continue(&self) -> bool {
        self.continue_as().is_some()
    }

    pub fn continue_as(&self) -> Option<usize> {
        match self {
            LexerStageStatus::Initial => Some(0),
            LexerStageStatus::ContinueWith(val) => Some(*val),
            _ => None
        }
    }
}

#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LexerResult<T> {
    Success(T),
    Partial(T),
    Failure(Option<T>)
}

#[cfg(test)]
mod test {
    extern crate test;

    use super::*;
    use lazy_reader::{Reader, BookmarkId};
    use lazy_reader::decoder::DecoderUTF8;

    #[allow(missing_docs)]
    fn run_test_on(str:&str) -> Vec<AST> {
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
