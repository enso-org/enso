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

use crate::prelude::*;
use lazy_reader::{Reader, BookmarkId};
use lazy_reader::decoder::DecoderUTF8;

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
    index: usize, // rename to id
}

#[allow(missing_docs)]
impl State {
    pub fn new(name: &str, index: usize) -> State {
        let name = String::from(name);
        State{name,index}
    }
}

#[allow(missing_docs)]
impl PartialEq for State {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
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
    def_current:Option<T>,
    def_initial_state: State,
    def_seen_first_word_state: State,
    def_matched_bookmark: BookmarkId,
    def_rule_bookmark: BookmarkId
}

impl <T> Lexer<'_,T> {
    pub fn new(mut reader: Reader<DecoderUTF8, &[u8]>) -> Lexer<T> {
        let mut state_stack = Vec::new();
        state_stack.reserve(1024);
        let current_match = String::from("");
        let status = LexerStageStatus::Initial;
        let mut tokens = Vec::new();
        tokens.reserve(1024);
        let def_current = None;
        let def_initial_state = State::new("INITIAL",0);
        let def_seen_first_word_state = State::new("SEEN FIRST WORD",1);
        state_stack.push(def_initial_state.index);
        let def_matched_bookmark = reader.add_bookmark();
        let def_rule_bookmark = reader.add_bookmark();

        Lexer{
             state_stack
            ,reader
            ,current_match
            ,status
            ,tokens
            ,def_current
            ,def_initial_state
            ,def_seen_first_word_state
            ,def_matched_bookmark
            ,def_rule_bookmark}
    }
}

#[allow(missing_docs)]
impl Lexer<'_,AST> {

    // TODO [AA] Lexer interface
    pub fn run(&mut self) -> LexerResult<Vec<AST>> {
        self.reader.bookmark(self.def_matched_bookmark);
        self.reader.next_char();

        while self.run_current_state() == LexerStageStatus::ExitSuccess {}

        match self.get_result() {
            Some(res) => match self.status {
                LexerStageStatus::ExitFinished => LexerResult::Success(res),
                LexerStageStatus::ExitFail     => LexerResult::Failure(Some(res)),
                _                              => LexerResult::Partial(res)
            }
            None => LexerResult::Failure(None)
        }
    }

    // TODO [AA] Internal helper (generated)
    fn get_result(&mut self) -> Option<Vec<AST>> {
        Some(self.tokens.clone())
    }

    // TODO [AA] Lexer interface
    pub fn root_state(&self) -> &State {
        &self.def_initial_state
    }

    // TODO [AA] Lexer interface
    pub fn begin_state(&mut self,state:usize) {
        self.state_stack.push(state);
    }

    // TODO [AA] Lexer interfaec
    pub fn current_state(&self) -> usize {
        *self.state_stack.last().expect("There should always be one state on the stack.")
    }

    // TODO [AA] Lexer interface
    pub fn end_state(&mut self) -> Option<usize> {
        if self.state_stack.len() > 1 {
            let ix = self.state_stack.pop().expect("There should be an item to pop.");
            Some(ix)
        } else {
            None
        }
    }

    // TODO [AA] Lexer interface
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

    // TODO [AA] Lexer interface
    pub fn in_state(&mut self, state: usize) -> bool {
        self.current_state() == state
    }

    // TODO [AA] Internal helper
    fn run_current_state(&mut self) -> LexerStageStatus {
        self.status = LexerStageStatus::Initial;

        while self.status.is_valid() {
            let status = self.status.value().expect("Value guaranteed to exist as `is_valid()`.");
            self.status = self.def_step(status);

            if self.reader.empty() {
                self.status = LexerStageStatus::ExitFinished
            }

            if self.status.is_valid() {
                if let Ok(char) = self.reader.character.char {
                    self.reader.result.push(char);
                }
                self.reader.next_char();
            }
        }

        self.status
    }

    // TODO [AA] Generated code
    fn def_step(&mut self, new_state_index:usize) -> LexerStageStatus {
        let current_state_index = self.current_state();

        // This match should be generated
        match current_state_index {
            0 => self.def_dispatch_in_state_0(new_state_index),
            1 => self.def_dispatch_in_state_1(new_state_index),
            _ => unreachable_panic!("Unreachable state reached in lexer.")
        }
    }

    // TODO [AA] Generated code
    fn def_dispatch_in_state_0(&mut self, new_state_index:usize) -> LexerStageStatus {
        match new_state_index {
            0 => self.def_state_0_to_0(),
            1 => self.def_state_0_to_1(),
            2 => self.def_state_0_to_2(),
            3 => self.def_state_0_to_3(),
            4 => self.def_state_0_to_4(),
            _ => unreachable_panic!("Unreachable state reached in lexer.")
        }
    }

    // TODO [AA] Generated code
    fn def_state_0_to_0(&mut self) -> LexerStageStatus {
        let temp = u32::from(self.reader.character);
        match u32::from(self.reader.character) {
            97 => LexerStageStatus::ContinueWith(1),
            98 => LexerStageStatus::ContinueWith(2),
            _  => {
                self.current_match = self.reader.pop_result();
                self.def_group_0_rule_2();
                self.reader.bookmark(self.def_matched_bookmark);
                LexerStageStatus::ExitSuccess
            }
        }
    }

    // TODO [AA] Generated code
    fn def_state_0_to_1(&mut self) -> LexerStageStatus {
        match u32::from(self.reader.character) {
            97 => LexerStageStatus::ContinueWith(3),
            _  => {
                self.current_match = self.reader.pop_result();
                self.def_group_0_rule_0();
                self.reader.bookmark(self.def_matched_bookmark);
                LexerStageStatus::ExitSuccess
            }
        }
    }

    // TODO [AA] Generated code
    fn def_state_0_to_2(&mut self) -> LexerStageStatus {
        match u32::from(self.reader.character) {
            98 => LexerStageStatus::ContinueWith(4),
            _  => {
                self.current_match = self.reader.pop_result();
                self.def_group_0_rule_1();
                self.reader.bookmark(self.def_matched_bookmark);
                LexerStageStatus::ExitSuccess
            }
        }
    }

    // TODO [AA] Generated code
    fn def_state_0_to_3(&mut self) -> LexerStageStatus {
        match u32::from(self.reader.character) {
            97 => LexerStageStatus::ContinueWith(3),
            _  => {
                self.current_match = self.reader.pop_result();
                self.def_group_0_rule_0();
                self.reader.bookmark(self.def_matched_bookmark);
                LexerStageStatus::ExitSuccess
            }
        }
    }

    // TODO [AA] Generated code
    fn def_state_0_to_4(&mut self) -> LexerStageStatus {
        match u32::from(self.reader.character) {
            98 => LexerStageStatus::ContinueWith(4),
            _  => {
                self.current_match = self.reader.pop_result();
                self.def_group_0_rule_1();
                self.reader.bookmark(self.def_matched_bookmark);
                LexerStageStatus::ExitSuccess
            }
        }
    }

    pub fn def_on_first_word_str(&mut self,str:String) {
        let ast = AST::Word(str);
        self.def_on_first_word(ast)
    }

    pub fn def_on_first_word(&mut self,ast:AST) {
        self.def_current = Some(ast);
        let state = self.def_seen_first_word_state.index;
        self.begin_state(state);
    }

    pub fn def_on_spaced_word_str(&mut self,str:String) {
        let ast = AST::Word(String::from(str.trim_end()));
        self.def_on_spaced_word(ast);
    }

    pub fn def_on_spaced_word(&mut self,ast:AST) {
        self.def_current = Some(ast);
    }

    pub fn def_on_no_err_suffix(&mut self) {
        self.def_submit();
        self.end_state();
    }

    pub fn def_on_err_suffix(&mut self) {
        let ast = AST::Unrecognised(self.current_match.clone());
        self.tokens.push(ast);
        self.def_current = None;
        self.end_state();
    }

    pub fn def_submit(&mut self) {
        let token = self.def_current.as_ref().unwrap().clone();
        self.tokens.push(token);
        self.def_current = None;
    }

    // TODO [AA] Generated code
    fn def_group_0_rule_0(&mut self) -> () {
        self.def_on_first_word_str(self.current_match.clone())
    }

    // TODO [AA] Generated code
    fn def_group_0_rule_1(&mut self) -> () {
        self.def_on_first_word_str(self.current_match.clone())
    }

    // TODO [AA] Generated code
    fn def_group_0_rule_2(&mut self) -> () {
        self.def_on_err_suffix()
    }

    // TODO [AA] Generated code
    fn def_dispatch_in_state_1(&mut self, new_state_index:usize) -> LexerStageStatus {
        match new_state_index {
            0 => self.def_state_1_to_0(),
            1 => self.def_state_1_to_1(),
            2 => self.def_state_1_to_2(),
            3 => self.def_state_1_to_3(),
            4 => self.def_state_1_to_4(),
            5 => self.def_state_1_to_5(),
            _ => unreachable_panic!("Unreachable state reached in lexer.")
        }
    }

    // TODO [AA] Generated code
    fn def_state_1_to_0(&mut self) -> LexerStageStatus {
        match u32::from(self.reader.character) {
            32 => {
                // We bookmark in the case of overlapping rules.
                self.reader.bookmark(self.def_rule_bookmark);
                LexerStageStatus::ContinueWith(1)
            }
            _  => {
                self.current_match = self.reader.pop_result();
                self.def_group_1_rule_2();

                self.reader.bookmark(self.def_matched_bookmark);
                LexerStageStatus::ExitSuccess
            }
        }
    }

    // TODO [AA] Generated code
    fn def_state_1_to_1(&mut self) -> LexerStageStatus {
        match u32::from(self.reader.character) {
            97 => LexerStageStatus::ContinueWith(2),
            98 => LexerStageStatus::ContinueWith(3),
            _  => {
                self.reader.bookmark(self.def_rule_bookmark);
                self.current_match = self.reader.pop_result();
                self.def_group_1_rule_2();
                self.reader.bookmark(self.def_matched_bookmark);
                LexerStageStatus::ExitSuccess
            }
        }
    }

    // TODO [AA] Generated code
    fn def_state_1_to_2(&mut self) -> LexerStageStatus {
        match u32::from(self.reader.character) {
            97 => LexerStageStatus::ContinueWith(4),
            _  => {
                self.current_match = self.reader.pop_result();
                self.def_group_1_rule_0();

                self.reader.bookmark(self.def_matched_bookmark);
                LexerStageStatus::ExitSuccess
            }
        }
    }

    // TODO [AA] Generated code
    fn def_state_1_to_3(&mut self) -> LexerStageStatus {
        match u32::from(self.reader.character) {
            98 => LexerStageStatus::ContinueWith(5),
            _  => {
                self.current_match = self.reader.pop_result();
                self.def_group_1_rule_1();

                // What determines that the rewinder gets used here?
                self.reader.bookmark(self.def_matched_bookmark);
                LexerStageStatus::ExitSuccess
            }
        }
    }

    // TODO [AA] Generated code
    fn def_state_1_to_4(&mut self) -> LexerStageStatus {
        match u32::from(self.reader.character) {
            97 => LexerStageStatus::ContinueWith(4),
            _  => {
                self.current_match = self.reader.pop_result();
                self.def_group_1_rule_0();

                self.reader.bookmark(self.def_matched_bookmark);
                LexerStageStatus::ExitSuccess
            }
        }
    }

    // TODO [AA] Generated code
    fn def_state_1_to_5(&mut self) -> LexerStageStatus {
        match u32::from(self.reader.character) {
            98 => LexerStageStatus::ContinueWith(5),
            _  => {
                self.current_match = self.reader.pop_result();
                self.def_group_1_rule_1();
                self.reader.bookmark(self.def_matched_bookmark);
                LexerStageStatus::ExitSuccess
            }
        }
    }

    // TODO [AA] Generated code
    fn def_group_1_rule_0(&mut self) {
        self.def_on_spaced_word_str(self.current_match.clone());
    }

    // TODO [AA] Generated code
    fn def_group_1_rule_1(&mut self) -> () {
        self.def_on_spaced_word_str(self.current_match.clone());
    }

    // TODO [AA] Generated code
    fn def_group_1_rule_2(&mut self) -> () {
        self.def_on_no_err_suffix();
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
    pub fn is_valid(&self) -> bool {
        match self {
            LexerStageStatus::Initial => true,
            LexerStageStatus::ContinueWith(_) => true,
            _ => false,
        }
    }
    pub fn value(&self) -> Option<usize> {
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
    fn test_single_word() {
        let input = "aaaaa";
        let expected_output = vec![AST::Word(String::from(input))];
        let result = run_test_on(input);
        assert_eq!(result,expected_output);
    }
}
