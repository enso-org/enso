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
#[derive(Clone, Debug, PartialEq)]
pub struct Lexer<T> {
    registry: Vec<State>,
    state_stack: Vec<usize>,
    reader: Reader,
    current_match: String,
    phantom: std::marker::PhantomData<T>,
    status:LexerStageStatus
}

#[allow(missing_docs)]
impl <T> Lexer<T> {

    // TODO [AA] Lexer interface
    pub fn run(&mut self) -> LexerResult<T> {
        // Reserve stack to bigger (1024)
        self.state_stack.reserve(self.registry.len());
        self.reader.rewinder.set_matched();
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
    fn get_result(&mut self) -> Option<T> {
        unimplemented!()
    }

    // TODO [AA] Lexer interface
    pub fn root_state(&self) -> &State {
        self.registry.first().expect("The root state should always exist.")
    }

    // TODO [AA] Lexer interface
    pub fn define_state(&mut self, name: &str) -> &State {
        let state = State::new(name, self.registry.len());
        self.registry.push(state);
        self.registry.last().expect("There should always be one state in the registry.")
    }

    // TODO [AA] Lexer interface
    pub fn begin_state(&mut self, state: &State) {
        self.state_stack.push(state.index);
    }

    // TODO [AA] Lexer interfaec
    pub fn current_state(&self) -> &State {
        let ix = *self.state_stack.last().expect("There should always be one state on the stack.");
        self.state_for(ix)
    }

    // TODO [AA] Lexer interface
    pub fn end_state(&mut self) -> Option<&State> {
        if self.state_stack.len() > 1 {
            let ix = self.state_stack.pop().expect("There should be an item to pop.");
            Some(self.state_for(ix))
        } else {
            None
        }
    }

    // TODO [AA] Lexer interface
    pub fn end_states_until(&mut self, state: &State) -> Vec<&State> {
        // Never drop the root state
        let position_of_target =
            self.state_stack.iter().positions(|elem| *elem == state.index).last().unwrap_or(0);
        let range = (position_of_target + 1)..self.state_stack.len();
        let ended_indices: Vec<usize> = self.state_stack.drain(range).collect();
        let mut ended_states = Vec::new();
        for ix in ended_indices {
            ended_states.push(self.state_for(ix));
        }
        ended_states
    }

    // TODO [AA] Internal helper
    fn state_for(&self,ix:usize) -> &State {
        self.registry.get(ix).expect("The state for the index should always exist.")
    }

    // TODO [AA] Internal helper
    fn current_state_ix(&self) -> usize {
        self.current_state().index
    }

    // TODO [AA] Lexer interface
    pub fn in_state(&mut self, state: &State) -> bool {
        self.current_state() == state
    }

    // TODO [AA] Internal helper
    fn run_current_state(&mut self) -> LexerStageStatus {
        let mut is_finished = false;

        self.status = LexerStageStatus::Initial;

        while self.status.is_valid() {
            let status = self.status.value().expect("Value guaranteed to exist as `is_valid()`.");
            let state_from_status = self.state_for(status).index;
            self.status = self.step(state_from_status);

            if is_finished && !self.reader.rewinder.is_rewinded() {
                self.status = LexerStageStatus::ExitFinished
            }

            is_finished = self.reader.is_finished();

            if self.status.is_valid() {
                if !self.reader.is_finished() {
                    self.reader.append_code_point(self.reader.char_code);
                }
                self.reader.next_char();
            }
        }

        self.status
    }

    // TODO [AA] Generated code
    fn step(&mut self,new_state_index:usize) -> LexerStageStatus {
        let current_state_index = self.current_state_ix();

        // This match should be generated
        // TODO [AA] Use unreachable_unchecked. Write macro for panic in debug / this in prod
        //  builds. Should take string.
        match current_state_index {
            0 => self.dispatch_in_state_0(new_state_index),
            1 => self.dispatch_in_state_1(new_state_index),
            _ => unreachable_panic!("Unreachable state reached in lexer.")
        }
    }

    // TODO [AA] Generated code
    fn dispatch_in_state_0(&mut self,new_state_index:usize) -> LexerStageStatus {
        match new_state_index {
            0 => self.state_0_to_0(),
            1 => self.state_0_to_1(),
            2 => self.state_0_to_2(),
            3 => self.state_0_to_3(),
            4 => self.state_0_to_4(),
            _ => unreachable_panic!("Unreachable state reached in lexer.")
        }
    }

    // TODO [AA] Generated code
    fn state_0_to_0(&mut self) -> LexerStageStatus {
        match self.reader.char_code {
            97 => LexerStageStatus::ContinueWith(1),
            98 => LexerStageStatus::ContinueWith(2),
            _  => {
                self.current_match = self.reader.result.to_string();
                self.group_0_rule_2();
                self.reader.set_result_length(0);
                self.reader.rewinder.set_matched();
                LexerStageStatus::ExitSuccess
            }
        }
    }

    // TODO [AA] Generated code
    fn state_0_to_1(&mut self) -> LexerStageStatus {
        match self.reader.char_code {
            97 => LexerStageStatus::ContinueWith(3),
            _  => {
                self.current_match = self.reader.result.to_string();
                self.group_0_rule_0();
                self.reader.set_result_length(0);
                self.reader.rewinder.set_matched();
                LexerStageStatus::ExitSuccess
            }
        }
    }

    // TODO [AA] Generated code
    fn state_0_to_2(&mut self) -> LexerStageStatus {
        match self.reader.char_code {
            98 => LexerStageStatus::ContinueWith(4),
            _  => {
                self.current_match = self.reader.result.to_string();
                self.group_0_rule_1();
                self.reader.set_result_length(0);
                self.reader.rewinder.set_matched();
                LexerStageStatus::ExitSuccess
            }
        }
    }

    // TODO [AA] Generated code
    fn state_0_to_3(&mut self) -> LexerStageStatus {
        match self.reader.char_code {
            97 => LexerStageStatus::ContinueWith(3),
            _  => {
                self.current_match = self.reader.result.to_string();
                self.group_0_rule_0();
                self.reader.set_result_length(0);
                self.reader.rewinder.set_matched();
                LexerStageStatus::ExitSuccess
            }
        }
    }

    // TODO [AA] Generated code
    fn state_0_to_4(&mut self) -> LexerStageStatus {
        match self.reader.char_code {
            98 => LexerStageStatus::ContinueWith(4),
            _  => {
                self.current_match = self.reader.result.to_string();
                self.group_0_rule_1();
                self.reader.set_result_length(0);
                self.reader.rewinder.set_matched();
                LexerStageStatus::ExitSuccess
            }
        }
    }

    // TODO [AA] Generated code
    fn group_0_rule_0(&self) -> () {
        // TODO [AA] These get implemented in terms of the lexer interface in the definition.
        unimplemented!()
    }

    // TODO [AA] Generated code
    fn group_0_rule_1(&self) -> () {
        unimplemented!()
    }

    // TODO [AA] Generated code
    fn group_0_rule_2(&self) -> () {
        unimplemented!()
    }

    // TODO [AA] Generated code
    fn dispatch_in_state_1(&mut self,new_state_index:usize) -> LexerStageStatus {
        match new_state_index {
            0 => self.state_1_to_0(),
            1 => self.state_1_to_1(),
            2 => self.state_1_to_2(),
            3 => self.state_1_to_3(),
            4 => self.state_1_to_4(),
            5 => self.state_1_to_5(),
            _ => unreachable_panic!("Unreachable state reached in lexer.")
        }
    }

    // TODO [AA] Generated code
    fn state_1_to_0(&mut self) -> LexerStageStatus {
        match self.reader.char_code {
            32 => {
                // TODO [AA] What determines use of the rewinder here?
                self.reader.rewinder.set_rule();
                LexerStageStatus::ContinueWith(1)
            }
            _  => {
                self.current_match = self.reader.result.to_string();
                self.group_1_rule_2();
                self.reader.set_result_length(0);
                self.reader.rewinder.set_matched();
                LexerStageStatus::ExitSuccess
            }
        }
    }

    // TODO [AA] Generated code
    fn state_1_to_1(&mut self) -> LexerStageStatus {
        match self.reader.char_code {
            97 => LexerStageStatus::ContinueWith(2),
            98 => LexerStageStatus::ContinueWith(3),
            _  => {
                self.reader.rewinder.run_rule();
                self.current_match = self.reader.result.to_string();
                self.group_1_rule_2();
                self.reader.set_result_length(0);
                self.reader.rewinder.set_matched();
                LexerStageStatus::ExitSuccess
            }
        }
    }

    // TODO [AA] Generated code
    fn state_1_to_2(&mut self) -> LexerStageStatus {
        match self.reader.char_code {
            97 => LexerStageStatus::ContinueWith(4),
            _  => {
                self.current_match = self.reader.result.to_string();
                self.group_1_rule_0();
                self.reader.set_result_length(0);
                self.reader.rewinder.set_matched();
                LexerStageStatus::ExitSuccess
            }
        }
    }

    // TODO [AA] Generated code
    fn state_1_to_3(&mut self) -> LexerStageStatus {
        match self.reader.char_code {
            98 => LexerStageStatus::ContinueWith(5),
            _  => {
                self.current_match = self.reader.result.to_string();
                self.group_1_rule_1();
                self.reader.set_result_length(0);
                self.reader.rewinder.set_matched();
                LexerStageStatus::ExitSuccess
            }
        }
    }

    // TODO [AA] Generated code
    fn state_1_to_4(&mut self) -> LexerStageStatus {
        match self.reader.char_code {
            97 => LexerStageStatus::ContinueWith(4),
            _  => {
                self.current_match = self.reader.result.to_string();
                self.group_1_rule_0();
                self.reader.set_result_length(0);
                self.reader.rewinder.set_matched();
                LexerStageStatus::ExitSuccess
            }
        }
    }

    // TODO [AA] Generated code
    fn state_1_to_5(&mut self) -> LexerStageStatus {
        match self.reader.char_code {
            98 => LexerStageStatus::ContinueWith(5),
            _  => {
                self.current_match = self.reader.result.to_string();
                self.group_1_rule_1();
                self.reader.set_result_length(0);
                self.reader.rewinder.set_matched();
                LexerStageStatus::ExitSuccess
            }
        }
    }

    // TODO [AA] Generated code
    fn group_1_rule_0(&self) -> () {
        unimplemented!()
    }

    // TODO [AA] Generated code
    fn group_1_rule_1(&self) -> () {
        unimplemented!()
    }

    // TODO [AA] Generated code
    fn group_1_rule_2(&self) -> () {
        unimplemented!()
    }
}

#[allow(missing_docs)]
impl <T> Default for Lexer<T> {
    fn default() -> Self {
        let status = LexerStageStatus::ExitSuccess;
        let phantom = PhantomData;
        let current_match = String::from("");
        let reader = Reader::default();
        let mut registry = Vec::new();
        let root_index = registry.len();
        let root_state = State::new("ROOT", root_index);
        registry.push(root_state);
        let state_stack = vec![root_index];
        Lexer{registry,state_stack,reader,current_match,phantom,status}
    }
}

#[allow(missing_docs)]
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Reader {
    rewinder:Rewinder,
    char_code:u32,
    result:String
}
#[allow(missing_docs)]
impl Reader {
    pub const ENDOFINPUT:u32 = 0;

    pub fn is_finished(&self) -> bool {
        self.char_code == Reader::ENDOFINPUT
    }

    pub fn append_code_point(&mut self,_code_point:u32) {
        unimplemented!()
    }

    pub fn next_char(&mut self) {
        unimplemented!()
    }

    pub fn set_result_length(&mut self,_len:usize) {
        unimplemented!()
    }
}
impl Default for Reader {
    fn default() -> Self {
        unimplemented!()
    }
}

#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Rewinder {}
#[allow(missing_docs)]
impl Rewinder {
    pub fn is_rewinded(&self) -> bool {
        unimplemented!()
    }

    pub fn set_rule(&self) {
        unimplemented!()
    }

    pub fn run_rule(&self) {
        unimplemented!()
    }

    pub fn set_matched(&self) {
        unimplemented!()
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

mod test {}
