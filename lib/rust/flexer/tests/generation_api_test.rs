//! This file contains tests for the intended generated code of the flexer, based on a very small
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

use flexer::*;
use flexer::prelude::*;
use lazy_reader::decoder::DecoderUTF8;
use lazy_reader::{BookmarkId,LazyReader,Reader};



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

// TODO [AA] Rename
#[derive(Debug)]
#[allow(missing_docs)]
pub struct EnsoLexer<Reader:LazyReader> {
    lexer : Flexer<Enso,AST,Reader>
}

/// Implementations of functionality used by the lexer.
///
/// These functions are provided by the user, by hand.
#[allow(missing_docs)]
impl<Reader:LazyReader> EnsoLexer<Reader> {
    pub fn def_on_first_word_str(&mut self,str:String) {
        let ast = AST::Word(str);
        self.def_on_first_word(ast);
    }

    pub fn def_on_first_word(&mut self,ast:AST) {
        self.tokens.push(ast);
        let id = self.def_seen_first_word_state.id;
        self.begin_state(id);
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
}

/// Generated functionality used at runtime by the lexer.
#[allow(missing_docs)]
impl<Reader:LazyReader> EnsoLexer<Reader> {

    /// Executes the lexer on the input provided by the reader, resulting in a
    /// series of tokens.
    pub fn run(&mut self) -> FlexerResult<AST> {
        self.reader.advance_char();

        while self.gen_run_current_state() == FlexerStageStatus::ExitSuccess {}

        match self.get_result() {
            Some(res) => match self.status {
                FlexerStageStatus::ExitFinished => FlexerResult::Success(res),
                FlexerStageStatus::ExitFail => FlexerResult::Failure(Some(res)),
                _ => FlexerResult::Partial(res)
            }
            None => FlexerResult::Failure(None)
        }
    }

    /// Executes the lexer in the current state.
    fn gen_run_current_state(&mut self) -> FlexerStageStatus {
        self.status = FlexerStageStatus::Initial;

        // Runs until reaching a state that no longer says to continue.
        while let Some(next_state) = self.status.continue_as() {
            self.status = self.gen_step(next_state);

            if self.reader.finished() {
                self.status = FlexerStageStatus::ExitFinished
            }

            if self.status.should_continue() {
                if let Ok(char) = self.reader.character().char {
                    self.reader.append_result(char);
                }
                self.reader.advance_char();
            }
        }

        self.status
    }

    /// The step function for the generated lexer.
    fn gen_step(&mut self,next_state:usize) -> FlexerStageStatus {
        let current_state = self.current_state();

        // This match should be generated
        match current_state {
            0 => self.gen_dispatch_in_state_0(next_state),
            1 => self.gen_dispatch_in_state_1(next_state),
            _ => unreachable_panic!("Unreachable state reached in lexer.")
        }
    }

    // === DFA Steps ===

    fn gen_dispatch_in_state_0(&mut self,new_state_index:usize) -> FlexerStageStatus {
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

    fn gen_state_0_to_0(&mut self) -> FlexerStageStatus {
        match u32::from(self.reader.character()) {
            97 => FlexerStageStatus::ContinueWith(3),
            98 => FlexerStageStatus::ContinueWith(4),
            _  => FlexerStageStatus::ContinueWith(2)
        }
    }

    fn gen_state_0_to_1(&mut self) -> FlexerStageStatus {
        // Code similar to this is duplicated _a lot_. This is intentional, as I can't find a way to
        // remove the duplication that doesn't incur dynamic dispatch overhead or run afoul of
        // borrowck.
        self.current_match = self.reader.pop_result();
        self.gen_group_0_rule_2();
        let t = self.def_matched_bookmark;
        self.reader.bookmark(t);
        FlexerStageStatus::ExitSuccess
    }

    fn gen_state_0_to_2(&mut self) -> FlexerStageStatus {
        self.current_match = self.reader.pop_result();
        self.gen_group_0_rule_3();
        let t = self.def_matched_bookmark;
        self.reader.bookmark(t);
        FlexerStageStatus::ExitSuccess
    }

    fn gen_state_0_to_3(&mut self) -> FlexerStageStatus {
        match u32::from(self.reader.character()) {
            97 => FlexerStageStatus::ContinueWith(5),
            _  => {
                self.current_match = self.reader.pop_result();
                self.gen_group_0_rule_0();
                let t = self.def_matched_bookmark;
                self.reader.bookmark(t);
                FlexerStageStatus::ExitSuccess
            }
        }
    }

    fn gen_state_0_to_4(&mut self) -> FlexerStageStatus {
        match u32::from(self.reader.character()) {
            98 => FlexerStageStatus::ContinueWith(6),
            _  => {
                self.current_match = self.reader.pop_result();
                self.gen_group_0_rule_1();
                let t = self.def_matched_bookmark;
                self.reader.bookmark(t);
                FlexerStageStatus::ExitSuccess
            }
        }
    }

    fn gen_state_0_to_5(&mut self) -> FlexerStageStatus {
        match u32::from(self.reader.character()) {
            97 => FlexerStageStatus::ContinueWith(5),
            _ => {
                self.current_match = self.reader.pop_result();
                self.gen_group_0_rule_0();
                let t = self.def_matched_bookmark;
                self.reader.bookmark(t);
                FlexerStageStatus::ExitSuccess
            }
        }
    }

    fn gen_state_0_to_6(&mut self) -> FlexerStageStatus {
        match u32::from(self.reader.character()) {
            98 => FlexerStageStatus::ContinueWith(6),
            _ => {
                self.current_match = self.reader.pop_result();
                self.gen_group_0_rule_1();
                let t = self.def_matched_bookmark;
                self.reader.bookmark(t);
                FlexerStageStatus::ExitSuccess
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

    fn gen_dispatch_in_state_1(&mut self, new_state_index:usize) -> FlexerStageStatus {
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

    fn gen_state_1_to_0(&mut self) -> FlexerStageStatus {
        match u32::from(self.reader.character()) {
            32 => FlexerStageStatus::ContinueWith(3),
            _  => FlexerStageStatus::ContinueWith(2)
        }
    }

    fn gen_state_1_to_1(&mut self) -> FlexerStageStatus {
        self.current_match = self.reader.pop_result();
        self.gen_group_1_rule_2();
        let t = self.def_matched_bookmark;
        self.reader.bookmark(t);
        FlexerStageStatus::ExitSuccess
    }

    fn gen_state_1_to_2(&mut self) -> FlexerStageStatus {
        self.current_match = self.reader.pop_result();
        self.gen_group_1_rule_3();
        let t = self.def_matched_bookmark;
        self.reader.bookmark(t);
        FlexerStageStatus::ExitSuccess
    }

    fn gen_state_1_to_3(&mut self) -> FlexerStageStatus {
        match u32::from(self.reader.character()) {
            97 => FlexerStageStatus::ContinueWith(4),
            98 => FlexerStageStatus::ContinueWith(5),
            _  => {
                self.current_match = self.reader.pop_result();
                self.gen_group_1_rule_3();
                let t = self.def_matched_bookmark;
                self.reader.bookmark(t);
                FlexerStageStatus::ExitSuccess
            }
        }
    }

    fn gen_state_1_to_4(&mut self) -> FlexerStageStatus {
        match u32::from(self.reader.character()) {
            97 => FlexerStageStatus::ContinueWith(6),
            _  => {
                self.current_match = self.reader.pop_result();
                self.gen_group_1_rule_0();
                let t = self.def_matched_bookmark;
                self.reader.bookmark(t);
                FlexerStageStatus::ExitSuccess
            }
        }
    }

    fn gen_state_1_to_5(&mut self) -> FlexerStageStatus {
        match u32::from(self.reader.character()) {
            98 => FlexerStageStatus::ContinueWith(7),
            _  => {
                self.current_match = self.reader.pop_result();
                self.gen_group_1_rule_1();
                let t = self.def_matched_bookmark;
                self.reader.bookmark(t);
                FlexerStageStatus::ExitSuccess
            }
        }
    }

    fn gen_state_1_to_6(&mut self) -> FlexerStageStatus {
        match u32::from(self.reader.character()) {
            97 => FlexerStageStatus::ContinueWith(6),
            _  => {
                self.current_match = self.reader.pop_result();
                self.gen_group_1_rule_0();
                let t = self.def_matched_bookmark;
                self.reader.bookmark(t);
                FlexerStageStatus::ExitSuccess
            }
        }
    }

    fn gen_state_1_to_7(&mut self) -> FlexerStageStatus {
        match u32::from(self.reader.character()) {
            98 => FlexerStageStatus::ContinueWith(7),
            _  => {
                self.current_match = self.reader.pop_result();
                self.gen_group_1_rule_1();
                let t = self.def_matched_bookmark;
                self.reader.bookmark(t);
                FlexerStageStatus::ExitSuccess
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


// === Trait Impls ===

impl<Reader:LazyReader> Deref for EnsoLexer<Reader> {
    type Target = Flexer<Enso,AST,Reader>;
    fn deref(&self) -> &Self::Target {
        &self.lexer
    }
}

impl<Reader:LazyReader> DerefMut for EnsoLexer<Reader> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.lexer
    }
}



// ===================
// === Lexer State ===
// ===================

// TODO [AA] Rename to test
#[derive(Debug)]
#[allow(missing_docs)]
pub struct Enso {
    /// The state entered when the first word has been seen.
    def_seen_first_word_state: LexingState,
    /// A bookmark that is set when a match occurs, allowing for rewinding if necessary.
    def_matched_bookmark: BookmarkId,
}


// === Trait Impls ===

impl <Reader:LazyReader> FlexerState<Reader> for Enso {
    fn new(reader:&mut Reader) -> Self {
        let def_seen_first_word_state = LexingState::new("SEEN FIRST WORD", 1);
        let def_matched_bookmark = reader.add_bookmark();
        Self {def_seen_first_word_state,def_matched_bookmark}
    }
}



// =============
// === Tests ===
// =============

/// Executes the test on the provided input string slice.
fn run_test_on(str:&str) -> Vec<AST> {
    // Hardcoded for ease of use here.
    let reader = Reader::new(str.as_bytes(),DecoderUTF8());
    let lexer: Flexer<Enso,AST,Reader<DecoderUTF8,&[u8]>> = Flexer::new(reader);
    let mut lexer = EnsoLexer{lexer};

    match lexer.run() {
        FlexerResult::Success(tokens) => tokens,
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
