//! This file contains tests for the intended generated code using the flexer, based on the
//! following small language.
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
//! `flexer_lexer_definition_test` file. This is to present the full view of what each portion of
//! the process looks like.

use flexer::*;
use flexer::prelude::*;
use lazy_reader::decoder::DecoderUTF8;
use lazy_reader::{BookmarkId,LazyReader,Reader};
use flexer::group;


// ===========
// === AST ===
// ===========

/// A very simple AST, sufficient for the simple language being defined.
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

/// Generated functionality used at runtime by the lexer.
#[allow(missing_docs)]
impl<Reader:LazyReader> TestLexer<Reader> {

    /// Executes the lexer on the input provided by the reader, resulting in a
    /// series of tokens.
    pub fn run(&mut self) -> Result<AST> {
        self.reader.advance_char();

        while self.gen_run_current_state() == StageStatus::ExitSuccess {}

            match self.status {
                StageStatus::ExitFinished => Result::success(mem::replace(&mut self.output,vec![])),
                StageStatus::ExitFail     => Result::failure(mem::replace(&mut self.output,vec![])),
                _                         => Result::partial(mem::replace(&mut self.output,vec![]))
            }
    }

    /// Executes the lexer in the current state.
    fn gen_run_current_state(&mut self) -> StageStatus {
        self.status = StageStatus::Initial;

        // Runs until reaching a state that no longer says to continue.
        while let Some(next_state) = self.status.continue_as() {
            self.status = self.gen_step(next_state);

            if self.reader.finished() {
                self.status = StageStatus::ExitFinished
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
    fn gen_step(&mut self, next_state:group::Identifier) -> StageStatus {
        let current_state:usize = self.current_state().into();

        // This match should be generated
        match current_state {
            0 => self.gen_dispatch_in_state_0(next_state),
            1 => self.gen_dispatch_in_state_1(next_state),
            _ => unreachable_panic!("Unreachable state reached in lexer.")
        }
    }

    // === DFA Steps ===

    fn gen_dispatch_in_state_0(&mut self, new_state_index:group::Identifier) -> StageStatus {
        match new_state_index.into() {
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

    fn gen_state_0_to_0(&mut self) -> StageStatus {
        match u32::from(self.reader.character()) {
            97 => StageStatus::ContinueWith(3.into()),
            98 => StageStatus::ContinueWith(4.into()),
            _  => StageStatus::ContinueWith(2.into())
        }
    }

    fn gen_state_0_to_1(&mut self) -> StageStatus {
        self.current_match = self.reader.pop_result();
        self.gen_group_0_rule_2();
        let t = self.matched_bookmark;
        self.reader.bookmark(t);
        StageStatus::ExitSuccess
    }

    fn gen_state_0_to_2(&mut self) -> StageStatus {
        self.current_match = self.reader.pop_result();
        self.gen_group_0_rule_3();
        let t = self.matched_bookmark;
        self.reader.bookmark(t);
        StageStatus::ExitSuccess
    }

    fn gen_state_0_to_3(&mut self) -> StageStatus {
        match u32::from(self.reader.character()) {
            97 => StageStatus::ContinueWith(5.into()),
            _  => {
                self.current_match = self.reader.pop_result();
                self.gen_group_0_rule_0();
                let t = self.matched_bookmark;
                self.reader.bookmark(t);
                StageStatus::ExitSuccess
            }
        }
    }

    fn gen_state_0_to_4(&mut self) -> StageStatus {
        match u32::from(self.reader.character()) {
            98 => StageStatus::ContinueWith(6.into()),
            _  => {
                self.current_match = self.reader.pop_result();
                self.gen_group_0_rule_1();
                let t = self.matched_bookmark;
                self.reader.bookmark(t);
                StageStatus::ExitSuccess
            }
        }
    }

    fn gen_state_0_to_5(&mut self) -> StageStatus {
        match u32::from(self.reader.character()) {
            97 => StageStatus::ContinueWith(5.into()),
            _ => {
                self.current_match = self.reader.pop_result();
                self.gen_group_0_rule_0();
                let t = self.matched_bookmark;
                self.reader.bookmark(t);
                StageStatus::ExitSuccess
            }
        }
    }

    fn gen_state_0_to_6(&mut self) -> StageStatus {
        match u32::from(self.reader.character()) {
            98 => StageStatus::ContinueWith(6.into()),
            _ => {
                self.current_match = self.reader.pop_result();
                self.gen_group_0_rule_1();
                let t = self.matched_bookmark;
                self.reader.bookmark(t);
                StageStatus::ExitSuccess
            }
        }
    }

    fn gen_group_0_rule_0(&mut self) -> () {
        self.on_first_word()
    }

    fn gen_group_0_rule_1(&mut self) -> () {
        self.on_first_word()
    }

    fn gen_group_0_rule_2(&mut self) -> () {
        self.on_err_suffix_first_word()
    }

    fn gen_group_0_rule_3(&mut self) {
        self.on_err_suffix_first_word()
    }

    fn gen_dispatch_in_state_1(&mut self, new_state_index:group::Identifier) -> StageStatus {
        match new_state_index.into() {
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

    fn gen_state_1_to_0(&mut self) -> StageStatus {
        match u32::from(self.reader.character()) {
            32 => StageStatus::ContinueWith(3.into()),
            _  => StageStatus::ContinueWith(2.into())
        }
    }

    fn gen_state_1_to_1(&mut self) -> StageStatus {
        self.current_match = self.reader.pop_result();
        self.gen_group_1_rule_2();
        let t = self.matched_bookmark;
        self.reader.bookmark(t);
        StageStatus::ExitSuccess
    }

    fn gen_state_1_to_2(&mut self) -> StageStatus {
        self.current_match = self.reader.pop_result();
        self.gen_group_1_rule_3();
        let t = self.matched_bookmark;
        self.reader.bookmark(t);
        StageStatus::ExitSuccess
    }

    fn gen_state_1_to_3(&mut self) -> StageStatus {
        match u32::from(self.reader.character()) {
            97 => StageStatus::ContinueWith(4.into()),
            98 => StageStatus::ContinueWith(5.into()),
            _  => {
                self.current_match = self.reader.pop_result();
                self.gen_group_1_rule_3();
                let t = self.matched_bookmark;
                self.reader.bookmark(t);
                StageStatus::ExitSuccess
            }
        }
    }

    fn gen_state_1_to_4(&mut self) -> StageStatus {
        match u32::from(self.reader.character()) {
            97 => StageStatus::ContinueWith(6.into()),
            _  => {
                self.current_match = self.reader.pop_result();
                self.gen_group_1_rule_0();
                let t = self.matched_bookmark;
                self.reader.bookmark(t);
                StageStatus::ExitSuccess
            }
        }
    }

    fn gen_state_1_to_5(&mut self) -> StageStatus {
        match u32::from(self.reader.character()) {
            98 => StageStatus::ContinueWith(7.into()),
            _  => {
                self.current_match = self.reader.pop_result();
                self.gen_group_1_rule_1();
                let t = self.matched_bookmark;
                self.reader.bookmark(t);
                StageStatus::ExitSuccess
            }
        }
    }

    fn gen_state_1_to_6(&mut self) -> StageStatus {
        match u32::from(self.reader.character()) {
            97 => StageStatus::ContinueWith(6.into()),
            _  => {
                self.current_match = self.reader.pop_result();
                self.gen_group_1_rule_0();
                let t = self.matched_bookmark;
                self.reader.bookmark(t);
                StageStatus::ExitSuccess
            }
        }
    }

    fn gen_state_1_to_7(&mut self) -> StageStatus {
        match u32::from(self.reader.character()) {
            98 => StageStatus::ContinueWith(7.into()),
            _  => {
                self.current_match = self.reader.pop_result();
                self.gen_group_1_rule_1();
                let t = self.matched_bookmark;
                self.reader.bookmark(t);
                StageStatus::ExitSuccess
            }
        }
    }

    fn gen_group_1_rule_0(&mut self) {
        self.on_spaced_word();
    }

    fn gen_group_1_rule_1(&mut self) -> () {
        self.on_spaced_word();
    }

    fn gen_group_1_rule_2(&mut self) {
        self.on_no_err_suffix();
    }

    fn gen_group_1_rule_3(&mut self) {
        self.on_err_suffix()
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

/// Executes the test on the provided input string slice.
fn run_test_on(str:impl AsRef<str>) -> Vec<AST> {
    // Hardcoded for ease of use here.
    let reader     = Reader::new(str.as_ref().as_bytes(),DecoderUTF8());
    let mut lexer  = TestLexer::new(reader);
    let run_result = lexer.run();

    match run_result.kind {
        flexer::ResultKind::Success => run_result.tokens,
        _                           => default()
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
