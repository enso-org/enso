//! This module contains the definition of the lexer for the Enso programming language.

use crate::prelude::*;
use flexer::*;

use crate::library::token::BlockType;
use crate::library::token::Token;
use crate::library::token;

use flexer::automata::pattern::Pattern;
use flexer::group::Group;
use flexer::group::Registry;
use flexer::prelude::logger::Disabled;
use flexer::prelude::reader;
use flexer::State as FlexerState;
use flexer;
use std::collections::VecDeque;
use std::cmp::Ordering;



// ====================
// === Type Aliases ===
// ====================

type Logger = Disabled;
type Flexer = flexer::Flexer<State<Logger>,token::Stream,Logger>;



// ==================
// === Enso Lexer ===
// ==================

/// The Enso lexer.
#[derive(Debug)]
pub struct EnsoLexer(Flexer);

impl Deref for EnsoLexer {
    type Target = Flexer;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for EnsoLexer {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

/// Functions for working with the lexer.
impl EnsoLexer {
    /// Construct a new instance of the Enso lexer.
    pub fn new() -> Self {
        let logger = Logger::new("EnsoLexer");
        let lexer  = Flexer::new(logger);
        EnsoLexer(lexer)
    }
}


// === Result Functionality ===

impl EnsoLexer {
    /// Push the current token stream onto the stack.
    pub fn push_tokens(&mut self) {
        let current_stream = mem::take(&mut self.output);
        debug!(self.logger,"Push Tokens: {&current_stream:?}");
        self.tokens_stack.push(current_stream);
    }

    /// Pop the top token stream from the stack and make it current.
    pub fn pop_tokens(&mut self) {
        let popped  = self.tokens_stack.pop().unwrap_or_default();
        debug!(self.logger,"Pop Tokens: {&popped:?}");
        self.output = popped;
    }

    /// Append the provided `token` to the lexer output.
    pub fn append_token(&mut self, token:Token) {
        debug!(self.logger,"Append: {&token:?}");
        self.output.append(token);
    }

    /// Get a reference to the last token in the current lexer output.
    pub fn last_token(&mut self) -> Option<&Token> {
        self.output.last()
    }

    /// Consume the currently active stream of tokens.
    pub fn consume_tokens(&mut self) -> token::Stream {
        mem::take(&mut self.output)
    }

    /// Consume the current match and replace it with the empty string.
    pub fn consume_current(&mut self) -> String {
        debug!(self.logger,"Consume: {self.current_match:?}");
        mem::take(&mut self.current_match)
    }

    /// Discard the current match and replace it with the empty string.
    pub fn discard_current(&mut self) {
        debug!(self.logger,"Discard: {self.current_match:?}");
        self.current_match = default();
    }
}



// === Common Patterns ===

/// Basic character classification
#[allow(dead_code)]
impl EnsoLexer {
    /// Match lower-case ASCII letters.
    fn lower_ascii_letter() -> Pattern {
        Pattern::range('a'..='z')
    }

    /// Match upper-case ASCII letters.
    fn upper_ascii_letter() -> Pattern {
        Pattern::range('A'..='Z')
    }

    /// Match ASCII digits.
    fn ascii_digit() -> Pattern {
        Pattern::range('0'..='9')
    }

    /// Match ASCII letters.
    fn ascii_letter() -> Pattern {
        EnsoLexer::lower_ascii_letter() | EnsoLexer::upper_ascii_letter()
    }

    /// Match ASCII alphanumeric characters.
    fn ascii_alpha_num() -> Pattern {
        EnsoLexer::ascii_digit() | EnsoLexer::ascii_letter()
    }

    /// Match at least one ASCII space character.
    fn spaces() -> Pattern {
        Pattern::char(' ').many1()
    }

    /// Match a newline.
    ///
    /// This matches both Unix (LF) and Windows (CRLF) styles of newlines. This is particularly
    /// important so as not to result in incorrect spans on windows clients.
    fn newline() -> Pattern {
        Pattern::char('\n') | Pattern::all_of("\r\n")
    }

    /// The allowable group characters in Enso.
    fn group_chars() -> String {
        String::from("()[]{}")
    }

    /// The allowable operator characters in Enso.
    fn operator_chars() -> String {
        String::from(";!$%&*+-/<>?^~|:\\")
    }

    /// The characters that break tokens in Enso.
    fn whitespace_break_chars() -> String {
        String::from("\t\r\n")
    }

    /// The characters that break token lexing in Enso.
    fn break_chars() -> String {
        let mut break_chars = String::from("`@#,. ");
        break_chars.push_str(&Self::operator_chars());
        break_chars.push_str(&Self::whitespace_break_chars());
        break_chars.push_str(&Self::group_chars());
        break_chars
    }
}


// === Operators ===

/// The set of rules for lexing Enso operator identifiers.
#[allow(dead_code)]
impl EnsoLexer {

    /// Create an arbitrary operator that requires no special handling.
    fn on_operator<R:LazyReader>(&mut self, _reader:&mut R) {
        let op_modifier_check = self.operator_modifier_check;
        let operator          = self.consume_current();
        let offset            = self.offset.consume();
        let token             = Token::Operator(operator,offset);
        self.append_token(token);
        self.push_state(op_modifier_check);
    }

    /// Create an operator that cannot have an associated modifier.
    fn on_operator_no_modifier<R:LazyReader>(&mut self, _reader:&mut R) {
        let op_suffix_check = self.operator_suffix_check;
        let operator        = self.consume_current();
        let offset          = self.offset.consume();
        let token           = Token::Operator(operator,offset);
        self.append_token(token);
        self.push_state(op_suffix_check);
    }

    /// Create a grouping operator.
    fn on_group<R:LazyReader>(&mut self, reader:&mut R) {
        let operator = self.consume_current();
        let offset   = self.offset.consume();
        let token    = Token::Operator(operator,offset);
        self.append_token(token);
        self.ident_on_no_error_suffix(reader);
    }

    /// Create an operator modifier.
    fn on_modifier<R:LazyReader>(&mut self, _reader:&mut R) {
        match self.output.pop() {
            Some(token) => match token.shape {
                token::Shape::Operator(name) => {
                    let new_token = Token::Modifier(name,token.offset);
                    self.discard_current();
                    self.append_token(new_token);
                },
                _ => unreachable_panic!("The preceding token should always be an operator."),
            }
            None => unreachable_panic!("There should always be a preceding token."),
        }
    }

    /// The rules for lexing Enso operators.
    fn add_operator_rules(lexer:&mut EnsoLexer) {
        let operator_char   = Pattern::any_of(Self::operator_chars().as_str());
        let equals          = c!('=');
        let comma           = c!(',');
        let dot             = c!('.');
        let error_char      = &operator_char | &equals | &comma | &dot;
        let error_suffix    = &error_char.many1();
        let operator_body   = &operator_char.many1();
        let ops_eq          = &equals | l!("==") | l!(">=") | l!("<=") | l!("!=") | l!("#=");
        let ops_in          = l!("in");
        let ops_dot         = dot | comma | l!("..") | l!("...");
        let ops_group       = Pattern::any_of(Self::group_chars().as_str());
        let ops_comment     = c!('#') | l!("##");
        let ops_no_modifier = &ops_eq | &ops_dot | &ops_comment | &ops_in;

        let initial_state_id = lexer.initial_state;
        let initial_state    = lexer.group_mut(initial_state_id);
        initial_state.create_rule(&operator_body,  "self.on_operator(reader)");
        initial_state.create_rule(&ops_no_modifier,"self.on_operator_no_modifier(reader)");
        initial_state.create_rule(&ops_group,      "self.on_group(reader)");

        let operator_mod_check_id = lexer.operator_modifier_check;
        let operator_mod_check    = lexer.group_mut(operator_mod_check_id);
        operator_mod_check.create_rule(&equals,"self.on_modifier(reader)");

        let operator_sfx_check_id = lexer.operator_suffix_check;
        let operator_sfx_check    = lexer.group_mut(operator_sfx_check_id);
        operator_sfx_check.create_rule(&error_suffix,"self.ident_on_error_suffix(reader)");
        operator_sfx_check.create_rule(&Pattern::always(),"self.ident_on_no_error_suffix(reader)");
    }
}


// === Identifiers ===

/// Lexing rules for Enso identifiers.
#[allow(dead_code)]
impl EnsoLexer {

    /// Create a variable identifier from the current match.
    fn on_variable_ident<R:LazyReader>(&mut self, _reader:&mut R) {
        let token        = Token::Variable(self.consume_current(),self.offset.consume());
        let suffix_check = self.ident_suffix_check;
        self.append_token(token);
        self.push_state(suffix_check);
    }

    /// Create a referent identifier from the current match.
    fn on_referent_ident<R:LazyReader>(&mut self, _reader:&mut R) {
        let token        = Token::Referent(self.consume_current(),self.offset.consume());
        let suffix_check = self.ident_suffix_check;
        self.append_token(token);
        self.push_state(suffix_check);
    }

    /// Create an external identifier from the current match.
    fn on_external_ident<R:LazyReader>(&mut self, _reader:&mut R) {
        let token        = Token::External(self.consume_current(),self.offset.consume());
        let suffix_check = self.ident_suffix_check;
        self.append_token(token);
        self.push_state(suffix_check);
    }

    /// Create a blank identifier from the current match.
    fn on_blank<R:LazyReader>(&mut self, _reader:&mut R) {
        let token        = Token::Blank(self.offset.consume());
        let suffix_check = self.ident_suffix_check;
        self.discard_current();
        self.append_token(token);
        self.push_state(suffix_check);
    }

    /// Tokenize an unexpected error suffix.
    fn ident_on_error_suffix<R:LazyReader>(&mut self, _reader:&mut R) {
        let token = Token::InvalidSuffix(self.consume_current(),self.offset.consume());
        self.append_token(token);
        self.pop_state();
    }

    /// Submit a non-error identifier.
    fn ident_on_no_error_suffix<R:LazyReader>(&mut self, _reader:&mut R) {
        self.pop_state();
    }

    /// The set of rules for lexing Enso identifiers.
    fn add_identifier_rules(lexer:&mut EnsoLexer) {
        let body_char      = (EnsoLexer::lower_ascii_letter() | EnsoLexer::ascii_digit()).many();
        let underscore     = c!('_');
        let ticks          = c!('\'').many();
        let init_var_seg   = EnsoLexer::lower_ascii_letter() >> &body_char;
        let var_seg        = (EnsoLexer::lower_ascii_letter() | EnsoLexer::ascii_digit()) >> &body_char;
        let init_ref_seg   = EnsoLexer::upper_ascii_letter() >> &body_char;
        let ref_seg        = (EnsoLexer::upper_ascii_letter() | EnsoLexer::ascii_digit()) >> &body_char;
        let external_start = EnsoLexer::ascii_letter() | &underscore;
        let external_body  = EnsoLexer::ascii_alpha_num() | &underscore;
        let variable_ident = &init_var_seg >> (&underscore >> &var_seg).many() >> &ticks;
        let referent_ident = &init_ref_seg >> (&underscore >> &ref_seg).many() >> &ticks;
        let external_ident = &external_start >> external_body.many() >> &ticks;
        let error_suffix   = Pattern::none_of(EnsoLexer::break_chars().as_str()).many1();

        let initial_state_id = lexer.initial_state;
        let initial_state    = lexer.group_mut(initial_state_id);
        initial_state.create_rule(&variable_ident,"self.on_variable_ident(reader)");
        initial_state.create_rule(&referent_ident,"self.on_referent_ident(reader)");
        initial_state.create_rule(&underscore,    "self.on_blank(reader)");
        initial_state.create_rule(&external_ident,"self.on_external_ident(reader)");

        let suffix_check_id = lexer.ident_suffix_check;
        let suffix_check    = lexer.group_mut(suffix_check_id);
        suffix_check.create_rule(&error_suffix,     "self.ident_on_error_suffix(reader)");
        suffix_check.create_rule(&Pattern::always(),"self.ident_on_no_error_suffix(reader)");
    }
}


// === Numbers ===

/// The set of rules for lexing numbers in Enso.
#[allow(dead_code)]
impl EnsoLexer {

    /// Finalize the lexer when it's done lexing a number with an explicit base.
    fn finalize_explicit_base(&mut self) {
        let number_part_2 = self.number_phase_two;
        self.pop_states_including(number_part_2);
        self.number_state.reset();
    }

    /// Triggered when the lexer matches an integer with an implicit base.
    fn on_integer<R:LazyReader>(&mut self, _reader:&mut R) {
        let number_phase_2 = self.number_phase_two;
        self.number_state.literal = self.consume_current();
        self.push_state(number_phase_2)
    }

    /// Triggered when the lexer matches a number annotated with an explicit base.
    fn on_explicit_base<R:LazyReader>(&mut self, _reader:&mut R) {
        let literal               = self.consume_current();
        self.number_state.literal = literal;
        let offset                = self.offset.consume();
        let token                 = self.number_state.consume_token(offset);
        self.append_token(token);
        self.finalize_explicit_base();
    }

    /// Triggered when the lexer has seen an explicit base definition that isn't followed by an
    /// actual number.
    fn on_dangling_base<R:LazyReader>(&mut self, _reader:&mut R) {
        let base   = self.number_state.consume_base();
        let offset = self.offset.consume();
        let token  = Token::DanglingBase(base,offset);
        self.append_token(token);
        self.discard_current();
        self.finalize_explicit_base();
    }

    /// Triggered when an explicit decimal number has been seen by the lexer.
    fn on_decimal<R:LazyReader>(&mut self, _reader:&mut R) {
        let decimal_suffix_check  = self.decimal_suffix_check;
        self.number_state.literal = self.consume_current();
        let offset                = self.offset.consume();
        let token                 = self.number_state.consume_token(offset);
        self.append_token(token);
        self.push_state(decimal_suffix_check);
    }

    /// Triggered when an explicit base annotation has been seen by the lexer.
    fn seen_base<R:LazyReader>(&mut self, _reader:&mut R) {
        let seen_base_id = self.number_seen_base;
        self.push_state(seen_base_id);
        self.number_state.swap_members();
    }

    /// Submit an integer token into the lexer.
    fn submit_integer<R:LazyReader>(&mut self, _reader:&mut R) {
        let offset = self.offset.consume();
        let token  = self.number_state.consume_token(offset);
        self.append_token(token);
        self.pop_state();
    }

    /// Triggered when a decimal number is followed by an erroneous suffix.
    fn decimal_error_suffix<R:LazyReader>(&mut self, _reader:&mut R) {
        let decimal_suffix_check = self.decimal_suffix_check;
        let current_match        = self.consume_current();
        let offset               = self.offset.consume();
        let token                = Token::InvalidSuffix(current_match,offset);
        self.append_token(token);
        self.pop_states_including(decimal_suffix_check);
    }

    /// Triggered when a decimal number is followed by a valid suffix.
    fn decimal_valid_suffix<R:LazyReader>(&mut self, _reader:&mut R) {
        let seen_decimal_id = self.decimal_suffix_check;
        self.pop_states_including(seen_decimal_id);
    }

    /// The rules for lexing numbers in Enso.
    fn add_number_rules(lexer:&mut EnsoLexer) {
        let digits            = EnsoLexer::ascii_digit().many1();
        let point             = c!('.');
        let underscore        = c!('_');
        let decimal           = &digits >> &point >> &digits;
        let arbitrary_digits  = EnsoLexer::ascii_alpha_num().many1();
        let arbitrary_decimal = &arbitrary_digits >> (&point >> &arbitrary_digits).opt();
        let error_suffix      = Pattern::none_of(EnsoLexer::break_chars().as_str()).many1();

        let initial_state_id = lexer.initial_state;
        let initial_state    = lexer.group_mut(initial_state_id);
        initial_state.create_rule(&digits,"self.on_integer(reader)");
        initial_state.create_rule(&decimal,"self.on_decimal(reader)");

        let number_phase_2_id = lexer.number_phase_two;
        let number_phase_2    = lexer.groups_mut().group_mut(number_phase_2_id);
        number_phase_2.create_rule(&underscore,       "self.seen_base(reader)");
        number_phase_2.create_rule(&Pattern::always(),"self.submit_integer(reader)");

        let seen_base_id = lexer.number_seen_base;
        let seen_base    = lexer.groups_mut().group_mut(seen_base_id);
        seen_base.create_rule(&arbitrary_decimal,"self.on_explicit_base(reader)");
        seen_base.create_rule(&Pattern::always(),"self.on_dangling_base(reader)");

        let decimal_suffix_check_id = lexer.decimal_suffix_check;
        let decimal_suffix_check    = lexer.groups_mut().group_mut(decimal_suffix_check_id);
        decimal_suffix_check.create_rule(&error_suffix,"self.decimal_error_suffix(reader)");
        decimal_suffix_check.create_rule(&Pattern::always(),"self.decimal_valid_suffix(reader)");
    }
}


// === Text Rules ===

/// The set of rules for lexing text literals in the Enso language.
#[allow(dead_code)]
impl EnsoLexer {

    /// Define the rules for lexing Enso text literals.
    fn add_text_rules(_lexer:&mut EnsoLexer) {
        // TODO [AA] Write the lexing rules for text literals.
    }
}


// === Block Rules ===

/// The set of rules for lexing blocks in the Enso language.
#[allow(dead_code)]
impl EnsoLexer {

    /// Triggered when a unix-style line ending is seen.
    fn block_on_lf<R:LazyReader>(&mut self, reader:&mut R) {
        self.block_state.push_line_ending(token::LineEnding::LF);
        self.block_on_line_ending(reader);
    }

    /// Triggered when a windows-style line ending is seen.
    fn block_on_crlf<R:LazyReader>(&mut self, reader:&mut R) {
        self.block_state.push_line_ending(token::LineEnding::CRLF);
        self.block_on_line_ending(reader);
    }

    /// Common functionality for both styles of line ending.
    fn block_on_line_ending<R:LazyReader>(&mut self, _reader:&mut R) {
        let block_newline             = self.block_newline;
        self.block_state.seen_newline = true;
        self.offset.push();
        self.push_state(block_newline);
    }

    /// Transitions the lexer into a state in which it knows it is lexing a block line.
    fn block_in_line<R:LazyReader>(&mut self, _reader:&mut R) {
        let indent_len = self.current_match.chars().count();
        self.offset.increase(indent_len,0);
        let in_block_line = self.in_block_line;
        self.push_state(in_block_line);
    }

    /// Triggered when lexing a non-blank line.
    fn block_on_non_empty_line<R:LazyReader>(&mut self, reader:&mut R) {
        let block_newline = self.block_newline;
        self.pop_states_including(block_newline);

        match self.offset.current.cmp(&self.block_state.current().indent) {
            Ordering::Equal => {
                self.offset.consume();
                self.block_submit_line(reader);
            },
            Ordering::Greater => {
                let new_indent = self.offset.consume();
                self.begin_block(new_indent,reader);
            },
            Ordering::Less => {
                let new_indent = self.offset.consume();
                self.on_block_end(new_indent,reader);
            }
        }
    }

    /// Triggered when lexing a block line that is empty and ends in a unix-style line ending.
    fn block_on_empty_lf_line<R:LazyReader>(&mut self, reader:&mut R) {
        self.block_state.push_line_ending(token::LineEnding::LF);
        self.block_in_empty_line(reader);
    }

    /// Triggered when lexing a block line that is empty and ends in a windows-style line ending.
    fn block_on_empty_crlf_line<R:LazyReader>(&mut self, reader:&mut R) {
        self.block_state.push_line_ending(token::LineEnding::CRLF);
        self.block_in_empty_line(reader);
    }

    /// Begin a new block.
    fn begin_block<R:LazyReader>(&mut self, block_indent:usize, _reader:&mut R) {
        let is_orphan = self.output.is_empty();
        self.push_tokens();
        self.block_state.begin_block(block_indent,is_orphan);
    }

    /// Triggered when lexing an empty line in a block.
    fn block_in_empty_line<R:LazyReader>(&mut self, reader:&mut R) {
        self.block_submit_line(reader);
        let offset        = self.offset.consume();
        let block_newline = self.block_newline;
        self.pop_states_until(block_newline);
        self.block_state.push_empty_line(offset);
    }

    /// Triggered when lexing a line in a block that ends a file.
    fn block_in_eof_line<R:LazyReader>(&mut self, reader:&mut R) {
        let initial_state = self.initial_state;
        self.pop_states_until(initial_state);
        self.on_eof(reader);
    }

    /// Triggered when beginning a top-level block.
    fn block_begin_top_level<R:LazyReader>(&mut self, reader:&mut R) {
        let matched_bookmark = self.bookmarks.matched_bookmark;
        let block_newline    = self.block_newline;
        let initial_state = self.initial_state;
        self.bookmarks.rewind(matched_bookmark,reader);
        self.offset.push();
        self.pop_states_until(initial_state);
        self.push_state(block_newline);
    }

    /// Triggered when a block is ended.
    fn on_block_end<R:LazyReader>(&mut self, new_indent:usize, reader:&mut R) {
        if self.block_state.seen_newline {
            while new_indent < self.block_state.current().indent {
                self.block_submit(reader);
            }
            if new_indent > self.block_state.current().indent {
                info!(self.logger,"Block with invalid indentation.");
                self.begin_block(new_indent, reader);
                self.block_state.current_mut().is_valid = false;
            } else {
                self.offset.push();
                self.block_submit_line(reader);
            }
        }
    }

    /// Create a block token from the current block state.
    fn build_block<R:LazyReader>(&mut self, reader:&mut R) -> Token {
        self.block_submit_line(reader);
        let offset        = self.offset.consume();
        let current_block = self.block_state.consume_current();
        current_block.into_token(offset)
    }

    /// Submit a block to the token stream of the lexer.
    fn block_submit<R:LazyReader>(&mut self, reader:&mut R) {
        let mut block = self.build_block(reader);
        self.pop_tokens();
        self.offset.pop();
        self.block_state.end_block();

        if let Some(Token{shape:token::Shape::Operator(_),..}) = self.last_token() {
            if let token::Shape::Block {indent,lines,..} = block.shape {
                block.shape = token::Shape::block(BlockType::Discontinuous,indent,lines);
            }
        }

        self.append_token(block);
        self.offset.push();
    }

    /// Submit a line in a block.
    ///
    /// It should be noted that lines that have content in blocks cannot have an offset.
    fn block_submit_line<R:LazyReader>(&mut self, _reader:&mut R) {
        if self.block_state.seen_newline {
            if !self.output.is_empty() {
                let token_stream = self.consume_tokens();
                let offset       = 0;
                self.block_state.append_line_to_current(token_stream.into(),offset);
            }
            debug!(self.logger,"Clear Output Buffer: Old Length = {self.output.len()}");
            self.output.clear();
        }
    }

    /// Triggered when the top-level block ends.
    fn block_end_top_level<R:LazyReader>(&mut self, _reader:&mut R) {
        let current_block = self.block_state.consume_current();
        if self.block_state.seen_newline {
            let offset          = self.offset.consume();
            let top_level_block = current_block.into_token(offset);
            self.append_token(top_level_block);
        } else {
            let additional_offset = current_block.indent;
            if let Some(token) = self.output.first_mut() { token.offset += additional_offset }
        }
    }

    /// The rule definitions for lexing blocks in Enso.
    fn add_block_rules(lexer:&mut EnsoLexer) {
        let spaces     = EnsoLexer::spaces();
        let lf         = c!('\n');
        let crlf       = l!("\r\n");
        let opt_spaces = spaces.opt();
        let eof_line   = &opt_spaces >> Pattern::eof();

        let root_state_id = lexer.initial_state;
        let root_state    = lexer.group_mut(root_state_id);
        root_state.create_rule(&lf,  "self.block_on_lf(reader)");
        root_state.create_rule(&crlf,"self.block_on_crlf(reader)");

        let block_newline_id = lexer.block_newline;
        let block_newline    = lexer.group_mut(block_newline_id);
        block_newline.create_rule(&opt_spaces,"self.block_in_line(reader)");
        block_newline.create_rule(&eof_line,  "self.block_in_eof_line(reader)");

        let in_block_line_id = lexer.in_block_line;
        let in_block_line    = lexer.group_mut(in_block_line_id);
        in_block_line.create_rule(&lf,               "self.block_on_empty_lf_line(reader)");
        in_block_line.create_rule(&crlf,             "self.block_on_empty_crlf_line(reader)");
        in_block_line.create_rule(&Pattern::always(),"self.block_on_non_empty_line(reader)");

        let block_module_id = lexer.block_top_level;
        let block_module    = lexer.group_mut(block_module_id);
        block_module.create_rule(&opt_spaces,"self.block_begin_top_level(reader)");
    }
}


// === Default Rules ===

/// The set of rules that apply as defaults in the root state.
#[allow(dead_code)]
impl EnsoLexer {

    /// Triggered on an arbitrary space character.
    fn on_space<R:LazyReader>(&mut self, _reader:&mut R) {
        let current_len = self.current_match.chars().count();
        self.offset.increase(current_len,0);
        self.discard_current();
    }

    /// Triggered on an arbitrary eof character.
    fn on_eof<R:LazyReader>(&mut self, reader:&mut R) {
        self.offset.push();
        self.block_submit_line(reader);
        self.on_block_end(0,reader);
        self.block_end_top_level(reader);
    }

    /// Triggered on any unrecognized character.
    fn on_unrecognized<R:LazyReader>(&mut self, _reader:&mut R) {
        let token = Token::Unrecognized(self.consume_current(),self.offset.consume());
        self.append_token(token);
    }

    /// The default rules for the lexer.
    fn add_default_rules(lexer:&mut EnsoLexer) {
        let space = Pattern::char(' ');
        let eof   = Pattern::eof();
        let any   = Pattern::any();

        let initial_state_id = lexer.initial_state;
        let initial_state    = lexer.group_mut(initial_state_id);
        initial_state.create_rule(&space,"self.on_space(reader)");
        initial_state.create_rule(&eof,  "self.on_eof(reader)");
        initial_state.create_rule(&any,  "self.on_unrecognized(reader)");
    }
}



// === Trait Impls ===

impl flexer::Definition for EnsoLexer {
    fn define() -> Self {
        let mut lexer = EnsoLexer::new();

        EnsoLexer::add_operator_rules(&mut lexer);
        EnsoLexer::add_identifier_rules(&mut lexer);
        EnsoLexer::add_number_rules(&mut lexer);
        EnsoLexer::add_text_rules(&mut lexer);
        EnsoLexer::add_block_rules(&mut lexer);
        EnsoLexer::add_default_rules(&mut lexer);

        lexer
    }

    fn groups(&self) -> &Registry {
        &self.lexer_states
    }

    fn set_up(&mut self) {
        let module_state_id = self.block_top_level;
        self.push_state(module_state_id);
    }

    fn tear_down(&mut self) {}
}

impl Default for EnsoLexer {
    fn default() -> Self {
        EnsoLexer::new()
    }
}



// ===================
// === Lexer State ===
// ===================

/// The state for the Enso lexer.
#[derive(Debug)]
pub struct State<Logger> {
    /// The logger for the lexing state.
    logger : Logger,
    /// The bookmarks used by the lexer.
    bookmarks : reader::BookmarkManager,
    /// The registry of states for the lexer.
    lexer_states : group::Registry,
    /// The initial state of the lexer.
    initial_state : group::Identifier,
    /// The state for checking the end of identifiers.
    ident_suffix_check : group::Identifier,
    /// The state for completing number lexing.
    number_phase_two : group::Identifier,
    /// The state where number lexing has seen an explicit base.
    number_seen_base : group::Identifier,
    /// The state where number lexing has seen a decimal.
    decimal_suffix_check : group::Identifier,
    /// The state for lexing operator suffixes.
    operator_suffix_check : group::Identifier,
    /// The state for lexing operator modifiers.
    operator_modifier_check : group::Identifier,
    /// The state for lexing the top-level block.
    block_top_level : group::Identifier,
    /// The state entered when a newline has been lexed.
    block_newline : group::Identifier,
    /// The state entered when within the line of a block.
    in_block_line : group::Identifier,
    /// A stack of token matches.
    tokens_stack : Vec<token::Stream>,
    /// Tracking for the current offset.
    offset : Offset<Logger>,
    /// State specifically for lexing Enso numbers.
    number_state : NumberLexingState<Logger>,
    /// State specifically for lexing Enso blocks.
    block_state : BlockLexingState<Logger>
}

impl<Logger:AnyLogger<Owned=Logger>> State<Logger> {
    /// Get a reference to the group for the provided identifier.
    pub fn group(&self, group:group::Identifier) -> &Group {
        self.groups().group(group)
    }

    /// Get a mutable reference to the group for the provided identifier.
    pub fn group_mut(&mut self, group:group::Identifier) -> &mut Group {
        self.groups_mut().group_mut(group)
    }
}


// === Trait Impls ===

impl<Logger:AnyLogger<Owned=Logger>> flexer::State for State<Logger> {
    fn new(parent_logger:&impl AnyLogger) -> Self {
        let logger                  = <Logger>::sub(parent_logger, "State");
        let bookmarks               = default();
        let mut lexer_states        = group::Registry::default();
        let initial_state           = lexer_states.define_group("ROOT",None);
        let ident_suffix_check      = lexer_states.define_group("IDENT_SFX_CHECK",None);
        let number_phase_two        = lexer_states.define_group("NUMBER_PHASE_2",None);
        let number_seen_base        = lexer_states.define_group("NUMBER_SEEN_BASE",None);
        let decimal_suffix_check    = lexer_states.define_group("NUMBER_SEEN_POINT",None);
        let operator_suffix_check   = lexer_states.define_group("OPERATOR_SUFFIX_CHECK",None);
        let operator_modifier_check =
            lexer_states.define_group("OPERATOR_MODIFIER_CHECK",Some(operator_suffix_check));
        let block_top_level     = lexer_states.define_group("BLOCK_MODULE", None);
        let block_newline       = lexer_states.define_group("BLOCK_NEWLINE",None);
        let in_block_line       = lexer_states.define_group("IN_BLOCK_LINE",None);
        let tokens_stack        = Vec::new();
        let offset_logger       = <Logger>::sub(&logger,"Offset");
        let offset              = Offset::new(offset_logger);
        let number_state_logger = <Logger>::sub(&logger,"NumberState");
        let number_state        = NumberLexingState::new(number_state_logger);
        let block_state_logger  = <Logger>::sub(&logger,"BlockLexingState");
        let block_state         = BlockLexingState::new(block_state_logger);

        Self
        { logger
        , bookmarks
        , lexer_states
        , initial_state
        , ident_suffix_check
        , number_phase_two
        , number_seen_base
        , decimal_suffix_check
        , operator_suffix_check
        , operator_modifier_check
        , block_top_level
        , block_newline
        , in_block_line
        , tokens_stack
        , offset
        , number_state
        , block_state
        }
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

    fn bookmarks(&self) -> &reader::BookmarkManager {
        &self.bookmarks
    }

    fn bookmarks_mut(&mut self) -> &mut reader::BookmarkManager {
        &mut self.bookmarks
    }

    fn specialize(&self) -> Result<String, GenError> {
        generate::specialize(self,"EnsoLexer","token::Stream")
    }
}



// =========================
// === Offset Management ===
// =========================

/// A manager for the current offset state of the lexer.
#[derive(Clone,Debug,Eq,PartialEq)]
pub struct Offset<Logger> {
    /// The current offset of the lexer.
    ///
    /// The offset is the number of leading spaces between the last-lexed token and the token that
    /// is currently being lexed.
    current : usize,
    /// The stack of current offsets in the lexer.
    stack : Vec<usize>,
    /// The logger for the offset state.
    logger : Logger
}

impl<Logger:AnyLogger> Offset<Logger> {
    /// Create a new offset state.
    pub fn new(logger:Logger) -> Self {
        let current = default();
        let stack   = default();
        Offset{current,stack,logger}
    }

    /// Push the current offset onto the offset stack.
    pub fn push(&mut self) {
        debug!(self.logger,"Push Offset: {self.current}");
        self.stack.push(self.current);
        self.current = 0;
    }

    /// Pop the top offset from the offset stack.
    pub fn pop(&mut self) {
        self.current = self.stack.pop().unwrap_or(0);
        debug!(self.logger,"Pop Offset: {self.current}");
    }

    /// Consume the current offset.
    pub fn consume(&mut self) -> usize {
        let offset   = self.current;
        self.current = 0;
        debug!(self.logger,"Consume Offset: {offset}");
        offset
    }

    /// Increase the current offset by `match_length` + `shift`.
    pub fn increase(&mut self, match_length:usize, shift:usize) {
        let diff = match_length + shift;
        debug!(self.logger,"Increase Offset By: {diff}");
        self.current += diff;
        debug!(self.logger,"Offset Now: {self.current}");
    }
}



// =========================
// === NumberLexingState ===
// =========================

/// The state for lexing an Enso number.
#[derive(Clone,Debug,Default,PartialEq,Eq)]
pub struct NumberLexingState<Logger> {
    /// The (optional) base for the number.
    pub base : String,
    /// The literal number, to be interpreted in `base`.
    pub literal : String,
    /// A logger for the number state.
    logger : Logger,
}

impl<Logger:AnyLogger> NumberLexingState<Logger> {
    /// Create a new number lexing state.
    pub fn new(logger:Logger) -> Self {
        let base = default();
        let literal = default();
        NumberLexingState{base,literal,logger}
    }

    /// Reset the number lexing state.
    pub fn reset(&mut self) {
        self.base.truncate(0);
        self.literal.truncate(0);
        debug!(self.logger,"Reset Number State");
    }

    /// Swap the `base` and `literal` in place.
    pub fn swap_members(&mut self) {
        debug!(self.logger,"Swap Number Fields");
        mem::swap(&mut self.base,&mut self.literal);
    }

    /// Convert `self` into a token, resetting the lexing state.
    pub fn consume_token(&mut self, offset:usize) -> Token {
        debug!(self.logger,"Consuming Number: Base = {self.base}, Number = {self.literal}");
        Token::Number(mem::take(&mut self.base),mem::take(&mut self.literal),offset)
    }

    /// Take the `literal` portion of the number lexing state.
    pub fn consume_literal(&mut self) -> String {
        mem::take(&mut self.literal)
    }

    /// Take the `base` portion of the number lexing state.
    pub fn consume_base(&mut self) -> String {
        mem::take(&mut self.base)
    }
}



// ========================
// === BlockLexingState ===
// ========================

/// The state for managing the lexing of blocks in Enso.
#[derive(Clone,Debug,PartialEq)]
pub struct BlockLexingState<Logger> {
    /// The stack of blocks being lexed.
    stack : NonEmptyVec<BlockState>,
    /// Whether or not the lexer has seen an explicit newline.
    seen_newline : bool,
    /// A logger for the lexing state.
    logger : Logger,
}

impl<Logger:AnyLogger> BlockLexingState<Logger> {
    /// Construct a new block lexing state.
    pub fn new(logger:Logger) -> Self {
        let stack        = NonEmptyVec::singleton(default());
        let seen_newline = false;
        BlockLexingState{stack,seen_newline,logger}
    }

    /// Set the last seen line ending.
    pub fn push_line_ending(&mut self, line_ending:token::LineEnding) {
        self.current_mut().seen_line_endings.push_back(line_ending);
        debug!(self.logger,"Push Line Ending: {line_ending:?}");
    }

    /// Consume the last seen line ending.
    pub fn pop_line_ending(&mut self) -> token::LineEnding {
        let popped = self.current_mut().seen_line_endings.pop_front();
        debug!(self.logger,"Pop Line Ending: {popped:?}");
        popped.unwrap_or(token::LineEnding::None)
    }

    /// Appends a line to the current block.
    pub fn append_line_to_current(&mut self, tokens:Vec<Token>, offset:usize) {
        let trailing_line_ending = self.pop_line_ending();
        debug!(
            self.logger,
            "Append Line: Line Ending = {trailing_line_ending:?}, Tokens = {&tokens:?}"
        );
        self.current_mut().push_line(tokens, offset, trailing_line_ending);
    }

    /// Get a reference to the current block.
    pub fn current(&self) -> &BlockState {
        self.stack.last()
    }

    /// Get a mutable reference to the current block.
    pub fn current_mut(&mut self) -> &mut BlockState {
        self.stack.last_mut()
    }

    /// Push a new block state onto the stack.
    pub fn begin_block(&mut self, new_offset:usize, is_orphan:bool) {
        debug!(self.logger,"Begin Block State: Indent = {new_offset}");
        self.stack.push(default());
        self.current_mut().is_orphan = is_orphan;
        self.current_mut().indent = new_offset;
    }

    /// Pop a block state from the stack.
    pub fn end_block(&mut self) -> Option<BlockState> {
        debug!(self.logger,"End Block State");
        self.stack.pop()
    }

    /// Consume the state of the current block.
    pub fn consume_current(&mut self) -> BlockState {
        let block = mem::take(self.stack.last_mut());
        debug!(self.logger,"Consume Block: {&block:?}");
        block
    }

    /// Push an empty line into the storage for them.
    pub fn push_empty_line(&mut self, offset:usize) {
        let trailing_line_ending = self.pop_line_ending();
        self.current_mut().push_empty_line(offset, trailing_line_ending);
        debug!(self.logger,"Append Empty line: Line Ending = {trailing_line_ending:?}");
    }
}



// ==================
// === BlockState ===
// ==================

/// The state for lexing a given block in Enso.
#[derive(Clone,Debug,PartialEq,Eq)]
pub struct BlockState {
    /// Whether or not the block is orphaned.
    ///
    /// An orphaned block is one that has no block parent.
    pub is_orphan : bool,
    /// Whether or not the block is well-formed.
    pub is_valid : bool,
    /// The root indentation level of the block.
    pub indent: usize,
    /// The remaining lines of the block.
    pub lines : Vec<Token>,
    /// The line endings that have been seen in this block's context.
    pub seen_line_endings : VecDeque<token::LineEnding>
}

impl BlockState {
    /// Construct a new block state.
    pub fn new() -> Self {
        let is_orphan         = false;
        let is_valid          = true;
        let offset            = 0;
        let lines             = default();
        let seen_line_endings = default();
        BlockState{is_orphan,is_valid, indent: offset,lines,seen_line_endings}
    }

    /// Push a line into the block.
    pub fn push_line
    (&mut self
     , tokens               : Vec<Token>
     , indent: usize
     , trailing_line_ending : token::LineEnding
    ) {
        let line = Token::Line(tokens,indent,trailing_line_ending);
        self.lines.push(line)
    }

    /// Push a blank line into the block.
    ///
    /// The offset here should be the offset from the baseline, not from the block indent level.
    pub fn push_empty_line(&mut self, offset:usize, trailing_line_ending:token::LineEnding) {
        let line = Token::BlankLine(offset, trailing_line_ending);
        self.lines.push(line);
    }

    /// Convert the block state into a block token.
    pub fn into_token(self, offset:usize) -> Token {
        Token::Block(
            BlockType::Continuous,
            self.indent,
            self.lines,
            offset
        )
    }

    /// Consume the lines in the block.
    pub fn consume_lines(&mut self) -> Vec<Token> {
        mem::take(&mut self.lines)
    }
}


// === Trait Impls ===

impl Default for BlockState {
    fn default() -> Self {
        BlockState::new()
    }
}
