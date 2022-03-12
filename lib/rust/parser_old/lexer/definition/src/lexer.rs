//! This module contains the definition of the lexer for the Enso programming language.
//!
//! Due to the architecture of the flexer, the lexer functions can't be separated into modules by
//! their responsibility, and therefore can't be namespaced. Despite this, the convention for this
//! lexer is that their `trace!` messages _should be_. This is simply because it makes it easier
//! to spot functions dealing with a certain class of lexemes in the logging output.

use crate::prelude::*;
use enso_flexer::*;

use crate::library::escape;
use crate::library::escape::EscapeSequence;
use crate::library::lexeme;
use crate::library::rules;
use crate::library::token;
use crate::library::token::BlockType;
use crate::library::token::Token;

use enso_flexer;
use enso_flexer::automata::pattern::Pattern;
use enso_flexer::automata::symbol::Symbol;
use enso_flexer::group::Group;
use enso_flexer::group::Registry;
use enso_flexer::prelude::logger::Disabled;
use enso_flexer::prelude::reader;
use enso_flexer::State as FlexerState;
use std::cmp::Ordering;
use std::collections::VecDeque;



// ====================
// === Type Aliases ===
// ====================

type Logger = Disabled;
type Flexer = enso_flexer::Flexer<State<Logger>, token::Stream, Logger>;
type DebugLevel = enso_flexer::prelude::logger::entry::level::Debug;



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
        let lexer = Flexer::new(logger);
        EnsoLexer(lexer)
    }
}


// === Result Functionality ===

impl EnsoLexer {
    /// Push the current token stream onto the stack.
    pub fn push_tokens(&mut self) {
        let current_stream = mem::take(&mut self.output);
        debug!(self.logger, "Push Tokens: {&current_stream:?}.");
        self.tokens_stack.push(current_stream);
    }

    /// Pop the top token stream from the stack and make it current.
    pub fn pop_tokens(&mut self) {
        let popped = self.tokens_stack.pop().unwrap_or_default();
        debug!(self.logger, "Pop Tokens: {&popped:?}.");
        self.output = popped;
    }

    /// Append the provided `token` to the lexer output.
    pub fn append_token(&mut self, token: Token) {
        debug!(self.logger, "Append: {&token:?}.");
        self.output.append(token);
        if self.block_state.has_delayed_lines() {
            let tokens = self.consume_tokens();
            self.block_state.append_line_to_current(tokens.into());
        }
    }

    /// Get a reference to the last token in the current lexer output.
    pub fn last_token(&mut self) -> Option<&Token> {
        self.output.last()
    }

    /// Consume the currently active stream of tokens.
    pub fn consume_tokens(&mut self) -> token::Stream {
        debug!(self.logger, "Consume Tokens: {&self.output:?}.");
        mem::take(&mut self.output)
    }

    /// Consume the current match and replace it with the empty string.
    pub fn consume_current(&mut self) -> String {
        debug!(self.logger, "Consume: {self.current_match:?}.");
        mem::take(&mut self.current_match)
    }

    /// Discard the current match and replace it with the empty string.
    pub fn discard_current(&mut self) {
        debug!(self.logger, "Discard: {self.current_match:?}.");
        self.current_match = default();
    }
}


// === Comments ===

/// The set of rules for lexing Enso comments.
#[allow(dead_code)]
impl EnsoLexer {
    // === Disable Comments ===

    /// Triggered when a disable comment is encountered.
    fn on_begin_disable_comment<R: ReaderOps>(&mut self, _reader: &mut R) {
        trace!(self.logger, "Comment::on_begin_disable_comment");
        let offset = self.offset.consume();
        let disable_comment = self.disable_comment;
        self.comment_state.set_offset(offset);
        self.push_state(disable_comment);
    }

    /// Accumulate the disable comment contents.
    fn on_build_disable_comment<R: ReaderOps>(&mut self, _reader: &mut R) {
        trace!(self.logger, "Comment::on_build_disable_comment");
        let current = self.consume_current();
        self.comment_state.append_to_line(current);
    }

    /// Triggered when a disable comment is ended.
    fn on_end_disable_comment<R: ReaderOps>(
        &mut self,
        reader: &mut R,
        line_end: token::LineEnding,
    ) {
        trace!(self.logger, "Comment::on_end_disable_comment");
        self.discard_current();
        let disable_comment = self.disable_comment;
        let comment = self.comment_state.consume_current();
        let token = comment.into();
        self.append_token(token);
        self.pop_states_including(disable_comment);
        match line_end {
            token::LineEnding::None => self.on_eof(reader),
            _ => {
                let block_seen_newline = self.block_newline;
                self.block_state.push_line_ending(line_end);
                self.block_submit_line(reader);
                self.push_state(block_seen_newline);
            }
        }
    }


    // === Doc Comments ===

    /// Triggered when starting a doc comment.
    fn on_begin_doc_comment<R: ReaderOps>(&mut self, _reader: &mut R) {
        trace!(self.logger, "Comment::on_begin_doc_comment");
        let offset = self.offset.consume();
        let indent = self.consume_current().chars().count();
        let doc_comment = self.doc_comment;
        self.comment_state.set_indent(indent);
        self.comment_state.set_offset(offset);
        self.push_state(doc_comment);
    }

    /// Accumulate the contents of the current doc comment line.
    fn on_build_doc_comment_line<R: ReaderOps>(&mut self, _reader: &mut R) {
        trace!(self.logger, "Comment::on_build_doc_comment_line");
        let current = self.consume_current();
        self.comment_state.append_to_line(current);
    }

    /// Triggered when a line is ended in a doc comment.
    fn on_doc_comment_end_of_line<R: ReaderOps>(
        &mut self,
        reader: &mut R,
        line_ending: token::LineEnding,
    ) {
        trace!(self.logger, "Comment::on_doc_comment_end_of_line");
        self.comment_state.submit_line(line_ending);
        match line_ending {
            token::LineEnding::None => {
                let matched_bookmark = self.bookmarks.matched_bookmark;
                self.bookmarks.rewind(matched_bookmark, reader);
                self.on_end_doc_comment(reader);
                self.on_eof(reader);
            }
            _ => {
                let comment_newline = self.doc_comment_newline;
                self.push_state(comment_newline);
            }
        }
    }

    /// Triggered when ending a doc comment.
    fn on_end_doc_comment<R: ReaderOps>(&mut self, _reader: &mut R) {
        trace!(self.logger, "Comment::on_end_doc_comment");
        let doc_comment = self.doc_comment;
        let mut comment = self.comment_state.consume_current();
        let blank_lines = comment.consume_blank_lines();
        if !blank_lines.is_empty() {
            self.block_state.seen_newline = true;
        }
        self.block_state.delayed_append_lines.extend(blank_lines);
        let comment_token = comment.into();
        self.append_token(comment_token);
        self.pop_states_including(doc_comment);
    }

    /// Triggered when a new line is discovered in a doc comment.
    fn doc_comment_on_new_line<R: ReaderOps>(&mut self, reader: &mut R) {
        trace!(self.logger, "Comment::doc_comment_on_new_line");
        self.pop_state();
        let indent = self.consume_current().chars().count();
        let comment_indent = self.comment_state.current_comment.indent;
        if indent < comment_indent {
            self.on_end_doc_comment(reader);
            self.block_on_newline(reader, token::LineEnding::None);
        } else if (indent - comment_indent) > 0 {
            let remaining_indent = lexeme::literal::SPACE.repeat(indent - comment_indent);
            self.comment_state.append_to_line(remaining_indent);
        }
    }

    /// Triggered when an empty line is discovered in a doc comment.
    fn doc_comment_on_empty_line<R: ReaderOps>(
        &mut self,
        reader: &mut R,
        line_ending: token::LineEnding,
    ) {
        trace!(self.logger, "Comment::doc_comment_on_empty_line");
        let current = self.consume_current();
        let indent = current.chars().count() - line_ending.size();
        self.comment_state.submit_blank_line(indent, line_ending);
        if let token::LineEnding::None = line_ending {
            let block_newline = self.block_newline;
            self.on_end_doc_comment(reader);
            self.push_state(block_newline);
            self.on_eof(reader);
        }
    }


    // === Comment Rules ===

    /// The rules for lexing Enso comments.
    fn add_comment_rules(lexer: &mut EnsoLexer) {
        let comment_char = lexeme::into_pattern(lexeme::literal::COMMENT);
        let lf = lexeme::into_pattern(lexeme::literal::LF);
        let crlf = lexeme::into_pattern(lexeme::literal::CRLF);
        let eof = lexeme::definition_pattern::eof();
        let spaces = lexeme::definition_pattern::spaces();
        let doc_comment_start = lexeme::into_pattern(lexeme::literal::DOC_COMMENT) >> &spaces.opt();
        let eof_newline = &spaces.opt() >> &eof;
        let empty_line_lf = &spaces.opt() >> &lf;
        let empty_line_crlf = &spaces.opt() >> &crlf;
        let any = Pattern::any();


        // === Initial State Rules ===

        let initial_state_id = lexer.initial_state;
        let initial_state = lexer.group_mut(initial_state_id);
        rules!(initial_state with
            doc_comment_start => self.on_begin_doc_comment(),
            comment_char      => self.on_begin_disable_comment(),
        );


        // === Disable Comment Rules ===

        let disable_comment_id = lexer.disable_comment;
        let disable_comment = lexer.group_mut(disable_comment_id);
        rules!(disable_comment with
            lf   => self.on_end_disable_comment(token::LineEnding::LF),
            crlf => self.on_end_disable_comment(token::LineEnding::CRLF),
            eof  => self.on_end_disable_comment(token::LineEnding::None),
            any  => self.on_build_disable_comment(),
        );


        // === Doc Comment Rules ===

        let doc_comment_id = lexer.doc_comment;
        let doc_comment = lexer.group_mut(doc_comment_id);
        rules!(doc_comment with
            lf   => self.on_doc_comment_end_of_line(token::LineEnding::LF),
            crlf => self.on_doc_comment_end_of_line(token::LineEnding::CRLF),
            eof  => self.on_doc_comment_end_of_line(token::LineEnding::None),
            any  => self.on_build_doc_comment_line(),
        );


        // === Newline Handling in Doc Comments ===

        let doc_comment_newline_id = lexer.doc_comment_newline;
        let doc_comment_newline = lexer.group_mut(doc_comment_newline_id);
        rules!(doc_comment_newline with
            spaces.opt()    => self.doc_comment_on_new_line(),
            empty_line_lf   => self.doc_comment_on_empty_line(token::LineEnding::LF),
            empty_line_crlf => self.doc_comment_on_empty_line(token::LineEnding::CRLF),
            eof_newline     => self.doc_comment_on_empty_line(token::LineEnding::None),
        );
    }
}


// === Operators ===

/// The set of rules for lexing Enso operator identifiers.
#[allow(dead_code)]
impl EnsoLexer {
    /// Create an arbitrary operator that requires no special handling.
    fn on_operator<R: ReaderOps>(&mut self, _reader: &mut R) {
        trace!(self.logger, "Operator::on_operator");
        let op_modifier_check = self.operator_modifier_check;
        let operator = self.consume_current();
        let offset = self.offset.consume();
        let token = Token::operator(operator, offset);
        self.append_token(token);
        self.push_state(op_modifier_check);
    }

    /// Create an operator that cannot have an associated modifier.
    fn on_operator_no_modifier<R: ReaderOps>(&mut self, _reader: &mut R) {
        trace!(self.logger, "Operator::on_operator_no_modifier");
        let op_suffix_check = self.operator_suffix_check;
        let operator = self.consume_current();
        let offset = self.offset.consume();
        let token = Token::operator(operator, offset);
        self.append_token(token);
        self.push_state(op_suffix_check);
    }

    /// Create a grouping operator.
    fn on_group<R: ReaderOps>(&mut self, reader: &mut R) {
        trace!(self.logger, "Operator::on_group");
        let suffix_check = self.ident_suffix_check;
        let operator = self.consume_current();
        let offset = self.offset.consume();
        let token = Token::operator(operator, offset);
        self.append_token(token);
        self.push_state(suffix_check);
        self.ident_on_no_error_suffix(reader);
    }

    /// Create an operator modifier.
    fn on_modifier<R: ReaderOps>(&mut self, _reader: &mut R) {
        trace!(self.logger, "Operator::on_modifier");
        match self.output.pop() {
            Some(token) => match token.shape {
                token::Shape::Operator(name) => {
                    let new_token = Token::modifier(name, token.offset);
                    self.discard_current();
                    self.append_token(new_token);
                }
                _ => unreachable_panic!("The preceding token should always be an operator."),
            },
            None => unreachable_panic!("There should always be a preceding token."),
        }
    }

    /// Triggered when a dot operator is immediately followed by another (e.g. `.+` or `. ==`).
    fn on_dotted_operator<R: ReaderOps>(&mut self, _reader: &mut R) {
        trace!(self.logger, "Operator::on_dot_operator");
        let suffix_check = self.operator_suffix_check;
        let offset = self.offset.consume();
        let dotted_op = Token::operator(lexeme::literal::DOT, offset);
        let current_match = self.consume_current();
        let second_op_str = current_match.trim_start_matches(|c| {
            let space = lexeme::unsafe_char(lexeme::literal::SPACE);
            let dot = lexeme::unsafe_char(lexeme::literal::DOT);
            c == space || c == dot
        });
        let second_offset = current_match.chars().count() - second_op_str.chars().count() - 1;
        let second_op = Token::operator(second_op_str, second_offset);
        self.append_token(dotted_op);
        self.append_token(second_op);
        self.push_state(suffix_check);
    }

    /// The rules for lexing Enso operators.
    fn add_operator_rules(lexer: &mut EnsoLexer) {
        let operator_char = Pattern::any_of(lexeme::literal::OPERATOR_CHARS);
        let equals = lexeme::into_pattern(lexeme::literal::EQUALS);
        let comma = lexeme::into_pattern(lexeme::literal::COMMA);
        let dot = lexeme::into_pattern(lexeme::literal::DOT);
        let spaces = lexeme::definition_pattern::spaces();
        let error_char = &operator_char | &equals | &comma | &dot;
        let error_suffix = &error_char.many1();
        let operator_body = &operator_char.many1();
        let equals_comp = lexeme::into_pattern(lexeme::literal::EQUALS_COMP);
        let ge_op = lexeme::into_pattern(lexeme::literal::GE_OPERATOR);
        let le_op = lexeme::into_pattern(lexeme::literal::LE_OPERATOR);
        let not_equal = lexeme::into_pattern(lexeme::literal::NOT_EQUAL);
        let hash_eq = lexeme::into_pattern(lexeme::literal::HASH_EQ);
        let fat_arrow = lexeme::into_pattern(lexeme::literal::WIDE_ARROW);
        let ops_eq = &equals | &equals_comp | &ge_op | &le_op | &not_equal | &hash_eq | &fat_arrow;
        let ops_in = lexeme::into_pattern(lexeme::literal::OPERATOR_IN);
        let ops_dot = &dot
            | comma
            | lexeme::into_pattern(lexeme::literal::TWO_DOTS)
            | lexeme::into_pattern(lexeme::literal::THREE_DOTS);
        let dotted_op = &dot >> &spaces.opt() >> (operator_body | &ops_eq);
        let ops_group = Pattern::any_of(lexeme::literal::GROUP_CHARS);
        let ops_no_modifier = &ops_eq | &ops_dot | &ops_in;


        // === Initial State Rules for Operators ===

        let initial_state_id = lexer.initial_state;
        let initial_state = lexer.group_mut(initial_state_id);
        rules!(initial_state with
            operator_body   => self.on_operator(),
            dotted_op       => self.on_dotted_operator(),
            ops_no_modifier => self.on_operator_no_modifier(),
            ops_group       => self.on_group(),
        );


        // === Modifier Checking for Operators ===

        let operator_mod_check_id = lexer.operator_modifier_check;
        let operator_mod_check = lexer.group_mut(operator_mod_check_id);
        rules!(operator_mod_check with equals => self.on_modifier());


        // === Suffix Checking for Operators ===

        let operator_sfx_check_id = lexer.operator_suffix_check;
        let operator_sfx_check = lexer.group_mut(operator_sfx_check_id);
        rules!(operator_sfx_check with
            error_suffix      => self.ident_on_error_suffix(),
            Pattern::always() => self.ident_on_no_error_suffix(),
        );
    }
}


// === Identifiers ===

/// Lexing rules for Enso identifiers.
#[allow(dead_code)]
impl EnsoLexer {
    /// Create a variable identifier from the current match.
    fn on_variable_ident<R: ReaderOps>(&mut self, _reader: &mut R) {
        trace!(self.logger, "Identifier::on_variable_ident");
        let token = Token::variable(self.consume_current(), self.offset.consume());
        let suffix_check = self.ident_suffix_check;
        self.append_token(token);
        self.push_state(suffix_check);
    }

    /// Create a referent identifier from the current match.
    fn on_referent_ident<R: ReaderOps>(&mut self, _reader: &mut R) {
        trace!(self.logger, "Identifier::on_referent_ident");
        let token = Token::referent(self.consume_current(), self.offset.consume());
        let suffix_check = self.ident_suffix_check;
        self.append_token(token);
        self.push_state(suffix_check);
    }

    /// Create an external identifier from the current match.
    fn on_external_ident<R: ReaderOps>(&mut self, _reader: &mut R) {
        trace!(self.logger, "Identifier::on_external_ident");
        let token = Token::external(self.consume_current(), self.offset.consume());
        let suffix_check = self.ident_suffix_check;
        self.append_token(token);
        self.push_state(suffix_check);
    }

    /// Create a blank identifier from the current match.
    fn on_blank<R: ReaderOps>(&mut self, _reader: &mut R) {
        trace!(self.logger, "Identifier::on_blank");
        let token = Token::blank(self.offset.consume());
        let suffix_check = self.ident_suffix_check;
        self.discard_current();
        self.append_token(token);
        self.push_state(suffix_check);
    }

    /// Create an annotation from the current match.
    fn on_annotation<R: ReaderOps>(&mut self, _reader: &mut R) {
        trace!(self.logger, "Identifier::on_annotation");
        let current = self.consume_current();
        let offset = self.offset.consume();
        let length_to_drop = lexeme::len(lexeme::literal::ANNOTATION_SYMBOL);
        let token = Token::annotation(&current[length_to_drop..], offset);
        let suffix_check = self.ident_suffix_check;
        self.append_token(token);
        self.push_state(suffix_check);
    }

    /// Tokenize an unexpected error suffix.
    fn ident_on_error_suffix<R: ReaderOps>(&mut self, _reader: &mut R) {
        trace!(self.logger, "Identifier::ident_on_error_suffix");
        let token = Token::invalid_suffix(self.consume_current(), self.offset.consume());
        self.append_token(token);
        self.pop_state();
    }

    /// Submit a non-error identifier.
    fn ident_on_no_error_suffix<R: ReaderOps>(&mut self, _reader: &mut R) {
        trace!(self.logger, "Identifier::ident_on_no_error_suffix");
        self.pop_state();
    }

    /// The set of rules for lexing Enso identifiers.
    fn add_identifier_rules(lexer: &mut EnsoLexer) {
        let lower_ascii = lexeme::definition_pattern::lower_ascii_letter();
        let upper_ascii = lexeme::definition_pattern::upper_ascii_letter();
        let body_char = (&lower_ascii | lexeme::definition_pattern::ascii_digit()).many();
        let blank = lexeme::into_pattern(lexeme::literal::BLANK_IDENT);
        let ident_seg_sep = lexeme::into_pattern(lexeme::literal::IDENT_SEGMENT_SEPARATOR);
        let ticks = lexeme::into_pattern(lexeme::literal::IDENTIFIER_TICK).many();
        let init_var_seg = &lower_ascii >> &body_char;
        let lower_ascii_alnum = &lower_ascii | lexeme::definition_pattern::ascii_digit();
        let var_seg = &lower_ascii_alnum >> &body_char;
        let init_ref_seg = &upper_ascii >> &body_char;
        let upper_ascii_alnum = &upper_ascii | lexeme::definition_pattern::ascii_digit();
        let ref_seg = &upper_ascii_alnum >> &body_char;
        let external_start = lexeme::definition_pattern::ascii_letter() | &ident_seg_sep;
        let external_body = lexeme::definition_pattern::ascii_alpha_num() | &ident_seg_sep;
        let variable_ident = &init_var_seg >> (&ident_seg_sep >> &var_seg).many() >> &ticks;
        let referent_ident = &init_ref_seg >> (&ident_seg_sep >> &ref_seg).many() >> &ticks;
        let external_ident = &external_start >> external_body.many() >> &ticks;
        let break_chars = lexeme::definition_pattern::break_chars();
        let error_suffix = Pattern::none_of(break_chars.as_str()).many1();
        let annotation_symbol = lexeme::into_pattern(lexeme::literal::ANNOTATION_SYMBOL);
        let annotation = annotation_symbol >> (&variable_ident | &referent_ident);


        // === Initial State Rules for Identifiers ===

        let initial_state_id = lexer.initial_state;
        let initial_state = lexer.group_mut(initial_state_id);
        rules!(initial_state with
            variable_ident => self.on_variable_ident(),
            referent_ident => self.on_referent_ident(),
            blank          => self.on_blank(),
            external_ident => self.on_external_ident(),
            annotation     => self.on_annotation(),
        );


        // === Identifier Suffix Checking Rules ===

        let suffix_check_id = lexer.ident_suffix_check;
        let suffix_check = lexer.group_mut(suffix_check_id);
        rules!(suffix_check with
            error_suffix      => self.ident_on_error_suffix(),
            Pattern::always() => self.ident_on_no_error_suffix(),
        );
    }
}


// === Numbers ===

/// The set of rules for lexing numbers in Enso.
#[allow(dead_code)]
impl EnsoLexer {
    /// Finalize the lexer when it's done lexing a number with an explicit base.
    fn finalize_explicit_base(&mut self) {
        trace!(self.logger, "Number::finalize_explicit_base");
        let number_part_2 = self.number_phase_two;
        self.pop_states_including(number_part_2);
        self.number_state.reset();
    }

    /// Triggered when the lexer matches an integer with an implicit base.
    fn on_integer<R: ReaderOps>(&mut self, _reader: &mut R) {
        trace!(self.logger, "Number::on_integer");
        let number_phase_2 = self.number_phase_two;
        self.number_state.literal = self.consume_current();
        self.push_state(number_phase_2)
    }

    /// Triggered when the lexer matches a number annotated with an explicit base.
    fn on_explicit_base<R: ReaderOps>(&mut self, _reader: &mut R) {
        trace!(self.logger, "Number::on_explicit_base");
        let literal = self.consume_current();
        self.number_state.literal = literal;
        let offset = self.offset.consume();
        let token = self.number_state.consume_token(offset);
        self.append_token(token);
        self.finalize_explicit_base();
    }

    /// Triggered when the lexer has seen an explicit base definition that isn't followed by an
    /// actual number.
    fn on_dangling_base<R: ReaderOps>(&mut self, _reader: &mut R) {
        trace!(self.logger, "Number::on_dangling_base");
        let base = self.number_state.consume_base();
        let offset = self.offset.consume();
        let token = Token::dangling_base(base, offset);
        self.append_token(token);
        self.discard_current();
        self.finalize_explicit_base();
    }

    /// Triggered when an explicit decimal number has been seen by the lexer.
    fn on_decimal<R: ReaderOps>(&mut self, _reader: &mut R) {
        trace!(self.logger, "Number::on_decimal");
        let decimal_suffix_check = self.decimal_suffix_check;
        self.number_state.literal = self.consume_current();
        let offset = self.offset.consume();
        let token = self.number_state.consume_token(offset);
        self.append_token(token);
        self.push_state(decimal_suffix_check);
    }

    /// Triggered when an explicit base annotation has been seen by the lexer.
    fn seen_base<R: ReaderOps>(&mut self, _reader: &mut R) {
        trace!(self.logger, "Number::seen_base");
        let seen_base_id = self.number_seen_base;
        self.push_state(seen_base_id);
        self.number_state.swap_members();
    }

    /// Submit an integer token into the lexer.
    fn submit_integer<R: ReaderOps>(&mut self, _reader: &mut R) {
        trace!(self.logger, "Number::submit_integer");
        let offset = self.offset.consume();
        let token = self.number_state.consume_token(offset);
        self.append_token(token);
        self.pop_state();
    }

    /// Triggered when a decimal number is followed by an erroneous suffix.
    fn decimal_error_suffix<R: ReaderOps>(&mut self, _reader: &mut R) {
        trace!(self.logger, "Number::decimal_error_suffix");
        let decimal_suffix_check = self.decimal_suffix_check;
        let current_match = self.consume_current();
        let offset = self.offset.consume();
        let token = Token::invalid_suffix(current_match, offset);
        self.append_token(token);
        self.pop_states_including(decimal_suffix_check);
    }

    /// Triggered when a decimal number is followed by a valid suffix.
    fn decimal_valid_suffix<R: ReaderOps>(&mut self, _reader: &mut R) {
        trace!(self.logger, "Number::decimal_valid_suffix");
        let seen_decimal_id = self.decimal_suffix_check;
        self.pop_states_including(seen_decimal_id);
    }

    /// The rules for lexing numbers in Enso.
    fn add_number_rules(lexer: &mut EnsoLexer) {
        let digits = lexeme::definition_pattern::ascii_digit().many1();
        let point = lexeme::into_pattern(lexeme::literal::DECIMAL_SEPARATOR);
        let base_separator = lexeme::into_pattern(lexeme::literal::NUMBER_BASE_SEPARATOR);
        let decimal = &digits >> &point >> &digits;
        let arbitrary_digits = lexeme::definition_pattern::ascii_alpha_num().many1();
        let arbitrary_decimal = &arbitrary_digits >> (&point >> &arbitrary_digits).opt();
        let break_chars = lexeme::definition_pattern::break_chars();
        let error_suffix = Pattern::none_of(break_chars.as_str()).many1();


        // === Initial State Rules for Number Literals ===

        let initial_state_id = lexer.initial_state;
        let initial_state = lexer.group_mut(initial_state_id);
        rules!(initial_state with
            digits  => self.on_integer(),
            decimal => self.on_decimal(),
        );


        // === Rules in "Phase 2" of Number Lexing (Checks for Bases) ===

        let number_phase_2_id = lexer.number_phase_two;
        let number_phase_2 = lexer.groups_mut().group_mut(number_phase_2_id);
        rules!(number_phase_2 with
            base_separator    => self.seen_base(),
            Pattern::always() => self.submit_integer(),
        );


        // === Rules for Seeing an Explicit Base in a Number Literal ===

        let seen_base_id = lexer.number_seen_base;
        let seen_base = lexer.groups_mut().group_mut(seen_base_id);
        rules!(seen_base with
            arbitrary_decimal => self.on_explicit_base(),
            Pattern::always() => self.on_dangling_base(),
        );


        // === Rules for Seeing an Explicit Decimal Number ===

        let decimal_suffix_check_id = lexer.decimal_suffix_check;
        let decimal_suffix_check = lexer.groups_mut().group_mut(decimal_suffix_check_id);
        rules!(decimal_suffix_check with
            error_suffix      => self.decimal_error_suffix(),
            Pattern::always() => self.decimal_valid_suffix(),
        );
    }
}


// === Text Rules ===

/// The set of rules for lexing text literals in the Enso language.
#[allow(dead_code)]
impl EnsoLexer {
    // === Error ===

    /// Triggered when encountering an invalid set of quotes.
    fn on_invalid_quote<R: ReaderOps>(&mut self, _reader: &mut R) {
        trace!(self.logger, "Text::on_invalid_quote");
        let offset = self.offset.consume();
        let bad_quotes = self.consume_current();
        let token = Token::invalid_quote(bad_quotes, offset);
        self.append_token(token);
    }

    /// Submit a missing quote in a nested text line literal.
    fn submit_missing_quote_nested<R: ReaderOps>(
        &mut self,
        reader: &mut R,
        line_ending: token::LineEnding,
    ) {
        trace!(self.logger, "Text::submit_missing_quote_nested");
        self.on_missing_quote(reader);
        let interpolate_is_closed = false;
        self.on_interpolate_end(reader, interpolate_is_closed);
        let top_level = self.text_state.unsafe_current_mut();
        let top_level_style = top_level.unsafe_get_style();
        top_level.style = Some(match top_level_style {
            token::TextStyle::FormatLine => token::TextStyle::UnclosedLine,
            _ => top_level_style,
        });
        let text = self.text_state.unsafe_end_literal();
        let literal = text.into();
        self.append_token(literal);
        self.pop_state();
        self.on_missing_quote_cleanup(reader, line_ending);
    }

    /// Submit a missing closing quote in a text line literal.
    fn submit_missing_quote<R: ReaderOps>(
        &mut self,
        reader: &mut R,
        line_ending: token::LineEnding,
    ) {
        trace!(self.logger, "Text::submit_missing_quote");
        self.on_missing_quote(reader);
        self.on_missing_quote_cleanup(reader, line_ending);
    }

    /// The common logic for dealing with a missing quote in a text line literal.
    fn on_missing_quote<R: ReaderOps>(&mut self, reader: &mut R) {
        trace!(self.logger, "Text::on_missing_quote");
        let matched_bookmark = self.bookmarks.matched_bookmark;
        self.bookmarks.rewind(matched_bookmark, reader);
        let current = self.text_state.unsafe_current_mut();
        current.style = Some(token::TextStyle::UnclosedLine);
        let text = self.text_state.unsafe_end_literal();
        let literal = text.into();
        self.append_token(literal);
        self.pop_state();
    }

    /// The common logic that must run after dealing with a missing quote in a text line literal.
    fn on_missing_quote_cleanup<R: ReaderOps>(
        &mut self,
        reader: &mut R,
        line_ending: token::LineEnding,
    ) {
        trace!(self.logger, "Text::on_missing_quote_after");
        match line_ending {
            token::LineEnding::None => self.on_eof(reader),
            _ => self.block_state.push_line_ending(line_ending),
        }
    }

    /// Triggered when encountering the opening of a text block in a nested context.
    fn on_text_block_nested<R: ReaderOps>(
        &mut self,
        reader: &mut R,
        line_ending: token::LineEnding,
        quote: &'static str,
    ) {
        trace!(self.logger, "Text::on_text_block_nested");
        let current = self.consume_current();
        let offset = self.offset.consume();
        let text = &current[0..lexeme::len(quote)];
        let unrecognized = Token::unrecognized(text, offset);
        self.append_token(unrecognized);
        self.on_newline_in_interpolate(reader, line_ending);
    }


    // === Lines ===

    /// Triggered when beginning a format line literal.
    fn on_begin_format_line<R: ReaderOps>(&mut self, _reader: &mut R) {
        trace!(self.logger, "Text::on_begin_format_line");
        let interpolate = self.text_interpolate;
        let literal = self.text_state.begin_literal();
        literal.style = Some(token::TextStyle::FormatLine);
        let state = if self.is_inside_state(interpolate) {
            self.text_format_line_nested
        } else {
            self.text_format_line
        };
        self.push_state(state);
    }

    /// Submits a text line literal.
    fn submit_text_line<R: ReaderOps>(
        &mut self,
        _reader: &mut R,
        end_including: group::Identifier,
    ) {
        trace!(self.logger, "Text::submit_text_line");
        let text = self.text_state.unsafe_end_literal();
        let token = text.into();
        self.append_token(token);
        self.pop_states_including(end_including);
    }

    /// Triggered when beginning a raw line literal.
    fn on_begin_raw_line<R: ReaderOps>(&mut self, _reader: &mut R) {
        trace!(self.logger, "Text::on_begin_raw_line");
        let interpolate = self.text_interpolate;
        let literal = self.text_state.begin_literal();
        literal.style = Some(token::TextStyle::RawLine);
        let state = if self.is_inside_state(interpolate) {
            self.text_raw_line_nested
        } else {
            self.text_raw_line
        };
        self.push_state(state);
    }


    // === Inline Blocks ===

    /// Triggered when beginning an inline text block.
    fn text_on_inline_block<R: ReaderOps>(
        &mut self,
        _reader: &mut R,
        style: token::TextStyle,
        enter_state: group::Identifier,
    ) {
        trace!(self.logger, "Text::text_on_inline_block");
        let offset = self.offset.consume();
        let text = self.text_state.begin_literal();
        text.style = Some(style);
        text.offset = offset;
        self.push_state(enter_state);
    }

    /// Triggered when ending an inline text block.
    fn text_end_inline_block(&mut self) {
        trace!(self.logger, "Text::text_end_inline_block");
        let text = self.text_state.unsafe_end_literal();
        let literal = text.into();
        self.append_token(literal);
        self.pop_state();
    }

    /// Triggered when encountering EOF inside an inline block.
    fn text_on_eof_in_inline_block<R: ReaderOps>(&mut self, reader: &mut R) {
        trace!(self.logger, "Text::text_on_eof_in_inline_block");
        self.text_end_inline_block();
        self.on_eof(reader);
    }

    /// Triggered when encountering a newline in an inline block.
    fn text_inline_block_on_newline<R: ReaderOps>(
        &mut self,
        _reader: &mut R,
        line_ending: token::LineEnding,
    ) {
        trace!(self.logger, "Text::text_inline_block_on_newline");
        let block_newline = self.block_newline;
        self.block_state.push_line_ending(line_ending);
        self.text_end_inline_block();
        self.block_submit_line(_reader);
        self.push_state(block_newline);
    }

    /// Triggered on completion of a format inline block.
    fn submit_format_inline_block<R: ReaderOps>(&mut self, _reader: &mut R) {
        trace!(self.logger, "Text::submit_format_inline_block");
        let format_inline_block = self.text_format_inline_block;
        let text = self.text_state.unsafe_end_literal();
        let token = text.into();
        self.append_token(token);
        self.pop_states_including(format_inline_block);
    }


    // === Blocks ===

    /// Triggered when beginning a text block.
    fn on_begin_text_block<R: ReaderOps>(
        &mut self,
        _reader: &mut R,
        block_style: token::TextStyle,
        enter_state: group::Identifier,
        line_ending: token::LineEnding,
    ) {
        trace!(self.logger, "Text::on_begin_text_block");
        let block_seen_newline = self.block_newline;
        let text_seen_newline = self.text_seen_newline;
        let current_state = self.current_state();
        let offset = if current_state == block_seen_newline {
            self.pop_state();
            self.block_state.current().indent
        } else {
            self.offset.consume()
        };
        let text = self.text_state.begin_literal();
        text.starting_line_ending = line_ending;
        text.style = Some(block_style);
        text.offset = offset;
        self.offset.push();
        self.push_state(enter_state);
        self.push_state(text_seen_newline);
    }

    /// Triggered when ending a text block.
    fn text_on_end_of_block<R: ReaderOps>(&mut self, _reader: &mut R) {
        trace!(self.logger, "Text::text_on_end_of_block");
        self.pop_state();
        let mut text_state = self.text_state.unsafe_end_literal();
        text_state
            .take_empty_lines()
            .into_iter()
            .for_each(|line| self.block_state.append_delayed_line(line));
        let text_literal = text_state.into();
        self.append_token(text_literal);
        self.offset.pop();
        let matched_bookmark = self.bookmarks.matched_bookmark;
        self.bookmarks.rewind(matched_bookmark, _reader);
    }

    /// Triggered when encountering a newline followed by an EOF in a text block.
    fn text_on_eof_new_line<R: ReaderOps>(&mut self, reader: &mut R) {
        trace!(self.logger, "Text::text_on_eof_new_line");
        let block_newline = self.block_newline;
        self.pop_state();
        self.text_on_end_of_block(reader);
        self.push_state(block_newline);
    }

    /// Triggered at the end of a line in a text block.
    fn text_on_end_of_line<R: ReaderOps>(
        &mut self,
        _reader: &mut R,
        line_ending: token::LineEnding,
    ) {
        trace!(self.logger, "Text::text_on_end_of_line");
        let text_newline = self.text_seen_newline;
        self.text_state.push_line_ending(line_ending);
        self.text_state.submit_current_line();
        self.push_state(text_newline);
    }

    /// Triggered when lexing a new line in a text block.
    fn text_on_new_line<R: ReaderOps>(&mut self, reader: &mut R) {
        trace!(self.logger, "Text::text_on_new_line");
        self.pop_state();
        let indent = self.consume_current().chars().count();
        let text = self.text_state.unsafe_current_mut();
        if text.indent == 0 {
            text.indent = indent;
        }
        if indent < text.indent {
            self.block_state.seen_newline = true;
            self.text_on_end_of_block(reader);
            self.block_on_newline(reader, token::LineEnding::None);
        } else if (indent - text.indent) > 0 {
            let segment_offset = 0;
            let literal_spaces = " ".repeat(indent - text.indent);
            let segment = Token::text_segment_raw(literal_spaces, segment_offset);
            text.append_segment_to_line(segment);
        }
    }

    /// Triggered when lexing an empty line in a text block.
    fn text_on_empty_line<R: ReaderOps>(
        &mut self,
        _reader: &mut R,
        line_ending: token::LineEnding,
    ) {
        trace!(self.logger, "Text::text_on_empty_line");
        let current = self.consume_current();
        let indent = current.chars().count() - line_ending.size();
        self.text_state.push_line_ending(line_ending);
        self.text_state.append_empty_line(indent);
    }


    // === Segments ===

    /// Triggered when beginning an interpolation in a format literal.
    fn on_interpolate_begin<R: ReaderOps>(&mut self, _reader: &mut R) {
        trace!(self.logger, "Text::on_interpolate_begin");
        let text_interpolate = self.text_interpolate;
        self.push_tokens();
        self.offset.push();
        self.push_state(text_interpolate);
    }

    /// Triggered when an interpolate quote is seen in a _nested_ text literal.
    fn on_nested_interpolate_quote<R: ReaderOps>(&mut self, reader: &mut R) {
        trace!(self.logger, "Text::on_nested_interpolate_quote");
        let mut nested_text = self.text_state.unsafe_end_literal();
        let new_style = match nested_text.style {
            Some(token::TextStyle::RawLine) => Some(token::TextStyle::UnclosedLine),
            Some(token::TextStyle::FormatLine) => Some(token::TextStyle::UnclosedLine),
            _ => nested_text.style,
        };
        nested_text.style = new_style;
        let literal = nested_text.into();
        let interpolate_is_closed = true;
        self.append_token(literal);
        self.on_interpolate_end(reader, interpolate_is_closed);
    }

    /// Triggered when ending an interpolation in a format text literal.
    fn on_interpolate_end<R: ReaderOps>(&mut self, reader: &mut R, is_closed: bool) {
        trace!(self.logger, "Text::on_interpolate_end");
        let text_interpolate = self.text_interpolate;
        let nested_format_line = self.text_format_line_nested;
        if self.is_inside_state(text_interpolate) {
            if self.is_inside_state(nested_format_line) {
                self.pop_states_until(nested_format_line);
                let current = self.text_state.unsafe_current_mut();
                current.style = Some(token::TextStyle::UnclosedLine);
                let text = self.text_state.unsafe_end_literal();
                let literal = text.into();
                self.append_token(literal);
            }
            self.pop_states_until(text_interpolate);
            let expr_tokens = self.consume_tokens();
            let offset = self.offset.consume();
            let expr_segment = if is_closed {
                Token::text_segment_interpolate(expr_tokens.into(), offset)
            } else {
                Token::text_segment_unclosed_interpolate(expr_tokens.into(), offset)
            };
            self.text_state.append_segment(expr_segment);
            self.pop_tokens();
            self.offset.pop();
            self.pop_state();
        } else {
            self.on_unrecognized(reader);
        }
    }

    /// Triggered when encountering an EOF inside an interpolation.
    fn on_eof_in_interpolate<R: ReaderOps>(&mut self, reader: &mut R) {
        trace!(self.logger, "Text::on_eof_in_interpolate");
        let interpolate_is_closed = false;
        self.on_interpolate_end(reader, interpolate_is_closed);
        let mut text = self.text_state.unsafe_end_literal();
        text.style = match text.style {
            Some(token::TextStyle::FormatLine) => Some(token::TextStyle::UnclosedLine),
            _ => text.style,
        };
        let token = text.into();
        self.append_token(token);
        self.on_eof(reader);
    }

    /// Triggered when encountering a newline inside an interpolation.
    fn on_newline_in_interpolate<R: ReaderOps>(
        &mut self,
        reader: &mut R,
        line_ending: token::LineEnding,
    ) {
        trace!(self.logger, "Text::on_newline_in_interpolate");
        let interpolate_is_closed = false;
        self.on_interpolate_end(reader, interpolate_is_closed);
        let current_style = self.text_state.unsafe_current().unsafe_get_style();
        match current_style {
            token::TextStyle::FormatLine => {
                let mut text = self.text_state.unsafe_end_literal();
                text.style = match text.style {
                    Some(token::TextStyle::FormatLine) => Some(token::TextStyle::UnclosedLine),
                    _ => text.style,
                };
                let token = text.into();
                self.append_token(token);
                self.pop_state();
                self.block_on_newline(reader, line_ending);
            }
            token::TextStyle::FormatInlineBlock => {
                self.text_inline_block_on_newline(reader, line_ending);
            }
            token::TextStyle::FormatBlock => {
                self.text_on_end_of_line(reader, line_ending);
            }
            _ => unreachable_panic!("To reach here the lexer must be inside a format literal."),
        }
    }

    /// Triggered when encountering a doc comment inside an interpolate.
    fn on_doc_comment_in_interpolate<R: ReaderOps>(&mut self, _reader: &mut R) {
        trace!(self.logger, "Text::on_doc_comment_in_interpolate");
        let offset = self.offset.consume();
        let token = Token::unrecognized(lexeme::literal::DOC_COMMENT, offset);
        let doc_len = lexeme::len(lexeme::literal::DOC_COMMENT);
        let new_offset = self.consume_current().chars().count() - doc_len;
        self.append_token(token);
        self.offset.increase(new_offset, 0);
    }

    /// Triggered when submitting a raw segment of text in a literal.
    fn submit_plain_segment<R: ReaderOps>(&mut self, _reader: &mut R) {
        trace!(self.logger, "Text::submit_plain_segment");
        let last_segment = self.text_state.consume_segment();
        let current_match = self.consume_current();
        let token = match last_segment {
            Some(token) => match token {
                Token { shape: token::Shape::TextSegmentRaw(lit), offset, .. } =>
                    Token::text_segment_raw(lit + &current_match, offset),
                _ => {
                    let offset = self.offset.consume();
                    self.text_state.append_segment(token);
                    Token::text_segment_raw(current_match, offset)
                }
            },
            _ => {
                let offset = self.offset.consume();
                Token::text_segment_raw(current_match, offset)
            }
        };
        self.text_state.append_segment(token);
    }


    // === Escape Sequences ===

    /// Triggered when encountering a literal escape sequence.
    fn on_escape_literal<R: ReaderOps>(&mut self, _reader: &mut R, escape_code: &str) {
        trace!(self.logger, "Text::on_escape_literal");
        let offset = self.offset.consume();
        let escape = Token::text_segment_escape(token::EscapeStyle::Literal, escape_code, offset);
        self.discard_current();
        self.text_state.append_segment(escape);
    }

    /// Triggered when encountering a byte escape sequence.
    fn on_escape_byte<R: ReaderOps>(&mut self, _reader: &mut R) {
        trace!(self.logger, "Text::on_escape_byte");
        let current_match = self.consume_current();
        let offset = self.offset.consume();
        let escape = escape::Byte::build(current_match);
        let token = Token::text_segment_escape_from_shape(escape, offset);
        self.text_state.append_segment(token);
    }

    /// Triggered when encountering a U16 unicode escape sequence.
    fn on_escape_u16<R: ReaderOps>(&mut self, _reader: &mut R) {
        trace!(self.logger, "Text::on_escape_u16");
        let current_match = self.consume_current();
        let offset = self.offset.consume();
        let escape = escape::U16::build(current_match);
        let token = Token::text_segment_escape_from_shape(escape, offset);
        self.text_state.append_segment(token);
    }

    /// Triggered when encountering a U21 unicode escape sequence.
    fn on_escape_u21<R: ReaderOps>(&mut self, _reader: &mut R) {
        trace!(self.logger, "Text::on_escape_u32");
        let current_match = self.consume_current();
        let offset = self.offset.consume();
        let escape = escape::U21::build(current_match);
        let token = Token::text_segment_escape_from_shape(escape, offset);
        self.text_state.append_segment(token);
    }

    /// Triggered when encountering a U32 unicode escape sequence.
    fn on_escape_u32<R: ReaderOps>(&mut self, _reader: &mut R) {
        trace!(self.logger, "Text::on_escape_u32");
        let current_match = self.consume_current();
        let offset = self.offset.consume();
        let escape = escape::U32::build(current_match);
        let token = Token::text_segment_escape_from_shape(escape, offset);
        self.text_state.append_segment(token);
    }

    /// Triggered when encountering an escaped raw quote.
    fn on_escape_raw_quote<R: ReaderOps>(&mut self, _reader: &mut R) {
        trace!(self.logger, "Text::on_escape_raw_quote");
        let offset = self.offset.consume();
        let escape = Token::text_segment_escape(
            token::EscapeStyle::Literal,
            lexeme::literal::RAW_QUOTE,
            offset,
        );
        self.discard_current();
        self.text_state.append_segment(escape);
    }

    /// Triggered when encountering an escaped format quote.
    fn on_escape_format_quote<R: ReaderOps>(&mut self, _reader: &mut R) {
        trace!(self.logger, "Text::on_escape_format_quote");
        let offset = self.offset.consume();
        let escape = Token::text_segment_escape(
            token::EscapeStyle::Literal,
            lexeme::literal::FORMAT_QUOTE,
            offset,
        );
        self.discard_current();
        self.text_state.append_segment(escape);
    }

    /// Triggered when encountering an escaped interpolate quote.
    fn on_escape_interpolate_quote<R: ReaderOps>(&mut self, _reader: &mut R) {
        trace!(self.logger, "Text::on_escape_interpolate_quote");
        let offset = self.offset.consume();
        let escape = Token::text_segment_escape(
            token::EscapeStyle::Literal,
            lexeme::literal::INTERPOLATE_QUOTE,
            offset,
        );
        self.discard_current();
        self.text_state.append_segment(escape);
    }

    /// Triggered when encountering an escaped backslash.
    fn on_escape_slash<R: ReaderOps>(&mut self, _reader: &mut R) {
        trace!(self.logger, "Text::on_escape_slash");
        let offset = self.offset.consume();
        let escape =
            Token::text_segment_escape(token::EscapeStyle::Literal, lexeme::literal::SLASH, offset);
        self.discard_current();
        self.text_state.append_segment(escape);
    }

    /// Triggered when encountering an unrecognized escape sequence.
    fn on_escape_invalid<R: ReaderOps>(&mut self, _reader: &mut R) {
        trace!(self.logger, "Text::on_escape_invalid");
        let offset = self.offset.consume();
        let current = self.consume_current();
        let escape = Token::text_segment_escape(token::EscapeStyle::Invalid, current, offset);
        self.text_state.append_segment(escape);
    }


    // === Rule Definitions ===

    /// Define the rules for lexing Enso text literals.
    fn add_text_rules(lexer: &mut EnsoLexer) {
        let format_quote = lexeme::into_pattern(lexeme::literal::FORMAT_QUOTE);
        let format_block_quote = lexeme::into_pattern(lexeme::literal::FORMAT_BLOCK_QUOTE);
        let raw_quote = lexeme::into_pattern(lexeme::literal::RAW_QUOTE);
        let raw_block_quote = lexeme::into_pattern(lexeme::literal::RAW_BLOCK_QUOTE);
        let backslash = lexeme::into_pattern(lexeme::literal::SLASH);
        let spaces = lexeme::definition_pattern::spaces();
        let opt_spaces = spaces.opt();
        let eof = lexeme::definition_pattern::eof();
        let interpolate_quote = lexeme::into_pattern(lexeme::literal::INTERPOLATE_QUOTE);
        let lf = lexeme::into_pattern(lexeme::literal::LF);
        let crlf = lexeme::into_pattern(lexeme::literal::CRLF);
        let format_block_lf = &format_block_quote >> spaces.opt() >> &lf;
        let format_block_crlf = &format_block_quote >> spaces.opt() >> &crlf;
        let raw_block_lf = &raw_block_quote >> spaces.opt() >> &lf;
        let raw_block_crlf = &raw_block_quote >> spaces.opt() >> &crlf;
        let format_raw_segment = lexeme::definition_pattern::format_line_raw_char().many1();
        let format_block_segment = lexeme::definition_pattern::format_block_raw_char().many1();
        let raw_segment = lexeme::definition_pattern::raw_line_raw_char().many1();
        let raw_block_segment = lexeme::definition_pattern::raw_block_raw_char().many1();
        let unicode_escape_char = lexeme::definition_pattern::unicode_escape_digit();
        let byte_escape_start = lexeme::into_pattern(lexeme::literal::BYTE_ESCAPE_START);
        let u16_escape_start = lexeme::into_pattern(lexeme::literal::U16_ESCAPE_START);
        let u21_escape_start = lexeme::into_pattern(lexeme::literal::U21_ESCAPE_START);
        let u21_escape_end = lexeme::into_pattern(lexeme::literal::U21_ESCAPE_END);
        let u32_escape_start = lexeme::into_pattern(lexeme::literal::U32_ESCAPE_START);
        let escape_byte_digits = Pattern::repeat_between(&unicode_escape_char, 0, 3);
        let escape_u16_digits = Pattern::repeat_between(&unicode_escape_char, 0, 5);
        let escape_u32_digits = Pattern::repeat_between(&unicode_escape_char, 0, 9);
        let escape_byte = &byte_escape_start >> &escape_byte_digits;
        let escape_u16 = &u16_escape_start >> &escape_u16_digits;
        let escape_u21 = u21_escape_start >> &unicode_escape_char.many() >> u21_escape_end;
        let escape_u32 = u32_escape_start >> &escape_u32_digits;
        let escape_slash = lexeme::into_pattern(lexeme::literal::ESCAPED_SLASH);
        let escape_invalid = &backslash >> Pattern::not_symbol(Symbol::eof()).opt();
        let escape_format_quote = &backslash >> &format_quote;
        let escape_raw_quote = &backslash >> &raw_quote;
        let escape_backtick = &backslash >> &interpolate_quote;
        let invalid_format_quote = &format_block_quote >> format_quote.many1();
        let invalid_raw_quote = &raw_block_quote >> raw_quote.many1();
        let eof_new_line = &spaces.opt() >> &eof;
        let empty_line_lf = &spaces.opt() >> &lf;
        let empty_line_crlf = &spaces.opt() >> &crlf;
        let comment_char = lexeme::into_pattern(lexeme::literal::COMMENT).many1();
        let doc_comment_start = lexeme::into_pattern(lexeme::literal::DOC_COMMENT) >> &opt_spaces;


        // === Initial State Rules for Text Literals ===

        let root_state_id = lexer.initial_state;
        let root_state = lexer.group_mut(root_state_id);
        rules!(root_state with
            interpolate_quote    => self.on_interpolate_end(true),
            invalid_format_quote => self.on_invalid_quote(),
            format_quote => self.on_begin_format_line(),
            format_block_lf => self.on_begin_text_block
                (token::TextStyle::FormatBlock,self.text_format_block,token::LineEnding::LF),
            format_block_crlf => self.on_begin_text_block
                (token::TextStyle::FormatBlock,self.text_format_block,token::LineEnding::CRLF),
            format_block_quote => self.text_on_inline_block
                (token::TextStyle::FormatInlineBlock,self.text_format_inline_block),
            invalid_raw_quote => self.on_invalid_quote(),
            raw_quote         => self.on_begin_raw_line(),
            raw_block_lf      => self.on_begin_text_block
                (token::TextStyle::RawBlock,self.text_raw_block,token::LineEnding::LF),
            raw_block_crlf => self.on_begin_text_block
                (token::TextStyle::RawBlock,self.text_raw_block,token::LineEnding::CRLF),
            raw_block_quote => self.text_on_inline_block
                (token::TextStyle::RawInlineBlock,self.text_raw_inline_block),
        );


        // === Rules for Handling Lines in Text Blocks ===

        let in_block_line_id = lexer.in_block_line;
        let in_block_line = lexer.group_mut(in_block_line_id);
        rules!(in_block_line with
            format_block_lf => self.on_begin_text_block
                (token::TextStyle::FormatBlock,self.text_format_block,token::LineEnding::LF),
            format_block_crlf => self.on_begin_text_block
                (token::TextStyle::FormatBlock,self.text_format_block,token::LineEnding::CRLF),
            raw_block_lf => self.on_begin_text_block
                (token::TextStyle::RawBlock,self.text_raw_block,token::LineEnding::LF),
            raw_block_crlf => self.on_begin_text_block
                (token::TextStyle::RawBlock,self.text_raw_block,token::LineEnding::CRLF),
        );


        // === Escape Sequences ===

        let text_escape_id = lexer.text_escape;
        let text_escape = lexer.group_mut(text_escape_id);
        for char_escape in escape::EscapeCharacter::codes() {
            let pattern = Pattern::all_of(&char_escape.pattern);
            let call = format!("self.on_escape_literal(reader,{:?})", &char_escape.repr.as_str());
            text_escape.create_rule(&pattern, call.as_str());
        }
        rules!(text_escape with
            escape_byte    => self.on_escape_byte(),
            escape_u21     => self.on_escape_u21(),
            escape_u16     => self.on_escape_u16(),
            escape_u32     => self.on_escape_u32(),
            escape_slash   => self.on_escape_slash(),
            escape_invalid => self.on_escape_invalid(),
        );


        // === Interpolation Rules ===

        let text_interpolate_id = lexer.text_interpolate;
        let text_interpolate = lexer.group_mut(text_interpolate_id);
        rules!(text_interpolate with
            doc_comment_start  => self.on_doc_comment_in_interpolate(),
            comment_char       => self.on_unrecognized(),
            format_block_quote => self.on_unrecognized(),
            format_block_lf    => self.on_text_block_nested
                (token::LineEnding::LF,lexeme::literal::FORMAT_BLOCK_QUOTE),
            format_block_crlf => self.on_text_block_nested
                (token::LineEnding::CRLF,lexeme::literal::FORMAT_BLOCK_QUOTE),
            invalid_format_quote => self.on_invalid_quote(),
            raw_block_quote      => self.on_unrecognized(),
            raw_block_lf         => self.on_text_block_nested
                (token::LineEnding::LF,lexeme::literal::RAW_BLOCK_QUOTE),
            raw_block_crlf => self.on_text_block_nested
                (token::LineEnding::CRLF,lexeme::literal::RAW_BLOCK_QUOTE),
            invalid_raw_quote => self.on_invalid_quote(),
            eof               => self.on_eof_in_interpolate(),
            lf                => self.on_newline_in_interpolate(token::LineEnding::LF),
            crlf              => self.on_newline_in_interpolate(token::LineEnding::CRLF),
        );


        // === Format Text Common Rules ===

        let text_format_id = lexer.text_format;
        let text_format = lexer.group_mut(text_format_id);
        rules!(text_format with
            interpolate_quote   => self.on_interpolate_begin(),
            escape_backtick     => self.on_escape_interpolate_quote(),
            escape_format_quote => self.on_escape_format_quote()
        );


        // === Format Text Line Rules ===

        let text_format_line_id = lexer.text_format_line;
        let text_format_line = lexer.group_mut(text_format_line_id);
        rules!(text_format_line with
            format_quote       => self.submit_text_line(self.text_format_line),
            format_raw_segment => self.submit_plain_segment(),
            eof                => self.submit_missing_quote(token::LineEnding::None),
            lf                 => self.submit_missing_quote(token::LineEnding::LF),
            crlf               => self.submit_missing_quote(token::LineEnding::CRLF),
        );


        // === Format Text Inline Block Rules ===

        let format_inline_block_id = lexer.text_format_inline_block;
        let format_inline_block = lexer.group_mut(format_inline_block_id);
        rules!(format_inline_block with
            format_block_segment => self.submit_plain_segment(),
            eof                  => self.text_on_eof_in_inline_block(),
            lf                   => self.text_inline_block_on_newline(token::LineEnding::LF),
            crlf                 => self.text_inline_block_on_newline(token::LineEnding::CRLF),
        );


        // === Format Text Block Rules ===

        let format_block_id = lexer.text_format_block;
        let format_block = lexer.group_mut(format_block_id);
        rules!(format_block with
            format_block_segment => self.submit_plain_segment(),
            eof                  => self.text_on_end_of_block(),
            lf                   => self.text_on_end_of_line(token::LineEnding::LF),
            crlf                 => self.text_on_end_of_line(token::LineEnding::CRLF),
        );


        // === Rules for Format Lines Nested Inside Interpolations ===

        let format_line_nested_id = lexer.text_format_line_nested;
        let format_line_nested = lexer.group_mut(format_line_nested_id);
        rules!(format_line_nested with
            interpolate_quote  => self.on_interpolate_end(true),
            format_quote       => self.submit_text_line(self.text_format_line_nested),
            format_raw_segment => self.submit_plain_segment(),
            eof                => self.submit_missing_quote_nested(token::LineEnding::None),
            lf                 => self.submit_missing_quote_nested(token::LineEnding::LF),
            crlf               => self.submit_missing_quote_nested(token::LineEnding::CRLF),
        );


        // === Raw Text Common Rules ===

        let text_raw_id = lexer.text_raw;
        let text_raw = lexer.group_mut(text_raw_id);
        rules!(text_raw with
            escape_raw_quote => self.on_escape_raw_quote(),
            escape_slash     => self.on_escape_slash(),
            escape_invalid   => self.submit_plain_segment(),
        );


        // === Raw Text Line Rules ===

        let text_raw_line_id = lexer.text_raw_line;
        let text_raw_line = lexer.group_mut(text_raw_line_id);
        rules!(text_raw_line with
            raw_quote   => self.submit_text_line(self.text_raw_line),
            raw_segment => self.submit_plain_segment(),
            eof         => self.submit_missing_quote(token::LineEnding::None),
            lf          => self.submit_missing_quote(token::LineEnding::LF),
            crlf        => self.submit_missing_quote(token::LineEnding::CRLF),
        );


        // === Raw Text Inline Block Rules ===

        let raw_inline_block_id = lexer.text_raw_inline_block;
        let raw_inline_block = lexer.group_mut(raw_inline_block_id);
        rules!(raw_inline_block with
            raw_block_segment => self.submit_plain_segment(),
            eof               => self.text_on_eof_in_inline_block(),
            lf                => self.text_inline_block_on_newline(token::LineEnding::LF),
            crlf              => self.text_inline_block_on_newline(token::LineEnding::CRLF),
        );


        // === Raw Text Block Rules ===

        let raw_block_id = lexer.text_raw_block;
        let raw_block = lexer.group_mut(raw_block_id);
        rules!(raw_block with
            raw_block_segment => self.submit_plain_segment(),
            eof               => self.text_on_end_of_block(),
            lf                => self.text_on_end_of_line(token::LineEnding::LF),
            crlf              => self.text_on_end_of_line(token::LineEnding::CRLF),
        );


        // === Rules for Raw Lines Nested Inside Interpolations ===

        let raw_line_nested_id = lexer.text_raw_line_nested;
        let raw_line_nested = lexer.group_mut(raw_line_nested_id);
        rules!(raw_line_nested with
            raw_quote   => self.submit_text_line(self.text_raw_line_nested),
            raw_segment => self.submit_plain_segment(),
            eof         => self.submit_missing_quote_nested(token::LineEnding::None),
            lf          => self.submit_missing_quote_nested(token::LineEnding::LF),
            crlf        => self.submit_missing_quote_nested(token::LineEnding::CRLF),
        );


        // === Text Block Line Handling ===

        let text_newline_id = lexer.text_seen_newline;
        let text_newline = lexer.group_mut(text_newline_id);
        rules!(text_newline with
            spaces.opt()    => self.text_on_new_line(),
            empty_line_lf   => self.text_on_empty_line(token::LineEnding::LF),
            empty_line_crlf => self.text_on_empty_line(token::LineEnding::CRLF),
            eof_new_line    => self.text_on_eof_new_line(),
        );
    }
}


// === Block Rules ===

/// The set of rules for lexing blocks in the Enso language.
#[allow(dead_code)]
impl EnsoLexer {
    /// Common functionality for both styles of line ending.
    fn block_on_newline<R: ReaderOps>(&mut self, _reader: &mut R, line_ending: token::LineEnding) {
        trace!(self.logger, "Block::block_on_newline");
        self.block_state.push_line_ending(line_ending);
        let block_newline = self.block_newline;
        self.block_state.seen_newline = true;
        self.offset.push();
        self.push_state(block_newline);
    }

    /// Transitions the lexer into a state in which it knows it is lexing a block line.
    fn block_in_line<R: ReaderOps>(&mut self, _reader: &mut R) {
        trace!(self.logger, "Block::block_in_line");
        let indent_len = self.current_match.chars().count();
        self.offset.increase(indent_len, 0);
        let in_block_line = self.in_block_line;
        self.push_state(in_block_line);
    }

    /// Triggered when lexing a non-blank line.
    fn block_on_non_empty_line<R: ReaderOps>(&mut self, reader: &mut R) {
        trace!(self.logger, "Block::block_on_non_empty_line");
        let block_newline = self.block_newline;
        self.pop_states_including(block_newline);
        match self.offset.current.cmp(&self.block_state.current().indent) {
            Ordering::Equal => {
                self.offset.consume();
                self.block_submit_line(reader);
            }
            Ordering::Greater => {
                let new_indent = self.offset.consume();
                self.begin_block(reader, new_indent);
            }
            Ordering::Less => {
                let new_indent = self.offset.consume();
                self.on_block_end(reader, new_indent);
            }
        }
    }

    /// Begin a new block.
    fn begin_block<R: ReaderOps>(&mut self, _reader: &mut R, block_indent: usize) {
        trace!(self.logger, "Block::begin_block");
        let is_orphan = self.output.is_empty();
        self.push_tokens();
        self.block_state.begin_block(block_indent, is_orphan);
    }

    /// Triggered when lexing an empty line in a block.
    fn block_on_empty_line<R: ReaderOps>(
        &mut self,
        reader: &mut R,
        line_ending: token::LineEnding,
    ) {
        trace!(self.logger, "Block::block_on_empty_line");
        self.block_state.push_line_ending(line_ending);
        self.block_submit_line(reader);
        let offset = self.offset.consume();
        let block_newline = self.block_newline;
        self.pop_states_until(block_newline);
        self.block_state.push_empty_line(offset);
    }

    /// Triggered when lexing a line in a block that ends a file.
    fn block_in_eof_line<R: ReaderOps>(&mut self, reader: &mut R) {
        trace!(self.logger, "Block::block_in_eof_line");
        let initial_state = self.initial_state;
        self.pop_states_until(initial_state);
        self.on_eof(reader);
    }

    /// Triggered when beginning a top-level block.
    fn block_begin_top_level<R: ReaderOps>(&mut self, reader: &mut R) {
        trace!(self.logger, "Block::block_begin_top_level");
        let matched_bookmark = self.bookmarks.matched_bookmark;
        let block_newline = self.block_newline;
        let initial_state = self.initial_state;
        self.bookmarks.rewind(matched_bookmark, reader);
        self.offset.push();
        self.pop_states_until(initial_state);
        self.push_state(block_newline);
    }

    /// Triggered when a block is ended.
    fn on_block_end<R: ReaderOps>(&mut self, reader: &mut R, new_indent: usize) {
        trace!(self.logger, "Block::on_block_end");
        if self.block_state.seen_newline {
            while new_indent < self.block_state.current().indent {
                self.block_submit(reader);
            }
            if new_indent > self.block_state.current().indent {
                info!(self.logger, "Block with invalid indentation.");
                self.begin_block(reader, new_indent);
                self.block_state.current_mut().set_invalid();
            } else {
                self.offset.push();
                self.block_submit_line(reader);
            }
        }
    }

    /// Create a block token from the current block state.
    fn build_block<R: ReaderOps>(&mut self, reader: &mut R) -> Token {
        trace!(self.logger, "Block::build_block");
        self.block_submit_line(reader);
        let offset = self.offset.consume();
        let current_block = self.block_state.consume_current();
        current_block.into_token(offset)
    }

    /// Submit a block to the token stream of the lexer.
    fn block_submit<R: ReaderOps>(&mut self, reader: &mut R) {
        trace!(self.logger, "Block::block_submit");
        let mut block = self.build_block(reader);
        self.pop_tokens();
        self.offset.pop();
        self.block_state.end_block();
        if let Some(Token { shape: token::Shape::Operator(_), .. }) = self.last_token() {
            if let token::Shape::Block { indent, lines, .. } = block.shape {
                block.shape = token::Shape::block(BlockType::Discontinuous, indent, lines);
            }
        }
        self.append_token(block);
        self.offset.push();
    }

    /// Submit a line in a block.
    ///
    /// It should be noted that lines that have content in blocks cannot have an offset.
    fn block_submit_line<R: ReaderOps>(&mut self, _reader: &mut R) {
        trace!(self.logger, "Block::block_submit_line");
        if self.block_state.seen_newline {
            if !self.output.is_empty() {
                let token_stream = self.consume_tokens();
                self.block_state.append_line_to_current(token_stream.into());
            }
            debug!(self.logger, "Clear Output Buffer: Old Length = {self.output.len()}.");
            self.output.clear();
        }
    }

    /// Triggered when the top-level block ends.
    fn block_end_top_level<R: ReaderOps>(&mut self, _reader: &mut R) {
        trace!(self.logger, "Block::block_end_top_level");
        let current_block = self.block_state.consume_current();
        if self.block_state.seen_newline {
            let offset = self.offset.consume();
            let top_level_block = current_block.into_token(offset);
            self.append_token(top_level_block);
        } else {
            let additional_offset = current_block.indent;
            if let Some(token) = self.output.first_mut() {
                token.offset += additional_offset
            }
        }
    }

    /// The rule definitions for lexing blocks in Enso.
    fn add_block_rules(lexer: &mut EnsoLexer) {
        let spaces = lexeme::definition_pattern::spaces();
        let lf = lexeme::into_pattern(lexeme::literal::LF);
        let crlf = lexeme::into_pattern(lexeme::literal::CRLF);
        let opt_spaces = spaces.opt();
        let eof_line = &opt_spaces >> Pattern::eof();
        let always = Pattern::always();


        // === Initial State Rules for Blocks ===

        let root_state_id = lexer.initial_state;
        let root_state = lexer.group_mut(root_state_id);
        rules!(root_state with
            lf   => self.block_on_newline(token::LineEnding::LF),
            crlf => self.block_on_newline(token::LineEnding::CRLF),
        );


        // === Rules for Blocks Having Seen a Newline ===

        let block_newline_id = lexer.block_newline;
        let block_newline = lexer.group_mut(block_newline_id);
        rules!(block_newline with
            opt_spaces => self.block_in_line(),
            eof_line   => self.block_in_eof_line(),
        );


        // === Rules for Lines in Blocks ===

        let in_block_line_id = lexer.in_block_line;
        let in_block_line = lexer.group_mut(in_block_line_id);
        rules!(in_block_line with
            lf     => self.block_on_empty_line(token::LineEnding::LF),
            crlf   => self.block_on_empty_line(token::LineEnding::CRLF),
            always => self.block_on_non_empty_line(),
        );


        // === Rules for Top-Level Blocks ===

        let block_module_id = lexer.block_top_level;
        let block_module = lexer.group_mut(block_module_id);
        rules!(block_module with opt_spaces => self.block_begin_top_level());
    }
}


// === Default Rules ===

/// The set of rules that apply as defaults in the root state.
#[allow(dead_code)]
impl EnsoLexer {
    /// Triggered on an arbitrary space character.
    fn on_space<R: ReaderOps>(&mut self, _reader: &mut R) {
        trace!(self.logger, "Root::on_space");
        let current_len = self.current_match.chars().count();
        self.offset.increase(current_len, 0);
        self.discard_current();
    }

    /// Triggered on an arbitrary eof character.
    fn on_eof<R: ReaderOps>(&mut self, reader: &mut R) {
        trace!(self.logger, "Root::on_eof");
        let base_block_indent = 0;
        self.offset.push();
        self.block_submit_line(reader);
        self.on_block_end(reader, base_block_indent);
        self.block_end_top_level(reader);
    }

    /// Triggered on any unrecognized character.
    fn on_unrecognized<R: ReaderOps>(&mut self, _reader: &mut R) {
        trace!(self.logger, "Root::on_unrecognized");
        let token = Token::unrecognized(self.consume_current(), self.offset.consume());
        self.append_token(token);
    }

    /// The default rules for the lexer.
    fn add_default_rules(lexer: &mut EnsoLexer) {
        let space = lexeme::into_pattern(lexeme::literal::SPACE);
        let eof = Pattern::eof();
        let any = Pattern::any();

        let initial_state_id = lexer.initial_state;
        let initial_state = lexer.group_mut(initial_state_id);
        rules!(initial_state with
            space => self.on_space(),
            eof   => self.on_eof(),
            any   => self.on_unrecognized(),
        );
    }
}


// === Trait Impls ===

impl enso_flexer::Definition for EnsoLexer {
    fn define() -> Self {
        let mut lexer = EnsoLexer::new();

        EnsoLexer::add_comment_rules(&mut lexer);
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
///
/// This state contains all of the stateful information required to lex the Enso language, as well
/// as the functionality for manipulating the reader of the lexer.
#[derive(Debug)]
pub struct State<Logger> {
    /// The logger for the lexing state.
    #[allow(dead_code)]
    logger:                   Logger,
    /// The bookmarks used by the lexer.
    bookmarks:                reader::BookmarkManager,
    /// The registry of states for the lexer.
    lexer_states:             group::Registry,
    /// The initial state of the lexer.
    initial_state:            group::Identifier,
    /// The state for checking the end of identifiers.
    ident_suffix_check:       group::Identifier,
    /// The state for completing number lexing.
    number_phase_two:         group::Identifier,
    /// The state where number lexing has seen an explicit base.
    number_seen_base:         group::Identifier,
    /// The state where number lexing has seen a decimal.
    decimal_suffix_check:     group::Identifier,
    /// The state for lexing operator suffixes.
    operator_suffix_check:    group::Identifier,
    /// The state for lexing operator modifiers.
    operator_modifier_check:  group::Identifier,
    /// The state for lexing the top-level block.
    block_top_level:          group::Identifier,
    /// The state entered when a newline has been lexed.
    block_newline:            group::Identifier,
    /// The state entered when within the line of a block.
    in_block_line:            group::Identifier,
    /// The state containing rules common to escape sequences in text literals.
    text_escape:              group::Identifier,
    /// The state containing rules common to format text literals.
    text_format:              group::Identifier,
    /// The state entered when lexing a format line text literal.
    text_format_line:         group::Identifier,
    /// The state entered when lexing a format inline block text literal.
    text_format_inline_block: group::Identifier,
    /// The state entered when lexing a format block text literal.
    text_format_block:        group::Identifier,
    /// The state entered when lexing a format line nested inside an interpolation.
    text_format_line_nested:  group::Identifier,
    /// The state containing rules common to raw text literals.
    text_raw:                 group::Identifier,
    /// The state entered when lexing a raw line text literal.
    text_raw_line:            group::Identifier,
    /// The state entered when lexing a raw inline block text literal.
    text_raw_inline_block:    group::Identifier,
    /// The state entered when lexing a raw block text literal.
    text_raw_block:           group::Identifier,
    /// The state entered when lexing a raw line literal nested inside an interpolation.
    text_raw_line_nested:     group::Identifier,
    /// The state entered when a literal newline is seen inside a text literal.
    text_seen_newline:        group::Identifier,
    /// The state entered when a literal CRLF is seen inside a text literal.
    text_interpolate:         group::Identifier,
    /// A parent group for all comments.
    #[allow(dead_code)]
    comment:                  group::Identifier,
    /// A state for lexing disable comments.
    disable_comment:          group::Identifier,
    /// A state for lexing doc comments.
    doc_comment:              group::Identifier,
    /// A state for seeing a newline in a doc comment.
    doc_comment_newline:      group::Identifier,
    /// A stack of token matches.
    tokens_stack:             Vec<token::Stream>,
    /// Tracking for the current offset.
    offset:                   Offset<Logger>,
    /// State specifically for lexing Enso numbers.
    number_state:             NumberLexingState<Logger>,
    /// State specifically for lexing Enso blocks.
    block_state:              BlockLexingState<Logger>,
    /// State specifically for lexing Enso text literals.
    text_state:               TextLexingState<Logger>,
    /// State specifically for lexing Enso comments.
    comment_state:            CommentLexingState<Logger>,
}

impl<Logger> State<Logger>
where Logger: AnyLogger<Owned = Logger> + LoggerOps<DebugLevel>
{
    /// Get a reference to the group for the provided identifier.
    pub fn group(&self, group: group::Identifier) -> &Group {
        self.groups().group(group)
    }

    /// Get a mutable reference to the group for the provided identifier.
    pub fn group_mut(&mut self, group: group::Identifier) -> &mut Group {
        self.groups_mut().group_mut(group)
    }
}


// === Trait Impls ===

impl<Logger> enso_flexer::State for State<Logger>
where Logger: AnyLogger<Owned = Logger> + LoggerOps<DebugLevel>
{
    fn new(parent_logger: &impl AnyLogger) -> Self {
        let logger = parent_logger.sub("State");
        let bookmarks = default();
        let mut lexer_states = group::Registry::default();
        let initial_state = lexer_states.define_group("ROOT", None);
        let ident_suffix_check = lexer_states.define_group("IDENT_SFX_CHECK", None);
        let number_phase_two = lexer_states.define_group("NUMBER_PHASE_2", None);
        let number_seen_base = lexer_states.define_group("NUMBER_SEEN_BASE", None);
        let decimal_suffix_check = lexer_states.define_group("NUMBER_SEEN_POINT", None);
        let operator_suffix_check = lexer_states.define_group("OPERATOR_SUFFIX_CHECK", None);
        let operator_modifier_check =
            lexer_states.define_group("OPERATOR_MODIFIER_CHECK", Some(operator_suffix_check));
        let block_top_level = lexer_states.define_group("BLOCK_MODULE", None);
        let block_newline = lexer_states.define_group("BLOCK_NEWLINE", None);
        let in_block_line = lexer_states.define_group("IN_BLOCK_LINE", None);
        let text_escape = lexer_states.define_group("TEXT_ESCAPE", None);
        let text_format = lexer_states.define_group("TEXT_FORMAT", Some(text_escape));
        let text_format_line = lexer_states.define_group("TEXT_FORMAT_LINE", Some(text_format));
        let text_format_inline_block =
            lexer_states.define_group("TEXT_FORMAT_INLINE_BLOCK", Some(text_format));
        let text_format_block = lexer_states.define_group("TEXT_FORMAT_BLOCK", Some(text_format));
        let text_format_line_nested =
            lexer_states.define_group("TEXT_FORMAT_LINE_NESTED", Some(text_format_line));
        let text_raw = lexer_states.define_group("TEXT_RAW", None);
        let text_raw_line = lexer_states.define_group("TEXT_RAW_LINE", Some(text_raw));
        let text_raw_inline_block =
            lexer_states.define_group("TEXT_RAW_INLINE_BLOCK", Some(text_raw));
        let text_raw_block = lexer_states.define_group("TEXT_RAW_BLOCK", Some(text_raw));
        let text_raw_line_nested =
            lexer_states.define_group("TEXT_RAW_LINE_NESTED", Some(text_raw_line));
        let text_seen_newline = lexer_states.define_group("TEXT_SEEN_NEWLINE", None);
        let text_interpolate = lexer_states.define_group("TEXT_INTERPOLATE", Some(initial_state));
        let comment = lexer_states.define_group("COMMENT", None);
        let disable_comment = lexer_states.define_group("DISABLE_COMMENT", Some(comment));
        let doc_comment = lexer_states.define_group("DOC_COMMENT", Some(comment));
        let doc_comment_newline = lexer_states.define_group("DOC_COMMENT_NEWLINE", None);
        let tokens_stack = Vec::new();
        let offset_logger = <Logger>::sub(&logger, "Offset");
        let offset = Offset::new(offset_logger);
        let number_state_logger = <Logger>::sub(&logger, "NumberLexingState");
        let number_state = NumberLexingState::new(number_state_logger);
        let block_state_logger = <Logger>::sub(&logger, "BlockLexingState");
        let block_state = BlockLexingState::new(block_state_logger);
        let text_state_logger = <Logger>::sub(&logger, "TextLexingState");
        let text_state = TextLexingState::new(text_state_logger);
        let comment_state_logger = <Logger>::sub(&logger, "CommentLexingState");
        let comment_state = CommentLexingState::new(comment_state_logger);

        Self {
            logger,
            bookmarks,
            lexer_states,
            initial_state,
            ident_suffix_check,
            number_phase_two,
            number_seen_base,
            decimal_suffix_check,
            operator_suffix_check,
            operator_modifier_check,
            block_top_level,
            block_newline,
            in_block_line,
            text_escape,
            text_format,
            text_format_line,
            text_format_inline_block,
            text_format_block,
            text_format_line_nested,
            text_raw,
            text_raw_line,
            text_raw_inline_block,
            text_raw_block,
            text_raw_line_nested,
            text_seen_newline,
            text_interpolate,
            comment,
            disable_comment,
            doc_comment,
            doc_comment_newline,
            tokens_stack,
            offset,
            number_state,
            block_state,
            text_state,
            comment_state,
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
        generate::specialize(self, "EnsoLexer", "token::Stream")
    }
}



// =========================
// === Offset Management ===
// =========================

/// A manager for the current offset state of the lexer.
///
/// The offset is the number of leading spaces between the last-lexed token and the token that is
/// currently being lexed.
///
/// In addition to containing the _current_ offset, it also provides facilities for manipulating a
/// _stack_ of offsets as the lexer changes state.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Offset<Logger> {
    /// The current offset of the lexer.
    ///
    /// The offset is the number of leading spaces between the last-lexed token and the token that
    /// is currently being lexed.
    current: usize,
    /// The stack of current offsets in the lexer.
    stack:   Vec<usize>,
    /// The logger for the offset state.
    logger:  Logger,
}

impl<Logger> Offset<Logger>
where Logger: AnyLogger + LoggerOps<DebugLevel>
{
    /// Create a new offset state.
    pub fn new(logger: Logger) -> Self {
        let current = default();
        let stack = default();
        Offset { current, stack, logger }
    }

    /// Push the current offset onto the offset stack.
    pub fn push(&mut self) {
        debug!(self.logger, "Push Offset: {self.current}.");
        self.stack.push(self.current);
        self.current = 0;
    }

    /// Pop the top offset from the offset stack.
    pub fn pop(&mut self) {
        self.current = self.stack.pop().unwrap_or(0);
        debug!(self.logger, "Pop Offset: {self.current}.");
    }

    /// Consume the current offset.
    pub fn consume(&mut self) -> usize {
        let offset = self.current;
        self.current = 0;
        debug!(self.logger, "Consume Offset: {offset}.");
        offset
    }

    /// Increase the current offset by `match_length` + `shift`.
    pub fn increase(&mut self, match_length: usize, shift: usize) {
        let diff = match_length + shift;
        debug!(self.logger, "Increase Offset By: {diff}.");
        self.current += diff;
        debug!(self.logger, "Offset Now: {self.current}.");
    }
}



// =========================
// === NumberLexingState ===
// =========================

/// The state for lexing an Enso number.
///
/// It contains the various portions of a number seen so far, and provides useful operations for the
/// manipulation of this state and subsequent conversion into a number token.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct NumberLexingState<Logger> {
    /// The (optional) base for the number.
    pub base:    String,
    /// The literal number, to be interpreted in `base`.
    pub literal: String,
    /// A logger for the number state.
    logger:      Logger,
}

impl<Logger> NumberLexingState<Logger>
where Logger: AnyLogger + LoggerOps<DebugLevel>
{
    /// Create a new number lexing state.
    pub fn new(logger: Logger) -> Self {
        let base = default();
        let literal = default();
        NumberLexingState { base, literal, logger }
    }

    /// Reset the number lexing state.
    pub fn reset(&mut self) {
        self.base.truncate(0);
        self.literal.truncate(0);
        debug!(self.logger, "Reset Number State.");
    }

    /// Swap the `base` and `literal` in place.
    pub fn swap_members(&mut self) {
        debug!(self.logger, "Swap Number Fields.");
        mem::swap(&mut self.base, &mut self.literal);
    }

    /// Convert `self` into a token, resetting the lexing state.
    pub fn consume_token(&mut self, offset: usize) -> Token {
        debug!(self.logger, "Consuming Number: Base = {self.base}, Number = {self.literal}.");
        Token::number(mem::take(&mut self.base), mem::take(&mut self.literal), offset)
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
///
/// It contains structures for tracking the nesting of block literals as lexing proceeds, as well
/// as tracking the overall block state of the lexer.
#[derive(Clone, Debug, PartialEq)]
pub struct BlockLexingState<Logger> {
    /// The stack of blocks being lexed.
    stack:                NonEmptyVec<BlockState>,
    /// Lines that will be included into the block upon encountering the _next_ valid line.
    ///
    /// This is necessary due to the language's grouping of trailing blank lines in block-like
    /// constructs, as it ensures that they get attributed to the parent block instead of being
    /// trailing blank lines in the current block.
    delayed_append_lines: VecDeque<Token>,
    /// Whether or not the lexer has seen an explicit newline.
    seen_newline:         bool,
    /// A logger for the lexing state.
    logger:               Logger,
}

impl<Logger> BlockLexingState<Logger>
where Logger: AnyLogger + LoggerOps<DebugLevel>
{
    /// Construct a new block lexing state.
    pub fn new(logger: Logger) -> Self {
        let stack = default();
        let delayed_append_lines = default();
        let seen_newline = false;
        BlockLexingState { stack, delayed_append_lines, seen_newline, logger }
    }

    /// Set the last seen line ending.
    pub fn push_line_ending(&mut self, line_ending: token::LineEnding) {
        self.seen_newline = true;
        self.current_mut().record_line_ending(line_ending);
        debug!(self.logger, "Push Line Ending: {line_ending:?}.");
    }

    /// Consume the last seen line ending.
    pub fn pop_line_ending(&mut self) -> token::LineEnding {
        let popped = self.current_mut().consume_line_ending();
        debug!(self.logger, "Pop Line Ending: {popped:?}.");
        popped
    }

    /// Appends a line to the current block.
    pub fn append_line_to_current(&mut self, tokens: Vec<Token>) {
        let trailing_line_ending = self.pop_line_ending();
        debug!(
            self.logger,
            "Append Line: Line Ending = {trailing_line_ending:?}, Tokens = {&tokens:?}."
        );
        let offset = 0;
        self.current_mut().push_line(tokens, offset, trailing_line_ending);
        self.process_delayed_lines();
    }

    /// Process the delayed lines in the block, turning them into real lines.
    pub fn process_delayed_lines(&mut self) {
        if self.has_delayed_lines() {
            let delayed = mem::take(&mut self.delayed_append_lines);
            debug!(self.logger, "Appending Delayed Lines: {&delayed:?}.");
            self.current_mut().lines.extend(delayed);
        }
    }

    /// Check if the block has delayed lines.
    pub fn has_delayed_lines(&self) -> bool {
        !self.delayed_append_lines.is_empty()
    }

    /// Delay appending a line to the current block until after `Self::append_line_to_current()`
    /// is called.
    pub fn append_delayed_line(&mut self, line: Token) {
        debug!(self.logger, "Delay Appending: {&line:?}.");
        self.delayed_append_lines.push_back(line)
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
    pub fn begin_block(&mut self, new_offset: usize, is_orphan: bool) {
        debug!(self.logger, "Begin Block State: Indent = {new_offset}.");
        self.stack.push(default());
        self.current_mut().is_orphan = is_orphan;
        self.current_mut().indent = new_offset;
    }

    /// Pop a block state from the stack.
    pub fn end_block(&mut self) -> Option<BlockState> {
        debug!(self.logger, "End Block State.");
        self.stack.pop()
    }

    /// Consume the state of the current block.
    pub fn consume_current(&mut self) -> BlockState {
        let block = mem::take(self.stack.last_mut());
        debug!(self.logger, "Consume Block: {&block:?}.");
        block
    }

    /// Push an empty line into the current block.
    pub fn push_empty_line(&mut self, offset: usize) {
        let trailing_line_ending = self.pop_line_ending();
        self.current_mut().push_empty_line(offset, trailing_line_ending);
        debug!(self.logger, "Append Empty Line: Line Ending = {trailing_line_ending:?}.");
    }
}



// ==================
// === BlockState ===
// ==================

/// The state for lexing a given block in Enso.
///
/// It tracks the particulars about a certain block in the program source code, including its
/// validity, whether or not it is orphaned, the root indentation of the block, as well as the lines
/// that make it up.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct BlockState {
    /// Whether or not the block is orphaned.
    ///
    /// An orphaned block is one that has no block parent.
    is_orphan:         bool,
    /// Whether or not the block is well-formed.
    is_invalid:        bool,
    /// The root indentation level of the block.
    indent:            usize,
    /// The remaining lines of the block.
    lines:             Vec<Token>,
    /// The line endings that have been seen in this block's context.
    seen_line_endings: VecDeque<token::LineEnding>,
}

impl BlockState {
    /// Push a line into the block.
    pub fn push_line(
        &mut self,
        tokens: Vec<Token>,
        indent: usize,
        trailing_line_ending: token::LineEnding,
    ) {
        let line = Token::line(tokens, indent, trailing_line_ending);
        self.lines.push(line)
    }

    /// Push a blank line into the block.
    ///
    /// The offset here should be the offset from the baseline, not from the block indent level.
    pub fn push_empty_line(&mut self, offset: usize, trailing_line_ending: token::LineEnding) {
        let line = Token::blank_line(offset, trailing_line_ending);
        self.lines.push(line);
    }

    /// Record seeing the `line_ending`.
    pub fn record_line_ending(&mut self, line_ending: token::LineEnding) {
        self.seen_line_endings.push_back(line_ending);
    }

    /// Consume a `line_ending`.
    pub fn consume_line_ending(&mut self) -> token::LineEnding {
        self.seen_line_endings.pop_front().unwrap_or(token::LineEnding::None)
    }

    /// Convert the block state into a block token.
    pub fn into_token(self, offset: usize) -> Token {
        Token::block(BlockType::Continuous, self.indent, self.lines, offset)
    }

    /// Consume the lines in the block.
    pub fn consume_lines(&mut self) -> Vec<Token> {
        mem::take(&mut self.lines)
    }

    /// Set the block as invalid.
    pub fn set_invalid(&mut self) {
        self.is_invalid = true;
    }
}



// =======================
// === TextLexingState ===
// =======================

/// The required state for managing the lexing of text literals in Enso.
///
/// This maintains a stack of text literals as it is possible to nest text literals via the use
/// of interpolated segments in format text literals.
#[derive(Clone, Debug, PartialEq)]
pub struct TextLexingState<Logger> {
    /// The stack of text lexing states.
    text_stack: Vec<TextState>,
    /// The logger.
    logger:     Logger,
}

impl<Logger> TextLexingState<Logger>
where Logger: AnyLogger + LoggerOps<DebugLevel>
{
    /// Construct a new text lexing state.
    pub fn new(logger: Logger) -> TextLexingState<Logger> {
        let text_stack = default();
        Self { text_stack, logger }
    }

    /// Get an immutable reference to the text literal currently being lexed.
    pub fn current(&self) -> Option<&TextState> {
        self.text_stack.last()
    }

    /// Get a mutable reference to the text literal currently being lexed.
    pub fn current_mut(&mut self) -> Option<&mut TextState> {
        self.text_stack.last_mut()
    }

    /// Unsafely get an immutable reference to the current text literal.
    ///
    /// # Panics
    /// If there is no text literal currently being lexed.
    pub fn unsafe_current(&self) -> &TextState {
        self.current().expect("Text state is present.")
    }

    /// Unsafely get a mutable reference to the current text literal.
    ///
    /// # Panics
    /// If there is no text literal currently being lexed.
    pub fn unsafe_current_mut(&mut self) -> &mut TextState {
        self.current_mut().expect("Text state is present.")
    }

    /// Set the last seen line ending.
    pub fn push_line_ending(&mut self, line_ending: token::LineEnding) {
        if let Some(current_mut) = self.current_mut() {
            current_mut.record_line_ending(line_ending);
            debug!(self.logger, "Push Line Ending: {line_ending:?}.");
        }
    }

    /// Consume the last seen line ending.
    pub fn pop_line_ending(&mut self) -> token::LineEnding {
        if let Some(current_mut) = self.current_mut() {
            let ending = current_mut.consume_line_ending();
            debug!(self.logger, "Pop Line Ending: {ending:?}.");
            ending
        } else {
            token::LineEnding::None
        }
    }

    /// Append `token` to the currently-active line in the current literal.
    pub fn append_segment(&mut self, token: Token) {
        debug!(self.logger, "Append Token to Current Line: {&token:?}.");
        if let Some(current_mut) = self.current_mut() {
            current_mut.append_segment_to_line(token);
        }
    }

    /// Consume the most-recently added token from the line.
    pub fn consume_segment(&mut self) -> Option<Token> {
        let result = self.current_mut().and_then(|t| t.consume_segment_from_line());
        debug!(self.logger, "Consume Segment: {result:?}.");
        result
    }

    /// Append a line to the current text literal.
    pub fn append_line(&mut self, line: Token) {
        debug!(self.logger, "Append Line to Current Literal: {&line:?}.");
        self.current_mut().for_each(|t| t.append_line(line));
    }

    /// Append an empty line to the current text literal with `offset` leading spaces.
    pub fn append_empty_line(&mut self, offset: usize) {
        let line_ending = self.pop_line_ending();
        self.current_mut().iter_mut().for_each(|t| t.append_empty_line(offset, line_ending));
        debug!(self.logger, "Append Empty Line: Line Ending = {line_ending:?}.");
    }

    /// Submit the current line into the block.
    pub fn submit_current_line(&mut self) {
        self.current_mut().for_each(|line| line.submit_current_line());
    }

    /// Begin a new text literal.
    pub fn begin_literal(&mut self) -> &mut TextState {
        debug!(self.logger, "Begin Text Literal.");
        self.text_stack.push_and_get_mut(default())
    }

    /// End the current text literal.
    pub fn end_literal(&mut self) -> Option<TextState> {
        debug!(self.logger, "End Text Literal.");
        self.text_stack.pop()
    }

    /// End the current text literal.
    ///
    /// # Panics
    /// Panics if there is no literal to end.
    pub fn unsafe_end_literal(&mut self) -> TextState {
        self.end_literal().unwrap()
    }

    /// Check if the lexer is currently in a nested text literal.
    pub fn is_in_nested_text(&self) -> bool {
        self.text_stack.len() > 1
    }
}



// =================
// === TextState ===
// =================

/// The state for lexing a single text literal.
///
/// This type is responsible for tracking the particulars of a given text literal. This includes its
/// positioning information (offset and indent), as well as the _type_ of literal it is, and any
/// lines and/or segments that make up the literal.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct TextState {
    /// The offset of the literal from the token preceding it.
    offset:                usize,
    /// The number of spaces used for the literal's indent in a block.
    indent:                usize,
    /// The style of text literal being lexed.
    style:                 Option<token::TextStyle>,
    /// The line ending used to open a block literal.
    starting_line_ending:  token::LineEnding,
    /// The lines that make up the current text token.
    lines:                 Vec<Token>,
    /// The unused empty lines that make up the current text token.
    empty_lines:           Vec<Token>,
    /// The segments that make up the current line in the text literal.
    segments:              Vec<Token>,
    /// A stack of line endings in the text literal.
    explicit_line_endings: VecDeque<token::LineEnding>,
}

impl TextState {
    /// Set the starting line ending of the literal to `line_ending`.
    pub fn set_starting_line_ending(&mut self, line_ending: token::LineEnding) {
        self.starting_line_ending = line_ending;
    }

    /// Append a line of text to the current literal.
    pub fn append_line(&mut self, line: Token) {
        if self.has_empty_lines() {
            let lines = self.take_empty_lines();
            lines.into_iter().for_each(|l| self.append_line(l));
        }
        self.lines.push(line);
    }

    /// Append an empty line with `offset` leading spaces to the current text literal.
    pub fn append_empty_line(&mut self, offset: usize, line_ending: token::LineEnding) {
        let line = Token::blank_line(offset, line_ending);
        self.empty_lines.push(line);
    }

    /// Check if the current text state has unprocessed empty lines remaining.
    pub fn has_empty_lines(&self) -> bool {
        !self.empty_lines.is_empty()
    }

    /// Take the empty lines from the literal.
    pub fn take_empty_lines(&mut self) -> Vec<Token> {
        mem::take(&mut self.empty_lines)
    }

    /// Append a token to the current line of the text literal.
    pub fn append_segment_to_line(&mut self, token: Token) {
        if self.has_empty_lines() {
            let lines = self.take_empty_lines();
            lines.into_iter().for_each(|l| self.append_line(l));
        }
        self.segments.push(token);
    }

    /// Consume the last token from the currently active line.
    pub fn consume_segment_from_line(&mut self) -> Option<Token> {
        self.segments.pop()
    }

    /// Push a line ending onto the line ending stack.
    pub fn record_line_ending(&mut self, line_ending: token::LineEnding) {
        self.explicit_line_endings.push_back(line_ending);
    }

    /// Consume a line ending from the line ending stack.
    pub fn consume_line_ending(&mut self) -> token::LineEnding {
        self.explicit_line_endings.pop_front().unwrap_or(token::LineEnding::None)
    }

    /// Consume the current line of the text literal as a line.
    pub fn consume_current_line(&mut self) -> Token {
        let line_ending = self.consume_line_ending();
        let tokens = mem::take(&mut self.segments);
        Token::line(tokens, 0, line_ending)
    }

    /// Submit the current line in the literal.
    pub fn submit_current_line(&mut self) {
        let line = self.consume_current_line();
        self.append_line(line);
    }

    /// Set the style of text literal being lexed to `style`.
    pub fn set_style(&mut self, style: token::TextStyle) {
        self.style = Some(style);
    }

    /// Reset the text lexing state.
    pub fn reset(&mut self) {
        *self = default();
    }

    /// Get the text style of the current literal, with the assumption that it's set.
    pub fn unsafe_get_style(&self) -> token::TextStyle {
        self.style.expect("The text style must be set.")
    }
}


// === Trait Impls ===

impl From<TextState> for Token {
    fn from(mut text: TextState) -> Self {
        let style = text.style.expect("The literal style must be set when consuming the literal.");
        if style.is_line_literal() {
            let tokens = mem::take(&mut text.segments);
            Token::text_line(style, tokens, text.offset)
        } else if style.is_block_literal() {
            if !text.segments.is_empty() {
                let last_line = text.consume_current_line();
                text.append_line(last_line);
            }
            let lines = mem::take(&mut text.lines);
            Token::text_block(text.starting_line_ending, style, lines, text.indent, text.offset)
        } else if style.is_inline_block_literal() {
            let tokens = mem::take(&mut text.segments);
            Token::text_inline_block(style, tokens, text.offset)
        } else {
            unreachable_panic!("The above cases should cover all styles.");
        }
    }
}



// ==========================
// === CommentLexingState ===
// ==========================

/// The state for lexing comments in Enso.
///
/// As it is impossible to nest comments, this serves as a non-consumable state for lexing them. It
/// tracks the information about the current comment being lexed.
#[derive(Clone, Debug, PartialEq)]
pub struct CommentLexingState<Logger> {
    /// The comment currently being lexed.
    current_comment: CommentState,
    /// A logger for the comment state.
    logger:          Logger,
}

impl<Logger> CommentLexingState<Logger>
where Logger: AnyLogger + LoggerOps<DebugLevel>
{
    /// Construct a new comment state with the provided `logger`.
    pub fn new(logger: Logger) -> Self {
        let current_comment = default();
        Self { current_comment, logger }
    }

    /// Append `text` to the current comment line.
    pub fn append_to_line(&mut self, text: String) {
        debug!(self.logger, "Append to Line: {&text:?}.");
        self.current_comment.append_to_line(text);
    }

    /// Submit the current line in the comment.
    pub fn submit_line(&mut self, line_ending: token::LineEnding) {
        debug!(self.logger, "Submit Line: Ending = {line_ending:?}.");
        self.current_comment.submit_line(line_ending);
    }

    /// Submit a blank line in the comment.
    pub fn submit_blank_line(&mut self, indent: usize, line_ending: token::LineEnding) {
        debug!(self.logger, "Submit Blank Line: Ending = {line_ending:?}.");
        self.current_comment.submit_blank_line(indent, line_ending);
    }

    /// Get a reference to the current comment line.
    pub fn current_line(&self) -> &String {
        &self.current_comment.current_line
    }

    /// Get a mutable reference to the current comment line.
    pub fn current_line_mut(&mut self) -> &String {
        &mut self.current_comment.current_line
    }

    /// Consume the current comment.
    pub fn consume_current(&mut self) -> CommentState {
        debug!(self.logger, "Consume Current Comment.");
        mem::take(&mut self.current_comment)
    }

    /// Set the indent of the current comment.
    pub fn set_indent(&mut self, indent: usize) {
        debug!(self.logger, "Set Indent = {indent}.");
        self.current_comment.indent = indent;
    }

    /// Set the offset of the current comment.
    pub fn set_offset(&mut self, offset: usize) {
        debug!(self.logger, "Set Offset = {offset}.");
        self.current_comment.offset = offset;
    }
}



// ====================
// === CommentState ===
// ====================

/// The state for lexing a single comment.
///
/// This type tracks the particulars of any given comment, including the lines that make up the
/// comment, as well as the indent and offset of it (the positioning information).
#[derive(Clone, Default, Debug, PartialEq)]
pub struct CommentState {
    /// The lines that make up the comment.
    lines:        Vec<Token>,
    /// A buffer of blank lines not yet appended.
    blank_lines:  Vec<Token>,
    /// The current line being lexed in the comment.
    current_line: String,
    /// The indent of the comment.
    indent:       usize,
    /// The offset of the comment.
    offset:       usize,
}

impl CommentState {
    /// Append `text` to the current comment line.
    pub fn append_to_line(&mut self, text: String) {
        self.current_line.push_str(text.as_ref());
    }

    /// Submit the current line into the comment.
    pub fn submit_line(&mut self, line_ending: token::LineEnding) {
        if self.has_blank_lines() {
            let blanks = mem::take(&mut self.blank_lines);
            self.lines.extend(blanks);
        }
        let text = self.consume_current_line();
        let text_token = Token::text_segment_raw(text, 0);
        let line_token = Token::line(vec![text_token], 0, line_ending);
        self.lines.push(line_token);
    }

    /// Submit a blank line.
    pub fn submit_blank_line(&mut self, offset: usize, line_ending: token::LineEnding) {
        let line = Token::blank_line(offset, line_ending);
        self.blank_lines.push(line);
    }

    /// Consume the current line.
    fn consume_current_line(&mut self) -> String {
        mem::take(&mut self.current_line)
    }

    /// Check if the comment has blank lines available.
    fn has_blank_lines(&self) -> bool {
        !self.blank_lines.is_empty()
    }

    /// Consume the blank lines from the comment.
    pub fn consume_blank_lines(&mut self) -> Vec<Token> {
        mem::take(&mut self.blank_lines)
    }
}


// === Trait Impls ===

impl From<CommentState> for Token {
    fn from(mut comment: CommentState) -> Self {
        if comment.lines.is_empty() {
            Token::disable_comment(comment.current_line, comment.offset)
        } else {
            if !comment.current_line.is_empty() {
                comment.submit_line(token::LineEnding::None);
            }
            Token::doc_comment(comment.lines, comment.indent, comment.offset)
        }
    }
}
