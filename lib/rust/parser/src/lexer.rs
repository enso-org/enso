//! Implementation of lexer, a utility transforming source code into stream of tokens. Read the docs
//! of the main module of this crate to learn more about the parsing process.
//!
//! TODO: Implement token validators - validating if the consumed token was OK and reporting human
//!       readable errors.

use crate::prelude::*;
use crate::source::*;
use crate::syntax::*;

use std::str;



// =================
// === Constants ===
// =================

/// An optimization constant. Based on it, the estimated memory is allocated on the beginning of
/// parsing.
pub const AVERAGE_TOKEN_LEN: usize = 5;
/// Within an indented text block, this sets the minimum whitespace to be trimmed from the start of
/// each line.
const MIN_TEXT_TRIM: VisibleOffset = VisibleOffset(4);



// ===============
// === Pattern ===
// ===============

/// Allows checking if the incoming char matches a predicate. The predicate can be another char
/// (then this is simply check for equality), or a function `FnMut(char) -> bool`. This trait allows
/// defining parsers which can work with both simple and function-based matchers.
pub trait Pattern {
    /// Check whether [`input`] matches this pattern.
    fn match_pattern(&mut self, input: char) -> bool;
}

impl<T: FnMut(char) -> bool> Pattern for T {
    #[inline(always)]
    fn match_pattern(&mut self, input: char) -> bool {
        (self)(input)
    }
}

impl Pattern for char {
    #[inline(always)]
    fn match_pattern(&mut self, input: char) -> bool {
        *self == input
    }
}

macro_rules! pattern_impl_for_char_slice {
    ($($num:tt),* $(,)?) => {$(
        impl Pattern for &[char; $num] {
            #[inline(always)]
            fn match_pattern(&mut self, input: char) -> bool {
                self.contains(&input)
            }
        }
    )*};
}
pattern_impl_for_char_slice!(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);



// =============
// === Lexer ===
// =============

/// Efficient lexer implementation with no backtracking an 1-character lookahead ([`current_char`]
/// contains character that is not consumed yet). The lexer does not use recursion and is
/// implemented as a single-pass input stream consumer.
///
/// Please note, that the lexer is able to parse invalid input, such as invalid operators, like
/// `===`. This is needed for allowing the parser to auto-recover from errors in the code, including
/// syntax errors.
#[derive(Debug, Deref, DerefMut)]
#[allow(missing_docs)]
pub struct Lexer<'s> {
    #[deref]
    #[deref_mut]
    pub state:         LexerState,
    pub input:         &'s str,
    pub iterator:      str::CharIndices<'s>,
    pub output:        Vec<Token<'s>>,
    /// Memory for storing tokens, reused as an optimization.
    pub token_storage: VecAllocation<Token<'s>>,
}

/// Internal state of the [`Lexer`].
#[derive(Debug, Default)]
#[allow(missing_docs)]
pub struct LexerState {
    pub current_char: Option<char>,
    pub current_offset: Bytes,
    pub last_spaces_offset: Bytes,
    pub last_spaces_visible_offset: VisibleOffset,
    pub current_block_indent: VisibleOffset,
    pub block_indent_stack: Vec<VisibleOffset>,
    pub internal_error: Option<String>,
    pub stack: Vec<State>,
}

/// Suspended states.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum State {
    /// Reading a single-line text literal.
    InlineText,
    /// Reading a multi-line text literal.
    MultilineText {
        /// Indentation level of the quote symbol introducing the block.
        block_indent:   VisibleOffset,
        /// Indentation level of the first line of the block.
        initial_indent: Option<VisibleOffset>,
    },
}

impl<'s> Lexer<'s> {
    /// Constructor.
    pub fn new(input: &'s str) -> Self {
        let iterator = input.char_indices();
        let capacity = input.len() / AVERAGE_TOKEN_LEN;
        let output = Vec::with_capacity(capacity);
        let state = default();
        let token_storage = default();
        Self { input, iterator, output, state, token_storage }.init()
    }

    fn init(mut self) -> Self {
        self.next_input_char();
        self
    }

    /// Move to the next input character. Returns [`false`] if it was the end of the stream and the
    /// move was impossible.
    #[inline(always)]
    fn next_input_char(&mut self) -> bool {
        let next = self.iterator.next();
        if let Some((current_offset, current_char)) = next {
            self.current_offset = Bytes(current_offset);
            self.current_char = Some(current_char);
            true
        } else if self.current_char.is_some() {
            self.current_offset = Bytes(self.input.len());
            self.current_char = None;
            true
        } else {
            false
        }
    }

    /// Run the provided function and compute how much input it consumed.
    #[inline(always)]
    pub fn run_and_get_offset<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> (T, Bytes) {
        let start_offset = self.current_offset;
        let out = f(self);
        let len = self.current_offset - start_offset;
        (out, len)
    }

    /// Run the provided function and check if it consumed any input.
    #[inline(always)]
    pub fn run_and_check_if_progressed(&mut self, f: impl FnOnce(&mut Self)) -> bool {
        self.run_and_get_offset(f).1.is_positive()
    }

    /// Consume spaces after parsing a [`Token`] and update the internal spacing info.
    #[inline(always)]
    fn spaces_after_lexeme(&mut self) {
        (self.last_spaces_visible_offset, self.last_spaces_offset) =
            self.run_and_get_offset(|this| this.spaces());
    }

    /// Consume spaces after parsing a [`Token`] and update the internal spacing info. Doesn't
    /// consume more than the specified [`VisibleOffset`] of spaces.
    #[inline(always)]
    fn spaces_after_lexeme_with_limit(&mut self, limit: VisibleOffset) {
        (self.last_spaces_visible_offset, self.last_spaces_offset) =
            self.run_and_get_offset(|this| this.spaces_with_limit(limit));
    }

    /// Run the provided function. If it consumed any chars, return the [`Token`] containing the
    /// provided function output. Returns [`None`] otherwise.
    #[inline(always)]
    pub fn token<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> Option<Token<'s, T>> {
        let start = self.current_offset;
        let (elem, len) = self.run_and_get_offset(f);
        len.is_positive().as_some_from(|| {
            let end = start + len;
            let code = self.input.slice(start..end);
            let left_offset_start = start - self.last_spaces_offset;
            let offset_code = self.input.slice(left_offset_start..start);
            let visible_offset = self.last_spaces_visible_offset;
            let offset = Offset(visible_offset, offset_code);
            self.spaces_after_lexeme();
            Token(offset, code, elem)
        })
    }

    /// A zero-length token which is placed before the last consumed spaces if they were not
    /// followed by any token.
    #[inline(always)]
    pub fn marker_token<T>(&mut self, elem: T) -> Token<'s, T> {
        let visible_offset = VisibleOffset(0);
        let start = self.current_offset - self.last_spaces_offset;
        let code = self.input.slice(start..start);
        let offset = Offset(visible_offset, code);
        Token(offset, code, elem)
    }

    /// Push the [`token`] to the result stream.
    #[inline(always)]
    pub fn submit_token(&mut self, token: Token<'s>) {
        self.output.push(token);
    }

    /// Start a new block.
    #[inline(always)]
    pub fn start_block(&mut self, new_indent: VisibleOffset) {
        let current_block_indent = self.current_block_indent;
        self.block_indent_stack.push(current_block_indent);
        self.current_block_indent = new_indent;
    }

    /// Finish the current block.
    #[inline(always)]
    pub fn end_block(&mut self) -> Option<VisibleOffset> {
        self.block_indent_stack.pop().map(|prev| {
            let out = self.current_block_indent;
            self.current_block_indent = prev;
            out
        })
    }
}



// =====================
// === Basic Parsers ===
// =====================

impl<'s> Lexer<'s> {
    /// Consume the next character, unconditionally.
    #[inline(always)]
    pub fn take_next(&mut self) -> bool {
        self.next_input_char()
    }

    /// Consume exactly one character if it matches the pattern. Returns [`true`] if it succeeded.
    #[inline(always)]
    pub fn take_1(&mut self, mut pat: impl Pattern) -> bool {
        match self.current_char.map(|t| pat.match_pattern(t)) {
            Some(true) => self.next_input_char(),
            _ => false,
        }
    }

    /// Version of [`take_1`] that discards its result.
    #[inline(always)]
    pub fn take_1_(&mut self, pat: impl Pattern) {
        self.take_1(pat);
    }

    /// Consume characters as long as they match the pattern.
    #[inline(always)]
    pub fn take_while(&mut self, mut pat: impl Pattern) {
        while let Some(true) = self.current_char.map(|t| pat.match_pattern(t)) {
            self.next_input_char();
        }
    }

    /// Consume characters as long as they match the pattern. Returns [`true`] if at least one
    /// character was consumed.
    #[inline(always)]
    pub fn take_while_1(&mut self, f: impl Copy + Pattern) -> bool {
        let ok = self.take_1(f);
        if ok {
            self.take_while(f);
        }
        ok
    }

    /// Version of [`take_while_1`] that discards its result.
    #[inline(always)]
    pub fn take_while_1_(&mut self, f: impl Copy + Pattern) {
        self.take_while_1(f);
    }
}



// =============
// === Space ===
// =============

/// Based on https://en.wikipedia.org/wiki/Whitespace_character.
const OTHER_UNICODE_SINGLE_SPACES: &str = "\u{1680}\u{202F}\u{205F}\u{3000}";
const OTHER_UNICODE_SINGLE_SPACES_RANGE: RangeInclusive<char> =
    RangeInclusive::new('\u{2000}', '\u{200A}');
#[cfg(test)]
const UNICODE_ZERO_SPACES: &str = "\u{180E}\u{200B}\u{200C}\u{200D}\u{2060}\u{FEFF}";

/// Checks whether the provided character is a visible space and returns its visible size. The tab
/// character always returns `4`. It is not made configurable, as described in the Language Spec
/// docs.
#[inline(always)]
pub fn space_char_visible_size(t: char) -> Option<VisibleOffset> {
    let off = match t {
        ' ' => Some(1),
        '\t' => Some(4),
        _ if t >= '\u{00A0}' => match t {
            // For ASCII code, we don't enter this branch.
            '\u{00A0}' => Some(1),
            _ if t >= '\u{1680}' => match t {
                '\u{1680}' => Some(1),
                _ if t >= '\u{2000}' => match t {
                    _ if OTHER_UNICODE_SINGLE_SPACES.contains(t) => Some(1),
                    _ if OTHER_UNICODE_SINGLE_SPACES_RANGE.contains(&t) => Some(1),
                    _ => None,
                },
                _ => None,
            },
            _ => None,
        },
        _ => None,
    };
    off.map(VisibleOffset)
}

/// Check whether the provided character is a visible space character.
#[inline(always)]
fn is_space_char(t: char) -> bool {
    space_char_visible_size(t).is_some()
}

impl<'s> Lexer<'s> {
    /// Consume a visible space character and return its visible offset.
    #[inline(always)]
    fn space(&mut self) -> Option<VisibleOffset> {
        let out = self.current_char.and_then(space_char_visible_size);
        if out.is_some() {
            self.next_input_char();
        }
        out
    }

    /// Consume all visible space characters and return their visible offset.
    #[inline(always)]
    fn spaces(&mut self) -> VisibleOffset {
        let mut total_visible_offset = VisibleOffset(0);
        while let Some(visible_offset) = self.space() {
            total_visible_offset += visible_offset;
        }
        total_visible_offset
    }

    /// Consume visible space characters and return their visible offset.
    #[inline(always)]
    fn spaces_with_limit(&mut self, limit: VisibleOffset) -> VisibleOffset {
        let mut total_visible_offset = VisibleOffset(0);
        while let Some(visible_offset) = self.space() {
            total_visible_offset += visible_offset;
            if total_visible_offset >= limit {
                break;
            }
        }
        total_visible_offset
    }
}



// ================================
// === Basic Character Checkers ===
// ================================

/// Check whether the provided character is a newline character.
#[inline(always)]
pub fn is_newline_char(t: char) -> bool {
    t == '\n' || t == '\r'
}

/// Check whether the provided character is a decimal digit.
#[inline(always)]
fn is_decimal_digit(t: char) -> bool {
    ('0'..='9').contains(&t)
}

/// Check whether the provided character is a binary digit.
#[inline(always)]
fn is_binary_digit(t: char) -> bool {
    ('0'..='1').contains(&t)
}

/// Check whether the provided character is an octal digit.
#[inline(always)]
fn is_octal_digit(t: char) -> bool {
    ('0'..='7').contains(&t)
}

/// Check whether the provided character is a hexadecimal digit.
#[inline(always)]
fn is_hexadecimal_digit(c: char) -> bool {
    decode_hexadecimal_digit(c).is_some()
}

#[inline(always)]
fn decode_hexadecimal_digit(c: char) -> Option<u8> {
    Some(match c {
        '0'..='9' => c as u8 - b'0',
        'a'..='f' => 10 + (c as u8 - b'a'),
        'A'..='F' => 10 + (c as u8 - b'A'),
        _ => return None,
    })
}

impl<'s> Lexer<'s> {
    #[inline(always)]
    fn take_rest_of_line(&mut self) {
        self.take_while(|t| !is_newline_char(t))
    }
}



// ========================
// === Ident & Operator ===
// ========================

/// # ASCII char table
/// Based on https://en.wikipedia.org/wiki/ASCII.
///
/// 21  !     3A  :     7B  {
/// 22  "     3B  ;     7C  |
/// 23  #     3C  <     7D  }
/// 24  $     3D  =     7E  ~
/// 25  %     3E  >
/// 26  &     3F  ?
/// 27  '     40  @
/// 28  (     [A-Z]
/// 29  )     
/// 2A  *     5B  [
/// 2B  +     5C  \
/// 2C  ,     5D  ]
/// 2D  -     5E  ^
/// 2E  .     5F  _
/// 2F  /     60  `
/// [0-9]     [a-z]

/// Check whether the provided character is an operator which should split the currently parsed
/// identifier.
#[inline(always)]
#[allow(clippy::manual_range_contains)]
fn is_ident_body_split_operator(t: char) -> bool {
    if t <= '\u{7E}' && t >= '\u{21}' {
        (t >= '\u{21}' && t <= '\u{26}') // ! " # $ % &
            // Skipped '
            || (t >= '\u{28}' && t <= '\u{2F}') // ( ) * + , - . /
            // Skipped [0-9]
            || (t >= '\u{3A}' && t <= '\u{40}') // : ; < = > ? @
            // Skipped [A-Z]
            || (t >= '\u{5B}' && t <= '\u{5E}') // [ \ ] ^
            // Skipped _
            || (t == '\u{60}') // `
            // Skipped [a-z]
            || (t >= '\u{7B}' && t <= '\u{7E}') // { | } ~
    } else {
        false
    }
}

/// Check if the provided character should be considered body of an operator name.
#[inline(always)]
#[allow(clippy::manual_range_contains)]
fn is_operator_body_char(t: char) -> bool {
    if t <= '\u{7E}' && t >= '\u{21}' {
        (t == '\u{21}') // !
            // Skipped " #
            || (t >= '\u{24}' && t <= '\u{26}') // $ % &
            // Skipped ' ( )
            || (t >= '\u{2A}' && t <= '\u{2F}') // * + , - . /
            // Skipped [0-9]
            || (t >= '\u{3A}' && t <= '\u{40}') // : ; < = > ? @
            // Skipped [A-Z]
            // Skipped [
            || (t == '\u{5C}') // \
            // Skipped ]
            || (t == '\u{5E}') // ^
            // Skipped _ `
            // Skipped [a-z]
            // Skipped {
            || (t == '\u{7C}') // |
            // Skipped }
            || (t == '\u{7E}') // ~
    } else {
        false
    }
}



// =============
// === Ident ===
// =============

/// Info about identifier being parsed.
#[derive(Clone, Copy, Debug)]
#[allow(missing_docs)]
pub struct IdentInfo {
    starts_with_underscore: bool,
    lift_level:             usize,
    starts_with_uppercase:  bool,
    is_default:             bool,
}

impl IdentInfo {
    /// Constructor.
    #[inline(always)]
    pub fn new(repr: &str) -> Self {
        let starts_with_underscore = repr.starts_with('_');
        let lift_level = repr.chars().rev().take_while(|t| *t == '\'').count();
        let starts_with_uppercase =
            repr.chars().next().map(|c| c.is_uppercase()).unwrap_or_default();
        let is_default = repr == "default";
        Self { starts_with_underscore, lift_level, starts_with_uppercase, is_default }
    }
}

/// Check whether the provided character should split currently parsed identifier.
#[inline(always)]
fn is_ident_split_char(t: char) -> bool {
    is_ident_body_split_operator(t) || is_space_char(t) || is_newline_char(t)
}

/// Check whether the provided character should be considered a body of identifier.
#[inline(always)]
fn is_ident_char(t: char) -> bool {
    !is_ident_split_char(t)
}

impl token::Variant {
    /// Convert the provided string to ident. The provided repr should contain valid identifier
    /// characters. This condition will not be checked.
    #[inline(always)]
    pub fn new_ident_unchecked(repr: &str) -> token::variant::Ident {
        let info = IdentInfo::new(repr);
        let is_operator = false;
        token::variant::Ident(
            info.starts_with_underscore,
            info.lift_level,
            info.starts_with_uppercase,
            is_operator,
            info.is_default,
        )
    }

    /// Convert the provided string to ident or wildcard. The provided repr should contain valid
    /// identifier characters. This condition will not be checked.
    #[inline(always)]
    pub fn new_ident_or_wildcard_unchecked(repr: &str) -> token::Variant {
        let info = IdentInfo::new(repr);
        if info.starts_with_underscore && repr.len() == 1 + info.lift_level {
            token::Variant::wildcard(info.lift_level)
        } else {
            let is_free = info.starts_with_underscore;
            let is_type = info.starts_with_uppercase;
            let is_operator = false;
            token::Variant::ident(is_free, info.lift_level, is_type, is_operator, info.is_default)
        }
    }
}

impl<'s> Lexer<'s> {
    /// Parse an identifier.
    fn ident(&mut self) {
        if let Some(token) = self.token(|this| {
            if this.ident_start_char() {
                this.take_while_1(is_ident_char);
            }
        }) {
            let tp = token::Variant::new_ident_or_wildcard_unchecked(&token.code);
            let token = token.with_variant(tp);
            self.submit_token(token);
        }
    }

    /// If the current char could start an identifier, consume it and return true; otherwise, return
    /// false.
    fn ident_start_char(&mut self) -> bool {
        if let Some(char) = self.current_char && is_ident_char(char) && char != '\'' {
            self.take_next();
            return true;
        }
        false
    }
}



// ================
// === Operator ===
// ================

impl<'s> Lexer<'s> {
    /// Parse an operator.
    fn operator(&mut self) {
        let token = self.token(|this| {
            if let Some(current) = this.current_char {
                match current {
                    '.' => this.take_while_1_('.'),
                    '=' => this.take_while_1_('='),
                    ':' | ',' => {
                        this.take_next();
                    }
                    _ => this.take_while_1_(is_operator_body_char),
                };
            }
        });
        if let Some(token) = token {
            if token.code == "+-" {
                let (left, right) = token.split_at_(Bytes(1));
                let lhs = analyze_operator(&left.code);
                self.submit_token(left.with_variant(token::Variant::operator(lhs)));
                let rhs = analyze_operator(&right.code);
                self.submit_token(right.with_variant(token::Variant::operator(rhs)));
                return;
            }
            if token.code == "..." {
                let token = token.with_variant(token::Variant::auto_scope());
                self.submit_token(token);
                return;
            }
            let tp = token::Variant::operator(analyze_operator(&token.code));
            let token = token.with_variant(tp);
            self.submit_token(token);
        }
    }
}


// === Precedence ===

fn analyze_operator(token: &str) -> token::OperatorProperties {
    let mut operator = token::OperatorProperties::new();
    if token.ends_with("->") && !token.starts_with("<-") {
        operator = operator.as_right_associative();
    }
    match token {
        // Operators that can be unary.
        "\\" =>
            return operator
                .with_unary_prefix_mode(token::Precedence::min_valid())
                .as_compile_time_operation(),
        "~" =>
            return operator
                .with_unary_prefix_mode(token::Precedence::max())
                .as_compile_time_operation()
                .as_suspension(),
        "@" =>
            return operator
                .with_unary_prefix_mode(token::Precedence::max())
                .with_binary_infix_precedence(20)
                .as_compile_time_operation()
                .as_annotation(),
        "-" =>
            return operator
                .with_unary_prefix_mode(token::Precedence::unary_minus())
                .with_binary_infix_precedence(15),
        // "There are a few operators with the lowest precedence possible."
        // - These 3 "consume everything to the right".
        "=" =>
            return operator
                .with_binary_infix_precedence(1)
                .as_right_associative()
                .with_lhs_section_termination(operator::SectionTermination::Unwrap)
                .as_assignment(),
        ":" =>
            return operator
                .with_binary_infix_precedence(2)
                .as_right_associative()
                .with_lhs_section_termination(operator::SectionTermination::Reify)
                .as_compile_time_operation()
                .as_type_annotation(),
        "->" =>
            return operator
                .with_binary_infix_precedence(2)
                .as_right_associative()
                .with_lhs_section_termination(operator::SectionTermination::Unwrap)
                .as_compile_time_operation()
                .as_arrow(),
        "!" => return operator.with_binary_infix_precedence(3),
        "||" | "\\\\" | "&&" => return operator.with_binary_infix_precedence(4),
        ">>" | "<<" => return operator.with_binary_infix_precedence(5),
        "|>" | "|>>" => return operator.with_binary_infix_precedence(6),
        "<|" | "<<|" => return operator.with_binary_infix_precedence(6).as_right_associative(),
        // Other special operators.
        "<=" | ">=" => return operator.with_binary_infix_precedence(14),
        "==" | "!=" => return operator.with_binary_infix_precedence(5),
        "," =>
            return operator
                .with_binary_infix_precedence(1)
                .as_compile_time_operation()
                .as_special()
                .as_sequence(),
        "." =>
            return operator.with_binary_infix_precedence(80).with_decimal_interpretation().as_dot(),
        _ => (),
    }
    // "The precedence of all other operators is determined by the operator's Precedence Character:"
    let mut precedence_char = None;
    for c in token.chars() {
        match (c, precedence_char) {
            ('<' | '-', None) | ('-', Some('<')) => {
                precedence_char = Some(c);
            }
            _ => {
                precedence_char = Some(c);
                break;
            }
        }
    }
    let binary = match precedence_char.unwrap() {
        '!' => 10,
        '|' => 11,
        '^' => 12,
        '&' => 13,
        '<' | '>' => 14,
        '+' | '-' => 15,
        '*' | '/' | '%' => 16,
        _ => 17,
    };
    operator.with_binary_infix_precedence(binary)
}



// ===============
// === Symbols ===
// ===============

impl<'s> Lexer<'s> {
    /// Parse a symbol.
    fn symbol(&mut self) {
        if let Some(token) = self.token(|this| this.take_1(&['(', '{', '['])) {
            self.submit_token(token.with_variant(token::Variant::open_symbol()));
        }
        if let Some(token) = self.token(|this| this.take_1(&[')', '}', ']'])) {
            self.submit_token(token.with_variant(token::Variant::close_symbol()));
        }
    }
}



// ==============
// === Number ===
// ==============

impl<'s> Lexer<'s> {
    /// Parse a number.
    fn number(&mut self) {
        let mut base = None;
        let token = self.token(|this| {
            let mut old_hex_chars_matched = 0;
            let mut old_bin_chars_matched = 0;
            let mut new_based_chars_matched = 0;
            match this.current_char {
                Some('0') => new_based_chars_matched = 1,
                Some('1') => old_hex_chars_matched = 1,
                Some('2') => old_bin_chars_matched = 1,
                Some(d) if is_decimal_digit(d) => (),
                _ => return,
            }
            this.next_input_char();
            let mut prev_was_underscore = false;
            match this.current_char {
                Some('_') if old_bin_chars_matched == 1 => base = Some(token::Base::Binary),
                Some('_') => prev_was_underscore = true,
                Some('b') if new_based_chars_matched == 1 => base = Some(token::Base::Binary),
                Some('o') if new_based_chars_matched == 1 => base = Some(token::Base::Octal),
                Some('x') if new_based_chars_matched == 1 => base = Some(token::Base::Hexadecimal),
                Some('6') if old_hex_chars_matched == 1 => old_hex_chars_matched = 2,
                Some(d) if is_decimal_digit(d) => (),
                _ => return,
            }
            this.next_input_char();
            if base.is_some() {
                return;
            }
            let mut was_underscore = false;
            match this.current_char {
                Some('_') if old_hex_chars_matched == 2 => {
                    base = Some(token::Base::Hexadecimal);
                    this.next_input_char();
                    return;
                }
                Some('_') if !prev_was_underscore => was_underscore = true,
                Some(d) if is_decimal_digit(d) => (),
                _ => return,
            }
            prev_was_underscore = was_underscore;
            this.next_input_char();
            loop {
                let mut was_underscore = false;
                match this.current_char {
                    Some('_') if !prev_was_underscore => was_underscore = true,
                    Some(d) if is_decimal_digit(d) => (),
                    _ => return,
                }
                prev_was_underscore = was_underscore;
                this.next_input_char();
            }
        });
        if let Some(token) = token {
            if let Some(base) = base {
                self.submit_token(token.with_variant(token::Variant::number_base()));
                let token = match base {
                    token::Base::Binary => self.token(|this| this.take_while(is_binary_digit)),
                    token::Base::Octal => self.token(|this| this.take_while(is_octal_digit)),
                    token::Base::Hexadecimal =>
                        self.token(|this| this.take_while(is_hexadecimal_digit)),
                };
                if let Some(token) = token {
                    self.submit_token(token.with_variant(token::Variant::digits(Some(base))));
                }
            } else {
                self.submit_token(token.with_variant(token::Variant::digits(None)));
            }
        }
    }
}



// ============
// === Text ===
// ============

impl<'s> Lexer<'s> {
    /// Read a text literal.
    fn text(&mut self) {
        let (quote_char, text_type) = match self.current_char {
            Some(char @ '"') => (char, TextType::Raw),
            Some(char @ '\'') => (char, TextType::Interpolated),
            Some('`') => {
                if let Some(state) = self.stack.pop() {
                    self.end_splice(state);
                } else {
                    let token = self.token(|this| this.take_next()).unwrap();
                    self.submit_token(token.with_variant(token::Variant::invalid()));
                }
                return;
            }
            _ => return,
        };
        let indent = self.current_block_indent;
        let open_quote_start = self.mark();
        self.last_spaces_visible_offset = VisibleOffset(0);
        self.last_spaces_offset = Bytes(0);
        self.take_next();
        // At least two quote characters.
        if let Some(char) = self.current_char && char == quote_char {
            let close_quote_start = self.mark();
            self.take_next();
            let mut multiline = false;
            // If more than two quote characters: Start a multiline quote.
            while let Some(char) = self.current_char && char == quote_char {
                multiline = true;
                self.take_next();
            }
            if multiline {
                self.multiline_text(open_quote_start, indent, text_type);
                return;
            } else {
                // Exactly two quote characters: Open and shut case.
                let close_quote_end = self.mark();
                let token = self.make_token(open_quote_start, close_quote_start.clone(),
                    token::Variant::text_start());
                self.output.push(token);
                let token = self.make_token(close_quote_start, close_quote_end,
                    token::Variant::text_end());
                self.output.push(token);
            }
        } else {
            // One quote followed by non-quote character: Inline quote.
            let open_quote_end = self.mark();
            let token = self.make_token(open_quote_start, open_quote_end,
                token::Variant::text_start());
            self.output.push(token);
            self.inline_quote(quote_char, text_type);
        }
        self.spaces_after_lexeme();
    }

    fn multiline_text(
        &mut self,
        open_quote_start: (Bytes, Offset<'s>),
        block_indent: VisibleOffset,
        text_type: TextType,
    ) {
        let open_quote_end = self.mark();
        let token =
            self.make_token(open_quote_start, open_quote_end.clone(), token::Variant::text_start());
        self.output.push(token);
        let mut initial_indent = None;
        if text_type.expects_initial_newline() && let Some(newline) = self.line_break() {
            self.output.push(newline.with_variant(token::Variant::text_initial_newline()));
            if self.last_spaces_visible_offset > block_indent {
                initial_indent = self.last_spaces_visible_offset.into();
            }
        }
        self.text_content(None, text_type.is_interpolated(), State::MultilineText {
            block_indent,
            initial_indent,
        });
    }

    fn inline_quote(&mut self, quote_char: char, text_type: TextType) {
        let is_interpolated = text_type.is_interpolated();
        self.text_content(quote_char.into(), is_interpolated, State::InlineText);
    }

    fn end_splice(&mut self, state: State) {
        let splice_quote_start = self.mark();
        self.take_next();
        let splice_quote_end = self.mark();
        let token =
            self.make_token(splice_quote_start, splice_quote_end, token::Variant::close_symbol());
        self.output.push(token);
        match state {
            State::InlineText => self.inline_quote('\'', TextType::Interpolated),
            State::MultilineText { .. } => {
                self.text_content(None, true, state);
            }
        }
    }

    fn text_content(
        &mut self,
        closing_char: Option<char>,
        interpolate: bool,
        mut state: State,
    ) -> TextEndedAt {
        let mut text_start = self.mark();
        let is_multiline = matches!(state, State::MultilineText { .. });
        while let Some(char) = self.current_char {
            if closing_char == Some(char) || (!is_multiline && is_newline_char(char)) {
                break;
            }
            if let State::MultilineText { block_indent, initial_indent } = &mut state {
                // Consume newlines and following whitespace until we encounter a line that, after
                // left-trimming, is not empty.
                //
                // Buffer the newline tokens, because whether they are interpreted as part of the
                // text content or code formatting after the block depends on whether non-empty text
                // lines follow.
                let mut newlines = vec![];
                let mut new_indent = None;
                loop {
                    let mut before_newline = self.mark();
                    if before_newline.0 == text_start.0 {
                        before_newline = text_start.clone();
                    }
                    let mut newline = self.take_1('\r');
                    newline = self.take_1('\n') || newline;
                    if !newline {
                        break;
                    }
                    let token = self.make_token(
                        text_start.clone(),
                        before_newline.clone(),
                        token::Variant::text_section(),
                    );
                    if !(token.code.is_empty() && token.left_offset.code.is_empty()) {
                        self.output.push(token);
                    } else {
                        before_newline = text_start;
                    }
                    let newline_end = self.mark();
                    let token =
                        self.make_token(before_newline, newline_end, token::Variant::newline());
                    newlines.push(token);
                    if let Some(initial) = *initial_indent {
                        let trim = std::cmp::max(initial, *block_indent + MIN_TEXT_TRIM);
                        self.spaces_after_lexeme_with_limit(trim);
                    } else {
                        self.spaces_after_lexeme();
                    }
                    let new_indent_ = self.last_spaces_visible_offset;
                    new_indent = new_indent_.into();
                    if initial_indent.is_none() && new_indent_ > *block_indent {
                        *initial_indent = new_indent_.into();
                    }
                    text_start = self.mark();
                }
                if let Some(indent) = new_indent {
                    if indent <= *block_indent {
                        self.output.push(Token::from(token::text_end("", "")));
                        self.end_blocks(indent);
                        self.output.extend(newlines);
                        if self.current_offset == text_start.0 {
                            self.last_spaces_visible_offset = text_start.1.visible;
                            self.last_spaces_offset = text_start.1.code.len();
                        }
                        return TextEndedAt::End;
                    }
                    let newlines = newlines
                        .into_iter()
                        .map(|token| token.with_variant(token::Variant::text_section()));
                    self.output.extend(newlines);
                    continue;
                }
            }
            if interpolate && char == '\\' {
                let mut backslash_start = self.mark();
                self.take_next();
                if let Some(char) = self.current_char {
                    let token = self.make_token(
                        text_start.clone(),
                        backslash_start.clone(),
                        token::Variant::text_section(),
                    );
                    if token.code.is_empty() {
                        backslash_start = text_start.clone();
                    } else {
                        self.output.push(token);
                    }
                    text_start = self.text_escape(backslash_start, char);
                    continue;
                }
                continue;
            }
            if interpolate && char == '`' {
                let mut splice_quote_start = self.mark();
                let token = self.make_token(
                    text_start.clone(),
                    splice_quote_start.clone(),
                    token::Variant::text_section(),
                );
                if token.code.is_empty() {
                    splice_quote_start = text_start;
                } else {
                    self.output.push(token);
                }
                self.take_next();
                let splice_quote_end = self.mark();
                let token = self.make_token(
                    splice_quote_start,
                    splice_quote_end.clone(),
                    token::Variant::open_symbol(),
                );
                self.output.push(token);
                self.stack.push(state);
                return TextEndedAt::Splice;
            }
            self.take_next();
        }
        let text_end = self.mark();
        let token = self.make_token(text_start, text_end.clone(), token::Variant::text_section());
        if !(token.code.is_empty() && token.left_offset.code.is_empty()) {
            self.output.push(token);
        }
        let end_token = if self.current_char == closing_char {
            self.take_next();
            let close_quote_end = self.mark();
            self.make_token(text_end, close_quote_end, token::Variant::text_end())
        } else {
            Token::from(token::text_end("", ""))
        };
        self.output.push(end_token);
        TextEndedAt::End
    }

    fn text_escape(
        &mut self,
        backslash_start: (Bytes, Offset<'s>),
        char: char,
    ) -> (Bytes, Offset<'s>) {
        let leader = match char {
            'x' => Some((2, false)),
            'u' => Some((4, true)),
            'U' => Some((8, false)),
            _ => None,
        };
        if let Some((mut expect_len, accepts_delimiter)) = leader {
            self.take_next();
            let delimited = accepts_delimiter && self.current_char == Some('{');
            if delimited {
                self.take_next();
                expect_len = 6;
            }
            let mut value: u32 = 0;
            for _ in 0..expect_len {
                if let Some(c) = self.current_char && let Some(x) = decode_hexadecimal_digit(c) {
                    value = 16 * value + x as u32;
                    self.take_next();
                } else {
                    break;
                }
            }
            let value = char::from_u32(value);
            if delimited && self.current_char == Some('}') {
                self.take_next();
            }
            let sequence_end = self.mark();
            let token = self.make_token(
                backslash_start,
                sequence_end.clone(),
                token::Variant::text_escape(value),
            );
            self.output.push(token);
            sequence_end
        } else {
            let value = match char {
                '0' => Some('\0'),
                'a' => Some('\x07'),
                'b' => Some('\x08'),
                'f' => Some('\x0C'),
                'n' => Some('\x0A'),
                'r' => Some('\x0D'),
                't' => Some('\x09'),
                'v' => Some('\x0B'),
                'e' => Some('\x1B'),
                '\\' => Some('\\'),
                '"' => Some('"'),
                '\'' => Some('\''),
                '`' => Some('`'),
                _ => None,
            };
            self.take_next();
            let escape_end = self.mark();
            let token = self.make_token(
                backslash_start,
                escape_end.clone(),
                token::Variant::text_escape(value),
            );
            self.output.push(token);
            escape_end
        }
    }

    fn mark(&mut self) -> (Bytes, Offset<'s>) {
        let start = self.current_offset;
        let left_offset_start = start - self.last_spaces_offset;
        let offset_code = self.input.slice(left_offset_start..start);
        let visible_offset = self.last_spaces_visible_offset;
        self.last_spaces_visible_offset = VisibleOffset(0);
        self.last_spaces_offset = Bytes(0);
        (start, Offset(visible_offset, offset_code))
    }

    fn make_token(
        &self,
        from: (Bytes, Offset<'s>),
        to: (Bytes, Offset<'s>),
        variant: token::Variant,
    ) -> Token<'s> {
        let (start, offset) = from;
        let end = to.0;
        let start = start.unchecked_raw();
        let end = end.unchecked_raw();
        Token(offset, &self.input[start..end], variant)
    }
}

#[derive(PartialEq, Eq)]
enum TextEndedAt {
    Splice,
    End,
}

#[derive(PartialEq, Eq, Copy, Clone, Debug)]
enum TextType {
    Raw,
    Interpolated,
    Documentation,
}

impl TextType {
    fn is_interpolated(self) -> bool {
        self == TextType::Interpolated
    }

    fn expects_initial_newline(self) -> bool {
        self != TextType::Documentation
    }
}



// ================
// === Comments ===
// ================

impl<'s> Lexer<'s> {
    #[inline(always)]
    fn submit_line_as(&mut self, kind: token::Variant) {
        let token = self.token(|this| this.take_rest_of_line());
        if let Some(token) = token {
            self.submit_token(token.with_variant(kind));
        }
    }

    fn comment(&mut self) {
        if let Some('#') = self.current_char {
            let indent = self.current_block_indent;
            let start = self.mark();
            self.take_next();
            if let Some('#') = self.current_char {
                self.take_next();
                self.multiline_text(start, indent, TextType::Documentation);
            } else {
                self.take_rest_of_line();
                let end_line = self.mark();
                self.output.push(self.make_token(start, end_line, token::Variant::newline()));
            }
        }
    }
}



// =============
// === Block ===
// =============

impl<'s> Lexer<'s> {
    #[allow(clippy::collapsible_if)]
    fn line_break(&mut self) -> Option<Token<'s, ()>> {
        self.token(|this| {
            if !this.take_1('\n') {
                if this.take_1('\r') {
                    this.take_1('\n');
                }
            }
        })
    }

    fn newline(&mut self) {
        if let Some(token) = self.line_break() {
            let mut newlines = self.token_storage.take();
            while let Some(token) = self.line_break() {
                newlines.push(token.with_variant(token::Variant::newline()));
            }
            let block_indent = self.last_spaces_visible_offset;
            if block_indent > self.current_block_indent {
                let block_start = self.marker_token(token::Variant::block_start());
                self.submit_token(block_start);
                self.start_block(block_indent);
            }
            self.end_blocks(block_indent);
            self.submit_token(token.with_variant(token::Variant::newline()));
            newlines.drain(..).for_each(|token| self.submit_token(token));
            self.token_storage.set_from(newlines);
        }
    }

    fn end_blocks(&mut self, block_indent: VisibleOffset) {
        while block_indent < self.current_block_indent {
            let Some(previous_indent) = self.block_indent_stack.last().copied() else {
                // If the file starts at indent > 0, we treat that as the root indent level
                // instead of creating a sub-block. If indent then decreases below that level,
                // there's no block to exit.
                break
            };
            if block_indent > previous_indent {
                // The new line indent is smaller than current block but bigger than the
                // previous one. We are treating the line as belonging to the
                // block. The warning should be reported by parser.
                break;
            }
            self.end_block();
            let block_end = self.marker_token(token::Variant::block_end());
            self.submit_token(block_end);
        }
    }
}



// ============
// === Glue ===
// ============

/// All defined parsers in order they should be fired. The order is determined by two factors:
/// 1. The most common parsers should be first in order to minimize comparison for each new char.
/// 2. Some parsers could consume input even if it should be qualified as something else. Thus, some
///    parsers should be run first in order to make the token consuming process correct.
const PARSERS: &[for<'r> fn(&'r mut Lexer<'_>)] = &[
    |t| t.number(),
    |t| t.ident(),
    |t| t.operator(),
    |t| t.newline(),
    |t| t.symbol(),
    |t| t.comment(),
    |t| t.text(),
];

impl<'s> Lexer<'s> {
    /// Run the lexer. Return hierarchical list of tokens (the token groups will be represented as a
    /// hierarchy).
    pub fn run(self) -> ParseResult<Vec<Item<'s>>> {
        self.run_flat().map(build_block_hierarchy)
    }

    /// Run the lexer. Return non-hierarchical list of tokens (the token groups will be represented
    /// as start and end tokens).
    pub fn run_flat(mut self) -> ParseResult<Vec<Token<'s>>> {
        self.spaces_after_lexeme();
        self.current_block_indent = self.last_spaces_visible_offset;
        let mut any_parser_matched = true;
        while any_parser_matched {
            any_parser_matched = false;
            for f in PARSERS {
                if self.run_and_check_if_progressed(f) {
                    any_parser_matched = true;
                    break;
                }
            }
        }
        while self.end_block().is_some() {
            let block_end = self.marker_token(token::Variant::block_end());
            self.submit_token(block_end);
        }
        if self.last_spaces_visible_offset != VisibleOffset(0) {
            let left_offset_start = self.current_offset - self.last_spaces_offset;
            let offset_code = self.input.slice(left_offset_start..self.current_offset);
            let visible_offset = self.last_spaces_visible_offset;
            let offset = Offset(visible_offset, offset_code);
            let eof = token::variant::Variant::Newline(token::variant::Newline());
            self.submit_token(Token(offset, "", eof));
        }
        let mut internal_error = self.internal_error.take();
        if self.current_char.is_some() {
            let message = format!("Lexer did not consume all input. State: {self:?}");
            internal_error.get_or_insert(message);
        }
        let value = self.output;
        trace!("Tokens:\n{:#?}", value);
        ParseResult { value, internal_error }
    }
}

/// Run the lexer. Return non-hierarchical list of tokens (the token groups will be represented
/// as start and end tokens).
pub fn run_flat(input: &'_ str) -> ParseResult<Vec<Token<'_>>> {
    Lexer::new(input).run_flat()
}

/// Run the lexer. Return hierarchical list of tokens (the token groups will be represented as a
/// hierarchy).
pub fn run(input: &'_ str) -> ParseResult<Vec<Item<'_>>> {
    Lexer::new(input).run()
}

/// Convert the flat token stream into hierarchical one. The token variants [`BlockStart`] and
/// [`BlockEnd`] will be replaced with [`Item::Group`].
pub fn build_block_hierarchy(tokens: Vec<Token<'_>>) -> Vec<Item<'_>> {
    let mut stack = vec![];
    let mut out: Vec<Item<'_>> = vec![];
    for token in tokens {
        match token.variant {
            token::Variant::BlockStart(_) => stack.push(mem::take(&mut out)),
            token::Variant::BlockEnd(_) => {
                let new_out = stack.pop().unwrap();
                let block = mem::replace(&mut out, new_out);
                out.push(Item::Block(block));
            }
            _ => out.push(token.into()),
        }
    }
    if !stack.is_empty() {
        panic!("Internal error. Block start token not paired with block end token.");
    }
    out
}



// =============
// === Tests ===
// =============

/// Test utils for fast mock tokens creation.
pub mod test {
    use super::*;
    pub use token::*;

    /// Constructor.
    pub fn ident_<'s>(left_offset: &'s str, code: &'s str) -> Token<'s> {
        let is_free = code.starts_with('_');
        let lift_level = code.chars().rev().take_while(|t| *t == '\'').count();
        let is_uppercase = code.chars().next().map(|c| c.is_uppercase()).unwrap_or_default();
        let is_operator = false;
        token::ident_(left_offset, code, is_free, lift_level, is_uppercase, is_operator, false)
    }

    /// Constructor.
    pub fn wildcard_<'s>(left_offset: &'s str, code: &'s str) -> Token<'s> {
        let lift_level = code.chars().rev().take_while(|t| *t == '\'').count();
        token::wildcard_(left_offset, code, lift_level)
    }

    /// Constructor.
    pub fn operator_<'s>(left_offset: &'s str, code: &'s str) -> Token<'s> {
        Token(left_offset, code, token::Variant::operator(analyze_operator(code)))
    }
}

#[cfg(test)]
mod tests {
    use super::test::*;
    use super::*;

    fn test_lexer_many<'s>(inputs: Vec<(&'s str, Vec<Token<'s>>)>) {
        for (input, output) in inputs {
            test_lexer(input, output)
        }
    }

    fn test_lexer<'s>(input: &'s str, expected: Vec<Token<'s>>) {
        assert_eq!(run_flat(input).unwrap(), expected);
    }

    fn lexer_case_idents<'s>(idents: &[&'s str]) -> Vec<(&'s str, Vec<Token<'s>>)> {
        idents.iter().map(|t| lexer_case_ident(t)).collect()
    }

    fn lexer_case_ident(code: &str) -> (&str, Vec<Token<'_>>) {
        (code, vec![ident_("", code)])
    }

    fn lexer_case_operators<'s>(operators: &[&'s str]) -> Vec<(&'s str, Vec<Token<'s>>)> {
        operators.iter().map(|t| lexer_case_operator(t)).collect()
    }

    fn lexer_case_operator(code: &str) -> (&str, Vec<Token<'_>>) {
        (code, vec![operator_("", code)])
    }

    #[test]
    fn test_case_block() {
        test_lexer_many(vec![
            ("\n", vec![newline_("", "\n")]),
            ("\n  foo\n  bar", vec![
                block_start_("", ""),
                newline_("", "\n"),
                ident_("  ", "foo"),
                newline_("", "\n"),
                ident_("  ", "bar"),
                block_end_("", ""),
            ]),
            ("foo\n    +", vec![
                ident_("", "foo"),
                block_start_("", ""),
                newline_("", "\n"),
                operator_("    ", "+"),
                block_end_("", ""),
            ]),
        ]);
    }

    #[test]
    fn test_case_block_bad_indents() {
        #[rustfmt::skip]
        test_lexer_many(vec![
            ("\n  foo\n bar\nbaz", vec![
                block_start_("", ""),
                newline_("", "\n"), ident_("  ", "foo"),
                newline_("", "\n"), ident_(" ", "bar"),
                block_end_("", ""),
                newline_("", "\n"), ident_("", "baz"),
            ]),
            ("\n  foo\n bar\n  baz", vec![
                block_start_("", ""),
                newline_("", "\n"), ident_("  ", "foo"),
                newline_("", "\n"), ident_(" ", "bar"),
                newline_("", "\n"), ident_("  ", "baz"),
                block_end_("", ""),
            ]),
        ]);
    }

    #[test]
    fn test_case_whitespace_only_line() {
        test_lexer_many(vec![("foo\n    \nbar", vec![
            ident_("", "foo"),
            newline_("", "\n"),
            newline_("    ", "\n"),
            ident_("", "bar"),
        ])]);
    }

    #[test]
    fn test_case_utf_8_idents() {
        test_lexer_many(lexer_case_idents(&[
            "test",
            "你好",
            "cześć",
            "GrüßGott",
            "Nǐhǎo",
            "hyvääpäivää",
            "Góðandag",
            "Moïen",
            "Namastē",
            "やあ",
            "đượchậuđải",
            "❤️foo",
        ]))
    }

    #[test]
    fn test_numeric_literal() {
        test_lexer("10", vec![digits_("", "10", None)]);
    }

    #[test]
    fn test_case_idents() {
        test_lexer_many(vec![
            ("", vec![]),
            ("_", vec![wildcard_("", "_")]),
            ("_'", vec![wildcard_("", "_'")]),
            ("_''", vec![wildcard_("", "_''")]),
        ]);
        test_lexer_many(lexer_case_idents(&[
            "a",
            "a'",
            "a''",
            "a'''",
            "_a",
            "_a'",
            "_a''",
            "_a'''",
            "__a",
            "___a",
            "_a_",
            "__a__",
            "_a_b_",
            "Test_Name",
            "Test_Name'",
            "a'b",
            "a'b'",
            "a'b''",
        ]));
        for zero_space in UNICODE_ZERO_SPACES.chars() {
            let var = format!("pre{}post", zero_space);
            test_lexer(&var, vec![ident_("", &var)])
        }
    }

    #[test]
    fn test_case_operators() {
        test_lexer_many(lexer_case_operators(&["+", "-", "=", "==", "===", ":", ","]));
        let properties = analyze_operator("-");
        let unary_minus = Token("", "-", token::Variant::operator(properties));
        test_lexer_many(vec![("+-", vec![operator_("", "+"), unary_minus])]);
    }

    /// Based on https://www.cl.cam.ac.uk/~mgk25/ucs/examples/UTF-8-test.txt.
    /// With additional explanation here:
    /// https://stackoverflow.com/questions/1319022/really-good-bad-utf-8-example-test-data.
    ///
    /// Please note, that there is a comment on StackOverflow:
    /// > I'd warn you his test is based on an outdated definition of UTF-8, when 5 and 6 byte
    /// > sequences were allowed, before planes 17 and above were deleted. And it implies that
    /// > codepoints U+FFFE and U+FFFF are invalid in UTF-8, when per the Unicode consortium
    /// > [they are not](http://www.unicode.org/faq/private_use.html#nonchar8).
    ///
    /// Our test case should check if these codes are correctly parsed as identifiers, so even if
    /// not all cases are covered and some comments in the original document are outdated, these
    /// are still nice test sets.
    #[test]
    fn test_case_utf8() {
        macro_rules! lexer_test_ident_stream {
            ($($input:literal)*) => {
                test_lexer_many(vec![ $( ($input, vec![ident_("", $input)]) ),* ])
            };
        }
        lexer_test_ident_stream! {
            // === 1. Example correct UTF-8 text. ===

            /* 1.1 The Greek word 'kosme': */ "κόσμε"


            // === 2. Boundary condition test cases. ===

            // 2.1. First possible sequence of a certain length.
            /* 2.1.1. 1 byte  (U-00000000): */  "�"
            /* 2.1.2. 2 bytes (U-00000080): */  ""
            /* 2.1.3. 3 bytes (U-00000800): */  "ࠀ"
            /* 2.1.4. 4 bytes (U-00010000): */  "𐀀"
            /* 2.1.5. 5 bytes (U-00200000): */  "�����"
            /* 2.1.6. 6 bytes (U-04000000): */  "������"

            // 2.2. Last possible sequence of a certain length.
            /* 2.2.1. 1 byte  (U-0000007F): */  ""
            /* 2.2.2. 2 bytes (U-000007FF): */  "߿"
            /* 2.2.3. 3 bytes (U-0000FFFF): */  "￿"
            /* 2.2.4. 4 bytes (U-001FFFFF): */  "����"
            /* 2.2.5. 5 bytes (U-03FFFFFF): */  "�����"
            /* 2.2.6. 6 bytes (U-7FFFFFFF): */  "������"

            // 2.3. Other boundary conditions.
            /* 2.3.1. U-0000D7FF = ed 9f bf = */    "퟿"
            /* 2.3.2. U-0000E000 = ee 80 80 = */    ""
            /* 2.3.3. U-0000FFFD = ef bf bd = */    "�"
            /* 2.3.4. U-0010FFFF = f4 8f bf bf = */ "􏿿"
            /* 2.3.5. U-00110000 = f4 90 80 80 = */ "����"


            // === 3. Malformed sequences ===

            // 3.1. Unexpected continuation bytes.
            /* 3.1.1. First continuation byte 0x80: */ "�"
            /* 3.1.2. Last  continuation byte 0xbf: */ "�"
            /* 3.1.3. 2 continuation bytes:         */ "��"
            /* 3.1.4. 3 continuation bytes:         */ "���"
            /* 3.1.5. 4 continuation bytes:         */ "����"
            /* 3.1.6. 5 continuation bytes:         */ "�����"
            /* 3.1.7. 6 continuation bytes:         */ "������"
            /* 3.1.8. 7 continuation bytes:         */ "�������"
            /* 3.1.9. Sequence of all 64 possible continuation bytes (0x80-0xbf):*/
                "����������������������������������������������������������������"
            // 3.2. Lonely start characters.
            /* 3.2.1. All 32 first bytes of 2-byte sequences (0xc0-0xdf): */
                "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�"
                "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�"
            /* 3.2.2. All 16 first bytes of 3-byte sequences (0xe0-0xef): */
               "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�"
            /* 3.2.3. All 8 first bytes of 4-byte sequences (0xf0-0xf7): */
               "�" "�" "�" "�" "�" "�" "�" "�"
            /* 3.2.4. All 4 first bytes of 5-byte sequences (0xf8-0xfb): */
               "�" "�" "�" "�"
            /* 3.2.5. All 2 first bytes of 6-byte sequences (0xfc-0xfd): */
               "�" "�"

            // 3.3. Sequences with last continuation byte missing.
            /* 3.3.1. 2-byte sequence with last byte missing (U+0000):     */ "�"
            /* 3.3.2. 3-byte sequence with last byte missing (U+0000):     */ "��"
            /* 3.3.3. 4-byte sequence with last byte missing (U+0000):     */ "���"
            /* 3.3.4. 5-byte sequence with last byte missing (U+0000):     */ "����"
            /* 3.3.5. 6-byte sequence with last byte missing (U+0000):     */ "�����"
            /* 3.3.6. 2-byte sequence with last byte missing (U-000007FF): */ "�"
            /* 3.3.7. 3-byte sequence with last byte missing (U-0000FFFF): */ "�"
            /* 3.3.8. 4-byte sequence with last byte missing (U-001FFFFF): */ "���"
            /* 3.3.9. 5-byte sequence with last byte missing (U-03FFFFFF): */ "����"
            /* 3.3.10. 6-byte sequence with last byte missing (U-7FFFFFFF): */ "�����"

            // 3.4. Concatenation of incomplete sequences.
                "�����������������������������"

            // 3.5. Impossible bytes. The following two bytes cannot appear in a correct UTF-8 str:
            /* 3.5.1. fe          = */ "�"
            /* 3.5.2. ff          = */ "�"
            /* 3.5.3. fe fe ff ff = */ "����"


            // === 4. Overlong sequences ===

            // 4.1. Examples of an overlong ASCII character.
            /* 4.1.1. U+002F = c0 af             = */ "��"
            /* 4.1.2. U+002F = e0 80 af          = */ "���"
            /* 4.1.3. U+002F = f0 80 80 af       = */ "����"
            /* 4.1.4. U+002F = f8 80 80 80 af    = */ "�����"
            /* 4.1.5. U+002F = fc 80 80 80 80 af = */ "������"

            // 4.2. Maximum overlong sequences.
            /* 4.2.1  U-0000007F = c1 bf             = */ "��"
            /* 4.2.2  U-000007FF = e0 9f bf          = */ "���"
            /* 4.2.3  U-0000FFFF = f0 8f bf bf       = */ "����"
            /* 4.2.4  U-001FFFFF = f8 87 bf bf bf    = */ "�����"
            /* 4.2.5  U-03FFFFFF = fc 83 bf bf bf bf = */ "������"

            // 4.3. Overlong representation of the NUL character.
            /* 4.3.1  U+0000 = c0 80             = */ "��"
            /* 4.3.2  U+0000 = e0 80 80          = */ "���"
            /* 4.3.3  U+0000 = f0 80 80 80       = */ "����"
            /* 4.3.4  U+0000 = f8 80 80 80 80    = */ "�����"
            /* 4.3.5  U+0000 = fc 80 80 80 80 80 = */ "������"


            // === 5. Illegal code positions ===

            // 5.1. Single UTF-16 surrogates.
            /* 5.1.1  U+D800 = ed a0 80 = */ "���"
            /* 5.1.2  U+DB7F = ed ad bf = */ "���"
            /* 5.1.3  U+DB80 = ed ae 80 = */ "���"
            /* 5.1.4  U+DBFF = ed af bf = */ "���"
            /* 5.1.5  U+DC00 = ed b0 80 = */ "���"
            /* 5.1.6  U+DF80 = ed be 80 = */ "���"
            /* 5.1.7  U+DFFF = ed bf bf = */ "���"

            // 5.2. Paired UTF-16 surrogates.
            /* 5.2.1  U+D800 U+DC00 = ed a0 80 ed b0 80 = */ "������"
            /* 5.2.2  U+D800 U+DFFF = ed a0 80 ed bf bf = */ "������"
            /* 5.2.3  U+DB7F U+DC00 = ed ad bf ed b0 80 = */ "������"
            /* 5.2.4  U+DB7F U+DFFF = ed ad bf ed bf bf = */ "������"
            /* 5.2.5  U+DB80 U+DC00 = ed ae 80 ed b0 80 = */ "������"
            /* 5.2.6  U+DB80 U+DFFF = ed ae 80 ed bf bf = */ "������"
            /* 5.2.7  U+DBFF U+DC00 = ed af bf ed b0 80 = */ "������"
            /* 5.2.8  U+DBFF U+DFFF = ed af bf ed bf bf = */ "������"
        }
    }
}



#[cfg(test)]
mod benches {
    use super::*;
    extern crate test;
    use test::Bencher;

    #[bench]
    fn bench_str_iter(b: &mut Bencher) {
        let reps = 1_000_000;
        let str = "test ".repeat(reps);

        b.iter(move || str.chars().for_each(drop));
    }

    #[bench]
    fn bench_str_iter_and_compare(b: &mut Bencher) {
        let reps = 1_000_000;
        let str = "test ".repeat(reps);

        b.iter(move || {
            let mut sum = 0;
            str.chars().for_each(|t| {
                if t == 't' {
                    sum += 1;
                }
            })
        });
    }

    /// 7-13x slowdown in comparison to [`bench_str_iter`] and [`bench_str_iter_and_compare`].
    #[bench]
    fn bench_idents(b: &mut Bencher) {
        let reps = 1_000_000;
        let str = "test ".repeat(reps);
        // Trim the trailing space off.
        let str = &str[..str.len() - 1];

        b.iter(move || {
            let lexer = Lexer::new(str);
            assert_eq!(lexer.run().unwrap().len(), reps);
        });
    }
}
