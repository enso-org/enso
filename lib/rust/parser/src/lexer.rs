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
    pub current_char:               Option<char>,
    pub current_offset:             Bytes,
    pub last_spaces_offset:         Bytes,
    pub last_spaces_visible_offset: VisibleOffset,
    pub current_block_indent:       VisibleOffset,
    pub block_indent_stack:         Vec<VisibleOffset>,
    pub internal_error:             Option<String>,
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
}



// ================================
// === Basic Character Checkers ===
// ================================

/// Check whether the provided character is a newline character.
#[inline(always)]
pub fn is_newline_char(t: char) -> bool {
    t == '\n' || t == '\r'
}

/// Check whether the provided character is a digit.
#[inline(always)]
#[allow(clippy::manual_range_contains)]
fn is_digit(t: char) -> bool {
    t >= '0' && t <= '9'
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
}

impl IdentInfo {
    /// Constructor.
    #[inline(always)]
    pub fn new(repr: &str) -> Self {
        let starts_with_underscore = repr.starts_with('_');
        let lift_level = repr.chars().rev().take_while(|t| *t == '\'').count();
        Self { starts_with_underscore, lift_level }
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
        token::variant::Ident(info.starts_with_underscore, info.lift_level)
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
            token::Variant::ident(is_free, info.lift_level)
        }
    }
}

impl<'s> Lexer<'s> {
    /// Parse an identifier.
    fn ident(&mut self) {
        if let Some(token) = self.token(|this| this.take_while_1(is_ident_char)) {
            let tp = token::Variant::new_ident_or_wildcard_unchecked(&token.code);
            let token = token.with_variant(tp);
            self.submit_token(token);
        }
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
                let (binary, unary) = compute_precedence(&left.code);
                self.submit_token(left.with_variant(token::Variant::operator(binary, unary)));
                let (_, unary) = compute_precedence(&right.code);
                self.submit_token(right.with_variant(token::Variant::operator(None, unary)));
            } else {
                let only_eq = token.code.chars().all(|t| t == '=');
                let is_mod = token.code.ends_with('=') && !only_eq;
                let tp = if is_mod {
                    token::Variant::modifier()
                } else {
                    let (binary, unary) = compute_precedence(&token.code);
                    token::Variant::operator(binary, unary)
                };
                let token = token.with_variant(tp);
                self.submit_token(token);
            }
        }
    }
}


// === Precedence ===

// FIXME: Compute precedences according to spec. Issue: #182497344
fn compute_precedence(token: &str) -> (Option<token::Precedence>, Option<token::Precedence>) {
    let binary = match token {
        // Special handling for tokens that can be unary.
        "~" => return (None, Some(token::Precedence { value: 100 })),
        "-" =>
            return (Some(token::Precedence { value: 14 }), Some(token::Precedence { value: 100 })),
        // "There are a few operators with the lowest precedence possible."
        "=" => 1,
        ":" => 2,
        "->" => 3,
        "|" | "\\\\" | "&" => 4,
        ">>" | "<<" => 5,
        "|>" | "|>>" | "<|" | "<<|" => 6,
        // "The precedence of all other operators is determined by the operator's Precedence
        // Character:"
        "!" => 10,
        "||" => 11,
        "^" => 12,
        "&&" => 13,
        "+" | "++" => 14,
        "*" | "/" | "%" => 15,
        // FIXME: Not sure about these:
        "==" => 1,
        "," => 1,
        "@" => 20,
        "." => 21,
        _ => return (None, None),
    };
    (Some(token::Precedence { value: binary }), None)
}



// ==============
// === Symbol ===
// ==============

impl<'s> Lexer<'s> {
    /// Parse a symbol.
    fn symbol(&mut self) {
        if let Some(token) = self.token(|this| this.take_1(&['(', ')', '{', '}', '[', ']'])) {
            self.submit_token(token.with_variant(token::Variant::symbol()));
        }
    }
}



// ==============
// === Number ===
// ==============

impl<'s> Lexer<'s> {
    /// Parse a number.
    fn number(&mut self) {
        let token = self.token(|this| {
            if this.take_1(is_digit) {
                this.take_while(|t| !is_ident_split_char(t));
            }
        });
        if let Some(token) = token {
            self.submit_token(token.with_variant(token::Variant::number()));
        }
    }
}



// ============
// === Text ===
// ============

#[inline(always)]
fn is_inline_text_body(t: char) -> bool {
    t != '"' && !is_newline_char(t) && t != '\\'
}

impl<'s> Lexer<'s> {
    /// Parse a text literal.
    fn text(&mut self) {
        let token = self.token(|this| this.take_1('"'));
        if let Some(token) = token {
            self.submit_token(token.with_variant(token::Variant::text_start()));
            let line_empty = self.current_char.map(is_newline_char).unwrap_or(true);
            if line_empty {
                // FIXME: Handle this case; test this function. Issue: #182496940
                let char = self.current_char;
                self.internal_error.get_or_insert_with(|| format!("text: line_empty ({:?})", char));
                return;
            }
            let mut parsed_element;
            loop {
                parsed_element = false;

                let section = self.token(|this| this.take_while_1(is_inline_text_body));
                if let Some(tok) = section {
                    parsed_element = true;
                    self.submit_token(tok.with_variant(token::Variant::text_section()));
                }

                let escape = self.token(|this| {
                    if this.take_1('\\') {
                        this.take_1('"');
                    }
                });
                if let Some(token) = escape {
                    parsed_element = true;
                    self.submit_token(token.with_variant(token::Variant::text_escape()));
                }

                let end = self.token(|this| this.take_1('"'));
                if let Some(token) = end {
                    self.submit_token(token.with_variant(token::Variant::text_end()));
                    break;
                }

                if !parsed_element {
                    break;
                }
            }
        }
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
        if let Some(current) = self.current_char {
            if current == '#' {
                self.submit_line_as(token::Variant::comment());
                let initial_ident = self.current_block_indent;
                let check_indent = |this: &mut Self| this.current_block_indent > initial_ident;
                while self.run_and_check_if_progressed(|t| t.newline()) && check_indent(self) {
                    self.submit_line_as(token::Variant::comment());
                }
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
            while block_indent < self.current_block_indent {
                let previous_indent = self.block_indent_stack.last().copied().unwrap_or_default();
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
            self.submit_token(token.with_variant(token::Variant::newline()));
            newlines.drain(..).for_each(|token| self.submit_token(token));
            self.token_storage.set_from(newlines);
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
        let mut internal_error = self.internal_error.take();
        if self.current_char != None {
            let message = format!("Lexer did not consume all input. State: {self:?}");
            internal_error.get_or_insert(message);
        }
        let value = self.output;
        event!(TRACE, "Tokens:\n{:#?}", value);
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

/// Lexer main function used for ad-hoc testing during development.
pub fn main() {
    println!("{:#?}", run_flat("\n  foo\n  bar"));
}

/// Test utils for fast mock tokens creation.
pub mod test {
    use super::*;
    pub use token::*;

    /// Constructor.
    pub fn ident_<'s>(left_offset: &'s str, code: &'s str) -> Token<'s> {
        let is_free = code.starts_with('_');
        let lift_level = code.chars().rev().take_while(|t| *t == '\'').count();
        token::ident_(left_offset, code, is_free, lift_level)
    }

    /// Constructor.
    pub fn wildcard_<'s>(left_offset: &'s str, code: &'s str) -> Token<'s> {
        let lift_level = code.chars().rev().take_while(|t| *t == '\'').count();
        token::wildcard_(left_offset, code, lift_level)
    }

    /// Constructor.
    pub fn operator_<'s>(left_offset: &'s str, code: &'s str) -> Token<'s> {
        let (binary, unary) = compute_precedence(code);
        Token(left_offset, code, token::Variant::operator(binary, unary))
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
        test_lexer("10", vec![number_("", "10")]);
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
        let (_, unary) = compute_precedence("-");
        let unary_minus = Token("", "-", token::Variant::operator(None, unary));
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

        b.iter(move || {
            let lexer = Lexer::new(&str);
            assert_eq!(lexer.run().unwrap().len(), reps);
        });
    }
}
