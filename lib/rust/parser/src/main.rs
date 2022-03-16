#![feature(allocator_api)]
#![feature(slice_index_methods)]
#![feature(test)]
#![feature(generic_associated_types)]

use enso_prelude::*;

use bumpalo::Bump;
use ouroboros::self_referencing;
use std::str;


// =================
// === Constants ===
// =================

pub const AVERAGE_TOKEN_LEN: usize = 5;



// ===============
// === BumpVec ===
// ===============

#[self_referencing]
pub struct BumpVec<T> {
    allocator: Bump,
    #[covariant]
    #[borrows(allocator)]
    pub vec:   Vec<T, &'this Bump>,
}

impl<T> Default for BumpVec<T> {
    fn default() -> Self {
        Self::new_with_capacity(0)
    }
}

impl<T> BumpVec<T> {
    #[inline(always)]
    fn new_with_capacity(capacity: usize) -> Self {
        let allocator = Bump::with_capacity(capacity);
        BumpVecBuilder { allocator, vec_builder: |allocator: &Bump| Vec::new_in(allocator) }.build()
    }

    #[inline(always)]
    pub fn push(&mut self, t: T) {
        self.with_mut(|fields| fields.vec.push(t))
    }

    #[inline(always)]
    pub fn last(&self) -> Option<&T> {
        self.borrow_vec().last()
    }

    #[inline(always)]
    pub fn len(&self) -> usize {
        self.borrow_vec().len()
    }

    #[inline(always)]
    pub fn iter(&self) -> std::slice::Iter<'_, T> {
        self.borrow_vec().into_iter()
    }
}

impl<T: Debug> Debug for BumpVec<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self.borrow_vec(), f)
    }
}



// =============
// === Bytes ===
// =============

#[derive(
    Add, AddAssign, Clone, Copy, Debug, Default, Eq, From, Hash, PartialEq, PartialOrd, Ord, Sub
)]
pub struct Bytes(usize);

impl Display for Bytes {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.0, f)
    }
}

#[inline(always)]
fn bytes_range_into_usize_range(range: Range<Bytes>) -> Range<usize> {
    unsafe { mem::transmute(range) }
}

pub trait BytesStrOps {
    fn slice(&self, range: Range<Bytes>) -> &str;
}

impl BytesStrOps for str {
    #[inline(always)]
    fn slice(&self, range: Range<Bytes>) -> &str {
        &self[bytes_range_into_usize_range(range)]
    }
}



// =============
// === Token ===
// =============

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Token<T = Kind> {
    left_visible_offset: usize,
    left_offset:         Bytes,
    start:               Bytes,
    len:                 Bytes,
    elem:                T,
}

impl<T: Debug> Debug for Token<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}({},{})", self.elem, self.left_visible_offset, self.len)
    }
}

impl<T> Token<T> {
    #[inline(always)]
    pub fn new_no_offset(start: Bytes, len: Bytes, elem: T) -> Self {
        let left_visible_offset = 0;
        let left_offset = Bytes::from(0);
        Self { left_visible_offset, left_offset, start, len, elem }
    }

    #[inline(always)]
    pub fn new_no_offset_phantom(start: Bytes, elem: T) -> Self {
        let len = Bytes::from(0);
        Self::new_no_offset(start, len, elem)
    }

    #[inline(always)]
    pub fn with_elem<S>(self, elem: S) -> Token<S> {
        let left_visible_offset = self.left_visible_offset;
        let left_offset = self.left_offset;
        let start = self.start;
        let len = self.len;
        Token { left_visible_offset, left_offset, start, len, elem }
    }

    #[inline(always)]
    pub fn split_at(self, offset: Bytes) -> Option<(Token<()>, Token<()>)> {
        (offset <= self.len).as_some_from(|| {
            let token_left = {
                let left_visible_offset = self.left_visible_offset;
                let left_offset = self.left_offset;
                let start = self.start;
                let len = self.len - offset;
                let elem = ();
                Token { left_visible_offset, left_offset, start, len, elem }
            };
            let token_right = {
                let left_visible_offset = 0;
                let left_offset = Bytes::from(0);
                let start = self.start + offset;
                let len = self.len - offset;
                let elem = ();
                Token { left_visible_offset, left_offset, start, len, elem }
            };
            (token_left, token_right)
        })
    }
}

impl PartialEq<Token> for &Token {
    #[inline(always)]
    fn eq(&self, other: &Token) -> bool {
        (*self).eq(other)
    }
}



// ===============
// === Pattern ===
// ===============

pub trait Pattern {
    fn match_pattern(&mut self, t: char) -> bool;
}

impl<T: FnMut(char) -> bool> Pattern for T {
    #[inline(always)]
    fn match_pattern(&mut self, t: char) -> bool {
        (self)(t)
    }
}

impl Pattern for char {
    #[inline(always)]
    fn match_pattern(&mut self, t: char) -> bool {
        *self == t
    }
}

macro_rules! pattern_impl_for_char_slice {
    ($($num:tt),* $(,)?) => {$(
        impl Pattern for &[char; $num] {
            #[inline(always)]
            fn match_pattern(&mut self, t: char) -> bool {
                self.contains(&t)
            }
        }
    )*};
}
pattern_impl_for_char_slice!(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);



// =============
// === Lexer ===
// =============

/// Efficient lexer implementation with no backtracking an 1-character lookahead (you can check the
/// [`current_char`] that is not yet being consumed).
pub struct Lexer<'s> {
    pub input:    &'s str,
    pub iterator: str::CharIndices<'s>,
    output:       BumpVec<Token>,
    pub state:    LexerState,
}

#[derive(Debug, Default)]
pub struct LexerState {
    pub current_char:               Option<char>,
    pub char_offset:                usize,
    pub offset:                     Bytes,
    pub last_spaces_offset:         Bytes,
    pub last_spaces_visible_offset: usize,
    pub current_block_indent:       usize,
    pub block_indent_stack:         Vec<usize>,
    pub line_contains_tokens:       bool,
}

impl<'s> Deref for Lexer<'s> {
    type Target = LexerState;
    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.state
    }
}

impl<'s> DerefMut for Lexer<'s> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.state
    }
}

impl<'s> Lexer<'s> {
    pub fn new(input: &'s str) -> Self {
        let iterator = input.char_indices();
        let capacity = input.len() / AVERAGE_TOKEN_LEN;
        let output = BumpVec::new_with_capacity(capacity);
        let state = default();
        Self { input, iterator, output, state }.init()
    }

    fn init(mut self) -> Self {
        self.next();
        self.char_offset = 0;
        self
    }

    #[inline(always)]
    fn next(&mut self) {
        let next = self.iterator.next();
        if let Some((offset, current_char)) = next {
            self.char_offset += 1;
            self.offset = Bytes::from(offset);
            self.current_char = Some(current_char);
        } else if self.current_char.is_some() {
            self.char_offset += 1;
            self.offset = Bytes::from(self.input.len());
            self.current_char = None;
        }
    }

    #[inline(always)]
    pub fn repr<T>(&self, token: Token<T>) -> &str {
        self.input.slice(token.start..token.start + token.len)
    }
}

impl<'s> Lexer<'s> {
    #[inline(always)]
    pub fn token<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> Option<Token<T>> {
        let start = self.offset;
        let (elem, len) = self.run_and_check_offset(f);
        (len > Bytes::from(0)).as_some_from(|| {
            let left_offset = self.last_spaces_offset;
            let left_visible_offset = self.last_spaces_visible_offset;
            (self.last_spaces_visible_offset, self.last_spaces_offset) = self.spaces();
            Token { left_visible_offset, left_offset, start, len, elem }
        })
    }

    #[inline(always)]
    pub fn run_and_check_offset<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> (T, Bytes) {
        let start = self.offset;
        let out = f(self);
        let len = self.offset - start;
        (out, len)
    }

    #[inline(always)]
    pub fn try_running(&mut self, f: impl FnOnce(&mut Self)) -> bool {
        let (_, offset) = self.run_and_check_offset(f);
        offset > Bytes::from(0)
    }

    #[inline(always)]
    pub fn submit_token(&mut self, token: Token) {
        self.output.push(token);
        self.line_contains_tokens = true;
    }

    #[inline(always)]
    pub fn push_block_indent(&mut self, new_indent: usize) {
        let current_block_indent = self.current_block_indent;
        self.block_indent_stack.push(current_block_indent);
        self.current_block_indent = new_indent;
    }

    #[inline(always)]
    pub fn pop_block_indent(&mut self) -> Option<usize> {
        self.block_indent_stack.pop().map(|prev| {
            let out = self.current_block_indent;
            self.current_block_indent = prev;
            out
        })
    }

    #[inline(always)]
    pub fn take_next(&mut self) -> bool {
        let ok = self.current_char.is_some();
        self.next();
        ok
    }

    #[inline(always)]
    pub fn take_1(&mut self, mut pat: impl Pattern) -> bool {
        let ok = self.current_char.map(|t| pat.match_pattern(t)) == Some(true);
        if ok {
            self.next();
        }
        ok
    }

    #[inline(always)]
    pub fn take_while(&mut self, mut pat: impl Pattern) {
        while let Some(t) = self.current_char {
            if pat.match_pattern(t) {
                self.next()
            } else {
                break;
            }
        }
    }

    #[inline(always)]
    pub fn take_while_1(&mut self, f: impl Copy + Pattern) -> bool {
        let ok = self.take_1(f);
        if ok {
            self.take_while(f);
        }
        ok
    }
}



// =================
// === Validator ===
// =================

pub trait Validator {
    type Output;
    fn validate(self, repr: &str) -> Vec<Self::Output>;
}

impl<'s> Lexer<'s> {
    pub fn validate<T: Copy + Validator>(&self, token: Token<T>) -> Vec<T::Output> {
        let repr = self.repr(token);
        token.elem.validate(repr)
    }
}



// =============
// === Space ===
// =============

/// Based on https://en.wikipedia.org/wiki/Whitespace_character.
const UNICODE_SINGLE_SPACES: &str = "\u{1680}\u{202F}\u{205F}\u{3000}";
const UNICODE_SINGLE_SPACES_RANGE: (char, char) = ('\u{2000}', '\u{200A}');
const UNICODE_ZERO_SPACES: &str = "\u{200B}\u{200C}\u{200D}\u{2060}\u{FEFF}";

#[inline(always)]
fn space_char_visible_size(t: char) -> Option<usize> {
    if t == ' ' {
        Some(1)
    } else if t == '\t' {
        Some(4)
    } else if t == '\u{00A0}' {
        Some(1)
    } else if t >= '\u{1680}' {
        if t == '\u{1680}' {
            Some(1)
        } else if t == '\u{180E}' {
            Some(0)
        } else if t >= '\u{2000}' {
            if UNICODE_SINGLE_SPACES.contains(t) {
                Some(1)
            } else if t >= UNICODE_SINGLE_SPACES_RANGE.0 && t <= UNICODE_SINGLE_SPACES_RANGE.1 {
                Some(1)
            } else if UNICODE_ZERO_SPACES.contains(t) {
                Some(0)
            } else {
                None
            }
        } else {
            None
        }
    } else {
        None
    }
}

#[inline(always)]
fn is_space_char(t: char) -> bool {
    space_char_visible_size(t).is_some()
}

impl<'s> Lexer<'s> {
    #[inline(always)]
    fn space_and_its_visible_offset(&mut self) -> Option<usize> {
        let out = self.current_char.and_then(space_char_visible_size);
        if out.is_some() {
            self.next();
        }
        out
    }

    #[inline(always)]
    fn spaces(&mut self) -> (usize, Bytes) {
        let mut total_visible_offset = 0;
        let mut total_byte_offset = Bytes::from(0);
        loop {
            if let Some(visible_offset) = self.space_and_its_visible_offset() {
                total_visible_offset += visible_offset;
                total_byte_offset += Bytes::from(1);
            } else {
                break;
            }
        }
        (total_visible_offset, total_byte_offset)
    }
}



// ============
// === Kind ===
// ============

macro_rules! tagged_enum {
    ($name:ident {
        $($l_variant:ident : $variant:ident $({$($arg:ident : $arg_tp:ty),* $(,)?})? ),* $(,)?
    }) => {
        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        pub enum $name {
            $($variant($variant)),*
        }

        $(
            #[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
            pub struct $variant {
                $($($arg : $arg_tp),*)?
            }

            impl From<$variant> for $name {
                #[inline(always)]
                fn from(t: $variant) -> Self {
                    Self::$variant(t)
                }
            }

            impl PartialEq<$variant> for &$variant {
                #[inline(always)]
                fn eq(&self, other: &$variant) -> bool {
                    $variant::eq(*self, other)
                }
            }
        )*

        impl $name {
            $(
                /// Constructor.
                #[inline(always)]
                pub fn $l_variant($($($arg : $arg_tp),*)?) -> Self {
                    Self::$variant($variant { $($($arg),*)? })
                }
            )*
        }
    };
}

tagged_enum!(Kind {
    newline:      Newline,
    symbol:       Symbol,
    block_start:  BlockStart,
    block_end:    BlockEnd,
    wildcard:     Wildcard,
    ident:        Ident { is_free: bool, lift_level: usize },
    operator:     Operator,
    modifier:     Modifier,
    comment:      Comment,
    doc_comment:  DocComment,
    number:       Number,
    text_start:   TextStart,
    text_end:     TextEnd,
    text_section: TextSection,
    text_escape:  TextEscape,
});



// =============
// === Ident ===
// =============

#[inline(always)]
fn is_digit(t: char) -> bool {
    t >= '0' && t <= '9'
}

#[inline(always)]
fn is_ident_split_char(t: char) -> bool {
    is_ident_body_split_operator(t) || is_space_char(t) || is_newline_char(t)
}

#[inline(always)]
fn is_ident_char(t: char) -> bool {
    !is_ident_split_char(t)
}

impl<'s> Lexer<'s> {
    fn ident(&mut self) {
        if let Some(tok) = self.token(|this| this.take_while_1(is_ident_char)) {
            let repr = self.repr(tok);
            let starts_with_underscore = repr.starts_with('_');
            let kind = if starts_with_underscore && repr.len() == 1 {
                Kind::wildcard()
            } else {
                let is_free = starts_with_underscore;
                let lift_level = repr.chars().rev().take_while(|t| *t == '\'').count();
                Kind::ident(is_free, lift_level)
            };
            let tok = tok.with_elem(kind);
            self.submit_token(tok);
        }
    }
}

pub struct IdentError {}

impl Validator for Ident {
    type Output = IdentError;
    fn validate(self, _repr: &str) -> Vec<Self::Output> {
        vec![] // TODO
    }
}



// ================
// === Operator ===
// ================

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

#[inline(always)]
fn is_ident_body_split_operator(t: char) -> bool {
    if t <= '\u{7E}' && t >= '\u{21}' {
        (t == '\u{21}')
            || (t >= '\u{21}' && t <= '\u{26}')
            || (t >= '\u{28}' && t <= '\u{2F}')
            || (t >= '\u{3A}' && t <= '\u{40}')
            || (t >= '\u{5B}' && t <= '\u{60}')
            || (t >= '\u{7B}' && t <= '\u{7E}')
    } else {
        false
    }
}

#[inline(always)]
fn is_operator_body_char(t: char) -> bool {
    if t <= '\u{7E}' && t >= '\u{21}' {
        (t == '\u{21}')
            || (t >= '\u{24}' && t <= '\u{26}')
            || (t >= '\u{2A}' && t <= '\u{2F}')
            || (t >= '\u{3A}' && t <= '\u{40}')
            || (t == '\u{5C}')
            || (t == '\u{5E}')
            || (t == '\u{7C}')
            || (t == '\u{7E}')
    } else {
        false
    }
}

impl<'s> Lexer<'s> {
    fn operator(&mut self) {
        let tok = self.token(|this| {
            if let Some(current) = this.current_char {
                match current {
                    '.' => this.take_while_1('.'),
                    '=' => this.take_while_1('='),
                    ':' => true,
                    ',' => true,
                    _ => this.take_while_1(is_operator_body_char),
                };
            }
        });
        if let Some(tok) = tok {
            let repr = self.repr(tok);
            if repr == "+-" {
                let (left, right) = tok.split_at(Bytes::from(1)).unwrap();
                self.submit_token(left.with_elem(Kind::operator()));
                self.submit_token(right.with_elem(Kind::operator()));
            } else {
                let kind = if repr.ends_with('=') { Kind::modifier() } else { Kind::operator() };
                let token = tok.with_elem(kind);
                self.submit_token(token);
            }
        }
    }
}



// ==============
// === Symbol ===
// ==============

impl<'s> Lexer<'s> {
    fn symbol(&mut self) {
        if let Some(tok) = self.token(|this| this.take_1(&['(', ')', '{', '}', '[', ']'])) {
            self.submit_token(tok.with_elem(Kind::symbol()));
        }
    }
}



// ==============
// === Number ===
// ==============

impl<'s> Lexer<'s> {
    fn number(&mut self) {
        let tok = self.token(|this| {
            if this.take_1(is_digit) {
                this.take_while(|t| !is_ident_split_char(t));
            }
        });
        if let Some(tok) = tok {
            self.submit_token(tok.with_elem(Kind::number()));
        }
    }
}



// ============
// === Text ===
// ============

fn is_inline_text_body(t: char) -> bool {
    t != '"' && !is_newline_char(t) && t != '\\'
}

impl<'s> Lexer<'s> {
    fn text(&mut self) {
        let tok = self.token(|this| this.take_1('"'));
        if let Some(tok) = tok {
            self.submit_token(tok.with_elem(Kind::text_start()));
            let line_empty = self.current_char.map(|t| is_newline_char(t)).unwrap_or(true);
            if line_empty {
                todo!()
            } else {
                let mut parsed_element;
                loop {
                    parsed_element = false;

                    let section = self.token(|this| this.take_while_1(is_inline_text_body));
                    if let Some(tok) = section {
                        parsed_element = true;
                        self.submit_token(tok.with_elem(Kind::text_section()));
                    }

                    let escape = self.token(|this| {
                        if this.take_1('\\') {
                            this.take_1('"');
                        }
                    });
                    if let Some(tok) = escape {
                        parsed_element = true;
                        self.submit_token(tok.with_elem(Kind::text_escape()));
                    }

                    let end = self.token(|this| this.take_1('"'));
                    if let Some(tok) = end {
                        self.submit_token(tok.with_elem(Kind::text_end()));
                        break;
                    }

                    if !parsed_element {
                        break;
                    }
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
    fn submit_line_as(&mut self, kind: Kind) {
        let tok = self.token(|this| this.take_line());
        if let Some(tok) = tok {
            self.submit_token(tok.with_elem(kind));
        }
    }

    fn comment(&mut self) {
        if let Some(current) = self.current_char {
            if current == '#' {
                self.submit_line_as(Kind::comment());
                let initial_ident = self.current_block_indent;
                let check_ident = |this: &mut Self| this.current_block_indent > initial_ident;
                while self.try_running(|this| this.newline()) && check_ident(self) {
                    self.submit_line_as(Kind::comment());
                }
            }
        }
    }
}



// =============
// === Block ===
// =============

#[inline(always)]
pub fn is_newline_char(t: char) -> bool {
    t == '\n' || t == '\r'
}

impl<'s> Lexer<'s> {
    #[inline(always)]
    pub fn take_line(&mut self) {
        self.take_while(|t| !is_newline_char(t))
    }

    fn newline(&mut self) {
        let tok = self.token(|this| {
            if !this.take_1('\n') {
                if this.take_1('\r') {
                    this.take_1('\n');
                }
            }
        });
        if let Some(tok) = tok {
            let block_indent = self.last_spaces_visible_offset;
            self.submit_token(tok.with_elem(Kind::newline()));
            let next_line_empty = self.current_char.map(|t| is_newline_char(t)).unwrap_or(true);
            if !next_line_empty {
                if block_indent > self.current_block_indent {
                    let block_start =
                        Token::new_no_offset_phantom(self.offset, Kind::block_start());
                    self.submit_token(block_start);
                    self.push_block_indent(block_indent);
                } else {
                    while block_indent < self.current_block_indent {
                        let err = "Lexer internal error. Inconsistent code block hierarchy.";
                        let parent_block_indent = self.pop_block_indent().expect(err);
                        if block_indent > self.current_block_indent {
                            // The new line indent is smaller than current block but bigger than the
                            // previous one. We are treating the line as belonging to the block. The
                            // warning should be reported by parser.
                            self.push_block_indent(parent_block_indent);
                            break;
                        } else {
                            let block_end =
                                Token::new_no_offset_phantom(self.offset, Kind::block_end());
                            self.submit_token(block_end);
                        }
                    }
                }
            }
            self.line_contains_tokens = false;
        }
    }
}



// ============
// === Glue ===
// ============

const PARSERS: &[for<'r> fn(&'r mut Lexer<'_>)] = &[
    |t| t.ident(),
    |t| t.operator(),
    |t| t.newline(),
    |t| t.symbol(),
    |t| t.comment(),
    |t| t.number(),
    |t| t.text(),
];

impl<'s> Lexer<'s> {
    fn run(&mut self) -> bool {
        (self.last_spaces_visible_offset, self.last_spaces_offset) = self.spaces();
        let mut any_parser_matched = true;
        while any_parser_matched {
            any_parser_matched = false;
            for f in PARSERS {
                if self.try_running(f) {
                    any_parser_matched = true;
                    break;
                }
            }
        }
        self.current_char == None
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! lexer_tests {
        ($($input:literal => $body:tt),* $(,)?) => {
            $( test_lexer($input, (&$body).to_vec()); )*
        };
    }

    fn test_lexer(input: &str, expected: Vec<Token>) {
        let mut lexer = Lexer::new(input);
        lexer.run();
        assert_eq!(lexer.output.iter().collect_vec(), expected);
    }

    #[test]
    fn test_manual() {
        let inp = "foo \"hello";
        let mut lexer = Lexer::new(&inp);
        lexer.run();
        println!("{:#?}", lexer.output);
    }

    fn ident(left_visible_offset: usize, left_offset: usize, repr: &str) -> Token {
        let is_free = repr.starts_with('_');
        let lift_level = repr.chars().rev().take_while(|t| *t == '\'').count();
        Token {
            left_visible_offset,
            left_offset: Bytes::from(left_offset),
            start: Bytes(0),
            len: Bytes(repr.len()),
            elem: Kind::ident(is_free, lift_level),
        }
    }

    #[test]
    fn test_case_idents() {
        lexer_tests! {
            ""            => [],
            "test"        => [ident(0, 0, "test")],
            "你好"         => [ident(0, 0, "你好")],
            "cześć"       => [ident(0, 0, "cześć")],
            "GrüßGott"    => [ident(0, 0, "GrüßGott")],
            "Nǐhǎo"       => [ident(0, 0, "Nǐhǎo")],
            "hyvääpäivää" => [ident(0, 0, "hyvääpäivää")],
            "Góðandag"    => [ident(0, 0, "Góðandag")],
            "Moïen"       => [ident(0, 0, "Moïen")],
            "Namastē"     => [ident(0, 0, "Namastē")],
            "やあ"         => [ident(0, 0, "やあ")],
            "đượchậuđải"  => [ident(0, 0, "đượchậuđải")],
            "❤️foo"       => [ident(0, 0, "❤️foo")],
        }
    }

    // left_visible_offset: usize,
    // left_offset:         Bytes,
    // start:               Bytes,
    // len:                 Bytes,
    // elem:                T,
    // #[test]
    // fn test_case_idents() {
    //     assert_token_eq("", None);
    //     assert_token_eq("_", Some(test_underscore("_", 0)));
    //     assert_token_eq("a", Some(test_ident("a", false, 0, 0)));
    //     assert_token_eq("a'", Some(test_ident("a'", false, 1, 0)));
    //     assert_token_eq("a''", Some(test_ident("a''", false, 2, 0)));
    //     assert_token_eq("a'''", Some(test_ident("a'''", false, 3, 0)));
    //     assert_token_eq("_a", Some(test_ident("_a", true, 0, 0)));
    //     assert_token_eq("_a'", Some(test_ident("_a'", true, 1, 0)));
    //     assert_token_eq("_a''", Some(test_ident("_a''", true, 2, 0)));
    //     assert_token_eq("_a'''", Some(test_ident("_a'''", true, 3, 0)));
    //     assert_token_eq("test", Some(test_ident("test", false, 0, 0)));
    //     assert_token_eq("__a", Some(test_ident("__a", true, 0, 0)));
    //     assert_token_eq("___a", Some(test_ident("___a", true, 0, 0)));
    //     assert_token_eq("_a_", Some(test_ident("_a_", true, 0, 0)));
    //     assert_token_eq("__a__", Some(test_ident("__a__", true, 0, 0)));
    //     assert_token_eq("_a_b_", Some(test_ident("_a_b_", true, 0, 0)));
    //
    //     assert_token_eq("Test_Name", Some(test_ident("Test_Name", false, 0, 0)));
    //     assert_token_eq("Test_Name'", Some(test_ident("Test_Name'", false, 1, 0)));
    //
    //     assert_token_eq("a'a", Some(test_ident("a'a", false, 1, 1)));
    //     assert_token_eq("a'b'", Some(test_ident("a'b'", false, 1, 2)));
    //     assert_token_eq("_'", Some(test_underscore("_'", 1)));
    //     assert_token_eq("_'a", Some(test_underscore("_'a", 2)));
    //
    //     // assert_token_eq("ą", Some(test_ident("ą", false, 0, 0)));
    // }
    //
    // #[test]
    // fn test_case_operator() {
    //     assert_token_eq("", None);
    //     assert_token_eq("+", Some(test_operator("+")));
    //     assert_token_eq("-", Some(test_operator("-")));
    //     assert_token_eq("=", Some(test_operator("=")));
    //     assert_token_eq("==", Some(test_operator("==")));
    //     assert_token_eq("===", Some(test_operator("===")));
    //     // +-
    // }
}


fn main() {
    let str = "_fooAr'' bar";
    let mut input = Lexer::new(str);
    println!("{:?}", input.ident());
}


#[cfg(test)]
mod benches {
    use super::*;
    extern crate test;
    use test::Bencher;

    #[bench]
    fn bench_str_iter(b: &mut Bencher) {
        let reps = 1000_000;
        let str = "test ".repeat(reps);

        b.iter(move || str.chars().for_each(drop));
    }

    #[bench]
    fn bench_str_iter_and_compare(b: &mut Bencher) {
        let reps = 1000_000;
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

    /// 12-13x slowdown in comparison to [`bench_str_iter`] and [`bench_str_iter_and_compare`].
    #[bench]
    fn bench_idents(b: &mut Bencher) {
        let reps = 1000_000;
        let str = "test ".repeat(reps);

        b.iter(move || {
            let mut input = Lexer::new(&str);
            let ok = input.run();
            assert_eq!(ok, true);
            assert_eq!(input.output.len(), reps);
        });
    }
}
