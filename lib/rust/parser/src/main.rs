#![feature(type_changing_struct_update)]
#![feature(allocator_api)]
#![feature(slice_index_methods)]
#![feature(test)]

use enso_prelude::*;

use bumpalo::Bump;
use std::str;



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
    #[inline(always)]
    fn slice(&self, range: Range<Bytes>) -> &str;
}

impl BytesStrOps for str {
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
        Token { elem, ..self }
    }

    #[inline(always)]
    pub fn split_at(self, offset: Bytes) -> Option<(Token<()>, Token<()>)> {
        (self.len >= offset).as_some_from(|| {
            let token1 = {
                let left_visible_offset = self.left_visible_offset;
                let left_offset = self.left_offset;
                let start = self.start;
                let len = self.len - offset;
                let elem = ();
                Token { left_visible_offset, left_offset, start, len, elem }
            };
            let token2 = {
                let left_visible_offset = 0;
                let left_offset = Bytes::from(0);
                let start = self.start + offset;
                let len = self.len - offset;
                let elem = ();
                Token { left_visible_offset, left_offset, start, len, elem }
            };
            (token1, token2)
        })
    }
}



pub type TokenVec<'s> = Vec<Token, &'s Bump>;

pub struct Model<'s> {
    pub input:    &'s str,
    pub iterator: str::CharIndices<'s>,
    pub output:   TokenVec<'s>,
    pub state:    ModelState,
}

#[derive(Debug, Default)]
pub struct ModelState {
    pub current_char:               Option<char>,
    pub char_offset:                usize,
    pub offset:                     Bytes,
    pub last_spaces_visible_offset: usize,
    pub last_spaces_offset:         Bytes,
    pub current_block_indent:       usize,
    pub block_indent_stack:         Vec<usize>,
}

impl<'s> Deref for Model<'s> {
    type Target = ModelState;
    fn deref(&self) -> &Self::Target {
        &self.state
    }
}

impl<'s> DerefMut for Model<'s> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.state
    }
}

impl<'s> Model<'s> {
    pub fn new(bump: &'s Bump, input: &'s str) -> Self {
        let iterator = input.char_indices();
        let output = Vec::new_in(bump);
        let state = default();
        Self { input, iterator, output, state }.init()
    }

    fn init(mut self) -> Self {
        self.next();
        self.char_offset = 0;
        self
    }

    #[inline(always)]
    pub fn token<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> Option<Token<T>> {
        let start = self.offset;
        let elem = f(self);
        let len = self.offset - start;
        (len > Bytes::from(0)).as_some_from(|| {
            let left_offset = self.last_spaces_offset;
            let left_visible_offset = self.last_spaces_visible_offset;
            (self.last_spaces_visible_offset, self.last_spaces_offset) = self.spaces();
            Token { left_visible_offset, left_offset, start, len, elem }
        })
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
    fn repr<T>(&self, token: Token<T>) -> &str {
        self.input.slice(token.start..token.start + token.len)
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
    pub fn char(&mut self, t: char) -> bool {
        self.take_1(|s| s == t)
    }

    #[inline(always)]
    pub fn count_char(&mut self, t: char) -> usize {
        self.count_chars(|s| s == t)
    }

    #[inline(always)]
    pub fn count_chars(&mut self, f: impl Fn(char) -> bool) -> usize {
        let start = self.char_offset;
        self.take_while(f);
        self.char_offset - start
    }

    #[inline(always)]
    pub fn count_with(&mut self, f: impl Fn(char) -> Option<usize>) -> usize {
        let mut result = 0;
        self.take_while(|t| {
            if let Some(count) = f(t) {
                result += count;
                true
            } else {
                false
            }
        });
        result
    }

    #[inline(always)]
    pub fn take_any(&mut self) -> bool {
        let ok = self.current_char.is_some();
        self.next();
        ok
    }

    #[inline(always)]
    pub fn take_1(&mut self, f: impl Fn(char) -> bool) -> bool {
        let ok = self.current_char.map(f) == Some(true);
        if ok {
            self.next();
        }
        ok
    }

    #[inline(always)]
    pub fn take_while(&mut self, mut f: impl FnMut(char) -> bool) {
        while let Some(t) = self.current_char {
            if f(t) {
                self.next()
            } else {
                break;
            }
        }
    }

    #[inline(always)]
    pub fn take_while_1(&mut self, f: impl Copy + Fn(char) -> bool) -> bool {
        let out = self.take_1(f);
        if out {
            self.take_while(f);
        }
        out
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
fn is_space_char(t: char) -> Option<usize> {
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

impl<'s> Model<'s> {
    #[inline(always)]
    fn space_and_its_visible_offset(&mut self) -> Option<usize> {
        let out = self.current_char.and_then(is_space_char);
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Kind {
    Newline,
    Symbol,
    BlockStart,
    BlockEnd,
    Wildcard,
    Ident(Ident),
    Operator,
    Modifier,
}



// =============
// === Ident ===
// =============

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Ident {
    is_free:    bool,
    lift_level: usize,
}

#[inline(always)]
fn is_ident_start_char(t: char) -> bool {
    t.is_alphabetic() || t == '_'
}

#[inline(always)]
fn is_ident_body(t: char) -> bool {
    is_ident_start_char(t) || (t >= '0' && t <= '9') || t == '\''
}

impl<'s> Model<'s> {
    fn ident(&mut self) {
        let tok = self.token(|this| {
            if this.take_1(is_ident_start_char) {
                this.take_while(is_ident_body);
            }
        });
        if let Some(tok) = tok {
            let repr = self.repr(tok);
            let starts_with_underscore = repr.starts_with('_');
            let kind = if starts_with_underscore && repr.len() == 1 {
                Kind::Wildcard
            } else {
                let is_free = starts_with_underscore;
                let lift_level = repr.chars().rev().take_while(|t| *t == '\'').count();
                Kind::Ident(Ident { is_free, lift_level })
            };
            let tok = tok.with_elem(kind);
            self.output.push(tok);
        }
    }
}



// ================
// === Operator ===
// ================

/// According to https://en.wikipedia.org/wiki/ASCII:
///
/// hex   char  is_opr?  single_opr?
/// 21    !     yes      ---
/// 22    "     ---      ---
/// 23    #     ---      ---
/// 24    $     yes      ---
/// 25    %     yes      ---
/// 26    &     yes      ---
/// 27    '     ---      ---
/// 28    (     ---      ---
/// 29    )     ---      ---
/// 2A    *     yes      ---
/// 2B    +     yes      sometimes (e.g. 4+-2)
/// 2C    ,     yes      ---
/// 2D    -     yes      sometimes (e.g. 4+-2)
/// 2E    .     yes      special: (..), (...)
/// 2F    /     yes      ---
///
/// .. 0-9 ..
///
/// 3A    :     yes      yes
/// 3B    ;     yes      yes
/// 3C    <     yes      ---
/// 3D    =     yes      special
/// 3E    >     yes      ---
/// 3F    ?     yes      ---
/// 40    @     yes      ---
///
/// .. A-Z ..
///
/// 5B    [     ---      ---
/// 5C    \     yes      ---
/// 5D    ]     ---      ---
/// 5E    ^     yes      ---
/// 5F    _     ---      ---
/// 60    `     ---      ---
///
/// .. a-z ..
///
/// 7B    {     ---      ---
/// 7C    |     yes      ---
/// 7D    }     ---      ---
/// 7E    ~     yes      ---

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

impl<'s> Model<'s> {
    fn operator(&mut self) {
        let tok = self.token(|this| {
            if let Some(current) = this.current_char {
                match current {
                    '.' => this.take_while_1(|t| t == '.'),
                    '=' => this.take_while_1(|t| t == '='),
                    ':' => this.take_while_1(|t| t == ':'),
                    _ => this.take_while_1(is_operator_body_char),
                };
            }
        });
        if let Some(tok) = tok {
            let repr = self.repr(tok);
            if repr == "+-" {
                let (left, right) = tok.split_at(Bytes::from(1)).unwrap();
                let left = left.with_elem(Kind::Operator);
                let right = right.with_elem(Kind::Operator);
                self.output.push(left);
                self.output.push(right);
            } else {
                let kind = if repr.ends_with('=') { Kind::Modifier } else { Kind::Operator };
                let token = tok.with_elem(kind);
                self.output.push(token);
            }
        }
    }
}



// ==============
// === Symbol ===
// ==============

impl<'s> Model<'s> {
    fn symbol(&mut self) {
        if let Some(current) = self.current_char {
            if "(){}[]`".contains(current) {
                let token = self.token(|this| this.take_any()).unwrap().with_elem(Kind::Symbol);
                self.output.push(token);
            }
        }
    }
}



// =============
// === Block ===
// =============

impl<'s> Model<'s> {
    fn newline(&mut self) {
        let tok = self.token(|this| {
            if !this.take_1(|t| t == '\n') {
                if this.take_1(|t| t == '\r') {
                    this.take_1(|t| t == '\n');
                }
            }
        });
        if let Some(tok) = tok {
            let block_indent = self.last_spaces_visible_offset;
            self.output.push(tok.with_elem(Kind::Newline));
            if block_indent > self.current_block_indent {
                let block_start = Token::new_no_offset_phantom(self.offset, Kind::BlockStart);
                self.output.push(block_start);
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
                        let block_end = Token::new_no_offset_phantom(self.offset, Kind::BlockEnd);
                        self.output.push(block_end);
                    }
                }
            }
        }
    }
}



// ============
// === Glue ===
// ============

const PARSERS: &[for<'r> fn(&'r mut Model<'_>)] =
    &[|t| t.ident(), |t| t.operator(), |t| t.newline(), |t| t.symbol()];

impl<'s> Model<'s> {
    fn lex(&mut self) -> bool {
        (self.last_spaces_visible_offset, self.last_spaces_offset) = self.spaces();
        let mut any_parser_matched = true;
        while any_parser_matched {
            any_parser_matched = false;
            for f in PARSERS {
                let offset = self.offset;
                f(self);
                if self.offset > offset {
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

fn assert_token_eq(str: &str, f: Option<Box<dyn Fn(usize, usize) -> Token>>) {
    // TODO: test tabs
    for l_offset in 0..4 {
        let inp = format!("{}{}", " ".repeat(l_offset), str);
        let result = f.as_ref().map(|f| f(l_offset, l_offset));
        let bump = Bump::new();
        let mut input = Model::new(&bump, &inp);
        input.lex();
        assert_eq!(input.output.last().copied(), result);
    }
}



#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_manual() {
        let inp = "f )";
        let bump = Bump::new();
        let mut input = Model::new(&bump, &inp);
        input.lex();
        println!("{:#?}", input.output);
    }

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


// pub fn count_with(&mut self, f: impl Fn(char) -> (bool, usize)) -> usize {


fn main() {
    let str = "_fooAr'' bar";
    let capacity = str.len() / 5;
    let bump = Bump::with_capacity(capacity);
    let mut input = Model::new(&bump, str);
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

    /// 4.5-5x slowdown in comparison to [`bench_str_iter_and_compare`].
    #[bench]
    fn bench_idents(b: &mut Bencher) {
        let reps = 1000_000;
        let str = "test ".repeat(reps);
        let capacity = str.len() / 5;

        b.iter(move || {
            let bump = Bump::with_capacity(capacity);
            let mut input = Model::new(&bump, &str);
            let ok = input.lex();
            assert_eq!(ok, true);
            assert_eq!(input.output.len(), reps);
        });
    }
}
