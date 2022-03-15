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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Token<T = Kind> {
    left_visible_offset: usize,
    left_offset:         Bytes,
    start:               Bytes,
    len:                 Bytes,
    elem:                T,
}

impl<T> Token<T> {
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
    pub input: &'s str,
    pub iterator: str::CharIndices<'s>,
    pub current: Option<char>,
    pub char_offset: usize,
    pub offset: Bytes,
    pub output: TokenVec<'s>,
    pub last_spaces_visible_offset: usize,
    pub last_spaces_offset: Bytes,
}

impl<'s> Model<'s> {
    pub fn new(bump: &'s Bump, input: &'s str) -> Self {
        let iterator = input.char_indices();
        let current = default();
        let char_offset = default();
        let offset = default();
        let output = Vec::new_in(bump);
        let last_spaces_visible_offset = default();
        let last_spaces_offset = default();
        Self {
            input,
            iterator,
            current,
            char_offset,
            offset,
            output,
            last_spaces_visible_offset,
            last_spaces_offset,
        }
        .init()
    }

    fn init(mut self) -> Self {
        self.next();
        self.char_offset = 0;
        self
    }

    #[inline(always)]
    fn repr<T>(&self, token: Token<T>) -> &str {
        self.input.slice(token.start..token.start + token.len)
    }

    #[inline(always)]
    fn next(&mut self) {
        let next = self.iterator.next();
        if let Some((offset, current)) = next {
            self.char_offset += 1;
            self.offset = Bytes::from(offset);
            self.current = Some(current);
        } else if self.current.is_some() {
            self.char_offset += 1;
            self.offset = Bytes::from(self.input.len());
            self.current = None;
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
    pub fn take_1(&mut self, f: impl Fn(char) -> bool) -> bool {
        let out = self.current.map(f) == Some(true);
        if out {
            self.next();
        }
        out
    }

    #[inline(always)]
    pub fn take_while(&mut self, mut f: impl FnMut(char) -> bool) {
        loop {
            if let Some(t) = self.current {
                if f(t) {
                    self.next()
                } else {
                    break;
                }
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
// === Token ===
// =============

#[macro_export]
macro_rules! token {
    ($self:ident $($ts:tt)*) => {
        let left_offset = $self.last_spaces;
        let start = $self.offset;
        let kind = { $($ts)* };
        let len = $self.offset - start;
        kind.mk_token(&mut $self.output, left_offset, start, len)
    };
}

#[macro_export]
macro_rules! token2 {
    ($self:ident $($ts:tt)*) => {{
        let left_offset = $self.last_spaces_offset;
        let left_visible_offset = $self.last_spaces_visible_offset;
        let start = $self.offset;
        let elem = { $($ts)* };
        let len = $self.offset - start;
        Token{left_visible_offset, left_offset, start, len, elem}
    }};
}



pub trait MkToken {
    type Output;
    fn mk_token(
        self,
        vec: &mut TokenVec,
        lv_off: usize,
        l_off: Bytes,
        start: Bytes,
        len: Bytes,
    ) -> Self::Output;
}

impl MkToken for Option<Kind> {
    type Output = bool;
    fn mk_token(
        self,
        vec: &mut TokenVec,
        left_visible_offset: usize,
        left_offset: Bytes,
        start: Bytes,
        len: Bytes,
    ) -> Self::Output {
        if let Some(tok) =
            self.map(|elem| Token { left_visible_offset, left_offset, start, len, elem })
        {
            vec.push(tok);
            true
        } else {
            false
        }
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
        let out = self.current.and_then(is_space_char);
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
    fn ident(&mut self) -> bool {
        let tok = token2! { self
            let ok = self.take_1(is_ident_start_char);
            if ok {
                self.take_while(is_ident_body);
            }
            ok
        };
        let ok = tok.elem;
        if ok {
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
        ok
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
    fn operator(&mut self) -> bool {
        let tok = token2! { self
            self.current.map(|current| match current {
                '.' => self.take_while_1(|t| t == '.'),
                '=' => self.take_while_1(|t| t == '='),
                _ => self.take_while_1(is_operator_body_char),
            })
        };
        let ok = tok.elem == Some(true);
        if ok {
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
        ok
    }
}



// ============
// === Glue ===
// ============

impl<'s> Model<'s> {
    fn lex(&mut self) -> bool {
        loop {
            (self.last_spaces_visible_offset, self.last_spaces_offset) = self.spaces();
            if !self.ident() {
                if !self.operator() {
                    break;
                }
            }
        }
        self.current == None
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

// fn test_ident(
//     s: &str,
//     is_free: bool,
//     lift_level: usize,
//     invalid_suffix: usize,
// ) -> Box<dyn Fn(usize, usize) -> Token> {
//     let len = s.len();
//     Box::new(move |left_offset: usize, start: usize| {
//         Token::new(
//             left_offset,
//             start,
//             len,
//             Kind::Ident(Ident::new(is_free, lift_level, invalid_suffix)),
//         )
//     })
// }

// fn test_underscore(s: &str, invalid_suffix: usize) -> Box<dyn Fn(usize, usize) -> Token> {
//     let len = s.len();
//     Box::new(move |left_offset: usize, start: usize| {
//         Token::new(left_offset, start, len, Kind::Underscore(Underscore::new(invalid_suffix)))
//     })
// }
//
// fn test_operator(s: &str) -> Box<dyn Fn(usize, usize) -> Token> {
//     let len = s.len();
//     Box::new(move |left_offset: usize, start: usize| {
//         Token::new(left_offset, start, len, Kind::Operator)
//     })
// }

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_manual() {
        let inp = "+-";
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
