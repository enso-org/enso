#![feature(type_changing_struct_update)]
#![feature(allocator_api)]
#![feature(test)]

use enso_prelude::*;

use bumpalo::Bump;
use std::str;



pub struct Bytes(usize);



// =============
// === Token ===
// =============



#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Token<T = Kind> {
    left_visible_offset: usize,
    left_byte_offset:    usize,
    start_byte_offset:   usize,
    bytes:               usize,
    elem:                T,
}

impl<T> Token<T> {
    #[inline(always)]
    pub fn new(
        left_visible_offset: usize,
        left_byte_offset: usize,
        start_byte_offset: usize,
        bytes: usize,
        elem: T,
    ) -> Self {
        Self { left_visible_offset, left_byte_offset, start_byte_offset, bytes, elem }
    }

    #[inline(always)]
    pub fn with_elem<S>(self, elem: S) -> Token<S> {
        Token { elem, ..self }
    }
}



pub type TokenVec<'s> = Vec<Token, &'s Bump>;

pub struct Model<'s> {
    pub input: &'s str,
    pub iterator: str::CharIndices<'s>,
    pub current: Option<char>,
    pub char_offset: usize,
    pub byte_offset: usize,
    pub output: TokenVec<'s>,
    pub last_spaces_visible_offset: usize,
    pub last_spaces_byte_offset: usize,
}

impl<'s> Model<'s> {
    pub fn new(bump: &'s Bump, input: &'s str) -> Self {
        let iterator = input.char_indices();
        let current = default();
        let char_offset = default();
        let byte_offset = default();
        let output = Vec::new_in(bump);
        let last_spaces_visible_offset = default();
        let last_spaces_byte_offset = default();
        Self {
            input,
            iterator,
            current,
            char_offset,
            byte_offset,
            output,
            last_spaces_visible_offset,
            last_spaces_byte_offset,
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
        &self.input[token.start_byte_offset..token.start_byte_offset + token.bytes]
    }

    #[inline(always)]
    fn next(&mut self) {
        if let Some((byte_offset, current)) = self.iterator.next() {
            self.char_offset += 1;
            self.byte_offset = byte_offset;
            self.current = Some(current);
        } else {
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
        let left_byte_offset = $self.last_spaces_byte_offset;
        let left_visible_offset = $self.last_spaces_visible_offset;
        let start_byte_offset = $self.byte_offset;
        let elem = { $($ts)* };
        let bytes = $self.byte_offset - start_byte_offset;
        Token::new(left_visible_offset, left_byte_offset, start_byte_offset, bytes, elem)
    }};
}

pub trait MkToken {
    type Output;
    fn mk_token(
        self,
        vec: &mut TokenVec,
        lv_off: usize,
        l_off: usize,
        start: usize,
        len: usize,
    ) -> Self::Output;
}

impl MkToken for Option<Kind> {
    type Output = bool;
    fn mk_token(
        self,
        vec: &mut TokenVec,
        lv_off: usize,
        l_off: usize,
        start: usize,
        len: usize,
    ) -> Self::Output {
        if let Some(tok) = self.map(|t| Token::new(lv_off, l_off, start, len, t)) {
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
    fn spaces(&mut self) -> (usize, usize) {
        let mut total_visible_offset = 0;
        let mut total_byte_offset = 0;
        loop {
            if let Some(visible_offset) = self.space_and_its_visible_offset() {
                total_visible_offset += visible_offset;
                total_byte_offset += 1;
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

// #[inline(always)]
// fn is_ident_split_char(t: char) -> bool {
//     if t <= '\u{7E}' && t >= '\u{21}' {
//         (t >= '\u{21}' && t <= '\u{26}')
//             || (t >= '\u{28}' && t <= '\u{2F}')
//             || (t >= '\u{3A}' && t <= '\u{40}')
//             || (t >= '\u{5B}' && t <= '\u{5E}')
//             || (t == '\u{60}')
//             || (t >= '\u{7B}' && t <= '\u{7E}')
//     } else {
//         false
//     }
// }


impl<'s> Model<'s> {
    fn operator(&mut self) -> bool {
        let tok = token2! { self
            self.take_1(is_operator_body_char).as_some_from(||{
                // match self.unchecked_last_char() {
                // '=' => {}
                // _ => {}
                self.take_while(is_operator_body_char);
                Kind::Operator
            })


        };
        if let Some(kind) = tok.elem {
            let repr = self.repr(tok);
            // for opr in repr.split('.') {
            //     if opr.is_empty() {
            //
            //     } else {
            //
            //     }
            // }
            // println!(">> {:?}", repr.split('.').collect_vec());
            let token = tok.with_elem(kind);
            self.output.push(token);
        }
        tok.elem.is_some()
    }
}



// ============
// === Glue ===
// ============

impl<'s> Model<'s> {
    fn lex(&mut self) -> bool {
        loop {
            (self.last_spaces_visible_offset, self.last_spaces_byte_offset) = self.spaces();
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
        let inp = "test test2'''_ tt .+= a";
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
