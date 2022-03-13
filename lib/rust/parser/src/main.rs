#![feature(allocator_api)]
#![feature(test)]

use enso_prelude::*;

use bumpalo::Bump;
use std::str;



// =============
// === Token ===
// =============

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Kind {
    Underscore(Underscore),
    Ident(Ident),
    Operator,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Token {
    left_offset: usize,
    start:       usize,
    len:         usize,
    kind:        Kind,
}

impl Token {
    #[inline(always)]
    pub fn new(left_offset: usize, start: usize, len: usize, kind: Kind) -> Self {
        Self { left_offset, start, len, kind }
    }
}



pub type TokenVec<'s> = Vec<Token, &'s Bump>;

pub struct Model<'s> {
    pub current:     Option<char>,
    pub iterator:    str::Chars<'s>,
    pub offset:      usize,
    pub output:      TokenVec<'s>,
    pub last_spaces: usize,
}

impl<'s> Model<'s> {
    pub fn new(bump: &'s Bump, input: &'s str) -> Self {
        let current = default();
        let iterator = input.chars();
        let offset = default();
        let output = Vec::new_in(bump);
        let last_spaces = default();
        Self { current, iterator, offset, output, last_spaces }.init()
    }

    fn init(mut self) -> Self {
        self.next();
        self.offset = 0;
        self
    }

    #[inline(always)]
    fn next(&mut self) {
        self.current = self.iterator.next();
        self.offset += 1;
    }

    #[inline(always)]
    pub fn char(&mut self, t: char) -> bool {
        self.take_1(|s| s == t)
    }

    #[inline(always)]
    pub fn count_char(&mut self, t: char) -> usize {
        self.count(|s| s == t)
    }

    #[inline(always)]
    pub fn count(&mut self, f: impl Fn(char) -> bool) -> usize {
        let start = self.offset;
        self.take_while(f);
        self.offset - start
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

pub trait MkToken {
    type Output;
    fn mk_token(self, vec: &mut TokenVec, l_off: usize, start: usize, len: usize) -> Self::Output;
}

impl MkToken for Option<Kind> {
    type Output = bool;
    fn mk_token(self, vec: &mut TokenVec, l_off: usize, start: usize, len: usize) -> Self::Output {
        if let Some(tok) = self.map(|t| Token::new(l_off, start, len, t)) {
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
    fn space(&mut self) -> Option<usize> {
        let out = self.current.and_then(is_space_char);
        if out.is_some() {
            self.next();
        }
        out
    }

    #[inline(always)]
    fn spaces(&mut self) -> usize {
        let mut offset = 0;
        loop {
            if let Some(off) = self.space() {
                offset += off;
            } else {
                break;
            }
        }
        offset
    }
}



// =============
// === Ident ===
// =============

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Ident {
    is_free:        bool,
    lift_level:     usize,
    invalid_suffix: usize,
}

impl Ident {
    #[inline(always)]
    pub fn new(is_free: bool, lift_level: usize, invalid_suffix: usize) -> Self {
        Self { is_free, lift_level, invalid_suffix }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Underscore {
    invalid_suffix: usize,
}

impl Underscore {
    #[inline(always)]
    pub fn new(invalid_suffix: usize) -> Self {
        Self { invalid_suffix }
    }
}

// #[inline(always)]
// fn is_ident_start_char(t: char) -> bool {
//     (t >= 'a' && t <= 'z') || (t >= 'A' && t <= 'Z')
// }
//
// #[inline(always)]
// fn is_ident_char(t: char) -> bool {
//     is_ident_start_char(t) || (t >= '0' && t <= '9') || (t == '_')
// }
//
// #[inline(always)]
// fn is_invalid_suffix(t: char) -> bool {
//     !is_space_char(t).is_some() && !is_ident_split_char(t)
// }

#[inline(always)]
fn is_ident_body2(t: char) -> bool {
    is_ident_start_char2(t) || (t >= '0' && t <= '9')
}

#[inline(always)]
fn is_ident_start_char2(t: char) -> bool {
    t.is_alphabetic() || t == '_' || t == '\''
}

impl<'s> Model<'s> {
    // fn ident(&mut self) -> bool {
    //     token! { self
    //         let underscore = self.char('_');
    //         let ident_start = self.take_1(is_ident_char);
    //         let invalid_suffix = 0;
    //         let mut kind = if ident_start {
    //             self.take_while(is_ident_char);
    //             let lift_level = self.count_char('\'');
    //             Some(Kind::Ident(Ident::new(underscore, lift_level, invalid_suffix)))
    //         } else {
    //             underscore.as_some(Kind::Underscore(Underscore::new(invalid_suffix)))
    //         };
    //         if let Some(kind_ref) = kind.as_mut() {
    //             let invalid_suffix = self.count(is_invalid_suffix);
    //             if invalid_suffix > 0 {
    //                 match kind_ref {
    //                     Kind::Ident(t) => t.invalid_suffix = invalid_suffix,
    //                     Kind::Underscore(t) => t.invalid_suffix = invalid_suffix,
    //                     _ => unreachable!()
    //                 }
    //             }
    //         }
    //         kind
    //     }
    // }

    fn ident(&mut self) -> bool {
        token! { self
            self.take_1(is_ident_start_char2).as_some_from(||{
                self.take_while(is_ident_body2);
                Kind::Ident(Ident::new(false,0,0))
            })
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
        token! { self
            self.take_while_1(is_operator_body_char).as_some_from(||{
                Kind::Operator
            })
        }
    }
}



// ============
// === Glue ===
// ============

impl<'s> Model<'s> {
    fn lex(&mut self) -> bool {
        loop {
            self.last_spaces = self.spaces();
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

fn test_ident(
    s: &str,
    is_free: bool,
    lift_level: usize,
    invalid_suffix: usize,
) -> Box<dyn Fn(usize, usize) -> Token> {
    let len = s.len();
    Box::new(move |left_offset: usize, start: usize| {
        Token::new(
            left_offset,
            start,
            len,
            Kind::Ident(Ident::new(is_free, lift_level, invalid_suffix)),
        )
    })
}

fn test_underscore(s: &str, invalid_suffix: usize) -> Box<dyn Fn(usize, usize) -> Token> {
    let len = s.len();
    Box::new(move |left_offset: usize, start: usize| {
        Token::new(left_offset, start, len, Kind::Underscore(Underscore::new(invalid_suffix)))
    })
}

fn test_operator(s: &str) -> Box<dyn Fn(usize, usize) -> Token> {
    let len = s.len();
    Box::new(move |left_offset: usize, start: usize| {
        Token::new(left_offset, start, len, Kind::Operator)
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_manual() {
        let inp = "test test2'''_ tt";
        let bump = Bump::new();
        let mut input = Model::new(&bump, &inp);
        input.lex();
        println!("{:#?}", input.output);
    }

    #[test]
    fn test_case_idents() {
        assert_token_eq("", None);
        assert_token_eq("_", Some(test_underscore("_", 0)));
        assert_token_eq("a", Some(test_ident("a", false, 0, 0)));
        assert_token_eq("a'", Some(test_ident("a'", false, 1, 0)));
        assert_token_eq("a''", Some(test_ident("a''", false, 2, 0)));
        assert_token_eq("a'''", Some(test_ident("a'''", false, 3, 0)));
        assert_token_eq("_a", Some(test_ident("_a", true, 0, 0)));
        assert_token_eq("_a'", Some(test_ident("_a'", true, 1, 0)));
        assert_token_eq("_a''", Some(test_ident("_a''", true, 2, 0)));
        assert_token_eq("_a'''", Some(test_ident("_a'''", true, 3, 0)));
        assert_token_eq("test", Some(test_ident("test", false, 0, 0)));
        assert_token_eq("__a", Some(test_ident("__a", true, 0, 0)));
        assert_token_eq("___a", Some(test_ident("___a", true, 0, 0)));
        assert_token_eq("_a_", Some(test_ident("_a_", true, 0, 0)));
        assert_token_eq("__a__", Some(test_ident("__a__", true, 0, 0)));
        assert_token_eq("_a_b_", Some(test_ident("_a_b_", true, 0, 0)));

        assert_token_eq("Test_Name", Some(test_ident("Test_Name", false, 0, 0)));
        assert_token_eq("Test_Name'", Some(test_ident("Test_Name'", false, 1, 0)));

        assert_token_eq("a'a", Some(test_ident("a'a", false, 1, 1)));
        assert_token_eq("a'b'", Some(test_ident("a'b'", false, 1, 2)));
        assert_token_eq("_'", Some(test_underscore("_'", 1)));
        assert_token_eq("_'a", Some(test_underscore("_'a", 2)));

        // assert_token_eq("ą", Some(test_ident("ą", false, 0, 0)));
    }

    #[test]
    fn test_case_operator() {
        assert_token_eq("", None);
        assert_token_eq("+", Some(test_operator("+")));
        assert_token_eq("-", Some(test_operator("-")));
        assert_token_eq("=", Some(test_operator("=")));
        assert_token_eq("==", Some(test_operator("==")));
        assert_token_eq("===", Some(test_operator("===")));
        // +-
    }
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

    // 8-8.5x slowdown.
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
