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
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Token {
    start:        usize,
    len:          usize,
    right_offset: usize,
    kind:         Kind,
}

impl Token {
    #[inline(always)]
    pub fn new(start: usize, len: usize, right_offset: usize, kind: Kind) -> Self {
        Self { start, len, right_offset, kind }
    }
}



// fn is_ident_char(t: char) -> bool {
//     is_ident_start_char(t) || (t >= '0' && t <= '9') || (t == '_')
// }
//
// fn ident(input: &str) -> IResult<&str, &str> {
//     let x = opt(char('_'))(input);
//     take_while1(is_ident_char)(input)
// }

pub struct Model<'s> {
    pub current:  Option<char>,
    pub iterator: str::Chars<'s>,
    pub offset:   usize,
    pub output:   Vec<Token, &'s Bump>,
}

impl<'s> Model<'s> {
    pub fn new(bump: &'s Bump, input: &'s str) -> Self {
        let current = default();
        let iterator = input.chars();
        let offset = default();
        let output = Vec::new_in(bump);
        Self { current, iterator, offset, output }.init()
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
        let start = $self.offset;
        let kind = { $($ts)* };
        let len = $self.offset - start;
        let right_offset = $self.spaces();
        kind.mk_token(&mut $self.output, start, len, right_offset)
    };
}

#[macro_export]
macro_rules! token_with_right_offset {
    ($self:ident $($ts:tt)*) => {
        let start = $self.offset;
        let (kind,right_offset) = { $($ts)* };
        let len = $self.offset - start - right_offset;
        kind.mk_token(&mut $self.output, start, len, right_offset)
    };
}

pub trait MkToken {
    type Output;
    fn mk_token(
        self,
        output: &mut Vec<Token, &Bump>,
        start: usize,
        len: usize,
        right_offset: usize,
    ) -> Self::Output;
}

impl MkToken for Option<Kind> {
    type Output = bool;
    fn mk_token(
        self,
        output: &mut Vec<Token, &Bump>,
        start: usize,
        len: usize,
        right_offset: usize,
    ) -> Self::Output {
        if let Some(tok) = self.map(|t| Token::new(start, len, right_offset, t)) {
            output.push(tok);
            true
        } else {
            false
        }
    }
}



// =============
// === Space ===
// =============

/// Based on https://jkorpela.fi/chars/spaces.html.
const UNICODE_SINGLE_SPACES: &str = "\u{0020}\u{00A0}\u{1680}\u{202F}\u{205F}\u{3000}";
const UNICODE_SINGLE_SPACES_RANGE: (char, char) = ('\u{2000}', '\u{200A}');
const UNICODE_ZERO_SPACES: &str = "\u{180E}\u{200B}\u{FEFF}";

#[inline(always)]
fn is_space_char(t: char) -> bool {
    (t == ' ')
        || (t == '\t')
        || UNICODE_SINGLE_SPACES.contains(t)
        || (t >= UNICODE_SINGLE_SPACES_RANGE.0 && t <= UNICODE_SINGLE_SPACES_RANGE.1)
        || (UNICODE_ZERO_SPACES.contains(t))
}

impl<'s> Model<'s> {
    #[inline(always)]
    fn space(&mut self) -> Option<usize> {
        let out = self.current.and_then(|t| {
            if t == ' ' {
                Some(1)
            } else if t == '\t' {
                Some(4)
            } else if UNICODE_SINGLE_SPACES.contains(t) {
                Some(1)
            } else if t >= UNICODE_SINGLE_SPACES_RANGE.0 && t <= UNICODE_SINGLE_SPACES_RANGE.1 {
                Some(1)
            } else if UNICODE_ZERO_SPACES.contains(t) {
                Some(0)
            } else {
                None
            }
        });
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

#[inline(always)]
fn is_ident_start_char(t: char) -> bool {
    (t >= 'a' && t <= 'z') || (t >= 'A' && t <= 'Z')
}

#[inline(always)]
fn is_ident_char(t: char) -> bool {
    is_ident_start_char(t) || (t >= '0' && t <= '9') || (t == '_')
}

#[inline(always)]
fn is_invalid_suffix(t: char) -> bool {
    !is_ident_split_char(t) && !is_space_char(t)
}

#[inline(always)]
fn is_operator_char(t: char) -> bool {
    ";!$%&*+-/<>?^~|:\\".contains(t)
}

#[inline(always)]
fn is_bracket_char(t: char) -> bool {
    "()[]{}".contains(t)
}

#[inline(always)]
fn is_ident_split_char(t: char) -> bool {
    is_bracket_char(t) || is_operator_char(t) || "\"".contains(t)
}

impl<'s> Model<'s> {
    fn ident(&mut self) -> bool {
        token_with_right_offset! { self
            let underscore = self.char('_');
            let ident_start = self.take_1(is_ident_char);
            let invalid_suffix = 0;
            let mut kind = if ident_start {
                self.take_while(is_ident_char);
                let lift_level = self.count_char('\'');
                Some(Kind::Ident(Ident::new(underscore, lift_level, invalid_suffix)))
            } else {
                underscore.as_some(Kind::Underscore(Underscore::new(invalid_suffix)))
            };
            let mut right_offset = 0;
            if let Some(kind_ref) = kind.as_mut() {
                right_offset = self.spaces();
                if right_offset == 0 {
                    let invalid_suffix = self.count(is_invalid_suffix);
                    if invalid_suffix > 0 {
                        match kind_ref {
                            Kind::Ident(t) => t.invalid_suffix = invalid_suffix,
                            Kind::Underscore(t) => t.invalid_suffix = invalid_suffix,
                            _ => unreachable!()
                        }
                        right_offset = self.spaces()
                    }
                }
            }
            (kind,right_offset)
        }
    }
}



// =============
// === Tests ===
// =============

fn assert_token_eq(str: &str, f: Option<Box<dyn Fn(usize, usize) -> Token>>) {
    // TODO: test tabs
    for r_offset in 0..4 {
        let inp = format!("{}{}", str, " ".repeat(r_offset));
        let result = f.as_ref().map(|f| f(0, r_offset));
        let bump = Bump::new();
        let mut input = Model::new(&bump, &inp);
        assert_eq!(input.ident().and_option_from(|| input.output.last().copied()), result);
    }
}

fn test_ident(
    s: &str,
    is_free: bool,
    lift_level: usize,
    invalid_suffix: usize,
) -> Box<dyn Fn(usize, usize) -> Token> {
    let len = s.len();
    Box::new(move |start: usize, right_offset: usize| {
        Token::new(
            start,
            len,
            right_offset,
            Kind::Ident(Ident::new(is_free, lift_level, invalid_suffix)),
        )
    })
}

fn test_underscore(s: &str, invalid_suffix: usize) -> Box<dyn Fn(usize, usize) -> Token> {
    let len = s.len();
    Box::new(move |start: usize, right_offset: usize| {
        Token::new(start, len, right_offset, Kind::Underscore(Underscore::new(invalid_suffix)))
    })
}

#[test]
fn test_idents() {
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

    assert_token_eq("a'a", Some(test_ident("a'a", false, 1, 1)));
    assert_token_eq("a'b'", Some(test_ident("a'b'", false, 1, 2)));
    assert_token_eq("_'", Some(test_underscore("_'", 1)));
    assert_token_eq("_'a", Some(test_underscore("_'a", 2)));
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

    // 16x slowdown.
    #[bench]
    fn bench_idents(b: &mut Bencher) {
        let reps = 1000_000;
        let str = "test ".repeat(reps);
        let capacity = str.len() / 5;

        b.iter(move || {
            let bump = Bump::with_capacity(capacity);
            let mut input = Model::new(&bump, &str);
            for _ in 0..reps {
                assert_eq!(input.ident(), true)
            }
        });
    }
}
