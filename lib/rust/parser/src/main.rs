use enso_prelude::*;
use std::str;



// =============
// === Token ===
// =============

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenKind {
    Underscore,
    Ident(Ident),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Token {
    start:        usize,
    len:          usize,
    right_offset: usize,
    kind:         TokenKind,
}

impl Token {
    pub fn new(start: usize, len: usize, right_offset: usize, kind: TokenKind) -> Self {
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

pub struct Input<'s> {
    pub current:  Option<char>,
    pub iterator: str::Chars<'s>,
    pub offset:   usize,
}

impl<'s> Input<'s> {
    pub fn new(input: &'s str) -> Self {
        let current = default();
        let iterator = input.chars();
        let offset = default();
        Self { current, iterator, offset }.init()
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

    pub fn take_while_1(&mut self, f: impl Copy + Fn(char) -> bool) -> bool {
        let out = self.take_1(f);
        if out {
            self.take_while(f);
        }
        out
    }
}


pub trait MkToken {
    type Output;
    fn mk_token(self, start: usize, len: usize, right_offset: usize) -> Self::Output;
}

impl MkToken for Option<TokenKind> {
    type Output = Option<Token>;
    fn mk_token(self, start: usize, len: usize, right_offset: usize) -> Self::Output {
        self.map(|t| Token::new(start, len, right_offset, t))
    }
}


// =============
// === Space ===
// =============

/// Based on https://jkorpela.fi/chars/spaces.html.
const UNICODE_SINGLE_SPACES: &str = "\u{0020}\u{00A0}\u{1680}\u{202F}\u{205F}\u{3000}";
const UNICODE_SINGLE_SPACES_RANGE: (char, char) = ('\u{2000}', '\u{200A}');
const UNICODE_ZERO_SPACES: &str = "\u{180E}\u{200B}\u{FEFF}";

impl<'s> Input<'s> {
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
    is_free:    bool,
    lift_level: usize,
}

impl Ident {
    pub fn new(is_free: bool, lift_level: usize) -> Self {
        Self { is_free, lift_level }
    }
}

fn is_ident_start_char(t: char) -> bool {
    (t >= 'a' && t <= 'z') || (t >= 'A' && t <= 'Z')
}

fn is_ident_char(t: char) -> bool {
    is_ident_start_char(t) || (t >= '0' && t <= '9') || (t == '_')
}

impl<'s> Input<'s> {
    fn ident(&mut self) -> Option<Token> {
        token! { self
            let underscore = self.char('_');
            let ident_start = self.take_1(is_ident_start_char);
            if ident_start {
                self.take_while(is_ident_char);
                let lift_level = self.count_char('\'');
                Some(TokenKind::Ident(Ident::new(underscore, lift_level)))
            } else {
                underscore.as_some(TokenKind::Underscore)
            }
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
        let mut input = Input::new(&inp);
        assert_eq!(input.ident(), result);
    }
}

fn test_ident(s: &str, is_free: bool, lift_level: usize) -> Box<dyn Fn(usize, usize) -> Token> {
    let len = s.len();
    Box::new(move |start: usize, right_offset: usize| {
        Token::new(start, len, right_offset, TokenKind::Ident(Ident::new(is_free, lift_level)))
    })
}

fn test_underscore() -> Box<dyn Fn(usize, usize) -> Token> {
    Box::new(move |start: usize, right_offset: usize| {
        Token::new(start, 1, right_offset, TokenKind::Underscore)
    })
}

#[test]
fn test_idents() {
    assert_token_eq("", None);
    assert_token_eq("_", Some(test_underscore()));
    assert_token_eq("a", Some(test_ident("a", false, 0)));
    assert_token_eq("a'", Some(test_ident("a'", false, 1)));
    assert_token_eq("a''", Some(test_ident("a''", false, 2)));
    assert_token_eq("a'''", Some(test_ident("a'''", false, 3)));
    assert_token_eq("_a", Some(test_ident("_a", true, 0)));
    // assert_token_eq("__a", Some(test_ident("__a", true, 0)));
    assert_token_eq("_a'", Some(test_ident("_a'", true, 1)));
    assert_token_eq("_a''", Some(test_ident("_a''", true, 2)));
    assert_token_eq("_a'''", Some(test_ident("_a'''", true, 3)));
    assert_token_eq("test", Some(test_ident("test", false, 0)));

    // assert_token_eq("test ", Some(Token::ident(0, 4, 0, false, 0)));
}

#[macro_export]
macro_rules! token {
    ($self:ident $($ts:tt)*) => {
        let start = $self.offset;
        let kind = { $($ts)* };
        let len = $self.offset - start;
        let right_offset = $self.spaces();
        kind.mk_token(start, len, right_offset)
    };
}

// pub fn count_with(&mut self, f: impl Fn(char) -> (bool, usize)) -> usize {


fn main() {
    let str = "_fooAr'' bar";
    let mut input = Input::new(str);
    println!("{:?}", input.ident());
}
