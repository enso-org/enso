use nom::bytes::complete::tag;
use nom::bytes::complete::take_while_m_n;
use nom::bytes::complete::*;
use nom::character::complete::*;
use nom::combinator::map_res;
use nom::combinator::*;
use nom::sequence::tuple;
use nom::IResult;
use nom::InputTakeAtPosition;
use std::str;

#[derive(Debug, PartialEq)]
pub struct Color {
    pub red:   u8,
    pub green: u8,
    pub blue:  u8,
}

fn from_hex(input: &str) -> Result<u8, std::num::ParseIntError> {
    u8::from_str_radix(input, 16)
}

fn is_hex_digit(c: char) -> bool {
    c.is_digit(16)
}

fn hex_primary(input: &str) -> IResult<&str, u8> {
    map_res(take_while_m_n(2, 2, is_hex_digit), from_hex)(input)
}

fn hex_color(input: &str) -> IResult<&str, Color> {
    let (input, _) = tag("#")(input)?;
    let (input, (red, green, blue)) = tuple((hex_primary, hex_primary, hex_primary))(input)?;

    Ok((input, Color { red, green, blue }))
}



// =============
// === Token ===
// =============

pub enum TokenKind {
    Underscore,
    Ident(Ident),
}

pub struct Token {
    kind: TokenKind,
}

impl Token {
    pub fn new(kind: TokenKind) -> Self {
        Self { kind }
    }
}


// =============
// === Ident ===
// =============

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

// fn is_ident_char(t: char) -> bool {
//     is_ident_start_char(t) || (t >= '0' && t <= '9') || (t == '_')
// }
//
// fn ident(input: &str) -> IResult<&str, &str> {
//     let x = opt(char('_'))(input);
//     take_while1(is_ident_char)(input)
// }

fn main() {
    let input = "fooAr bar";
    let x: str::Chars = input.chars();
    // println!("{:?}", ident(input));
}
