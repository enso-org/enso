//! This crate describes valid escape sequences inside Enso text literals.

use crate::prelude::*;

use crate::library::token::Token;
use crate::library::token;



// =======================
// === EscapeCharacter ===
// =======================

/// A representation of an escape character.
#[derive(Clone,Debug,Default,Eq,PartialEq)]
pub struct EscapeCharacter {
    /// The lexing representation of the escape.
    pub pattern : String,
    /// The literal representation of the escape.
    pub repr : String,

}
impl EscapeCharacter {
    fn new(pattern_str:impl Str, repr_str:impl Str) -> EscapeCharacter {
        let pattern = pattern_str.into();
        let repr    = repr_str.into();
        Self{pattern,repr}
    }

    /// The set of character escape codes that Enso supports.
    pub fn codes() -> Vec<EscapeCharacter> {
        vec![
            // === Null ===
            Self::new("\\0","\0"),

            // === Newlines ===
            Self::new("\\n","\n"),
            Self::new("\\r","\r"),
            Self::new("\\f","\x0C"),

            // === Tabs ===
            Self::new("\\t","\t"),
            Self::new("\\v","\x0B"),

            // === Backspace ===
            Self::new("\\b","\x08"),

            // === Misc ===
            Self::new("\\a","\x07"),
        ]
    }
}



// ======================
// === EscapeSequence ===
// ======================

/// A trait representing various kinds of escape sequence.
pub trait EscapeSequence {
    /// Create a token of the relevant escape sequence type.
    ///
    /// This function should be passed the _full_ match, including the delimiters.
    fn build(repr:impl Str, offset:usize) -> Token;

    /// Obtain the digits portion of the escape sequence.
    fn get_digits(repr:&str) -> &str;

    /// Validate the provided unicode string for this type of escape sequence.
    fn validate(repr:&str) -> Option<String>;

    /// Check if `c` is a hexadecimal digit.
    fn is_hex_digit(c:char) -> bool {
        let small_letters = 'a'..='f';
        let large_letters = 'A'..='F';
        let digits        = '0'..='9';
        small_letters.contains(&c) || large_letters.contains(&c) || digits.contains(&c)
    }
}



// ==================
// === ByteEscape ===
// ==================

/// A validator for ASCII escapes.
#[derive(Clone,Copy,Default,Debug,Eq,PartialEq)]
pub struct ByteEscape;
impl EscapeSequence for ByteEscape {
    fn build(repr: impl Str, offset: usize) -> Token {
        if let Some(digits) = Self::validate(repr.as_ref()) {
            Token::TextSegmentEscape(token::EscapeStyle::Byte,digits,offset)
        } else {
            Token::TextSegmentEscape(token::EscapeStyle::Invalid,repr,offset)
        }
    }

    fn get_digits(repr: &str) -> &str {
        &repr[2..]
    }

    fn validate(repr: &str) -> Option<String> {
        let digits = Self::get_digits(repr);
        let valid_length = digits.len() == 2;
        let valid_digits = digits.chars().map(Self::is_hex_digit).fold(true, |l,r| l & r);
        (valid_length && valid_digits).and_option(Some(digits.into()))
    }
}



// ===========
// === U16 ===
// ===========

/// A validator for U16 unicode escapes.
#[derive(Clone,Copy,Default,Debug,Eq,PartialEq)]
pub struct U16;
impl EscapeSequence for U16 {
    fn build(repr:impl Str, offset:usize) -> Token {
        if let Some(digits) = Self::validate(repr.as_ref()) {
            Token::TextSegmentEscape(token::EscapeStyle::U16,digits,offset)
        } else {
            Token::TextSegmentEscape(token::EscapeStyle::InvalidUnicode,repr,offset)
        }
    }

    fn get_digits(repr:&str) -> &str {
        &repr[2..]
    }

    fn validate(repr:&str) -> Option<String> {
        let digits = Self::get_digits(repr);
        let valid_length = digits.len() == 4;
        let valid_digits = digits.chars().map(Self::is_hex_digit).fold(true, |l,r| l & r);
        (valid_length && valid_digits).and_option(Some(digits.into()))
    }
}



// ===========
// === U21 ===
// ===========

/// A validator for U16 unicode escapes.
#[derive(Clone,Copy,Default,Debug,Eq,PartialEq)]
pub struct U21;
impl EscapeSequence for U21 {
    fn build(repr:impl Str, offset:usize) -> Token {
        if let Some(digits) = Self::validate(repr.as_ref()) {
            Token::TextSegmentEscape(token::EscapeStyle::U21,digits,offset)
        } else {
            Token::TextSegmentEscape(token::EscapeStyle::InvalidUnicode,repr,offset)
        }
    }

    fn get_digits(repr:&str) -> &str {
        let limit = repr.len() - 1;
        &repr[3..limit]
    }

    fn validate(repr:&str) -> Option<String> {
        let digits = Self::get_digits(repr);
        let valid_length = !digits.is_empty() && digits.len() <= 6;
        let valid_digits = digits.chars().map(Self::is_hex_digit).fold(true, |l,r| l & r);
        (valid_length && valid_digits).and_option(Some(digits.into()))
    }
}



// ===========
// === U32 ===
// ===========

/// A validator for U16 unicode escapes.
#[derive(Clone,Copy,Default,Debug,Eq,PartialEq)]
pub struct U32;
impl EscapeSequence for U32 {
    fn build(repr:impl Str, offset:usize) -> Token {
        if let Some(digits) = Self::validate(repr.as_ref()) {
            Token::TextSegmentEscape(token::EscapeStyle::U32,digits,offset)
        } else {
            Token::TextSegmentEscape(token::EscapeStyle::InvalidUnicode,repr,offset)
        }
    }

    fn get_digits(repr:&str) -> &str {
        &repr[2..]
    }

    fn validate(repr:&str) -> Option<String> {
        let digits = Self::get_digits(repr);
        let valid_length = digits.len() == 8;
        let valid_prefix = digits.starts_with("00");
        let valid_digits = digits.chars().map(Self::is_hex_digit).fold(true, |l,r| l & r);
        (valid_length && valid_prefix && valid_digits).and_option(Some(digits.into()))
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_is_hex_digit() {
        assert!(U16::is_hex_digit('f'));
        assert!(U16::is_hex_digit('9'));
        assert!(!U16::is_hex_digit('z'));
    }

    #[test]
    fn test_u16_get_digits() {
        let escape = "\\u4fe3";
        let digits = U16::get_digits(escape);
        assert_eq!(digits,"4fe3");
    }

    #[test]
    fn test_u16_validate_valid() {
        let escape = "\\u4fe3";
        assert_eq!(U16::validate(escape),Some("4fe3".into()));
    }

    #[test]
    fn test_u16_validate_invalid() {
        let escape = "\\u4fz3";
        assert_eq!(U16::validate(escape),None);
    }

    #[test]
    fn test_u16_build_valid() {
        let escape = "\\u4fe3";
        let token  = Token::TextSegmentEscape(token::EscapeStyle::U16,"4fe3",0);
        assert_eq!(U16::build(escape,0),token);
    }

    #[test]
    fn test_u16_build_invalid() {
        let escape = "\\u4f";
        let token  = Token::TextSegmentEscape(token::EscapeStyle::InvalidUnicode,"\\u4f",0);
        assert_eq!(U16::build(escape,0),token);
    }

    #[test]
    fn test_u21_get_digits() {
        let escape = "\\u{E9}";
        let digits = U21::get_digits(escape);
        assert_eq!(digits,"E9");
    }

    #[test]
    fn test_u21_validate_valid() {
        let escape = "\\u{E9}";
        assert_eq!(U21::validate(escape),Some("E9".into()));
    }

    #[test]
    fn test_u21_validate_invalid() {
        let escape = "\\u{fffffff}";
        assert_eq!(U21::validate(escape),None);
    }

    #[test]
    fn test_u21_build_valid() {
        let escape = "\\u{fa4e}";
        let token  = Token::TextSegmentEscape(token::EscapeStyle::U21,"fa4e",0);
        assert_eq!(U21::build(escape,0),token);
    }

    #[test]
    fn test_u21_build_invalid() {
        let escape = "\\u{4z}";
        let token  = Token::TextSegmentEscape(token::EscapeStyle::InvalidUnicode,"\\u{4z}",0);
        assert_eq!(U21::build(escape,0),token);
    }

    #[test]
    fn test_u32_get_digits() {
        let escape = "\\U00AFD213";
        let digits = U32::get_digits(escape);
        assert_eq!(digits,"00AFD213");
    }

    #[test]
    fn test_u32_validate_valid() {
        let escape = "\\U00AFD143";
        assert_eq!(U32::validate(escape),Some("00AFD143".into()));
    }

    #[test]
    fn test_u32_validate_invalid() {
        let escape = "\\Uffffffff";
        assert_eq!(U32::validate(escape),None);
    }

    #[test]
    fn test_u32_build_valid() {
        let escape = "\\U0014A89D";
        let token  = Token::TextSegmentEscape(token::EscapeStyle::U32,"0014A89D",0);
        assert_eq!(U32::build(escape,0),token);
    }

    #[test]
    fn test_u32_build_invalid() {
        let escape = "\\Uafafafaf";
        let token  = Token::TextSegmentEscape(token::EscapeStyle::InvalidUnicode,"\\Uafafafaf",0);
        assert_eq!(U32::build(escape,0),token);
    }

    // TODO [AA] Macro this.
}
