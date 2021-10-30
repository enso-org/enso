//! This crate describes valid escape sequences inside Enso text literals.

use crate::prelude::*;

use crate::lexeme;
use crate::library::token;
use crate::token::Shape;
use crate::token::EscapeStyle;



// =======================
// === EscapeCharacter ===
// =======================

/// A representation of an escape character.
#[derive(Clone,Debug,Default,Eq,PartialEq)]
pub struct EscapeCharacter {
    /// The lexing representation of the escape.
    ///
    /// This is the literal string that must occur in the Enso source code to be interpreted as this
    /// escape code.
    pub pattern : String,
    /// The literal representation of the escape.
    ///
    /// This is the character-level encoding of this escape character in Rust, as the Rust escape
    /// representation and the Enso escape representation may differ, or Rust may not support the
    /// same literal escape code as Enso.
    pub repr : String,

}
impl EscapeCharacter {
    fn new(pattern:impl Str, repr:impl Str) -> EscapeCharacter {
        let pattern = pattern.into();
        let repr    = repr.into();
        Self{pattern,repr}
    }

    /// The set of character escape codes that Enso supports.
    pub fn codes() -> Vec<EscapeCharacter> {
        vec![
            // === Null ===
            Self::new(r"\0","\0"),

            // === Newlines ===
            Self::new(r"\n","\n"),
            Self::new(r"\r","\r"),
            Self::new(r"\f","\x0C"),

            // === Tabs ===
            Self::new(r"\t","\t"),
            Self::new(r"\v","\x0B"),

            // === Backspace ===
            Self::new(r"\b","\x08"),

            // === Misc ===
            Self::new(r"\a","\x07"),
        ]
    }
}



// =================
// === Utilities ===
// =================

/// Check if `c` is a hexadecimal digit.
fn is_hex_digit(c:char) -> bool {
    let small_letters = 'a'..='f';
    let large_letters = 'A'..='F';
    let digits        = '0'..='9';
    small_letters.contains(&c) || large_letters.contains(&c) || digits.contains(&c)
}



// ======================
// === EscapeSequence ===
// ======================

/// A trait representing various kinds of escape sequence.
///
/// An escape sequence built using this trait will have its digits calculated by stripping the
/// [`Self::prefix_length()`] and [`Self::suffix_length()`] from the input string, and then
/// validated using [`Self::digits_min_length()`], [`Self::digits_max_length()`], and
/// [`Self::validator()`]. All digits must be valid hexadecimal digits as defined by
/// [`is_hex_digit`] above.
///
/// In addition, the implementation must define [`Self::style_on_success()`] and
/// [`Self::style_on_failure()`] to determine the type of escape output on success and failure.
pub trait EscapeSequence {
    /// Create a token of the relevant escape sequence type.
    ///
    /// This function should be passed the _full_ match for the escape sequence as `repr`, including
    /// the delimiters. For example, if we have the escape sequence `\uAFAF`, we want to pass the
    /// whole string `"\uAFAF"`, not just `"AFAF"` to this function..
    fn build(repr:impl Str) -> Shape {
        if let Some(digits) = Self::validate(repr.as_ref()) {
            Shape::text_segment_escape(Self::style_on_success(),digits)
        } else {
            Shape::text_segment_escape(Self::style_on_failure(),repr)
        }
    }

    /// Obtain the digits portion of the escape sequence.
    fn get_digits(repr:&str) -> &str {
        let start = Self::prefix_length();
        let end   = repr.len().saturating_sub(Self::suffix_length());
        &repr[start..end]
    }

    /// Validate the provided unicode string for this type of escape sequence.
    fn validate(repr:&str) -> Option<String> {
        let digits       = Self::get_digits(repr);
        let ge_min       = digits.len() >= Self::digits_min_length();
        let le_max       = digits.len() <= Self::digits_max_length();
        let valid_length = ge_min && le_max;
        let valid_escape = Self::validator(digits);
        let valid_digits = digits.chars().all(is_hex_digit);
        let is_valid     = valid_length && valid_escape && valid_digits;
        is_valid.as_some(digits.into())
    }

    /// Return the length of the escape prefix.
    ///
    /// The suffix is the characters that need to be stripped from the front of the escape sequence
    /// to get, in conjunction with [`EscapeSequence::suffix_length()`] the escape value itself.
    fn prefix_length() -> usize;

    /// Return the length of the escape suffix.
    ///
    /// The suffix is the characters that need to be stripped from the end of the escape sequence to
    /// get, in conjunction with [`EscapeSequence::prefix_length()`] the escape value itself.
    ///
    /// This defaults to `0`.
    fn suffix_length() -> usize { 0 }

    /// Return the minimum number of digits accepted by the escape sequence type.
    fn digits_min_length() -> usize;

    /// Return the maximum number of digits accepted by the escape sequence type.
    ///
    /// This defaults to `digits_min_length()`.
    fn digits_max_length() -> usize { Self::digits_min_length() }

    /// A validator for any additional properties of the escape sequence.
    ///
    /// It will be passed the _digits_ of the escape sequence, as defined by
    /// [`EscapeSequence::get_digits()`], and has a default implementation that always succeeds.
    /// Please implement this validator yourself if you would like to assert _additional_ properties
    /// on your escape sequence.
    fn validator(_digits:&str) -> bool { true }

    /// The style of escape after successful validation.
    fn style_on_success() -> token::EscapeStyle;

    /// The style of escape after unsuccessful validation.
    fn style_on_failure() -> token::EscapeStyle;
}



// ==================
// === ByteEscape ===
// ==================

/// A validator for ASCII escapes.
///
/// An ascii escape begins with the sequence `\x` and is followed by two hexadecimal digits (e.g.
/// `\x0F`.
#[derive(Clone,Copy,Default,Debug,Eq,PartialEq)]
pub struct Byte;
impl EscapeSequence for Byte {
    fn prefix_length()     -> usize { lexeme::len(lexeme::literal::BYTE_ESCAPE_START) }
    fn digits_min_length() -> usize { 2 }
    fn style_on_success()  -> EscapeStyle { token::EscapeStyle::Byte }
    fn style_on_failure()  -> EscapeStyle { token::EscapeStyle::Invalid }
}



// ===========
// === U16 ===
// ===========

/// A validator for U16 unicode escapes.
///
/// A U16 unicode escape begins with the sequence `\u` and is followed by four hexadecimal digits,
/// e.g. `\u0F0F`.
#[derive(Clone,Copy,Default,Debug,Eq,PartialEq)]
pub struct U16;
impl EscapeSequence for U16 {
    fn prefix_length()     -> usize { lexeme::len(lexeme::literal::U16_ESCAPE_START) }
    fn digits_min_length() -> usize { 4 }
    fn style_on_success()  -> EscapeStyle { token::EscapeStyle::U16 }
    fn style_on_failure()  -> EscapeStyle { token::EscapeStyle::InvalidUnicode }
}



// ===========
// === U21 ===
// ===========

/// A validator for U21 unicode escapes.
///
/// A U21 unicode escape begins with the sequence `\u`, followed by a sequence of 1-6 hexadecimal
/// digits enclosed in braces (`{}`). Both `\u{F}` and `\u{AFAFAF}` are valid U21 escapes.
#[derive(Clone,Copy,Default,Debug,Eq,PartialEq)]
pub struct U21;
impl EscapeSequence for U21 {
    fn prefix_length()     -> usize { lexeme::len(lexeme::literal::U21_ESCAPE_START) }
    fn suffix_length()     -> usize { lexeme::len(lexeme::literal::U21_ESCAPE_END) }
    fn digits_min_length() -> usize { 1 }
    fn digits_max_length() -> usize { 6 }
    fn style_on_success()  -> EscapeStyle { token::EscapeStyle::U21 }
    fn style_on_failure()  -> EscapeStyle { token::EscapeStyle::InvalidUnicode }
}



// ===========
// === U32 ===
// ===========

/// A validator for U32 unicode escapes.
///
/// A U32 unicode escape begins with the sequence \U, followed by 8 hexadecimal digits. Due to the
/// restrictions of unicode, the first two digits _must_ be zero (e.g. `\U00AFAFAF`).
#[derive(Clone,Copy,Default,Debug,Eq,PartialEq)]
pub struct U32;
impl EscapeSequence for U32 {
    fn prefix_length()         -> usize { lexeme::len(lexeme::literal::U32_ESCAPE_START) }
    fn digits_min_length()     -> usize { 8 }
    fn validator(digits: &str) -> bool { digits.starts_with("00") }
    fn style_on_success()      -> EscapeStyle { token::EscapeStyle::U32 }
    fn style_on_failure()      -> EscapeStyle { token::EscapeStyle::InvalidUnicode }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod test {
    use super::*;


    // === Utilities ===

    /// Tests a valid input to ensure that it succeeds.
    fn test_valid<Esc:EscapeSequence>(escape:&str, out:&str, out_style:token::EscapeStyle) {
        let shape = Shape::text_segment_escape(out_style,out);
        assert_eq!(Esc::build(escape),shape);
    }

    /// Tests invalid inputs to ensure they fail for the provided escape type `Esc`.
    fn test_invalid<Esc:EscapeSequence>(invalid_cases:Vec<&str>, fail_with:token::EscapeStyle) {
        for escape in invalid_cases {
            let shape  = Shape::text_segment_escape(fail_with,escape);
            assert_eq!(Esc::build(escape),shape)
        }
    }


    // === Is Hex Digit ===

    #[test]
    fn test_is_hex_digit() {
        for val in u8::min_value()..=u8::max_value() {
            let char            = char::from(val);
            let is_in_small     = ('a'..='f').contains(&char);
            let is_in_large     = ('A'..='F').contains(&char);
            let is_in_dec_digit = ('0'..='9').contains(&char);
            let expected_result = is_in_small || is_in_large || is_in_dec_digit;
            assert_eq!(is_hex_digit(char),expected_result);
        }
    }


    // === Build ===

    #[test]
    fn test_byte_build_valid() {
        test_valid::<Byte>(r"\x05","05",token::EscapeStyle::Byte);
    }

    #[test]
    fn test_byte_build_invalid() {
        test_invalid::<Byte>(vec![
            r"\x5",
            r"\x",
            r"\x033",
            r"\xz2",
        ],token::EscapeStyle::Invalid);
    }

    #[test]
    fn test_u16_build_valid() {
        test_valid::<U16>(r"\u4fe3","4fe3",token::EscapeStyle::U16);
    }

    #[test]
    fn test_u16_build_invalid() {
        test_invalid::<U16>(vec![
            r"\u123",
            r"\u",
            r"\u123aff",
            r"\uazaz",
        ],token::EscapeStyle::InvalidUnicode);
    }

    #[test]
    fn test_u21_build_valid() {
        test_valid::<U21>(r"\u{fa4e}","fa4e",token::EscapeStyle::U21);
    }

    #[test]
    fn test_u21_build_invalid() {
        test_invalid::<U21>(vec![
            r"\u{1234567}",
            r"\u{}",
        ],token::EscapeStyle::InvalidUnicode);
    }

    #[test]
    fn test_u32_build_valid() {
        test_valid::<U32>(r"\U0014A890","0014A890",token::EscapeStyle::U32);
    }

    #[test]
    fn test_u32_build_invalid() {
        test_invalid::<U32>(vec![
            r"\U12121212",
            r"\U",
            r"\U001234",
            r"\U001234567"
        ],token::EscapeStyle::InvalidUnicode);
    }
}
