//! This module defines the base lexemes for the Enso language.

use crate::prelude::*;

use enso_flexer::automata::pattern::Pattern;



// =================================
// === Basic Pattern Definitions ===
// =================================

/// Match lower-case ASCII letters.
pub fn lower_ascii_letter() -> Pattern {
    Pattern::range('a'..='z')
}

/// Match upper-case ASCII letters.
pub fn upper_ascii_letter() -> Pattern {
    Pattern::range('A'..='Z')
}

/// Match ASCII digits.
pub fn ascii_digit() -> Pattern {
    Pattern::range('0'..='9')
}

/// Match ASCII letters.
pub fn ascii_letter() -> Pattern {
    lower_ascii_letter() | upper_ascii_letter()
}

/// Match ASCII alphanumeric characters.
pub fn ascii_alpha_num() -> Pattern {
    ascii_digit() | ascii_letter()
}

/// Match at least one ASCII space character.
pub fn spaces() -> Pattern {
    space().into_pattern().many1()
}

/// Match a windows-style newline.
pub fn crlf() -> Pattern {
    let lf = lf().into_pattern();
    let cr = cr().into_pattern();
    cr >> lf
}

/// Match the end-of-file character.
pub fn eof() -> Pattern {
    Pattern::eof()
}

/// Match a newline.
///
/// This matches both Unix (LF) and Windows (CRLF) styles of newlines. This is particularly
/// important so as not to result in incorrect spans on windows clients.
pub fn newline() -> Pattern {
    let lf = lf().into_pattern();
    lf | crlf()
}

/// The allowable group characters in Enso.
pub fn group_chars() -> String {
    String::from("()[]{}")
}

/// The allowable operator characters in Enso.
pub fn operator_chars() -> String {
    String::from(";!$%&*+-/<>?^~|:\\")
}

/// The characters that break tokens in Enso.
pub fn whitespace_break_chars() -> String {
    let mut text = String::new();
    text.push_str(tab().literal());
    text.push_str(cr().literal());
    text.push_str(lf().literal());
    text
}

/// The characters that break token lexing in Enso.
pub fn break_chars() -> String {
    let mut break_chars = String::new();
    break_chars.push_str(interpolate_quote().literal());
    break_chars.push_str(comment().literal());
    break_chars.push_str(annotation_symbol().literal());
    break_chars.push_str(space().literal());
    break_chars.push_str(comma().literal());
    break_chars.push_str(dot().literal());
    break_chars.push_str(&operator_chars());
    break_chars.push_str(&whitespace_break_chars());
    break_chars.push_str(&group_chars());
    break_chars
}

/// Adds the basic characters not allowed in a raw segment in a format text literal.
fn add_base_format_disallows(chars:&mut String) {
    chars.push_str(interpolate_quote().literal());
    chars.push_str(slash().literal());
    chars.push_str(lf().literal());
    chars.push_str(cr().literal());
}

/// Characters allowable inside a raw segment in a format line.
pub fn format_line_raw_char() -> Pattern {
    let mut chars = String::new();
    chars.push_str(format_quote().literal());
    add_base_format_disallows(&mut chars);
    Pattern::none_of(&chars)
}

/// Characters allowable inside a raw segment in a format block.
pub fn format_block_raw_char() -> Pattern {
    let mut chars = String::new();
    add_base_format_disallows(&mut chars);
    Pattern::none_of(&chars)
}

/// Adds the basic characters not allowed in a raw segment in a raw text literal.
fn add_base_raw_disallows(chars:&mut String) {
    chars.push_str(slash().literal());
    chars.push_str(lf().literal());
    chars.push_str(cr().literal());
}

/// Characters allowable inside a raw segment in a raw line.
pub fn raw_line_raw_char() -> Pattern {
    let mut chars = String::new();
    chars.push_str(raw_quote().literal());
    add_base_raw_disallows(&mut chars);
    Pattern::none_of(&chars)
}

/// Characters allowable inside a raw segment in a raw block.
pub fn raw_block_raw_char() -> Pattern {
    let mut chars = String::new();
    add_base_raw_disallows(&mut chars);
    Pattern::none_of(&chars)
}

/// The characters allowed as digits in a unicode escape.
pub fn unicode_escape_digit() -> Pattern {
    let mut chars = String::new();
    chars.push_str(format_quote().literal());
    chars.push_str(raw_quote().literal());
    chars.push_str(interpolate_quote().literal());
    chars.push_str(slash().literal());
    chars.push_str(lf().literal());
    chars.push_str(cr().literal());
    chars.push_str("{}");
    Pattern::none_of(&chars)
}



// ===============================
// === Enso Lexeme Definitions ===
// ===============================

/// The number of repetitions of a quote required to define a text block.
pub const BLOCK_QUOTE_LEN:usize = 3;

/// The space character.
pub fn space() -> Lexeme {
    Lexeme::new(" ")
}

/// The line-feed character.
pub fn lf() -> Lexeme {
    Lexeme::new("\n")
}

/// The carriage-return character.
pub fn cr() -> Lexeme {
    Lexeme::new("\r")
}

/// The tab character.
pub fn tab() -> Lexeme {
    Lexeme::new("\t")
}

/// The comment character.
pub fn comment() -> Lexeme {
    Lexeme::new("#")
}

/// The doc comment character.
pub fn doc_comment() -> Lexeme {
    Lexeme::new(comment().literal().repeat(2))
}

/// The symbol for beginning an annotation.
pub fn annotation_symbol() -> Lexeme {
    Lexeme::new("@")
}

/// The dot symbol.
pub fn dot() -> Lexeme {
    Lexeme::new(".")
}

/// Two dots.
pub fn two_dots() -> Lexeme {
    Lexeme::new(dot().literal().repeat(2))
}

/// Three dots.
pub fn three_dots() -> Lexeme {
    Lexeme::new(dot().literal().repeat(3))
}

/// The comma symbol.
pub fn comma() -> Lexeme {
    Lexeme::new(",")
}

/// The `in` operator.
pub fn operator_in() -> Lexeme {
    Lexeme::new("in")
}

/// The tick allowable at the end of an identifier.
pub fn identifier_tick() -> Lexeme {
    Lexeme::new("'")
}

/// The quote used to delimit interpolations in format text literals.
pub fn interpolate_quote() -> Lexeme {
    Lexeme::new("`")
}

/// The quote used to delimit format text literals.
pub fn format_quote() -> Lexeme {
    Lexeme::new("'")
}

/// The quote used to delimit format block literals.
pub fn format_block_quote() -> Lexeme {
    Lexeme::new(format_quote().literal().repeat(BLOCK_QUOTE_LEN))
}

/// The quote used to delimit raw text literals.
pub fn raw_quote() -> Lexeme {
    Lexeme::new("\"")
}

/// The quote used to delimit raw block literals.
pub fn raw_block_quote() -> Lexeme {
    Lexeme::new(raw_quote().literal().repeat(BLOCK_QUOTE_LEN))
}

/// The equals operator.
pub fn equals_op() -> Lexeme {
    Lexeme::new("=")
}

/// The equality comparison operator.
pub fn equals_comp() -> Lexeme {
    Lexeme::new("==")
}

/// Greater-than or equal.
pub fn ge_operator() -> Lexeme {
    Lexeme::new(">=")
}

/// Less-than or equal.
pub fn le_operator() -> Lexeme {
    Lexeme::new("<=")
}

/// Inequality comparison operator.
pub fn not_equal() -> Lexeme {
    Lexeme::new("!=")
}

/// The hash eq operator.
pub fn hash_eq() -> Lexeme {
    Lexeme::new("#=")
}

/// The fat arrow operator.
pub fn fat_arrow() -> Lexeme {
    Lexeme::new("=>")
}

/// The blank identifier.
pub fn blank_ident() -> Lexeme {
    Lexeme::new("_")
}

/// The separator for identifier segments.
pub fn ident_segment_separator() -> Lexeme {
    Lexeme::new("_")
}

/// The separator between a number literal's explicit base and the number itself.
pub fn number_base_separator() -> Lexeme {
    Lexeme::new("_")
}

/// The separator between the integer and fractional parts of the number literal.
pub fn decimal_separator() -> Lexeme {
    Lexeme::new(".")
}

/// The backslash character.
pub fn slash() -> Lexeme {
    Lexeme::new(r"\")
}

/// An escaped slash character.
pub fn escaped_slash() -> Lexeme {
    Lexeme::new(r"\\")
}

/// The beginning of a byte escape.
pub fn byte_escape_start() -> Lexeme {
    Lexeme::new(r"\x")
}

/// The beginning of a u16 escape.
pub fn u16_escape_start() -> Lexeme {
    Lexeme::new(r"\u")
}

/// The beginning of a u21 escape.
pub fn u21_escape_start() -> Lexeme {
    Lexeme::new(r"\u{")
}

/// The end of a u21 escape.
pub fn u21_escape_end() -> Lexeme {
    Lexeme::new("}")
}

/// The beginning of a u32 escape.
pub fn u32_escape_start() -> Lexeme {
    Lexeme::new(r"\U")
}



// ==============
// === Lexeme ===
// ==============

/// A lexeme for the Enso Language.
#[derive(Clone,Debug,Default,Eq,PartialEq)]
#[allow(missing_docs)]
pub struct Lexeme {
    literal : String
}

impl Lexeme {
    /// Constructor.
    pub fn new(literal:impl Str) -> Self {
        let literal = literal.into();
        Self{literal}
    }

    /// Get the size of the lexeme.
    pub fn length(&self) -> usize {
        self.literal.len()
    }

    /// Get a reference to the literal of the lexeme.
    pub fn literal(&self) -> &str {
        self.literal.as_str()
    }

    /// Get the character of the lexeme, if it is a single character.
    pub fn char(&self) -> Option<char> {
        if self.length() == 1 {
            Some(self.literal.chars().nth(0).unwrap())
        } else {
            None
        }
    }

    /// Obtain the character of the lexeme, assuming it exists.
    pub fn unsafe_char(&self) -> char {
        self.char().expect("Lexeme is a single character.")
    }

    /// Consume the lexeme as a literal pattern.
    pub fn into_pattern(self) -> Pattern {
        self.into()
    }
}


// === Trait Impls ===

impl From<Lexeme> for Pattern {
    fn from(lexeme:Lexeme) -> Self {
        lexeme.literal.as_str().into()
    }
}
