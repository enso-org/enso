//! This module defines the base lexemes for the Enso language.

use crate::prelude::*;

use enso_flexer::automata::pattern::Pattern;



// =================================
// === Basic Pattern Definitions ===
// =================================

/// Basic lexemes as patterns.
///
/// These must _only_ be used as part of the lexer definition, not used at runtime as they are not
/// performant at all.
pub mod definition_pattern {
    use super::*;

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
        into_pattern(literal::SPACE).many1()
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
        let lf   = into_pattern(literal::LF);
        let crlf = into_pattern(literal::CRLF);
        lf | crlf
    }

    /// The characters that break tokens in Enso.
    pub fn whitespace_break_chars() -> String {
        [literal::TAB,literal::LF,literal::CR].concat()
    }

    /// The characters that break token lexing in Enso.
    pub fn break_chars() -> String {
        [
            literal::INTERPOLATE_QUOTE,
            literal::COMMENT,
            literal::ANNOTATION_SYMBOL,
            literal::SPACE,
            literal::COMMA,
            literal::DOT,
            literal::OPERATOR_CHARS,
            literal::GROUP_CHARS,
            &whitespace_break_chars()
        ].concat()
    }

    /// Adds the basic characters not allowed in a raw segment in a format text literal.
    fn add_base_format_disallows(chars:&mut String) {
        chars.push_str(literal::INTERPOLATE_QUOTE);
        chars.push_str(literal::SLASH);
        chars.push_str(literal::LF);
        chars.push_str(literal::CR);
    }

    /// Characters allowable inside a raw segment in a format line.
    pub fn format_line_raw_char() -> Pattern {
        let mut chars = String::new();
        chars.push_str(literal::FORMAT_QUOTE);
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
        chars.push_str(literal::SLASH);
        chars.push_str(literal::LF);
        chars.push_str(literal::CR);
    }

    /// Characters allowable inside a raw segment in a raw line.
    pub fn raw_line_raw_char() -> Pattern {
        let mut chars = String::new();
        chars.push_str(literal::RAW_QUOTE);
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
        let chars = &[
            literal::FORMAT_QUOTE,
            literal::RAW_QUOTE,
            literal::INTERPOLATE_QUOTE,
            literal::SLASH,
            literal::LF,
            literal::CR,
            "{}"
        ].concat();
        Pattern::none_of(chars)
    }
}



// ===============================
// === Enso Lexeme Definitions ===
// ===============================

/// The literal lexemes that make up the Enso language.
pub mod literal {

    /// The type of a literal lexeme.
    pub type Literal = &'static str;

    // === The Lexemes ===

    /// The space character.
    pub const SPACE:Literal = " ";

    /// The line-feed character.
    pub const LF:Literal = "\n";

    /// The carriage-return character.
    pub const CR:Literal = "\r";

    /// The crlf windows-style line ending.
    pub const CRLF:Literal = "\r\n";

    /// The tab character.
    pub const TAB:Literal = "\t";

    /// The comment character.
    pub const COMMENT:Literal = "#";

    /// The doc comment character.
    pub const DOC_COMMENT:Literal = "##";

    /// The symbol for beginning an annotation.
    pub const ANNOTATION_SYMBOL:Literal = "@";

    /// The dot symbol
    pub const DOT:Literal = ".";

    /// Two dots.
    pub const TWO_DOTS:Literal = "..";

    /// Three dots.
    pub const THREE_DOTS:Literal = "...";

    /// Three dots.
    pub const COMMA:Literal = ",";

    /// The `in` operator.
    pub const OPERATOR_IN:Literal = "in";

    /// The tick allowable at the end of an identifier.
    pub const IDENTIFIER_TICK:Literal = "'";

    /// The quote used to delimit interpolations in format text literals.
    pub const INTERPOLATE_QUOTE:Literal = "`";

    /// The quote used to delimit format text literals.
    pub const FORMAT_QUOTE:Literal = "'";

    /// The quote used to delimit format block literals.
    pub const FORMAT_BLOCK_QUOTE:Literal = "'''";

    /// The quote used to delimit raw text literals.
    pub const RAW_QUOTE:Literal = "\"";

    /// The quote used to delimit raw block literals.
    pub const RAW_BLOCK_QUOTE:Literal = "\"\"\"";

    /// The equals operator.
    pub const EQUALS:Literal = "=";

    /// The equality comparison operator.
    pub const EQUALS_COMP:Literal = "==";

    /// Greater-than or equal.
    pub const GE_OPERATOR:Literal = ">=";

    /// Less-than or equal.
    pub const LE_OPERATOR:Literal = "<=";

    /// Inequality comparison operator.
    pub const NOT_EQUAL:Literal = "!=";

    /// The hash eq operator.
    pub const HASH_EQ:Literal = "#=";

    /// The wide arrow operator.
    pub const WIDE_ARROW:Literal = "=>";

    /// The blank identifier.
    pub const BLANK_IDENT:Literal = "_";

    /// The identifier segment separator.
    pub const IDENT_SEGMENT_SEPARATOR:Literal = "_";

    /// The separator between a number literal's explicit base and the number itself.
    pub const NUMBER_BASE_SEPARATOR:Literal = "_";

    /// The separator between the integer and fractional parts of the number literal.
    pub const DECIMAL_SEPARATOR:Literal = ".";

    /// The backslash character.
    pub const SLASH:Literal = r"\";

    /// An escaped [`SLASH`].
    pub const ESCAPED_SLASH:Literal = r"\\";

    /// The beginning of a byte escape.
    pub const BYTE_ESCAPE_START:Literal = r"\x";

    /// The beginning of a u16 escape.
    pub const U16_ESCAPE_START:Literal = r"\u";

    /// The beginning of a u21 escape.
    pub const U21_ESCAPE_START:Literal = r"\u{";

    /// The end of a u21 escape.
    pub const U21_ESCAPE_END:Literal = "}";

    /// The beginning of a u32 escape.
    pub const U32_ESCAPE_START:Literal = r"\U";

    /// The allowable group characters in Enso.
    pub const GROUP_CHARS:Literal = "()[]{}";

    /// The allowable operator characters in Enso.
    pub const OPERATOR_CHARS:Literal = ";!$%&*+-/<>?^~|:\\";
}



// =========================
// === Utility Functions ===
// =========================

/// Get the first character of the lexeme, if it exists.
pub fn char(literal:&'static str) -> Option<char> {
    literal.chars().next()
}

/// Get the first character of the lexeme, assuming that it exists.
pub fn unsafe_char(literal:&'static str) -> char {
    char(literal).expect("The first character of the literal exists.")
}

/// Convert the lexeme into a pattern.
pub fn into_pattern(literal:&'static str) -> Pattern {
    literal.into()
}

/// The proper length of the `literal`.
pub fn len(literal:&'static str) -> usize {
    literal.chars().count()
}
