#![allow(unsafe_code)]

//! This module exports various UTF decoders for decoding UTF32 characters.

use std::fmt::Debug;



// ===============
// === Decoder ===
// ===============

/// The error for an invalid character.
#[derive(Debug,Clone,Copy)]
pub struct InvalidChar();

/// Trait for decoding UTF32 characters.
pub trait Decoder {
    /// The input of the decoder.
    type Word : Default + Copy + Debug;
    /// The maximum amount of words needed to decode one symbol.
    const MAX_CODEPOINT_LEN: usize;

    /// Decodes the first symbol from the slice and returns it with its length (in words).
    ///
    /// This function can panic if `words.len() < MAX_CODEPOINT_LEN`.
    fn decode(words:&[Self::Word]) -> Char<InvalidChar>;
}


// === Char ===

/// The result of `decoder.decode`.
#[derive(Debug,Clone,Copy,PartialEq)]
pub struct Char<Error> {
    /// The decoded character.
    pub char: Result<char,Error>,
    /// The number of words read.
    pub size: usize,
}



// =====================
// === UTF-8 Decoder ===
// =====================

/// Decoder for UTF-8.
///
/// For more info on UTF-8 and the algorithm used see [UTF-8](https://en.wikipedia.org/wiki/UTF-8).
#[derive(Debug,Copy,Clone)]
pub struct DecoderUTF8();


// === Trait Impls ===

impl Decoder for DecoderUTF8 {
    type Word = u8;

    const MAX_CODEPOINT_LEN: usize = 4;

    fn decode(words: &[u8]) -> Char<InvalidChar> {
        let size = match !words[0] >> 4 {
            0     => 4,
            1     => 3,
            2 | 3 => 2,
            _     => 1,
        };

        let mut char = (words[0] << size >> size) as u32;
        for word in &words[1..size] {
            char = char << 6 | (word & 0b_0011_1111) as u32;
        }

        Char{char:std::char::from_u32(char).ok_or_else(InvalidChar),size}
    }
}



// ======================
// === UTF-16 Decoder ===
// ======================

/// Decoder for UTF-16.
///
/// For more info on UTF-16 and the algorithm used see [UTF-16](https://en.wikipedia.org/wiki/UTF-16).
#[derive(Debug,Copy,Clone)]
pub struct DecoderUTF16();


// === Trait Impls ===

impl Decoder for DecoderUTF16 {
    type Word = u16;

    const MAX_CODEPOINT_LEN: usize = 2;

    fn decode(words: &[u16]) -> Char<InvalidChar> {
        if words[0] < 0xD800 || 0xDFFF < words[0] {
            let char = Ok(unsafe{std::char::from_u32_unchecked(words[0] as u32)});
            return Char{char,size:1};
        }
        let char = (((words[0] - 0xD800) as u32) << 10 | (words[1] - 0xDC00) as u32) + 0x1_0000;

        Char{char:std::char::from_u32(char).ok_or_else(InvalidChar), size:2}
    }
}



// ======================
// === UTF-32 Decoder ===
// ======================

/// Trivial decoder for UTF-32 (`char`).
#[derive(Debug,Copy,Clone)]
pub struct DecoderUTF32();


// === Trait Impls ===

impl Decoder for DecoderUTF32 {
    type Word = char;

    const MAX_CODEPOINT_LEN: usize = 1;

    fn decode(words: &[char]) -> Char<InvalidChar> {
        Char{char:Ok(words[0]), size:1}
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;
    use itertools::Itertools;



    #[test]
    fn test_utf8() {
        let string  = "a.b^c! #𤭢界んにち𤭢#𤭢";
        let mut buf = string.as_bytes();
        let mut str = String::from("");
        while !buf.is_empty() {
            let char = DecoderUTF8::decode(buf);
            str.push(char.char.unwrap());
            buf = &buf[char.size..];
        }
        assert_eq!(str, string);
    }

    #[test]
    fn test_utf16() {
        let string  = "a.b^c! #𤭢界んにち𤭢#𤭢";
        let buffer  = string.encode_utf16().collect_vec();
        let mut buf = &buffer[..];
        let mut str = String::from("");
        while !buf.is_empty() {
            let char = DecoderUTF16::decode(buf);
            str.push(char.char.unwrap());
            buf = &buf[char.size..];
        }
        assert_eq!(str, string);
    }

    #[test]
    fn test_utf32() {
        let string  = "a.b^c! #𤭢界んにち𤭢#𤭢".chars().collect_vec();
        let mut buf = &string[..];
        let mut str = vec![];
        while !buf.is_empty() {
            let char = DecoderUTF32::decode(buf);
            str.push(char.char.unwrap());
            buf = &buf[char.size..];
        }
        assert_eq!(str, string);
    }
}
