use std::fmt::Debug;



/// Trait for decoding UTF32 characters.
pub trait Decoder {
    /// The input of the decoder.
    type Word : Default + Copy + Debug;
    /// The maximum amount of words needed to decode one symbol.
    const MAX_SYMBOL_LEN: usize;

    /// Decodes the first symbol from the slice and returns it with its length (in words).
    fn decode(words:&[Self::Word]) -> (u32,usize);
}



// ====================
// === UTF8 Decoder ===
// ====================

/// Decoder for UTF-8.
///
/// For more info on UTF-8 and the algorithms used see [UTF-8](https://en.wikipedia.org/wiki/UTF-8).
#[derive(Debug,Copy,Clone)]
pub struct DecoderUTF8();

impl Decoder for DecoderUTF8 {
    type Word = u8;

    const MAX_SYMBOL_LEN: usize = 4;

    fn decode(words: &[u8]) -> (u32,usize) {
        let length = match !words[0] >> 4 {
            0     => 4,
            1     => 3,
            2 | 3 => 2,
            _     => 1,
        };

        let mut char = (words[0] << length >> length) as u32;
        for word in &words[1..length] {
            char = char << 6 | (word & 0b_0011_1111) as u32;
        }
        (char, length)
    }
}



// =====================
// === UTF-16 Decoder ===
// =====================

/// Decoder for UTF-16.
///
/// For more info on UTF-16 and the algorithms used see [UTF-16](https://en.wikipedia.org/wiki/UTF-16).
#[derive(Debug,Copy,Clone)]
pub struct DecoderUTF16();

impl Decoder for DecoderUTF16 {
    type Word = u16;

    const MAX_SYMBOL_LEN: usize = 2;

    fn decode(words: &[u16]) -> (u32,usize) {
        unimplemented!()
    }
}



// ======================
// === UTF-32 Decoder ===
// ======================

/// Decoder for UTF-32 (`char`).
///
/// This decoder is implemented with just trivial cast of `char` into `u32`.
#[derive(Debug,Copy,Clone)]
pub struct DecoderUTF32();

impl Decoder for DecoderUTF32 {
    type Word = char;

    const MAX_SYMBOL_LEN: usize = 1;

    fn decode(words: &[char]) -> (u32,usize) {
        (words[0] as u32, 1)
    }
}
