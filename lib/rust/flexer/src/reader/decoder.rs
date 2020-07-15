/// Trait for decoding UTF32 characters.
pub trait Decoder {
    /// The input of the decoder.
    type Word;
    /// The maximum amount of words needed to decode one symbol.
    const MAX_SYMBOL_LEN: usize;

    /// Decodes the first symbol from the slice and returns it with its length (in words).
    fn decode(words:&[Self::Word]) -> (u32,u8);
}



// ====================
// === UTF8 Decoder ===
// ====================

/// Decodes UTF-8 encoded symbols.
///
/// For more info on UTF-8 and the algorithms used see [UTF-8](https://en.wikipedia.org/wiki/UTF-8).
#[derive(Debug,Copy,Clone)]
pub struct DecoderUTF8();

impl Decoder for DecoderUTF8 {
    type Word = u8;

    const MAX_SYMBOL_LEN: usize = 4;

    fn decode(words: &[u8]) -> (u32,u8) {
        let length:u8 = match !words[0] >> 4 {
            0     => 4,
            1     => 3,
            2 | 3 => 2,
            _     => 1,
        };

        let mut char = (words[0] << length >> length) as u32;
        for word in &words[1..length as usize] {
            char = char << 6 | (word & 0b_0011_1111) as u32;
        }
        (char, length)
    }
}



// =====================
// === UTF16 Decoder ===
// =====================

#[derive(Debug,Copy,Clone)]
pub struct DecoderUTF16();

impl Decoder for DecoderUTF16 {
    type Word = u16;

    const MAX_SYMBOL_LEN: usize = 2;

    fn decode(words: &[u16]) -> (u32, u8) {
        unimplemented!()
    }
}