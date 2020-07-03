//! The pen is a point on the text _baseline_ used to locate glyph. It moves along the _baseline_
//! with each glyph rendered. For details, see
//! [freetype documentation](https://www.freetype.org/freetype2/docs/glyphs/glyphs-3.html#section-1)

use crate::prelude::*;

use super::font::Font;



// ================
// === CharInfo ===
// ================

/// Information about the char at the pen position.
#[derive(Clone,Copy,Debug)]
#[allow(missing_docs)]
pub struct CharInfo {
    pub char   : char,
    pub offset : f32,
}



// ===========
// === Pen ===
// ===========

//pub trait CharIterator = std::iter::Pen<Item=char>;

/// Pen iterates over chars producing the position for a given char.
///
/// The pen is a font-specific term (see
/// [freetype documentation](https://www.freetype.org/freetype2/docs/glyphs/glyphs-3.html#section-1)
/// for details).
#[derive(Debug)]
pub struct Pen {
    offset       : f32,
    current_char : Option<(char,f32)>,
    font         : Font,
}

impl Pen {
    /// Create a new pen iterator for a given font.
    pub fn new (font:&Font) -> Self {
        let offset       = default();
        let current_char = default();
        let font         = font.clone_ref();
        Self {offset,current_char,font}
    }

    /// Advance the pen to the next position.
    pub fn advance(&mut self, next_char:char, next_char_size:f32) -> CharInfo {
        if let Some((current_char,current_char_size)) = self.current_char {
            let kerning = self.font.get_kerning(current_char,next_char);
            let advance = self.font.get_glyph_info(current_char).advance + kerning;
            let offset  = advance * current_char_size;
            self.offset += offset;
        }
        self.current_char = Some((next_char,next_char_size));
        let offset        = self.offset;
        CharInfo {char:next_char,offset}
    }

    /// Advance the pen to the last position (after all chars were consumed).
    pub fn advance_final(&mut self) -> f32 {
        if let Some((current_char,current_char_size)) = self.current_char {
            let advance = self.font.get_glyph_info(current_char).advance;
            let offset  = advance * current_char_size;
            self.offset += offset;
        }
        self.current_char = None;
        self.offset
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    use crate::typeface::font;
    use crate::typeface::font::GlyphRenderInfo;

    use wasm_bindgen_test::wasm_bindgen_test;

    #[wasm_bindgen_test(async)]
    async fn moving_pen(){
         ensogl_core_msdf_sys::initialized().await;
        let font = Font::mock("Test font");
        mock_a_glyph_info(font.clone_ref());
        mock_w_glyph_info(font.clone_ref());
        font.mock_kerning_info('A', 'W', -0.16);
        font.mock_kerning_info('W', 'A', 0.0);

        let mut pen    = Pen::new(&font);
        let mut result = Vec::new();
        for chr in "AWA".chars() {
            result.push(pen.advance(chr,1.0).offset);
        }
        let expected = vec![0.0,0.4,1.1];
        assert_eq!(expected,result);
    }

    fn mock_a_glyph_info(font:Font) -> GlyphRenderInfo {
        let advance = 0.56;
        let scale   = Vector2::new(0.5, 0.8);
        let offset  = Vector2::new(0.1, 0.2);
        font.mock_char_info('A',scale,offset,advance)
    }

    fn mock_w_glyph_info(font:Font) -> GlyphRenderInfo {
        let advance = 0.7;
        let scale   = Vector2::new(0.6, 0.9);
        let offset  = Vector2::new(0.1, 0.2);
        font.mock_char_info('W',scale,offset,advance)
    }
}
