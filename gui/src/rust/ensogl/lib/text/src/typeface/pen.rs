//! The pen is a point on the text _baseline_ used to locate glyph. It moves along the _baseline_
//! with each glyph rendered. For details, see
//! [freetype documentation](https://www.freetype.org/freetype2/docs/glyphs/glyphs-3.html#section-1)

use crate::prelude::*;

use super::font::Font;



// =====================
// === AdvanceResult ===
// =====================

/// Information about the char at the pen position.
#[derive(Clone, Copy, Debug)]
#[allow(missing_docs)]
pub struct AdvanceResult {
    pub char:   Option<char>,
    pub offset: f32,
}



// ================
// === CharInfo ===
// ================

/// Information about the char used to transform the pen.
#[derive(Clone, Copy, Debug)]
#[allow(missing_docs)]
pub struct CharInfo {
    pub char: char,
    pub size: f32,
}

impl CharInfo {
    pub fn new(char: char, size: f32) -> Self {
        Self { char, size }
    }
}



// ===========
// === Pen ===
// ===========

/// Pen iterates over chars producing the position for a given char.
///
/// The pen is a font-specific term (see
/// [freetype documentation](https://www.freetype.org/freetype2/docs/glyphs/glyphs-3.html#section-1)
/// for details).
#[derive(Debug)]
pub struct Pen {
    offset:       f32,
    current_char: Option<CharInfo>,
    font:         Font,
}

impl Pen {
    /// Create a new pen iterator for a given font.
    pub fn new(font: &Font) -> Self {
        let offset = default();
        let current_char = default();
        let font = font.clone_ref();
        Self { offset, current_char, font }
    }

    /// Advance the pen to the next position.
    pub fn advance(&mut self, next: Option<CharInfo>) -> AdvanceResult {
        let next_char = next.map(|t| t.char);
        if let Some(current) = self.current_char {
            let kerning =
                next_char.map(|ch| self.font.kerning(current.char, ch)).unwrap_or_default();
            let advance = self.font.glyph_info(current.char).advance + kerning;
            let offset = advance * current.size;
            self.offset += offset;
        }
        self.current_char = next;
        let offset = self.offset;
        AdvanceResult { char: next_char, offset }
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    use crate::typeface::font::GlyphRenderInfo;

    use wasm_bindgen_test::wasm_bindgen_test;

    #[wasm_bindgen_test(async)]
    async fn moving_pen() {
        ensogl_text_msdf_sys::initialized().await;
        let font = Font::mock("Test font");
        mock_a_glyph_info(font.clone_ref());
        mock_w_glyph_info(font.clone_ref());
        font.mock_kerning_info('A', 'W', -0.16);
        font.mock_kerning_info('W', 'A', 0.0);

        let mut pen = Pen::new(&font);
        let mut result = Vec::new();
        for chr in "AWA".chars() {
            let char_info = CharInfo::new(chr, 1.0);
            result.push(pen.advance(Some(char_info)).offset);
        }
        let expected = vec![0.0, 0.4, 1.1];
        assert_eq!(expected, result);
    }

    fn mock_a_glyph_info(font: Font) -> GlyphRenderInfo {
        let advance = 0.56;
        let scale = Vector2::new(0.5, 0.8);
        let offset = Vector2::new(0.1, 0.2);
        font.mock_char_info('A', scale, offset, advance)
    }

    fn mock_w_glyph_info(font: Font) -> GlyphRenderInfo {
        let advance = 0.7;
        let scale = Vector2::new(0.6, 0.9);
        let offset = Vector2::new(0.1, 0.2);
        font.mock_char_info('W', scale, offset, advance)
    }
}
