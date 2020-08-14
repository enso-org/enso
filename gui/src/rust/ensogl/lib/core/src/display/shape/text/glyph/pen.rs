//! The pen is a point on the text _baseline_ used to locate glyph. It moves along the _baseline_
//! with each glyph rendered. For details, see
//! [freetype documentation](https://www.freetype.org/freetype2/docs/glyphs/glyphs-3.html#section-1)

use crate::prelude::*;

use crate::display::shape::text::glyph::font;



// ===================
// === PenIterator ===
// ===================

/// Iterator over chars producing also the pen position for a given char.
///
/// The pen is a font-specific term (see
/// [freetype documentation](https://www.freetype.org/freetype2/docs/glyphs/glyphs-3.html#section-1)
/// for details).
#[derive(Debug)]
pub struct PenIterator<CharIterator> {
    x_offset     : f32,
    line_height  : f32,
    current_char : Option<char>,
    next_chars   : CharIterator,
    next_advance : f32,
    font         : font::Handle,
}

impl<CharIterator> Iterator for PenIterator<CharIterator>
where CharIterator : Iterator<Item=char> {
    type Item = (char,f32);
    fn next(&mut self) -> Option<Self::Item> {
        self.next_chars.next().map(|ch| self.next_char(ch))
    }
}

impl<I> PenIterator<I>
where I : Iterator<Item=char> {
    /// Create iterator wrapping `chars`, with pen starting from given position.
    pub fn new (line_height:f32, next_chars:I, font:font::Handle) -> Self {
        let x_offset     = 0.0;
        let next_advance = 0.0;
        let current_char = None;
        Self {x_offset,line_height,current_char,next_chars,next_advance,font}
    }

    fn next_char(&mut self, ch:char) -> (char,f32) {
        if let Some(current_ch) = self.current_char {
            self.move_pen(current_ch,ch)
        }
        self.next_advance = self.font.get_glyph_info(ch).advance;
        self.current_char = Some(ch);
        (ch,self.x_offset)
    }

    fn move_pen(&mut self, current:char, next:char) {
        let kerning    = self.font.get_kerning(current,next);
        let advance    = self.next_advance + kerning;
        let offset     = advance * self.line_height;
        self.x_offset += offset;
    }
}



#[cfg(test)]
mod tests {
    use super::*;

    use crate::display::shape::text::glyph::font;
    use crate::display::shape::text::glyph::font::GlyphRenderInfo;

    use wasm_bindgen_test::wasm_bindgen_test;

    #[wasm_bindgen_test(async)]
    async fn moving_pen(){
        ensogl_text_msdf_sys::initialized().await;
        let font = font::Handle::new(font::RenderInfo::mock_font("Test font".to_string()));
        mock_a_glyph_info(font.clone_ref());
        mock_w_glyph_info(font.clone_ref());
        font.mock_kerning_info('A', 'W', -0.16);
        font.mock_kerning_info('W', 'A', 0.0);

        let chars    = "AWA".chars();
        let iter     = PenIterator::new(1.0,chars,font);
        let result   = iter.collect_vec();
        let expected = vec!
            [ ('A', 0.0)
            , ('W', 0.4)
            , ('A', 1.1)
            ];
        assert_eq!(expected,result);
    }

    fn mock_a_glyph_info(font:font::Handle) -> GlyphRenderInfo {
        let advance = 0.56;
        let scale   = Vector2::new(0.5, 0.8);
        let offset  = Vector2::new(0.1, 0.2);
        font.mock_char_info('A',scale,offset,advance)
    }

    fn mock_w_glyph_info(font:font::Handle) -> GlyphRenderInfo {
        let advance = 0.7;
        let scale   = Vector2::new(0.6, 0.9);
        let offset  = Vector2::new(0.1, 0.2);
        font.mock_char_info('W',scale,offset,advance)
    }
}