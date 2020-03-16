//! The pen is a point on the text _baseline_ used to locate glyph. It moves along the _baseline_
//! with each glyph rendered. For details, see
//! [freetype documentation](https://www.freetype.org/freetype2/docs/glyphs/glyphs-3.html#section-1)

use crate::prelude::*;

use crate::display::shape::text::glyph::font::FontHandle;

use nalgebra::Vector2;



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
    position     : Vector2<f32>,
    line_height  : f32,
    current_char : Option<char>,
    next_chars   : CharIterator,
    next_advance : f32,
    font         : FontHandle,
}

impl<CharIterator> Iterator for PenIterator<CharIterator>
where CharIterator : Iterator<Item=char> {
    type Item = (char,Vector2<f32>);

    fn next(&mut self) -> Option<Self::Item> {
        self.next_chars.next().map(|ch| self.next_char(ch))
    }
}

impl<CharIterator> PenIterator<CharIterator>
where CharIterator : Iterator<Item=char> {
    /// Create iterator wrapping `chars`, with pen starting from given position.
    pub fn new
    ( start_from:Vector2<f32>
    , line_height:f32
    , chars:CharIterator
    , font:FontHandle
    ) -> Self {
        Self {font,line_height,
            position     : start_from,
            current_char : None,
            next_chars   : chars,
            next_advance : 0.0,
        }
    }

    fn next_char(&mut self, ch:char) -> (char,Vector2<f32>) {
        if let Some(current_ch) = self.current_char {
            self.move_pen(current_ch,ch)
        }
        self.next_advance = self.font.get_glyph_info(ch).advance;
        self.current_char = Some(ch);
        (ch,self.position)
    }

    fn move_pen(&mut self, current:char, next:char) {
        let kerning   = self.font.get_kerning(current,next);
        self.position.x += (self.next_advance + kerning) * self.line_height;
    }
}



#[cfg(test)]
mod tests {
    use super::*;

    use crate::display::shape::text::glyph::font::FontRenderInfo;
    use crate::display::shape::text::glyph::font::GlyphRenderInfo;

    use wasm_bindgen_test::wasm_bindgen_test;

    #[wasm_bindgen_test(async)]
    async fn moving_pen(){
        ensogl_core_msdf_sys::initialized().await;
        let font = FontHandle::new(FontRenderInfo::mock_font("Test font".to_string()));
        mock_a_glyph_info(font.clone_ref());
        mock_w_glyph_info(font.clone_ref());
        font.mock_kerning_info('A', 'W', -0.16);
        font.mock_kerning_info('W', 'A', 0.0);

        let initial_position = Vector2::new(0.0,0.0);
        let chars            = "AWA".chars();
        let iter             = PenIterator::new(initial_position,1.0,chars,font);
        let result           = iter.collect_vec();
        let expected = vec!
            [ ('A', Vector2::new(0.0, 0.0))
            , ('W', Vector2::new(0.4, 0.0))
            , ('A', Vector2::new(1.1, 0.0))
            ];
        assert_eq!(expected,result);
    }

    fn mock_a_glyph_info(font:FontHandle) -> GlyphRenderInfo {
        let advance = 0.56;
        let scale   = Vector2::new(0.5, 0.8);
        let offset  = Vector2::new(0.1, 0.2);
        font.mock_char_info('A',scale,offset,advance)
    }

    fn mock_w_glyph_info(font:FontHandle) -> GlyphRenderInfo {
        let advance = 0.7;
        let scale   = Vector2::new(0.6, 0.9);
        let offset  = Vector2::new(0.1, 0.2);
        font.mock_char_info('W',scale,offset,advance)
    }
}