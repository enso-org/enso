//! The pen is a point on the text _baseline_ used to locate glyph. It moves along the _baseline_
//! with each glyph rendered. For details, see
//! [freetype documentation](https://www.freetype.org/freetype2/docs/glyphs/glyphs-3.html#section-1)

use crate::prelude::*;

use crate::font;

use super::Font;



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
#[derive(Clone, Debug)]
#[allow(missing_docs)]
pub struct CharInfo {
    pub char:                     char,
    pub size:                     f32,
    non_variable_font_variations: font::NonVariableFaceHeader,
    variable_font_variations:     font::VariationAxes,
}

impl CharInfo {
    /// Constructor.
    pub fn new(
        char: char,
        size: f32,
        non_variable_font_variations: font::NonVariableFaceHeader,
        variable_font_variations: font::VariationAxes,
    ) -> Self {
        Self { char, size, non_variable_font_variations, variable_font_variations }
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

    // FIXME[WD] all unwraps. https://www.pivotaltracker.com/story/show/182746060
    /// Advance the pen to the next position.
    pub fn advance(&mut self, next: Option<CharInfo>) -> AdvanceResult {
        let next_char = next.as_ref().map(|t| t.char);
        if let Some(current) = &self.current_char {
            let a = self
                .font
                .glyph_id_of_code_point(
                    current.non_variable_font_variations,
                    &current.variable_font_variations,
                    current.char,
                )
                .unwrap();
            let b = next.as_ref().map(|t| {
                self.font
                    .glyph_id_of_code_point(
                        t.non_variable_font_variations,
                        &t.variable_font_variations,
                        t.char,
                    )
                    .unwrap()
            });
            let kerning = b
                .map(|ch| {
                    self.font.kerning(
                        current.non_variable_font_variations,
                        &current.variable_font_variations,
                        a,
                        ch,
                    )
                })
                .unwrap_or_default();
            let advance = self
                .font
                .glyph_info(
                    current.non_variable_font_variations,
                    &current.variable_font_variations,
                    a,
                )
                .unwrap()
                .advance
                + kerning;
            let offset = advance * current.size;
            self.offset += offset;
        }
        self.current_char = next;
        let offset = self.offset;
        AdvanceResult { char: next_char, offset }
    }
}
