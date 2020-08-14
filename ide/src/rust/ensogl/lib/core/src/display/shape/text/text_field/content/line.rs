//! Structures and methods related to single line of TextField content.
use crate::prelude::*;

use crate::display::shape::text::glyph::font;
use crate::display::shape::text::glyph::pen::PenIterator;

use nalgebra::Vector2;
use std::ops::Range;


// ============
// === Line ===
// ============

/// A line of text in TextComponent.
///
/// The line is kept as vector of chars instead of `str`, because we often want to have a quick
/// access to a concrete fragment of the line. Additionally, this structure keeps cache of x
/// position of the char in a _text space_ (where value of 1.0 is equal to lines height). The cache
/// is initially empty and is load on demand - so the `char_x_position` vector will be often shorter
/// than number of characters in line.
#[derive(Clone,Debug)]
pub struct Line {
    chars            : Vec<char>,
    char_x_positions : Vec<f32>,
}

impl Line {
    /// Create line from given string.
    pub fn new<S:Str>(string:S) -> Self {
        Self::new_raw(string.as_ref().chars().collect())
    }

    /// Create line from given characters vector.
    pub fn new_raw(chars:Vec<char>) -> Self {
        Line {chars,
            char_x_positions : Vec::new(),
        }
    }

    /// Create an empty line.
    pub fn empty() -> Self {
        Line{
            chars            : Vec::new(),
            char_x_positions : Vec::new(),
        }
    }

    /// A immutable reference to characters vector.
    pub fn chars(&self) -> &[char] {
        &self.chars.as_ref()
    }

    /// Get lines length.
    pub fn len(&self) -> usize {
        self.chars.len()
    }

    /// Check if line is empty.
    pub fn is_empty(&self) -> bool {
        self.chars.is_empty()
    }

    /// Get the mutable reference to characters. Because we're allowing for modifications here,
    /// the `chars_x_position` cache is cleared.
    pub fn modify(&mut self) -> &mut Vec<char> {
        self.char_x_positions.clear();
        &mut self.chars
    }
}


// === ToString ===

impl ToString for Line {
    fn to_string(&self) -> String {
        String::from_iter(self.chars.iter())
    }
}



// ======================
// === Line Full Info ===
// ======================

/// A structure wrapping line reference with information about line number and font. These
/// information allows to get more information from line about chars position in rendered text.
#[derive(Debug,Shrinkwrap)]
#[shrinkwrap(mutable)]
#[allow(missing_docs)]
pub struct LineFullInfo<'a> {
    #[shrinkwrap(main_field)]
    pub line    : &'a mut Line,
    pub line_id : usize,
    pub font    : font::Handle,
    pub height  : f32,
}

impl<'a> LineFullInfo<'a> {
    /// Get the y position of the top of the line.
    pub fn y_position(&self) -> f32 {
        self.height * self.line_id as f32
    }

    /// Get the point where a _baseline_ of current line begins (The _baseline_ is a font specific
    /// term, for details see [freetype documentation]
    /// (https://www.freetype.org/freetype2/docs/glyphs/glyphs-3.html#section-1)).
    pub fn baseline_start(&self) -> Vector2<f32> {
        Vector2::new(0.0, -self.y_position() - self.height * 0.85)
    }

    /// Get x position of character with given index. The position is in _text space_.
    pub fn get_char_x_position(&mut self, index:usize) -> f32 {
        self.fill_chars_x_position_up_to(index);
        self.char_x_positions[index]
    }

    /// Get range of x coordinates containing the given character.
    pub fn get_char_x_range(&mut self, index:usize) -> Range<f32> {
        let start   = self.get_char_x_position(index);
        let advance = self.font.get_glyph_info(self.chars[index]).advance * self.height;
        start..(start + advance)
    }

    /// Find the character occupying the given `x_position` in a _text space_. If there are two
    /// characters under this x coordinate (e.g. due to a kerning) the char on the left will be
    /// returned.
    pub fn find_char_at_x_position(&mut self, x_position:f32)
    -> Option<usize> {
        if self.chars.is_empty() {
            None
        } else {
            let comparator   = |f:&f32| f.partial_cmp(&x_position).unwrap();
            self.fill_chars_x_position_up_to_value(x_position);
            let last_index   = self.len() - 1;
            let found        = self.char_x_positions.binary_search_by(comparator);
            let mut in_range = || self.get_char_x_range(last_index).end >= x_position;
            match found {
                Ok(index)                        => Some(index),
                Err(0)                           => None,
                Err(index) if index > last_index => in_range().and_option_from(|| Some(last_index)),
                Err(index)                       => Some(index-1)
            }
        }
    }

    /// Fill the `chars_x_position` cache so it will contain information about character with given
    /// index.
    pub fn fill_chars_x_position_up_to(&mut self, index:usize) {
        let baseline_start = self.baseline_start();
        let new_len        = index + 1;
        let from_index     = self.char_x_positions.len().saturating_sub(1);
        let to_fill        = new_len.saturating_sub(self.char_x_positions.len());
        let x_position     = self.char_x_positions.last().cloned().unwrap_or(baseline_start.x);
        let line           = &mut self.line;
        let chars          = line.chars[from_index..].iter().cloned();
        let to_skip        = if line.char_x_positions.is_empty() {0} else {1};
        let pen            = PenIterator::new(self.height,chars,self.font.clone_ref());

        for (_,x_offset) in pen.skip(to_skip).take(to_fill) {
            line.char_x_positions.push(x_position + x_offset);
        }
    }

    /// Fill the `chars_x_position` cache so it will contain information about character being
    /// under given `x_position`.
    pub fn fill_chars_x_position_up_to_value(&mut self, x_position:f32) {
        let last_cached    = self.char_x_positions.last();
        let already_filled = last_cached.map_or(false, |cached| *cached >= x_position);
        if !already_filled {
            for index in self.char_x_positions.len()..self.chars.len() {
                self.fill_chars_x_position_up_to(index);
                let current = self.char_x_positions[index];
                if current >= x_position {
                    break;
                }
            }
        }
    }
}


#[cfg(test)]
mod test {
    use super::*;

    use crate::display::shape::text::glyph::font;

    use wasm_bindgen_test::wasm_bindgen_test;

    #[wasm_bindgen_test(async)]
    async fn getting_chars_x_position() {
        ensogl_text_msdf_sys::initialized().await;
        let mut line     = Line::new("ABA");
        let mut line_ref = LineFullInfo {
            line    : &mut line,
            font    : prepare_font_with_ab(),
            line_id : 0,
            height  : 1.0,
        };

        assert_eq!(0, line_ref.char_x_positions.len());
        let first_pos = line_ref.get_char_x_position(0);
        assert_eq!(1, line_ref.char_x_positions.len());
        let third_pos = line_ref.get_char_x_position(2);
        assert_eq!(3, line_ref.char_x_positions.len());

        assert_eq!(0.0, first_pos);
        assert_eq!(2.5, third_pos);
    }

    #[wasm_bindgen_test(async)]
    async fn finding_char_by_x_position() {
        ensogl_text_msdf_sys::initialized().await;
        let mut line     = Line::new("ABBA");
        let mut line_ref = LineFullInfo {
            line    : &mut line,
            font    : prepare_font_with_ab(),
            line_id : 0,
            height  : 1.0,
        };

        let before_first       = line_ref.find_char_at_x_position(-0.1);
        assert_eq!(1, line_ref.char_x_positions.len());
        let first              = line_ref.find_char_at_x_position(0.5);
        assert_eq!(2, line_ref.char_x_positions.len());
        let first_again        = line_ref.find_char_at_x_position(0.5);
        assert_eq!(2, line_ref.char_x_positions.len());
        let third              = line_ref.find_char_at_x_position(3.0);
        assert_eq!(4, line_ref.char_x_positions.len());
        let last               = line_ref.find_char_at_x_position(4.5);
        assert_eq!(4, line_ref.char_x_positions.len());
        let after_last         = line_ref.find_char_at_x_position(5.5);
        let third_again        = line_ref.find_char_at_x_position(3.0);
        let before_first_again = line_ref.find_char_at_x_position(-0.5);

        assert_eq!(None, before_first);
        assert_eq!(Some(0), first);
        assert_eq!(Some(0), first_again);
        assert_eq!(Some(2), third);
        assert_eq!(Some(3), last);
        assert_eq!(None, after_last);
        assert_eq!(Some(2), third_again);
        assert_eq!(None, before_first_again);
    }

    #[wasm_bindgen_test(async)]
    async fn finding_char_by_x_position_in_empty_line() {
        ensogl_text_msdf_sys::initialized().await;
        let mut line     = Line::new("");
        let mut line_ref = LineFullInfo {
            line    : &mut line,
            font    : prepare_font_with_ab(),
            line_id : 0,
            height  : 1.0,
        };
        let below_0  = line_ref.find_char_at_x_position(-0.1);
        let above_0  = line_ref.find_char_at_x_position( 0.1);
        assert_eq!(None,below_0);
        assert_eq!(None,above_0);
    }

    #[wasm_bindgen_test(async)]
    async fn modifying_line() {
        ensogl_text_msdf_sys::initialized().await;
        let mut line     = Line::new("AB");
        let mut line_ref = LineFullInfo {
            line    : &mut line,
            font    : prepare_font_with_ab(),
            line_id : 0,
            height  : 1.0,
        };
        let before_edit = line_ref.get_char_x_position(1);
        assert_eq!(2, line_ref.char_x_positions.len());
        line_ref.modify().insert(0, 'B');
        assert_eq!(0, line_ref.char_x_positions.len());
        let after_edit = line_ref.get_char_x_position(1);

        assert_eq!(1.0, before_edit);
        assert_eq!(1.5, after_edit);
    }

    fn prepare_font_with_ab() -> font::Handle {
        let font   = font::RenderInfo::mock_font("Test font".to_string());
        let scale  = Vector2::new(1.0, 1.0);
        let offset = Vector2::new(0.0, 0.0);
        font.mock_char_info('A',scale,offset,1.0);
        font.mock_char_info('B',scale,offset,1.5);
        font.mock_kerning_info('A', 'B', 0.0);
        font.mock_kerning_info('B', 'A', 0.0);
        font.mock_kerning_info('A', 'A', 0.0);
        font.mock_kerning_info('B', 'B', 0.0);
        font::Handle::new(font)
    }
}