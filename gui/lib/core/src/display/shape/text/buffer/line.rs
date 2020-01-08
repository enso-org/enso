#![allow(missing_docs)]

use crate::prelude::*;

use crate::display::shape::text::buffer::glyph_square::GlyphAttributeBuilder;
use crate::display::shape::text::buffer::glyph_square::GlyphVertexPositionBuilder;
use crate::display::shape::text::buffer::glyph_square::GlyphTextureCoordsBuilder;


// ============================
// === LineAttributeBuilder ===
// ============================

/// Buffer data for line of text builder
///
/// This builder makes a fixed-size buffers for a specific attribute (e.g. vertex position or
/// texture coordinates) for one line. If line is longer than `max_line_size`, it is
/// cut. When line is shorter, the buffer is padded with empty values (obtained from
/// `GlyphAttributeBuilder::empty()`).
pub struct LineAttributeBuilder<'a,GlyphBuilder:GlyphAttributeBuilder> {
    max_line_size    : usize,
    squares_produced : usize,
    glyph_builder    : GlyphBuilder,
    chars            : &'a[char],
}

pub type LineVerticesBuilder<'a,'b,'c> =
    LineAttributeBuilder<'a,GlyphVertexPositionBuilder<'b,'c>>;
pub type LineTextureCoordsBuilder<'a,'b> = LineAttributeBuilder<'a,GlyphTextureCoordsBuilder<'b>>;

impl<'a,GlyphBuilder: GlyphAttributeBuilder> LineAttributeBuilder<'a,GlyphBuilder> {
    /// Create new LineAttributeBuilder based on `glyph_builder`
    pub fn new(chars: &'a[char], glyph_builder: GlyphBuilder, max_line_size:usize)
        -> LineAttributeBuilder<'a, GlyphBuilder> {
        LineAttributeBuilder {max_line_size,glyph_builder,chars,
            squares_produced : 0,
        }
    }
}

impl<'a,GlyphBuilder: GlyphAttributeBuilder> Iterator for LineAttributeBuilder<'a,GlyphBuilder> {
    type Item = GlyphBuilder::Output;

    /// Get buffer data for next glyph
    ///
    /// If we have reached end of line before, the empty data is returned. Iterator stops when
    /// number of items  produced reach `max_line_size` value.
    fn next(&mut self) -> Option<Self::Item> {
        let values_remain = self.squares_produced < self.max_line_size;
        let next_char     = self.chars.get(self.squares_produced);
        values_remain.and_option_from(|| {
            self.squares_produced += 1;
            let next_char_attrs = next_char.map(|ch| self.glyph_builder.build_for_next_glyph(*ch));
            let returned_value  = next_char_attrs.unwrap_or_else(GlyphBuilder::empty);
            Some(returned_value)
        })
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    struct GlyphAttributeBuilderMock<'a> {
        iteration       : usize,
        processed_chars : &'a mut Vec<char>
    }

    type TestOutput = SmallVec<[usize;2]>;

    impl<'a> GlyphAttributeBuilder for GlyphAttributeBuilderMock<'a> {
        const OUTPUT_SIZE: usize = 2;
        type Output = TestOutput;

        fn build_for_next_glyph(&mut self, ch: char) -> Self::Output {
            self.iteration += 1;
            self.processed_chars.push(ch);
            SmallVec::from_buf([self.iteration, self.iteration+1])
        }

        fn empty() -> Self::Output {
            SmallVec::from_buf([0;2])
        }
    }

    #[test]
    fn line_attribute_builder_with_short_line() {
        let line                = "SKR".chars().collect_vec();
        let mut processed_chars = Vec::<char>::new();
        let max_line_size       = 6;
        let glyph_builder       = GlyphAttributeBuilderMock {
            iteration           : 0,
            processed_chars     : &mut processed_chars
        };
        let line_builder        = LineAttributeBuilder::new(line.as_slice(),glyph_builder,max_line_size);
        let data                = line_builder.collect::<Vec<TestOutput>>();

        let expected_data = vec!
            [ SmallVec::from_buf([1, 2])
            , SmallVec::from_buf([2, 3])
            , SmallVec::from_buf([3, 4])
            , SmallVec::from_buf([0, 0])
            , SmallVec::from_buf([0, 0])
            , SmallVec::from_buf([0, 0])
            ];
        assert_eq!(expected_data, data);
        let expected_chars = vec!['S', 'K', 'R'];
        assert_eq!(expected_chars, processed_chars);
    }

    #[test]
    fn line_attribute_builder_with_long_line() {
        let line                = "XIXAXA XOXAXA XUXAXA".chars().collect_vec();
        let mut processed_chars = Vec::<char>::new();
        let max_line_size       = 6;
        let glyph_builder       = GlyphAttributeBuilderMock {
            iteration           : 0,
            processed_chars     : &mut processed_chars
        };
        let line_builder        = LineAttributeBuilder::new(line.as_slice(),glyph_builder,max_line_size);
        let data                = line_builder.collect::<Vec<TestOutput>>();

        let expected_data = vec!
            [ SmallVec::from_buf([1, 2])
            , SmallVec::from_buf([2, 3])
            , SmallVec::from_buf([3, 4])
            , SmallVec::from_buf([4, 5])
            , SmallVec::from_buf([5, 6])
            , SmallVec::from_buf([6, 7])
            ];
        assert_eq!(expected_data, data);
        let expected_chars = vec!['X', 'I', 'X', 'A', 'X', 'A'];
        assert_eq!(expected_chars, processed_chars);
    }
}