use crate::prelude::*;

use crate::text::buffer::glyph_square::
{GlyphAttributeBuilder,GlyphVertexPositionBuilder,GlyphTextureCoordsBuilder};


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
    chars_iterator   : Option<std::str::Chars<'a>>,
}

pub type LineVerticesBuilder<'a,'b>      = LineAttributeBuilder<'a,GlyphVertexPositionBuilder<'b>>;
pub type LineTextureCoordsBuilder<'a,'b> = LineAttributeBuilder<'a,GlyphTextureCoordsBuilder<'b>>;

impl<'a,GlyphBuilder: GlyphAttributeBuilder> LineAttributeBuilder<'a,GlyphBuilder> {
    /// Create new LineAttributeBuilder based on `glyph_builder`
    pub fn new(line: &'a str, glyph_builder: GlyphBuilder, max_line_size:usize)
        -> LineAttributeBuilder<'a, GlyphBuilder> {
        LineAttributeBuilder {
            max_line_size,
            glyph_builder,
            squares_produced: 0,
            chars_iterator: Some(line.chars())
        }
    }

    fn next_item(&mut self) -> GlyphBuilder::Output {
        let chars_iterator_output = self.chars_iterator.as_mut().map(|iter| iter.next());
        match chars_iterator_output {
            Some(Some(ch)) => self.chars_iterator_returned_value(ch),
            Some(None)     => self.chars_iterator_returned_none(),
            None           => self.chars_iterator_exthaused_before()
        }
    }

    fn chars_iterator_returned_value(&mut self, value:char) -> GlyphBuilder::Output {
        self.glyph_builder.build_for_next_glyph(value)
    }

    fn chars_iterator_returned_none(&mut self) -> GlyphBuilder::Output {
        self.chars_iterator = None;
        GlyphBuilder::empty()
    }

    fn chars_iterator_exthaused_before(&mut self) -> GlyphBuilder::Output {
        GlyphBuilder::empty()
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
        values_remain.and_option_from(|| {
            self.squares_produced += 1;
            Some(self.next_item())
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
        let line                = "SKR";
        let mut processed_chars = Vec::<char>::new();
        let max_line_size       = 6;
        let glyph_builder       = GlyphAttributeBuilderMock {
            iteration           : 0,
            processed_chars     : &mut processed_chars
        };
        let line_builder        = LineAttributeBuilder::new(line,glyph_builder,max_line_size);
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
        let line                = "XIXAXA XOXAXA XUXAXA";
        let mut processed_chars = Vec::<char>::new();
        let max_line_size       = 6;
        let glyph_builder       = GlyphAttributeBuilderMock {
            iteration           : 0,
            processed_chars     : &mut processed_chars
        };
        let line_builder        = LineAttributeBuilder::new(line,glyph_builder,max_line_size);
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