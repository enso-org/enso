#![allow(missing_docs)]

use crate::prelude::*;

use crate::display::shape::text::font::FontRenderInfo;
use crate::display::shape::text::msdf::MsdfTexture;

use nalgebra::Point2;
use nalgebra::Translation2;
use nalgebra::Affine2;
use nalgebra::Matrix3;
use nalgebra::Scalar;
use std::ops::RangeInclusive;

// ============================
// === Base vertices layout ===
// ============================

pub const BASE_LAYOUT_SIZE : usize = 6;

lazy_static! {
    pub static ref GLYPH_SQUARE_VERTICES_BASE_LAYOUT : [Point2<f64>;BASE_LAYOUT_SIZE] =
        [ Point2::new(0.0, 0.0)
        , Point2::new(0.0, 1.0)
        , Point2::new(1.0, 0.0)
        , Point2::new(1.0, 0.0)
        , Point2::new(0.0, 1.0)
        , Point2::new(1.0, 1.0)
        ];
}

pub fn point_to_iterable<T:Scalar>(p:Point2<T>) -> SmallVec<[T;2]> {
    p.iter().cloned().collect()
}


// ===========
// === Pen ===
// ===========

/// A pen position
///
/// The pen is a font-specific term (see
/// [freetype documentation](https://www.freetype.org/freetype2/docs/glyphs/glyphs-3.html#section-1)
/// for details). The structure keeps pen position _before_ rendering the `current_char`.
#[derive(Clone,Copy,Debug)]
pub struct Pen {
    pub position     : Point2<f64>,
    pub current_char : Option<char>,
    pub next_advance : f64,
}

impl Pen {
    /// Create the pen structure, where the first char will be rendered at `position`.
    pub fn new(position:Point2<f64>) -> Pen {
        Pen {position,
            current_char : None,
            next_advance : 0.0
        }
    }

    pub fn new_with_char(position:Point2<f64>, ch:char, font:&mut FontRenderInfo) -> Self {
        Pen {position,
            current_char : Some(ch),
            next_advance : font.get_glyph_info(ch).advance
        }
    }

    /// Move pen to the next character
    ///
    /// The new position will be the base for `ch` rendering with applied kerning.
    pub fn next_char(&mut self, ch:char, font:&mut FontRenderInfo) -> &mut Self {
        if let Some(current_ch) = self.current_char {
            self.move_pen(current_ch,ch,font)
        }
        self.next_advance = font.get_glyph_info(ch).advance;
        self.current_char = Some(ch);
        self
    }

    fn move_pen(&mut self, current:char, next:char, font:&mut FontRenderInfo) {
        let kerning   = font.get_kerning(current, next);
        let transform = Translation2::new(self.next_advance + kerning, 0.0);
        self.position = transform * self.position;
    }

    pub fn is_in_x_range(&self, range:&RangeInclusive<f64>) -> bool {
        let x_min  = self.position.x;
        let x_max  = x_min + self.next_advance;
        range.contains(&x_min) || range.contains(&x_max) || (x_min..x_max).contains(range.start())
    }

    pub fn current_char_x_range(&self) -> RangeInclusive<f64> {
        self.position.x..=(self.position.x + self.next_advance)
    }
}

// =========================================
// === GlyphSquareVertexAttributeBuilder ===
// =========================================

/// Builder for specific attribute of glyph square's vertices
///
/// Builder is meant to be used for producing attribute data for squares of glyph placed in one
/// line of text. The attribute may be vertices position, texture coordinates, etc.
pub trait GlyphAttributeBuilder {
    const OUTPUT_SIZE : usize;
    type Output;

    /// Build attribute data for next glyph in line.
    fn build_for_next_glyph(&mut self, ch:char) -> Self::Output;

    /// Create empty attribute data
    ///
    /// The empty data are used for squares that are not actually rendered, but instead reserved
    /// for future use (due to optimisation).
    fn empty() -> Self::Output;
}


// ==================================
// === GlyphVertexPositionBuilder ===
// ==================================

/// Builder for glyph square vertex positions
///
/// `pen` field points to the position of last built glyph.
#[derive(Debug)]
pub struct GlyphVertexPositionBuilder<'a,'b> {
    pub font          : &'a mut FontRenderInfo,
    pub pen           : &'b mut Pen,
}

impl<'a,'b> GlyphVertexPositionBuilder<'a,'b> {
    /// New GlyphVertexPositionBuilder
    ///
    /// The newly created builder start to place glyph at location pointed by given pen.
    pub fn new(font:&'a mut FontRenderInfo, pen:&'b mut Pen) -> Self {
        GlyphVertexPositionBuilder {font,pen}
    }

    fn translation_by_pen_position(&self) -> Translation2<f64>{
        Translation2::new(self.pen.position.x, self.pen.position.y)
    }
}

impl<'a,'b> GlyphAttributeBuilder for GlyphVertexPositionBuilder<'a,'b> {
    const OUTPUT_SIZE : usize = BASE_LAYOUT_SIZE * 2;
    type Output = SmallVec<[f64;12]>; // Note[Output size]

    /// Compute vertices for the next glyph.
    ///
    /// The vertices position are the final vertices passed to webgl buffer. It takes the previous
    /// built glyph into consideration for proper spacing.
    fn build_for_next_glyph(&mut self, ch:char) -> Self::Output {
        self.pen.next_char(ch, self.font);
        let to_pen_position          = self.translation_by_pen_position();
        let glyph_info               = self.font.get_glyph_info(ch);
        let glyph_specific_transform = &glyph_info.from_base_layout;
        let base                     = GLYPH_SQUARE_VERTICES_BASE_LAYOUT.iter();
        let glyph_fixed              = base                 .map(|p| glyph_specific_transform * p);
        let moved_to_pen_position    = glyph_fixed          .map(|p| to_pen_position * p);
        moved_to_pen_position.map(point_to_iterable).flatten().collect()
    }

    fn empty() -> Self::Output {
        SmallVec::from_buf([0.0;12]) // Note[Output size]
    }
}

// ======================================
// === GlyphTextureCoordinatesBuilder ===
// ======================================

/// Builder for glyph MSDF texture coordinates
#[derive(Debug)]
pub struct GlyphTextureCoordsBuilder<'a> {
    pub font : &'a mut FontRenderInfo
}

impl<'a> GlyphTextureCoordsBuilder<'a> {

    /// Create new builder using given font.
    pub fn new(font:&'a mut FontRenderInfo) -> GlyphTextureCoordsBuilder<'a> {
        GlyphTextureCoordsBuilder {font}
    }

    /// Convert base layout to msdf space.
    ///
    /// The base layout contains vertices within (0.0, 0.0) - (1.0, 1.0) range. In msdf
    /// space we use distances expressed in msdf cells.
    pub fn base_layout_to_msdf_space() -> Affine2<f64> {
        let scale_x = MsdfTexture::WIDTH as f64;
        let scale_y = MsdfTexture::ONE_GLYPH_HEIGHT as f64;
        let matrix = Matrix3::new
            ( scale_x, 0.0    , 0.0
            , 0.0    , scale_y, 0.0
            , 0.0    , 0.0    , 1.0
            );
        Affine2::from_matrix_unchecked(matrix)
    }

    /// Transformation aligning borders to MSDF cell center
    ///
    /// Each cell in MSFD contains a distance measured from its center, therefore the borders of
    /// glyph's square should be matched with center of MSDF cells to read distance properly.
    ///
    /// The transformation's input should be a point in _single MSDF space_, where (0.0, 0.0) is
    /// the bottom-left corner of MSDF, and each cell have size of 1.0.
    pub fn align_borders_to_msdf_cell_center_transform() -> Affine2<f64> {
        let columns       = MsdfTexture::WIDTH            as f64;
        let rows          = MsdfTexture::ONE_GLYPH_HEIGHT as f64;

        let translation_x = 0.5;
        let translation_y = 0.5;
        let scale_x       = (columns - 1.0) / columns;
        let scale_y       = (rows - 1.0) / rows;
        let matrix = Matrix3::new
            ( scale_x, 0.0    , translation_x
            , 0.0    , scale_y, translation_y
            , 0.0    , 0.0    , 1.0
            );
        Affine2::from_matrix_unchecked(matrix)
    }

    /// Transformation MSDF texture fragment associated with given glyph
    ///
    /// The MSDF texture contains MSDFs for many glyph, so this translation moves points expressed
    /// in "single" msdf space to actual texture coordinates.
    pub fn glyph_texture_fragment_transform(&mut self, ch:char) -> Translation2<f64> {
        let glyph_info     = self.font.get_glyph_info(ch);
        let offset_y       = glyph_info.msdf_texture_glyph_id as f64 * ;
        Translation2::new(0.0, offset_y)
    }
}

impl<'a> GlyphAttributeBuilder for GlyphTextureCoordsBuilder<'a> {

    const OUTPUT_SIZE : usize = BASE_LAYOUT_SIZE * 2;

    type Output = SmallVec<[f64; 12]>; // Note[Output size]

    /// Compute texture coordinates for `ch`.
    fn build_for_next_glyph(&mut self, ch:char) -> Self::Output {
        let to_msdf            = Self::base_layout_to_msdf_space();
        let border_align       = Self::align_borders_to_msdf_cell_center_transform();
        let to_proper_fragment = self.glyph_texture_fragment_transform(ch);

        let base               = GLYPH_SQUARE_VERTICES_BASE_LAYOUT.iter();
        let aligned_to_border  = base             .map(|p| border_align * to_msdf * p);
        let transformed        = aligned_to_border.map(|p| to_proper_fragment * p);
        transformed.map(point_to_iterable).flatten().collect()
    }

    fn empty() -> Self::Output {
        SmallVec::from_buf([0.0;12]) // Note[Output size]
    }
}

/* Note [Output size]
 *
 * We can use `Self::OUTPUT_SIZE` instead of current hardcode 12 once the rustc bug will be fixed:
 * https://github.com/rust-lang/rust/issues/62708
 */


#[cfg(test)]
mod tests {
    use super::*;

    use crate::display::shape::text::font::GlyphRenderInfo;

    use basegl_core_msdf_sys::test_utils::TestAfterInit;
    use std::future::Future;
    use wasm_bindgen_test::wasm_bindgen_test;

    #[wasm_bindgen_test(async)]
    fn moving_pen() -> impl Future<Output=()> {
        TestAfterInit::schedule(|| {
            let mut font = FontRenderInfo::mock_font("Test font".to_string());
            mock_a_glyph_info(&mut font);
            mock_w_glyph_info(&mut font);
            font.mock_kerning_info('A', 'W', -0.16);
            font.mock_kerning_info('W', 'A', 0.0);

            let mut pen = Pen::new(Point2::new(0.0, 0.0));
            pen.next_char('A', &mut font);
            assert_eq!(Some('A'), pen.current_char);
            assert_eq!(0.56     , pen.next_advance);
            assert_eq!(0.0      , pen.position.x);
            assert_eq!(0.0      , pen.position.y);
            pen.next_char('W', &mut font);
            assert_eq!(Some('W'), pen.current_char);
            assert_eq!(0.7      , pen.next_advance);
            assert_eq!(0.4      , pen.position.x);
            assert_eq!(0.0      , pen.position.y);
            pen.next_char('A', &mut font);
            assert_eq!(Some('A'), pen.current_char);
            assert_eq!(0.56     , pen.next_advance);
            assert_eq!(1.1      , pen.position.x);
            assert_eq!(0.0      , pen.position.y);
        })
    }

    #[wasm_bindgen_test(async)]
    fn build_vertices_for_glyph_square() -> impl Future<Output=()> {
        TestAfterInit::schedule(|| {
            let mut font = FontRenderInfo::mock_font("Test font".to_string());
            mock_a_glyph_info(&mut font);
            mock_w_glyph_info(&mut font);
            font.mock_kerning_info('A', 'W', -0.16);

            let mut pen     = Pen::new(Point2::new(0.0,0.0));
            let mut builder = GlyphVertexPositionBuilder::new(&mut font,&mut pen);
            let a_vertices  = builder.build_for_next_glyph('A');
            let w_vertices  = builder.build_for_next_glyph('W');

            let expected_a_vertices = &
                [ 0.1 , 0.2
                , 0.1 , 1.0
                , 0.6 , 0.2
                , 0.6 , 0.2
                , 0.1 , 1.0
                , 0.6 , 1.0
                ];
            let expected_w_vertices = &
                [ 0.5 , 0.2
                , 0.5 , 1.1
                , 1.1 , 0.2
                , 1.1 , 0.2
                , 0.5 , 1.1
                , 1.1 , 1.1
                ];

            assert_eq!(expected_a_vertices, a_vertices.as_ref());
            assert_eq!(expected_w_vertices, w_vertices.as_ref());
            assert_eq!(Point2::new(0.4,0.0), pen.position);
            assert_eq!(Some('W')           , pen.current_char);
        })
    }

    #[wasm_bindgen_test(async)]
    fn build_texture_coords_for_glyph_square() -> impl Future<Output=()> {
        TestAfterInit::schedule(|| {
            let mut font = FontRenderInfo::mock_font("Test font".to_string());
            font.mock_char_info('A');
            font.mock_char_info('W');

            let mut builder      = GlyphTextureCoordsBuilder::new(&mut font);
            let a_texture_coords = builder.build_for_next_glyph('A');
            let w_texture_coords = builder.build_for_next_glyph('W');

            let expected_a_coords = &
                [  0.5 ,  0.5
                ,  0.5 , 31.5
                , 31.5 ,  0.5
                , 31.5 ,  0.5
                ,  0.5 , 31.5
                , 31.5 , 31.5
                ];
            let expected_w_coords = &
                [  0.5 ,  32.5
                ,  0.5 ,  63.5
                , 31.5 ,  32.5
                , 31.5 ,  32.5
                ,  0.5 ,  63.5
                , 31.5 ,  63.5
                ];

            assert_eq!(expected_a_coords, a_texture_coords.as_ref());
            assert_eq!(expected_w_coords, w_texture_coords.as_ref());
        })
    }
}
