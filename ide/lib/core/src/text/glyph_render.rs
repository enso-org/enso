use crate::prelude::*;

use crate::text::font::FontRenderInfo;
use crate::text::msdf::MsdfTexture;

use nalgebra::{Point2,Transform2,Translation2,Affine2,Matrix3,Scalar};

// ============================
// === Base vertices layout ===
// ============================

pub const GLYPH_SQUARE_VERTICES_BASE_LAYOUT: &[(f64, f64)] = &
    [ (0.0, 0.0)
    , (0.0, 1.0)
    , (1.0, 0.0)
    , (1.0, 0.0)
    , (0.0, 1.0)
    , (1.0, 1.0)
    ];

fn point_to_iterable<T:Scalar>(p:Point2<T>) -> SmallVec<[T;2]> {
    p.iter().cloned().collect()
}

// ==================================
// === GylphSquareVerticesBuilder ===
// ==================================

const GLYPH_SQUARE_VERTICES_SIZE : usize = GLYPH_SQUARE_VERTICES_BASE_LAYOUT.len() * 2;

pub type GlyphSquareVertices = SmallVec<[f64;GLYPH_SQUARE_VERTICES_SIZE]>;

/// Builder for glyph vertices
///
/// Once created, each `build_for_next_glyph` gives vertices of the next glyph's square
pub struct GylphSquareVerticesBuilder<'a> {
    pub previous_char : Option<char>,
    pub font          : &'a mut FontRenderInfo,
    pub pen_position  : Point2<f64>,
    pub to_window     : Transform2<f64>,
}

impl<'a> GylphSquareVerticesBuilder<'a> {

    /// New GylphSquareVerticesBuilder
    ///
    /// The newly created builder start to place glyphs with pen located at `to_window` * (0.0, 0.0)
    pub fn new(font:&'a mut FontRenderInfo, to_window:Transform2<f64>)
    -> GylphSquareVerticesBuilder<'a> {
        GylphSquareVerticesBuilder {
            previous_char : None,
            font,
            pen_position  : Point2::new(0.0, 0.0),
            to_window
        }
    }

    /// Compute vertices for one glyph and move pen for next position
    pub fn build_for_next_glyph(&mut self, ch:char) -> GlyphSquareVertices {
        let apply_kerning            = self.translation_by_kerning_value(ch);
        let to_pen_position          = self.translation_by_pen_position();
        let glyph_info               = self.font.get_glyph_info(ch);
        let glyph_specific_transform = &glyph_info.from_base_layout;
        let to_window                = &self.to_window;
        let advance_pen              = Translation2::new(glyph_info.advance, 0.0);
        let pen_transformation       = apply_kerning * advance_pen;

        self.pen_position            = pen_transformation * self.pen_position;
        self.previous_char           = Some(ch);
        let plain_base               = GLYPH_SQUARE_VERTICES_BASE_LAYOUT.iter();
        let base                     = plain_base           .map(|(x, y)| Point2::new(*x, *y));
        let glyph_fixed              = base                 .map(|p| glyph_specific_transform * p);
        let moved_to_pen_position    = glyph_fixed          .map(|p| to_pen_position * p);
        let kerning_applied          = moved_to_pen_position.map(|p| apply_kerning * p);
        let mapped_to_window         = kerning_applied      .map(|p| to_window * p);
        mapped_to_window.map(point_to_iterable).flatten().collect()
    }

    fn translation_by_kerning_value(&mut self, ch:char) -> Translation2<f64> {
        let prev_char = self.previous_char;
        let opt_value = prev_char.map(|lc| self.font.get_kerning(lc, ch));
        let value     = opt_value.unwrap_or(0.0);
        Translation2::new(value, 0.0)
    }

    fn translation_by_pen_position(&self) -> Translation2<f64>{
        Translation2::new(self.pen_position.x, self.pen_position.y)
    }
}

// =================================
// === TextureCoordinatesBuilder ===
// =================================

const GLYPH_SQUARE_TEXTURE_COORDINATES_SIZE : usize = GLYPH_SQUARE_VERTICES_BASE_LAYOUT.len() * 2;

pub type GlyphTextureCoordinates = SmallVec<[f64;GLYPH_SQUARE_TEXTURE_COORDINATES_SIZE]>;

/// Builder for glyph MSDF texture coordinates
pub struct GlyphSquareTextureCoordinatesBuilder<'a> {
    pub font : &'a mut FontRenderInfo
}

impl<'a> GlyphSquareTextureCoordinatesBuilder<'a> {
    /// Create new builder using given font
    pub fn new(font:&'a mut FontRenderInfo) -> GlyphSquareTextureCoordinatesBuilder<'a> {
        GlyphSquareTextureCoordinatesBuilder {font}
    }

    /// Compute texture coordinates for `ch`
    pub fn build_for_next_glyph(&mut self, ch:char) -> GlyphTextureCoordinates {
        let border_align        = self.align_borders_to_msdf_cell_center_transform();
        let to_proper_fragment  = self.glyph_texture_fragment_transform(ch);

        let plain_base          = GLYPH_SQUARE_VERTICES_BASE_LAYOUT.iter();
        let base                = plain_base       .map(|(x,y)| Point2::new(*x, *y));
        let aligned_to_border   = base             .map(|p| border_align * p);
        let transformed         = aligned_to_border.map(|p| to_proper_fragment * p);
        transformed.map(point_to_iterable).flatten().collect()
    }

    /// Transformation aligning borders to MSDF cell center
    ///
    /// Each cell in MSFD contains a distance measured from its center, therefore the borders of
    /// glyph's square should be matched with center of MSDF cells to read distance properly
    ///
    /// The transformation's input should be a point in _single MSDF space_, where (0.0, 0.0) is
    /// the bottom-left corner of MSDF, and (1.0, 1.0) is the top-right corner.
    pub fn align_borders_to_msdf_cell_center_transform(&self) -> Affine2<f64> {
        let columns       = MsdfTexture::WIDTH            as f64;
        let rows          = MsdfTexture::ONE_GLYPH_HEIGHT as f64;
        let column_size   = 1.0 / columns;
        let row_size      = 1.0 / rows;

        let translation_x = column_size / 2.0;
        let translation_y = row_size    / 2.0;
        let scale_x       = 1.0 - column_size;
        let scale_y       = 1.0 - row_size;
        let matrix = Matrix3::new
        ( scale_x, 0.0    , translation_x
        , 0.0    , scale_y, translation_y
        , 0.0    , 0.0    , 1.0
        );
        Affine2::from_matrix_unchecked(matrix)
    }

    /// Transformation MSDF texture fragment associated with given glyph
    ///
    /// The MSDF texture contains MSDFs for many glyphs. The returned transform maps the point in
    /// a _single MSDF space_ to actual texture space. In other words, a (0.0, 0.0) point will be
    /// mapped to bottom-left corner of `ch` texture fragment, and a (1.0, 1.0) will be mapped to
    /// upper-right corner.
    pub fn glyph_texture_fragment_transform(&mut self, ch:char) -> Affine2<f64> {
        let one_glyph_rows = MsdfTexture::ONE_GLYPH_HEIGHT as f64;
        let all_rows       = self.font.msdf_texture.rows() as f64;

        let fraction       = one_glyph_rows / all_rows;
        let glyph_info     = self.font.get_glyph_info(ch);
        let offset         = glyph_info.msdf_texture_rows.start as f64 / all_rows;
        let matrix         = nalgebra::Matrix3::new
        ( 1.0, 0.0     , 0.0
        , 0.0, fraction, offset
        , 0.0, 0.0     , 1.0
        );
        Affine2::from_matrix_unchecked(matrix)
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    use crate::text::font::GlyphRenderInfo;

    use basegl_core_msdf_sys::test_utils::TestAfterInit;
    use std::future::Future;
    use wasm_bindgen_test::wasm_bindgen_test;

    #[wasm_bindgen_test(async)]
    fn build_vertices_for_glyph_square() -> impl Future<Output=()> {
        TestAfterInit::schedule(|| {
            let mut font = FontRenderInfo::mock_font("Test font".to_string());
            mock_a_glyph_info(&mut font);
            mock_w_glyph_info(&mut font);
            font.mock_kerning_info('A', 'W', -0.16);
            let to_window_transformation = {
                let to_window_mtx = nalgebra::Matrix3::new
                    ( 0.1, 0.0, -1.0
                    , 0.0, 0.1, -0.5
                    , 0.0, 0.0,  1.0
                    );
                nalgebra::Transform2::from_matrix_unchecked(to_window_mtx)
            };

            let mut builder = GylphSquareVerticesBuilder::new(&mut font,to_window_transformation);
            let a_vertices  = builder.build_for_next_glyph('A');
            assert_eq!(Some('A'), builder.previous_char);
            assert_eq!(0.56     , builder.pen_position.x);
            assert_eq!(0.0      , builder.pen_position.y);
            let w_vertices  = builder.build_for_next_glyph('W');
            assert_eq!(Some('W'), builder.previous_char);
            assert_eq!(1.1      , builder.pen_position.x);
            assert_eq!(0.0      , builder.pen_position.y);

            let expected_a_vertices = &
                [ -0.99 , -0.48
                , -0.99 , -0.4
                , -0.94 , -0.48
                , -0.94 , -0.48
                , -0.99 , -0.4
                , -0.94 , -0.4
                ];
            let expected_w_vertices = &
                [ -0.95 , -0.48
                , -0.95 , -0.39
                , -0.89 , -0.48
                , -0.89 , -0.48
                , -0.95 , -0.39
                , -0.89 , -0.39
                ];

            assert_eq!(expected_a_vertices, a_vertices.as_ref());
            assert_eq!(expected_w_vertices, w_vertices.as_ref());
        })
    }

    #[wasm_bindgen_test(async)]
    fn build_texture_coordinates_for_glyph_square() -> impl Future<Output=()> {
        TestAfterInit::schedule(|| {
            let mut font = FontRenderInfo::mock_font("Test font".to_string());
            font.mock_char_info('A');
            font.mock_char_info('W');

            let mut builder           = GlyphSquareTextureCoordinatesBuilder::new(&mut font);
            let a_texture_coordinates = builder.build_for_next_glyph('A');
            let w_texture_coordinates = builder.build_for_next_glyph('W');

            let expected_a_coordinates = &
                [  1./64. ,  1./128.
                ,  1./64. , 63./128.
                , 63./64. ,  1./128.
                , 63./64. ,  1./128.
                ,  1./64. , 63./128.
                , 63./64. , 63./128.
                ];
            let expected_w_coordinates = &
                [  1./64. ,  65./128.
                ,  1./64. , 127./128.
                , 63./64. ,  65./128.
                , 63./64. ,  65./128.
                ,  1./64. , 127./128.
                , 63./64. , 127./128.
                ];

            assert_eq!(expected_a_coordinates, a_texture_coordinates.as_ref());
            assert_eq!(expected_w_coordinates, w_texture_coordinates.as_ref());
        })
    }

    fn mock_a_glyph_info(font:&mut FontRenderInfo) -> &mut GlyphRenderInfo {
        let a_info = font.mock_char_info('A');
        a_info.advance = 0.56;
        let trans_mtx = nalgebra::Matrix3::new
            ( 0.5, 0.0, 0.1
            , 0.0, 0.8, 0.2
            , 0.0, 0.0, 1.0
            );
        a_info.from_base_layout = nalgebra::Projective2::from_matrix_unchecked(trans_mtx);
        a_info
    }

    fn mock_w_glyph_info(font:&mut FontRenderInfo) -> &mut GlyphRenderInfo {
        let a_info = font.mock_char_info('W');
        a_info.advance = 0.7;
        let trans_mtx = nalgebra::Matrix3::new
            ( 0.6, 0.0, 0.1
            , 0.0, 0.9, 0.2
            , 0.0, 0.0, 1.0
            );
        a_info.from_base_layout = nalgebra::Projective2::from_matrix_unchecked(trans_mtx);
        a_info
    }
}
