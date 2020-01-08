#![allow(missing_docs)]

use basegl_core_msdf_sys as msdf_sys;
use msdf_sys::MultichannelSignedDistanceField;
use nalgebra::clamp;

// ====================
// === MSDF Texture ===
// ====================

/// Texture with msdf for all loaded glyphs of font
///
/// This structure keeps texture data in 8-bit-per-channel RGB format, which
/// is ready to be passed to webgl texImage2D. The texture contains MSDFs for
/// all loaded glyphs, organized in vertical column.
#[derive(Debug)]
pub struct MsdfTexture {
    pub data : Vec<u8>
}

impl MsdfTexture {
    pub const CHANNELS_COUNT   : usize = MultichannelSignedDistanceField::CHANNELS_COUNT;
    pub const WIDTH            : usize = 32;
    pub const ROW_SIZE         : usize = Self::CHANNELS_COUNT * Self::WIDTH;
    pub const ONE_GLYPH_HEIGHT : usize = 32;
    pub const ONE_GLYPH_SIZE   : usize = Self::ROW_SIZE * Self::ONE_GLYPH_HEIGHT;

    /// Number of rows in texture
    pub fn rows(&self) -> usize {
        self.data.len() / Self::ROW_SIZE
    }

    fn convert_cell_from_f32(value : f32) -> u8 {
        const UNSIGNED_BYTE_MIN : f32 = 0.0;
        const UNSIGNED_BYTE_MAX : f32 = 255.0;

        let scaled_to_byte            = value * UNSIGNED_BYTE_MAX;
        let clamped_to_byte           = clamp(scaled_to_byte,UNSIGNED_BYTE_MIN,UNSIGNED_BYTE_MAX);
        clamped_to_byte as u8
    }
}

impl Extend<f32> for MsdfTexture {
    /// Extends texture with new MSDF data in f32 format
    fn extend<T:IntoIterator<Item=f32>>(&mut self, iter:T) {
        let f32_iterator       = iter.into_iter();
        let converted_iterator = f32_iterator.map(Self::convert_cell_from_f32);
        self.data.extend(converted_iterator);
    }
}

// ==================================
// === msdf-sys values converting ===
// ==================================

/// Converts x dimension distance obtained from msdf-sys to vertex-space values
///
/// The values obtained from `msdf-sys` are expressed in MSDF cells. This
/// function convert them to normalized coordinates, where
/// (0.0, 0.0) is initial pen position for an character, and `y` = 1.0 is
/// _ascender_.
pub fn x_distance_from_msdf_value(msdf_value:f64) -> f64 {
    msdf_value / MsdfTexture::WIDTH as f64
}

/// Converts y dimension distance obtained from msdf-sys to vertex-space values
///
/// The values obtained from `msdf-sys` are expressed in MSDF cells. This
/// function convert them to normalized coordinates, where
/// (0.0, 0.0) is initial pen position for an character, and `y` = 1.0 is
/// _ascender_.
pub fn y_distance_from_msdf_value(msdf_value:f64) -> f64 {
    msdf_value / MsdfTexture::ONE_GLYPH_HEIGHT as f64
}

/// Converts transformation obtained from msdf-sys to vertex-space values
///
/// This function get the transformation obtained from `msdf_sys` which is
/// expressed in MSDF units, and convert it to  normalized coordinates, where
/// (0.0, 0.0) is initial pen position for an character, and `y` = 1.0 is
/// _ascender_.
pub fn convert_msdf_transformation(msdf:&MultichannelSignedDistanceField)
-> nalgebra::Projective2<f64> {
    let translate_converted_x = x_distance_from_msdf_value(msdf.translation.x);
    let translate_converted_y = y_distance_from_msdf_value(msdf.translation.y);
    let translate_scaled_x    = translate_converted_x * msdf.scale.x;
    let translate_scaled_y    = translate_converted_y * msdf.scale.y;
    let msdf_transformation_matrix = nalgebra::Matrix3::new(
        msdf.scale.x, 0.0,          translate_scaled_x,
        0.0,          msdf.scale.y, translate_scaled_y,
        0.0,          0.0,          1.0
    );
    nalgebra::Projective2::from_matrix_unchecked(msdf_transformation_matrix)
}

#[cfg(test)]
mod test {
    use super::*;

    use basegl_core_msdf_sys::test_utils::TestAfterInit;
    use nalgebra::Vector2;
    use std::future::Future;
    use wasm_bindgen_test::wasm_bindgen_test;

    #[test]
    fn extending_msdf_texture() {
        let mut texture = MsdfTexture{data : Vec::new()};
        let msdf_values: &[f32] = &[-0.5, 0.0, 0.25, 0.5, 0.75, 1.0, 1.25];
        texture.extend(msdf_values[..4].iter().cloned());
        texture.extend(msdf_values[4..].iter().cloned());

        assert_eq!([0, 0, 63, 127, 191, 255, 255], texture.data.as_slice());
    }

    #[test]
    fn x_dimension_converting() {
        assert_eq!(1.0/8.0, x_distance_from_msdf_value(4.0));
        assert_eq!(1.0/2.0, x_distance_from_msdf_value(16.0));
    }

    #[test]
    fn y_dimension_converting() {
        assert_eq!(1.0/8.0, y_distance_from_msdf_value(4.0));
        assert_eq!(1.0/2.0, y_distance_from_msdf_value(16.0));
    }

    #[wasm_bindgen_test(async)]
    fn msdf_transformation_converting() -> impl Future<Output=()> {
        TestAfterInit::schedule(|| {
            let mut msdf = MultichannelSignedDistanceField::mock_results();
            msdf.scale       = Vector2::new(2.0,  3.0);
            msdf.translation = Vector2::new(16.0, 4.0);

            let converted = convert_msdf_transformation(&msdf);

            let expected_mtx = nalgebra::Matrix3::new(
                2.0, 0.0, 1.0,
                0.0, 3.0, 3.0/8.0,
                0.0, 0.0, 1.0
            );

            assert_eq!(expected_mtx, *converted.matrix());
        })
    }
}
