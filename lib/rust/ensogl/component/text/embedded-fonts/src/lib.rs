//! Implementation of embedded fonts loading and a definition of the [`DefaultFamily`] used in the
//! app.

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
// === Non-Standard Linter Configuration ===
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]

use enso_prelude::*;

use ensogl_text_embedded_fonts_names as embedded_fonts_names;



// ===========================
// === Exports and Aliases ===
// ===========================

pub use embedded_fonts_names::FontFamily as Family;

/// The default font family used in the app.
pub type DefaultFamily = embedded_fonts_names::DejaVuSans;



// =====================
// === EmbeddedFonts ===
// =====================

/// A base of built-in fonts in application
///
/// The structure keeps only a binary data in ttf format. The data should be then interpreted by
/// user (e.g. by using msdf-sys crate).
///
/// For list of embedded fonts, see FONTS_TO_EXTRACT constant in `build.rs`.
#[allow(missing_docs)]
pub struct EmbeddedFonts {
    pub font_data_by_name: HashMap<&'static str, &'static [u8]>,
}

impl EmbeddedFonts {
    /// Creates an embedded fonts base filled with data.
    ///
    /// For list of embedded fonts, see `FONTS_TO_EXTRACT` constant in `build.rs`
    pub fn create_and_fill() -> EmbeddedFonts {
        let mut font_data_by_name = HashMap::<&'static str, &'static [u8]>::new();
        include!(concat!(env!("OUT_DIR"), "/fill_map.rs"));
        EmbeddedFonts { font_data_by_name }
    }
}

impl Debug for EmbeddedFonts {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("<Embedded fonts>")
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod test {
    use crate::*;

    use ensogl_text_embedded_fonts_names::DejaVuSans;
    use ensogl_text_embedded_fonts_names::FontFamily;

    #[test]
    fn loading_embedded_fonts() {
        let fonts = EmbeddedFonts::create_and_fill();
        let example_font = fonts.font_data_by_name.get(DejaVuSans::regular()).unwrap();

        assert_eq!(0x00, example_font[0]);
        assert_eq!(0x01, example_font[1]);
        assert_eq!(0x1d, example_font[example_font.len() - 1]);
    }
}
