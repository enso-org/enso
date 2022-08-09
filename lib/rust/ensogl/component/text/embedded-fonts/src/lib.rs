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

pub use embedded_fonts_names::*;

// ==============
// === Export ===
// ==============

// pub use embedded_fonts_names::FontFamily as Family;



include!(concat!(env!("OUT_DIR"), "/embedded_fonts_data.rs"));

// /// The default font family used in the app.
// pub type DefaultFamily = embedded_fonts_names::DejaVuSans;



// =====================
// === EmbeddedFontsData ===
// =====================

/// A base of built-in fonts in application.
///
/// The structure keeps a map from a font name to its binary ttf representation. The binary data can
/// be further interpreted by such libs as the `msdf-gen` one.
///
/// For list of embedded fonts, see FONTS_TO_EXTRACT constant in `build.rs`.
#[allow(missing_docs)]
#[derive(Clone, Deref, DerefMut)]
pub struct EmbeddedFontsData {
    pub data: HashMap<&'static str, &'static [u8]>,
}

impl EmbeddedFontsData {
    pub fn new() -> EmbeddedFontsData {
        EmbeddedFontsData { data: embedded_fonts_data() }
    }
}

impl Debug for EmbeddedFontsData {
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
        let fonts = EmbeddedFontsData::new();
        let example_font = fonts.ttf_binary_data.get(DejaVuSans::regular()).unwrap();

        assert_eq!(0x00, example_font[0]);
        assert_eq!(0x01, example_font[1]);
        assert_eq!(0x1d, example_font[example_font.len() - 1]);
    }
}
