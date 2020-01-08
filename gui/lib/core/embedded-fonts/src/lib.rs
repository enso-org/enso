#![allow(missing_docs)]

use basegl_prelude::*;
use basegl_prelude::fmt::{Formatter, Error};

/// A base of built-in fonts in application
///
/// The structure keeps only a binary data in ttf format. The data should be
/// then interpreted by user (e.g. by using msdf-sys crate)
///
/// For list of embedded fonts, see FONTS_TO_EXTRACT constant in `build.rs`
pub struct EmbeddedFonts {
    pub font_data_by_name: HashMap<&'static str,&'static [u8]>
}

impl EmbeddedFonts {
    /// Creates an embedded fonts base filled with data
    ///
    /// For list of embedded fonts, see `FONTS_TO_EXTRACT` constant in
    /// `build.rs`
    pub fn create_and_fill() -> EmbeddedFonts {
        let mut font_data_by_name = HashMap::<&'static str,&'static [u8]>::new();
        include!(concat!(env!("OUT_DIR"), "/fill_map.rs"));
        EmbeddedFonts{font_data_by_name}
    }
}

impl Debug for EmbeddedFonts {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        f.write_str("<Embedded fonts>")
    }
}

#[cfg(test)]
mod test {
    use crate::*;

    #[test]
    fn loading_embedded_fonts() {
        let fonts        = EmbeddedFonts::create_and_fill();
        let example_font = fonts.font_data_by_name.get("DejaVuSans").unwrap();

        assert_eq!(0x00, example_font[0]);
        assert_eq!(0x01, example_font[1]);
        assert_eq!(0x1d, example_font[example_font.len()-1]);
    }
}