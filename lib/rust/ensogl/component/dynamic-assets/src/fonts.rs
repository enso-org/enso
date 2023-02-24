use ensogl_core::system::web::JsCast;
use ensogl_core::system::web::{JsValue, Map, Reflect};



// =================
// === Interface ===
// =================

/// Build atlas sources, and return as JavaScript data.
pub fn build_atlases() -> JsValue {
    let fonts_to_build = &[ensogl_text::font::DEFAULT_FONT_MONO];
    let fonts = Map::new();
    for font_name in fonts_to_build {
        let font = build_atlas(font_name);
        fonts.set(&font_name.to_string().into(), &font.into());
    }
    fonts.into()
}

/// Load an atlas from JavaScript data.
pub fn set_atlas(name: String, data: JsValue) {
    let atlas = Atlas::try_from(data).unwrap();
    load_atlas(name, atlas);
}



// ==================
// === Atlas Data ===
// ==================

#[derive(Debug)]
pub struct Atlas {
    atlas: js_sys::ArrayBuffer,
    metadata: String,
}

const ATLAS_FILE: &str = "atlas.ppm";
const METADATA_FILE: &str = "metadata.json";

impl From<Atlas> for JsValue {
    fn from(value: Atlas) -> Self {
        Map::new()
            .set(&ATLAS_FILE.into(), &value.atlas.into())
            .set(&METADATA_FILE.into(), &value.metadata.into())
            .into()
    }
}

impl TryFrom<JsValue> for Atlas {
    type Error = ();
    fn try_from(value: JsValue) -> Result<Self, Self::Error> {
        let map = Map::from(value);
        let atlas = map.get(&ATLAS_FILE.into());
        let metadata = map.get(&METADATA_FILE.into());
        let atlas = atlas.dyn_into().unwrap();
        let metadata = metadata.as_string().unwrap();
        Ok(Self { atlas, metadata })
    }
}



// =======================================
// === Creating Atlases at Build-Time ===
// =======================================

/// Generate MSDF data for a font.
fn build_atlas(name: &str) -> Atlas {
    use ensogl_text::font;
    use ensogl_text::font::Font;
    let fonts = font::Embedded::new();
    let font = fonts.load_font(name.into());
    let font = font.unwrap();
    let font = match font {
        Font::NonVariable(font) => font,
        Font::Variable(_) => panic!(),
    };
    for i in 0..100 {
        let _ = font.glyph_info(&Default::default(), font::GlyphId(i));
    }
    let cache = font.cache_snapshot();
    let atlas = js_sys::Uint8Array::from(&cache.atlas.0[..]).buffer();
    let metadata = cache.glyphs;
    Atlas { atlas, metadata }
}



// ===================================
// === Loading Atlases at Run-Time ===
// ===================================

/// Attach the given MSDF data to a font to enable efficient rendering.
fn load_atlas(name: String, data: Atlas) {
    todo!()
}
