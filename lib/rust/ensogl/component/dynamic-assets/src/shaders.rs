//! Offline optimization of runtime-generated shader programs.

use enso_prelude::*;
use ensogl_core::system::web::JsValue;
use ensogl_core::system::web::Map;



// =================
// === Constants ===
// =================

/// Path within the asset directory to store the vertex shader.
const VERTEX_FILE: &'static str = "vertex.glsl";
/// Path within the asset directory to store the fragment shader.
const FRAGMENT_FILE: &'static str = "fragment.glsl";



// ===============
// === Shaders ===
// ===============

/// Gather unoptimized shader code for precompilation.
pub fn gather() -> JsValue {
    let map = Map::new();
    let shaders = ensogl_core::display::world::gather_shaders();
    let vertex_file = VERTEX_FILE.into();
    let fragment_file = FRAGMENT_FILE.into();
    for (path, shader) in shaders {
        let assets = Map::new();
        assets.set(&vertex_file, &shader.vertex.into());
        assets.set(&fragment_file, &shader.fragment.into());
        map.set(&path.into(), &assets.into());
    }
    map.into()
}

/// Set optimized shader code (at early runtime).
pub fn set(key: String, value: HashMap<String, Vec<u8>>) {
    try_set(key, value).unwrap_or_else(|e| error!("Failed to load shader: {e}."));
}

fn try_set(key: String, mut value: HashMap<String, Vec<u8>>) -> anyhow::Result<()> {
    let vertex = String::from_utf8(
        value.remove(VERTEX_FILE).ok_or_else(|| anyhow!("Missing vertex file."))?,
    )?;
    let fragment = String::from_utf8(
        value.remove(FRAGMENT_FILE).ok_or_else(|| anyhow!("Missing fragment file."))?,
    )?;
    ensogl_core::display::world::set_shader_code(key, vertex, fragment);
    Ok(())
}
