use enso_prelude::*;
use ensogl_core::system::web::Map;
use ensogl_core::system::web::JsValue;



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
pub fn set(key: String, mut value: HashMap<String, Vec<u8>>) {
    let vertex = String::from_utf8(value.remove(VERTEX_FILE).unwrap()).unwrap();
    let fragment = String::from_utf8(value.remove(FRAGMENT_FILE).unwrap()).unwrap();
    ensogl_core::display::world::set_shader_code(key, vertex, fragment);
}

const VERTEX_FILE: &'static str = "vertex.glsl";
const FRAGMENT_FILE: &'static str = "fragment.glsl";
