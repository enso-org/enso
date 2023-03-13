//! *Dynamic assets* are assets that the application itself is used to build. This enables certain
//! types of performance optimization:
//!
//! # Cache pre-seeding
//!
//! When the application uses a cache for an expensive computation, in some cases it is possible to
//! serialize the result of the work, and load it at startup to reduce work done at runtime. Dynamic
//! assets make this process simple to apply to any type of cache.
//!
//! # Offline shader optimization
//!
//! The application generates many shader programs; these programs can be optimized by external
//! tools. However, the programs are generated at runtime, so build-time optimization requires
//! dynamic analysis. Dynamic asset extraction fills this role.

#![feature(local_key_cell_methods)]
#![feature(let_chains)]
#![cfg(target_arch = "wasm32")]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![allow(clippy::option_map_unit_fn)]
#![allow(clippy::precedence)]
#![allow(dead_code)]
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]



use enso_prelude::*;
use enso_shapely::before_main;
use ensogl_core::system::js;
use ensogl_core::system::web::Closure;
use ensogl_core::system::web::JsCast;
use ensogl_core::system::web::JsValue;
use ensogl_core::system::web::Map;

pub mod fonts;
pub mod shaders;



// ======================
// === Dynamic Assets ===
// ======================

/// Register the functions to get and set dynamic assets, to be invoked from JavaScript.
#[before_main]
pub fn register_dynamic_assets_fns() {
    let js_app = js::app_or_panic();
    let closure = Closure::new(get_dynamic_assets_sources);
    js_app.register_get_dynamic_assets_sources_rust_fn(&closure);
    mem::forget(closure);
    let closure = Closure::new(set_dynamic_asset);
    js_app.register_set_dynamic_asset_rust_fn(&closure);
    mem::forget(closure);
}

fn get_dynamic_assets_sources() -> JsValue {
    let builders = Map::new();
    builders.set(&"font".to_string().into(), &fonts::build_atlases());
    builders.set(&"shader".to_string().into(), &shaders::gather());
    builders.into()
}

fn set_dynamic_asset(builder: JsValue, key: JsValue, asset: JsValue) {
    try_set_dynamic_asset(builder, key, asset)
        .unwrap_or_else(|e| error!("Setting dynamic asset: {e}"));
}

fn try_set_dynamic_asset(builder: JsValue, key: JsValue, asset: JsValue) -> anyhow::Result<()> {
    let builder = builder.as_string().unwrap();
    let key = key.as_string().unwrap();
    let asset: Map = asset.dyn_into().unwrap();
    info!("Loading a dynamic asset of type `{builder}`: `{key}`.");
    let mut asset_ = HashMap::new();
    asset.for_each(&mut |value: JsValue, key: JsValue| {
        asset_.insert(key.as_string().unwrap(), js_sys::Uint8Array::new(&value).to_vec());
    });
    match builder.as_ref() {
        "font" => fonts::set_atlas(key, asset_),
        "shader" => shaders::set(key, asset_),
        _ => anyhow::bail!("Unknown builder."),
    }
    Ok(())
}
