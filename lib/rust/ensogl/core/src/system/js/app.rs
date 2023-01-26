//! Binding to the EnsoGL TypeScript App class. This module does not provide docs for the app
//! methods. You can find them in the TypeScript source code.

// === Non-Standard Linter Configuration ===
#![allow(missing_docs)]

use crate::prelude::*;
use crate::system::web::traits::*;
use crate::system::web::*;



// ===================
// === JS Bindings ===
// ===================

#[cfg(target_arch = "wasm32")]
pub mod js_bindings {
    use wasm_bindgen::prelude::*;

    #[wasm_bindgen]
    extern "C" {
        pub type App;
        pub type Config;
        pub type Params;
        pub type Param;

        /// Register in JS a closure to get non-precompiled shaders from Rust.
        #[allow(unsafe_code)]
        #[wasm_bindgen(method)]
        #[wasm_bindgen(js_name = registerGetShadersRustFn)]
        pub fn register_get_shaders_rust_fn(this: &App, closure: &Closure<dyn FnMut() -> JsValue>);

        /// Register in JS a closure to set precompiled shaders in Rust.
        #[allow(unsafe_code)]
        #[wasm_bindgen(method)]
        #[wasm_bindgen(js_name = registerSetShadersRustFn)]
        pub fn register_set_shaders_rust_fn(this: &App, closure: &Closure<dyn FnMut(JsValue)>);
    }
}

#[cfg(not(target_arch = "wasm32"))]
pub mod js_bindings {
    use super::*;
    use enso_web::mock_data;

    mock_data! { App => JsValue }
    mock_data! { Config => JsValue }
    mock_data! { Params => JsValue }
    mock_data! { Param => JsValue }

    impl App {
        pub fn register_get_shaders_rust_fn(&self, _closure: &Closure<dyn FnMut() -> JsValue>) {}
        pub fn register_set_shaders_rust_fn(&self, _closure: &Closure<dyn FnMut(JsValue)>) {}
    }
}

use js_bindings::*;



// ===========
// === App ===
// ===========

impl App {
    pub fn config(&self) -> Config {
        Reflect::get(self, &"config".into()).unwrap().unchecked_into::<Config>()
    }
}

impl Config {
    pub fn params(&self) -> Params {
        Reflect::get(self, &"params".into()).unwrap().unchecked_into()
    }
}

impl Params {
    pub fn get(&self, name: &str) -> Result<Param, JsValue> {
        Reflect::get(self, &name.into()).map(|t| t.unchecked_into())
    }

    pub fn to_vec(&self) -> Vec<Param> {
        let obj = (*self).clone().unchecked_into::<Object>();
        let keys = Object::keys_vec(&obj);
        keys.iter().map(|key| self.get(key).unwrap()).collect()
    }

    pub fn to_hash_map(&self) -> HashMap<String, Param> {
        let obj = (*self).clone().unchecked_into::<Object>();
        let keys = Object::keys_vec(&obj);
        keys.iter().map(|key| (key.clone(), self.get(key).unwrap())).collect()
    }
}

impl Param {
    pub fn value(&self) -> Option<String> {
        let val = Reflect::get(self, &"value".into()).unwrap();
        if val.is_null() || val.is_undefined() {
            None
        } else {
            Some(val.print_to_string())
        }
    }
}

pub fn app() -> Result<App, JsValue> {
    Reflect::get_nested_object(&window, &["ensoglApp"]).map(|t| t.unchecked_into())
}

pub fn app_or_panic() -> App {
    match app() {
        Ok(app) => app,
        Err(_) => panic!("Failed to get JavaScript EnsoGL app."),
    }
}
