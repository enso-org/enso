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

        /// Show a spinner covering the whole viewport.
        #[allow(unsafe_code)]
        #[wasm_bindgen(method)]
        #[wasm_bindgen(js_name = showProgressIndicator)]
        pub fn show_progress_indicator(this: &App, progress: f32);

        /// Hide a spinner.
        #[allow(unsafe_code)]
        #[wasm_bindgen(method)]
        #[wasm_bindgen(js_name = hideProgressIndicator)]
        pub fn hide_progress_indicator(this: &App);
    }
}

#[cfg(not(target_arch = "wasm32"))]
pub mod js_bindings {
    use super::*;
    use enso_web::mock_data;

    mock_data! { App => JsValue }
    mock_data! { Config => JsValue }
    mock_data! { Param => JsValue }

    impl App {
        pub fn register_get_shaders_rust_fn(&self, _closure: &Closure<dyn FnMut() -> JsValue>) {}
        pub fn register_set_shaders_rust_fn(&self, _closure: &Closure<dyn FnMut(JsValue)>) {}

        pub fn show_progress_indicator(&self, _progress: f32) {}
        pub fn hide_progress_indicator(&self) {}
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
    pub fn params(&self) -> Vec<Param> {
        let opts_fn =
            Reflect::get(self, &"optionsRecursive".into()).unwrap().unchecked_into::<Function>();
        let js_arr = opts_fn.call0(self).unwrap().unchecked_into::<Array>();
        js_arr.to_vec().into_iter().map(|t| t.unchecked_into::<Param>()).collect()
    }
}

impl Param {
    pub fn name(&self) -> String {
        let val = Reflect::get(self, &"name".into()).unwrap();
        val.print_to_string()
    }

    pub fn qualified_name(&self) -> String {
        let js_field = Reflect::get(self, &"qualifiedName".into()).unwrap();
        let js_fn = js_field.unchecked_into::<Function>();
        js_fn.call0(self).unwrap().print_to_string()
    }

    pub fn structural_name(&self) -> String {
        let js_field = Reflect::get(self, &"structuralName".into()).unwrap();
        let js_fn = js_field.unchecked_into::<Function>();
        js_fn.call0(self).unwrap().print_to_string()
    }

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
