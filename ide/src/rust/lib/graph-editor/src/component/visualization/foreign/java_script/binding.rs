//! This module contains the javascript base class for visualizations.

use crate::prelude::*;

use super::PreprocessorCallback;

use ensogl::display::DomSymbol;
use fmt::Formatter;
use wasm_bindgen::prelude::*;
use web_sys::HtmlDivElement;



// =================
// === Constants ===
// =================

/// Name of the visualization base class in JavaScript sources.
pub const JS_CLASS_NAME : &str = "Visualization";



// ===========================
// === JavaScript Bindings ===
// ===========================

#[wasm_bindgen(module="/src/component/visualization/foreign/java_script/visualization.js")]
extern "C" {
    #[allow(unsafe_code)]
    fn __Visualization__() -> JsValue;

    #[allow(unsafe_code)]
    pub type Visualization;

    #[allow(unsafe_code)]
    #[wasm_bindgen(constructor)]
    fn new(init:JsConsArgs) -> Visualization;

    #[allow(unsafe_code)]
    #[wasm_bindgen(method)]
    fn setPreprocessor(this:&Visualization, code:String);
}

/// Provides reference to the visualizations JavaScript base class.
pub fn js_class() -> JsValue {
    __Visualization__()
}



// =====================
// === Rust Bindings ===
// =====================

/// Data that is passed into the javascript Visualization baseclass.
#[allow(missing_docs)]
#[wasm_bindgen]
pub struct JsConsArgs {
    #[wasm_bindgen(skip)]
    pub root : HtmlDivElement,
    #[wasm_bindgen(skip)]
    pub set_preprocessor : Box<dyn PreprocessorCallback>,
}

impl Debug for JsConsArgs {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f,"JsConsArgs({:?})", &self.root)
    }
}

impl JsConsArgs {
    /// Constructor.
    pub fn new<F:'static+PreprocessorCallback>(root:DomSymbol, closure:F) -> Self {
        let set_preprocessor = Box::new(closure);
        let root = root.dom().clone();
        JsConsArgs {root,set_preprocessor}
    }
}

#[wasm_bindgen]
impl JsConsArgs {
    /// Getter for the root element for the visualization.
    pub fn root(&self) -> JsValue {
        self.root.clone().into()
    }

    /// Helper method to emit an preprocessor change event from the visualisation.
    pub fn emit_preprocessor_change(&self, code:String){
        let closure = &self.set_preprocessor;
        (*closure)(code);
    }
}
