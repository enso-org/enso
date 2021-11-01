//! This module contains the javascript base class for visualizations.

use crate::prelude::*;

use crate::component::type_coloring;
use crate::component::visualization::foreign::java_script::PreprocessorCallback;
use crate::component::visualization::instance::PreprocessorConfiguration;
use crate::Type;

use ensogl::data::color;
use ensogl::display::shape::StyleWatch;
use ensogl::display::style::data::DataMatch;
use ensogl::display::DomSymbol;
use ensogl_theme;
use fmt::Formatter;
use wasm_bindgen::prelude::*;
use web_sys::HtmlDivElement;


// =================
// === Constants ===
// =================

/// Name of the visualization base class in JavaScript sources.
pub const JS_CLASS_NAME: &str = "Visualization";



// ===========================
// === JavaScript Bindings ===
// ===========================

#[wasm_bindgen(module = "/src/component/visualization/foreign/java_script/visualization.js")]
extern "C" {
    #[allow(unsafe_code)]
    fn __Visualization__() -> JsValue;

    #[allow(unsafe_code)]
    #[wasm_bindgen(extends = js_sys::Object)]
    pub type Visualization;

    #[allow(unsafe_code)]
    #[wasm_bindgen(constructor)]
    fn new(init: JsConsArgs) -> Visualization;

    #[allow(unsafe_code)]
    #[wasm_bindgen(catch, js_name = __emitPreprocessorChange__, method)]
    pub fn emitPreprocessorChange(this: &Visualization) -> Result<(), JsValue>;
}

/// Provides reference to the visualizations JavaScript base class.
pub fn js_class() -> JsValue {
    __Visualization__()
}



// =============
// === Theme ===
// =============

/// The theming API that we expose to JS visualizations
#[wasm_bindgen]
#[derive(Clone, Debug)]
pub struct JsTheme {
    styles: StyleWatch,
}

/// A color in RGBA representation. Can be passed to JavaScript. Implements the `From` trait for
/// convertion from `color::Lcha`.
#[wasm_bindgen]
#[derive(Debug, Copy, Clone)]
pub struct JsColor {
    /// The red part as a float between 0 and 1
    pub red:   f32,
    /// The green part as a float between 0 and 1
    pub green: f32,
    /// The blue part as a float between 0 and 1
    pub blue:  f32,
    /// The opacity as a float between 0 and 1
    pub alpha: f32,
}

impl From<color::Rgba> for JsColor {
    fn from(rgba: color::Rgba) -> Self {
        JsColor { red: rgba.red, green: rgba.green, blue: rgba.blue, alpha: rgba.alpha }
    }
}

impl From<color::Lcha> for JsColor {
    fn from(lcha: color::Lcha) -> Self {
        color::Rgba::from(lcha).into()
    }
}

#[allow(non_snake_case)]
#[wasm_bindgen]
impl JsTheme {
    /// Takes a qualified type name and returns the color that is used in the GUI for that type.
    pub fn getColorForType(&self, tp_name: &str) -> JsColor {
        let tp = Type::from(tp_name.to_string());
        type_coloring::compute(&tp, &self.styles).into()
    }

    /// Takes a qualified type name and returns the color that should be used for foreground
    /// (e.g. text) that is shown on top of the background color returned by getColorForType.
    pub fn getForegroundColorForType(&self, _tp_name: &str) -> JsColor {
        self.styles.get_color(ensogl_theme::code::types::selected).into()
    }

    /// Queries style sheet value for a value.
    pub fn get(&self, path: &str) -> Option<JsColor> {
        Some(self.styles.get(path).color()?.into())
    }
}



// ========================
// === Constructor Args ===
// ========================

/// Data that is passed into the javascript Visualization baseclass.
#[allow(missing_docs)]
#[wasm_bindgen]
pub struct JsConsArgs {
    #[wasm_bindgen(skip)]
    pub root:             HtmlDivElement,
    theme:                JsTheme,
    #[wasm_bindgen(skip)]
    pub set_preprocessor: Box<dyn PreprocessorCallback>,
}

impl Debug for JsConsArgs {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "JsConsArgs({:?})", &self.root)
    }
}

impl JsConsArgs {
    /// Constructor.
    pub fn new<F: 'static + PreprocessorCallback>(
        root: DomSymbol,
        styles: StyleWatch,
        closure: F,
    ) -> Self {
        let set_preprocessor = Box::new(closure);
        let theme = JsTheme { styles };
        let root = root.dom().clone();
        JsConsArgs { root, theme, set_preprocessor }
    }
}

#[wasm_bindgen]
impl JsConsArgs {
    /// Getter for the root element for the visualization.
    pub fn root(&self) -> JsValue {
        self.root.clone().into()
    }

    /// Getter for the theming API that we expose to JS visualizations
    pub fn theme(&self) -> JsTheme {
        self.theme.clone()
    }

    /// Helper method to emit an preprocessor change event from the visualisation.
    pub fn emit_preprocessor_change(&self, code: Option<String>, module: Option<String>) {
        let closure = &self.set_preprocessor;
        let preprocessor_config = PreprocessorConfiguration::from_options(code, module);
        (*closure)(preprocessor_config);
    }
}
