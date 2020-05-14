//! This module contains functionality that allows the usage of JavaScript to define visualizations.
//!
//! The `JsRenderer` defines a generic way to wrap JS function calls and allow interaction with
//! JS code and the visualisation system.
//!
//! There are at the moment three way to generate a `JsRenderer`:
//! 1. `JsRenderer::from_functions` where the bodies of the required functions are provided as
//!    source code.
//! 2. `JsRenderer::from_object` where the a piece of JS code is provided that must evaluate to an
//!     object that has the required methods that will be called at runtime.
//! 3. `JsRenderer::from_constructor`where the body of a constructor function needs to be
//!     provided. The returned object needs to fulfill the same specification as in (2).
//!
//! Right now the only functions required on the wrapped object are
//!  * `onDataReceived(root, data)`, which receives the html element that the visualisation should be
//!     appended on, as well as the data that should be rendered.
//!  * `setSize(root, size)`, which receives the node that the visualisation should be appended on,
//!    as well as the intended size.
//!
//! TODO: refine spec and add functions as needed, e.g., init, callback hooks or type indicators.

use crate::prelude::*;

use crate::component::visualization::Data;
use crate::component::visualization::DataError;
use crate::component::visualization::DataRenderer;
use crate::component::visualization::DataRendererFrp;

use ensogl::display::DomScene;
use ensogl::display::DomSymbol;
use ensogl::display;
use ensogl::system::web::JsValue;
use ensogl::system::web;
use js_sys;



// ==============
// === Errors ===
// ==============

/// Errors that can occur when transforming JS source to a visualization.
#[derive(Clone,Debug)]
#[allow(missing_docs)]
pub enum JsVisualisationError {
    NotAnObject  { inner:JsValue },
    NotAFunction { inner:JsValue },
    /// An unknown error occurred on the JS side. Inspect the content for more information. .
    Unknown      { inner:JsValue }
}

impl From<JsValue> for JsVisualisationError {
    fn from(value:JsValue) -> Self {
        // TODO add differentiation if we encounter specific errors and return new variants.
        JsVisualisationError::Unknown {inner:value}
    }
}



// ==================
// === JsRenderer ===
// ==================

/// `JsVisualizationGeneric` allows the use of arbitrary javascript to create visualisations. It
/// takes function definitions as strings and proved those functions with data.
#[derive(Clone,Debug)]
#[allow(missing_docs)]
pub struct JsRenderer {
    pub root_node        : DomSymbol,
    pub logger           : Logger,
        on_data_received : js_sys::Function,
        set_size         : js_sys::Function,
        frp              : DataRendererFrp,
}

impl JsRenderer {
    /// Constructor from single functions.
    ///
    /// `fn_set_data` and `fn_set_size` need to be strings that contain valid JavaScript code. This
    /// code will be executed as the function body of the respective functions.
    ///
    /// `fn_set_data` will be called with two arguments: the first argument (`root) )will be the
    /// root node that the visualisation should use to build its output, the second argument
    /// (`data`)will be the data that it should visualise.
    ///
    /// `fn_set_size` will be called with a tuple of floating point values indicating the desired
    /// width and height. This can be used by the visualisation to ensure proper scaling.
    ///
    /// For a full example see
    /// `crate::component::visualization::renderer::example::function_sample_js_bubble_chart`
    pub fn from_functions(fn_set_data:&str, fn_set_size:&str) -> Self {
        let set_data = js_sys::Function::new_no_args(fn_set_data);
        let set_size = js_sys::Function::new_no_args(fn_set_size);

        let logger    = Logger::new("JsRendererGeneric");
        let frp       = default();
        let div       = web::create_div();
        let root_node = DomSymbol::new(&div);
        root_node.dom().set_attribute("id","vis").unwrap();

        JsRenderer { on_data_received: set_data,set_size,root_node,frp,logger }
    }

    /// Internal helper that tries to convert a JS object into a `JsRenderer`.
    fn from_object_js(object:js_sys::Object) -> Result<JsRenderer,JsVisualisationError> {
        let set_data = js_sys::Reflect::get(&object,&"onDataReceived".into())?;
        let set_size = js_sys::Reflect::get(&object,&"setSize".into())?;
        if !set_data.is_function() {
            return Err(JsVisualisationError::NotAFunction { inner:set_data })
        }
        if !set_size.is_function() {
            return Err(JsVisualisationError::NotAFunction { inner:set_size })
        }
        let set_data:js_sys::Function = set_data.into();
        let set_size:js_sys::Function = set_size.into();

        let logger  = Logger::new("JsRenderer");
        let frp     = default();
        let div     = web::create_div();
        let root_node = DomSymbol::new(&div);
        root_node.dom().set_attribute("id","vis")?;

        Ok(JsRenderer { on_data_received: set_data,set_size,root_node,frp,logger })
    }

    /// Constructor from a source that evaluates to an object with specific methods.
    ///
    /// Example:
    /// --------
    ///
    /// ```no_run
    /// use graph_editor::component::visualization::JsRenderer;
    ///
    /// let renderer = JsRenderer::from_object("function() {
    ///   class Visualization {
    ///       onDataReceived(root, data) {};
    ///       setSize(root, size) {};
    ///   }
    ///   return new Visualisation();
    /// }()").unwrap();
    ///
    /// ```
    ///
    /// For a full example see
    /// `crate::component::visualization::renderer::example::object_sample_js_bubble_chart`
    pub fn from_object(source: &str) -> Result<JsRenderer,JsVisualisationError> {
        let object = js_sys::eval(source)?;
        if !object.is_object() {
            return Err(JsVisualisationError::NotAnObject { inner:object } )
        }
        Self::from_object_js(object.into())
    }

    /// Constructor from function body that returns a object with specific functions.
    ///
    /// Example:
    /// --------
    ///
    /// ```no_run
    /// use graph_editor::component::visualization::JsRenderer;
    ///
    /// let renderer = JsRenderer::from_constructor("
    ///   class Visualization {
    ///       onDataReceived(root, data) {};
    ///       setSize(root, size) {};
    ///   }
    ///   return new Visualisation();
    ///   ").unwrap();
    ///
    /// ```
    /// For a full example see
    /// `crate::component::visualization::renderer::example::constructor_sample_js_bubble_chart`
    pub fn from_constructor(source:&str) -> Result<JsRenderer,JsVisualisationError> {
        let context     = JsValue::NULL;
        let constructor = js_sys::Function::new_no_args(source);
        let object      = constructor.call0(&context)?;
        if !object.is_object() {
            return Err(JsVisualisationError::NotAnObject { inner:object } )
        }
        Self::from_object_js(object.into())
    }

    /// Hooks the root node into the given scene.
    ///
    /// MUST be called to make this visualisation visible.
    // TODO[mm] find a better mechanism to ensure this. Probably through the registry later on.
    pub fn set_dom_layer(&self, scene:&DomScene) {
        scene.manage(&self.root_node);
    }
}

impl DataRenderer for JsRenderer {

    fn receive_data(&self, data:Data) -> Result<(),DataError> {
        let context   = JsValue::NULL;
        let data_json = data.as_json()?;
        let data_js   = match JsValue::from_serde(&data_json) {
            Ok(value) => value,
            Err(_)    => return Err(DataError::InvalidDataType),
        };
        if let Err(error) = self.on_data_received.call2(&context, &self.root_node.dom(), &data_js) {
            self.logger.warning(
                || format!("Failed to set data in {:?} with error: {:?}",self,error));
            return Err(DataError::InternalComputationError)
        }
        Ok(())
    }

    fn set_size(&self, size:Vector2<f32>) {
        let context       = JsValue::NULL;
        let data_json     = JsValue::from_serde(&size).unwrap();
        if let Err(error) = self.set_size.call2(&context, &self.root_node.dom(), &data_json) {
            self.logger.warning(
                || format!("Failed to set size in {:?} with error: {:?}", self, error));
        }
    }

    fn frp(&self) -> &DataRendererFrp {
        &self.frp
    }
}

impl display::Object for JsRenderer {
    fn display_object(&self) -> &display::object::Instance {
        &self.root_node.display_object()
    }
}
