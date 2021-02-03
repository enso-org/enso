//! Definition of visualization JavaScript API.
//!
//! Visualizations can be defined as a JavaScript function which returns a class of a shape
//! specified below. Consider the following definition:
//!
//! ```javascript
//! console.log("Hi, this definition is being registered now!")
//!
//! return class BubbleVisualization extends Visualization {
//!     static inputType = "Any"
//!
//!     onDataReceived(data) {
//!         const xmlns = "http://www.w3.org/2000/svg";
//!         while (this.dom.firstChild) {
//!             this.dom.removeChild(this.dom.lastChild);
//!         }
//!         const width   = this.dom.getAttributeNS(null, "width");
//!         const height  = this.dom.getAttributeNS(null, "height");
//!         const svgElem = document.createElementNS(xmlns, "svg");
//!         svgElem.setAttributeNS(null, "id"     , "vis-svg");
//!         svgElem.setAttributeNS(null, "viewBox", "0 0 " + width + " " + height);
//!         svgElem.setAttributeNS(null, "width"  , "100%");
//!         svgElem.setAttributeNS(null, "height" , "100%");
//!         this.dom.appendChild(svgElem);
//!         data.forEach(data => {
//!             const bubble = document.createElementNS(xmlns,"circle");
//!             bubble.setAttributeNS(null,"stroke", "black");
//!             bubble.setAttributeNS(null,"fill"  , "red");
//!             bubble.setAttributeNS(null,"r"     , data[2]);
//!             bubble.setAttributeNS(null,"cx"    , data[0]);
//!             bubble.setAttributeNS(null,"cy"    , data[1]);
//!             svgElem.appendChild(bubble);
//!         });
//!     }
//!
//!     setSize(size) {
//!         this.dom.setAttributeNS(null, "width", size[0]);
//!         this.dom.setAttributeNS(null, "height", size[1]);
//!     }
//! }
//! ```
//!
//! In particular:
//!
//! - [Required] **Source code**
//!   Visualization definition has to be a valid body of JavaScript function which returns a class
//!   definition. Instances of that class will be considered separate visualizations. You are
//!   allowed to use global variables / global state across visualizations of the same type, but you
//!   are highly advised not to do so.
//!
//! - [Required] **`Visualization` superclass**
//!   The class returned by the definition function should extend the predefined `Visualization`
//!   class. Classes which do not extend it, will not be registered as visualizations. The
//!   superclass defines a default constructor and a set of utils:
//!   - The `setPreprocessor(code)` method allowing setting an Enso code which will be evaluated on
//!     server-side before sending data to visualization.
//!   - The `dom` field, which will be initialized in the constructor to the DOM symbol used to host
//!     the visualization content. You are free to modify the DOM element, including adding other
//!     elements as its children.
//!
//! - [Optional] **Field `label`**
//!   The static field `label` is an user-facing name used to identify the visualization. You are
//!   not allowed to define several visualizations of the same name in the same Enso library. In
//!   case the field is missing, the name will be inferred from the class name by splitting the
//!   camel-case name into chunks and converting them to lowercase string.
//!
//! - [Optional] **Field `inputType`**
//!   The static field `inputType` is used to determine which Enso data types this visualization
//!   can be used for. Its value should be a valid Enso type, like "String | Int". In case the field
//!   is an empty string or it is missing, it will default to "Any", which is a type containing all
//!   other types. It is a rare case when you want to define a visualization which is able to work
//!   with just any data type, so you are highly advised to provide the type definition.
//!
//! - [Optional] **Field `inputFormat`**
//!   The static field `inputFormat` is used to determine what format the data should be provided
//!   to the `onDataReceived` function. Currently, the only valid option is "json", but it will be
//!   possible to set it to "binary" in the future. In the later case, it is up to the visualization
//!   author to manage the binary stream received from the server.
//!
//! - [Optional] **Constructor**
//!   The visualization will be instantiated by providing the constructor with a configuration
//!   object. The shape of the configuration object is not part of the public API and can change
//!   between releases of this library. You have to pass it unchanged to the superclass constructor.
//!
//! - [Optional] **Function `onDataReceived`**
//!   The `onDataReceived(data)` method is called on every new data chunk received from the server.
//!   Note that the visualization will receive the "full data" if you are not using the
//!   `setPreprocessor` method.
//!
//! - [Optional] **Function `setSize`**
//!   The `setSize(size)` method is called on every size change of the visualization. You should not
//!   draw outside of the provided area, however, if you do so, it will be clipped to the provided
//!   area automatically. The `size` parameter contains two fields `width` and `height` expressed in
//!   pixels.

// TODO: Connect the `setPreprocessor` method on Rust side.

// FIXME: Can we simplify the above definition so its more minimal, yet functional?


use crate::prelude::*;

use crate::component::visualization::InstantiationError;
use crate::component::visualization::InstantiationResult;
use crate::component::visualization;
use crate::data::enso;
use super::binding;
use super::instance::Instance;

use ensogl::display::Scene;
use ensogl::system::web::JsValue;
use js_sys::JsString;
use js_sys;
use wasm_bindgen::JsCast;
use fmt::Formatter;
use std::str::FromStr;


// =================
// === Constants ===
// =================

#[allow(missing_docs)]
pub mod field {
    pub const LABEL        : &str = "label";
    pub const INPUT_TYPE   : &str = "inputType";
    pub const INPUT_FORMAT : &str = "inputFormat";
}

#[allow(missing_docs)]
pub mod method {
    pub const ON_DATA_RECEIVED : &str = "onDataReceived";
    pub const SET_SIZE         : &str = "setSize";
}



// ==================
// === Definition ===
// ==================

/// JavaScript visualization definition.
#[derive(Clone,CloneRef,Debug)]
#[allow(missing_docs)]
pub struct Definition {
    class     : JsValue,
    signature : visualization::Signature,
}

impl Definition {

    /// Create a visualization source from piece of JS source code. Signature needs to be inferred.
    pub fn new (library:impl Into<enso::LibraryName>, source:impl AsRef<str>) -> Result<Self,Error> {
        let source       = source.as_ref();
        let source       = source;
        let context      = JsValue::NULL;
        let function     = js_sys::Function::new_with_args(binding::JS_CLASS_NAME,&source);
        let js_class     = binding::js_class();
        let class        = function.call1(&context,&js_class).map_err(Error::InvalidFunction)?;

        let library      = library.into();
        let input_type   = try_str_field(&class,field::INPUT_TYPE).unwrap_or_default();

        let input_format = try_str_field(&class,field::INPUT_FORMAT).unwrap_or_default();
        let input_format = visualization::data::Format::from_str(&input_format).unwrap_or_default();

        let label        = label(&class)?;
        let path         = visualization::Path::new(library,label);
        let signature    = visualization::Signature::new(path,input_type,input_format);

        Ok(Self{class,signature})
    }

    fn new_instance(&self, scene:&Scene) -> InstantiationResult {
        let instance = Instance::new(&self.class, scene)
            .map_err(InstantiationError::ConstructorError)?;
        Ok(instance.into())
    }
}

impl From<Definition> for visualization::Definition {
    fn from(t:Definition) -> Self {
        Self::new(t.signature.clone_ref(),move |scene| t.new_instance(scene))
    }
}


// === Utils ===

fn try_str_field(obj:&JsValue, field:&str) -> Option<String> {
    let field     = js_sys::Reflect::get(obj,&field.into()).ok()?;
    let js_string = field.dyn_ref::<JsString>()?;
    Some(js_string.into())
}

// TODO: convert camel-case names to nice names
fn label(class:&JsValue) -> Result<String,Error> {
    try_str_field(class,field::LABEL).map(Ok).unwrap_or_else(|| {
        let class_name = try_str_field(class,"name").ok_or(Error::InvalidClass(InvalidClass::MissingName))?;
        Ok(class_name)
    })
}



// =============
// === Error ===
// =============

/// Visualization definition or an error occurred during its construction.
pub type FallibleDefinition = Result<Definition,Error>;

/// Error occurred during visualization definition.
#[derive(Clone,Debug)]
#[allow(missing_docs)]
pub enum Error {
    InvalidFunction(JsValue),
    InvalidClass(InvalidClass),
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Error::InvalidFunction(value)  => {
                f.write_fmt(format_args!("Provided value is not a valid function: {:?}",value))
            },
            Error::InvalidClass(value)  => {
                f.write_fmt(format_args!("Provided value is not a valid class: {:?}",value))
            },
        }
    }
}

/// Subset of `Error` related to invalid JavaScript class definition.
#[derive(Clone,Debug)]
#[allow(missing_docs,missing_copy_implementations)]
pub enum InvalidClass {
    MissingName,
}
