//! Definition of visualization JavaScript API.
//!
//! For details of the API please see:
//! * docstrings in the `visualization.js` file;
//! * [visualization documentation](https://dev.enso.org/docs/ide/product/visualizations.html).
// FIXME: Can we simplify the above definition so its more minimal, yet functional?

mod function;
use function::Function;

use crate::prelude::*;

use super::binding;
use super::instance::Instance;
use crate::component::visualization;
use crate::component::visualization::InstantiationError;
use crate::component::visualization::InstantiationResult;

use ensogl::display::Scene;
use ensogl::system::web::JsValue;
use fmt::Formatter;
use js_sys;
use js_sys::JsString;
use std::str::FromStr;
use wasm_bindgen::JsCast;



// =================
// === Constants ===
// =================

#[allow(missing_docs)]
pub mod field {
    pub const LABEL: &str = "label";
    pub const INPUT_TYPE: &str = "inputType";
    pub const INPUT_FORMAT: &str = "inputFormat";
}

#[allow(missing_docs)]
pub mod method {
    pub const ON_DATA_RECEIVED: &str = "onDataReceived";
    pub const SET_SIZE: &str = "setSize";
}



// ==================
// === Definition ===
// ==================

/// JavaScript visualization definition.
#[derive(Clone, CloneRef, Debug)]
#[allow(missing_docs)]
pub struct Definition {
    class:     JsValue,
    signature: visualization::Signature,
}

impl Definition {
    /// Create a visualization source from piece of JS source code. Signature needs to be inferred.
    pub fn new(
        project: visualization::path::Project,
        source: impl AsRef<str>,
    ) -> Result<Self, Error> {
        let source = source.as_ref();
        let source = source;
        let context = JsValue::NULL;
        let function = Function::new_with_args(binding::JS_CLASS_NAME, source)
            .map_err(Error::InvalidFunction)?;
        let js_class = binding::js_class();
        let class = function.call1(&context, &js_class).map_err(Error::InvalidFunction)?;

        let input_type = try_str_field(&class, field::INPUT_TYPE).unwrap_or_default();

        let input_format = try_str_field(&class, field::INPUT_FORMAT).unwrap_or_default();
        let input_format = visualization::data::Format::from_str(&input_format).unwrap_or_default();

        let label = label(&class)?;
        let path = visualization::Path::new(project, label);
        let signature = visualization::Signature::new(path, input_type, input_format);

        Ok(Self { class, signature })
    }

    /// Create a definition of visualization that is built into the IDE.
    pub fn new_builtin(source: impl AsRef<str>) -> Result<Self, Error> {
        Self::new(visualization::path::Project::Builtin, source)
    }

    fn new_instance(&self, scene: &Scene) -> InstantiationResult {
        let instance =
            Instance::new(&self.class, scene).map_err(InstantiationError::ConstructorError)?;
        Ok(instance.into())
    }
}

impl From<Definition> for visualization::Definition {
    fn from(t: Definition) -> Self {
        Self::new(t.signature.clone_ref(), move |scene| t.new_instance(scene))
    }
}


// === Utils ===

fn try_str_field(obj: &JsValue, field: &str) -> Option<String> {
    let field = js_sys::Reflect::get(obj, &field.into()).ok()?;
    let js_string = field.dyn_ref::<JsString>()?;
    Some(js_string.into())
}

// TODO: convert camel-case names to nice names
fn label(class: &JsValue) -> Result<String, Error> {
    try_str_field(class, field::LABEL).map(Ok).unwrap_or_else(|| {
        let class_name =
            try_str_field(class, "name").ok_or(Error::InvalidClass(InvalidClass::MissingName))?;
        Ok(class_name)
    })
}



// =============
// === Error ===
// =============

/// Visualization definition or an error occurred during its construction.
pub type FallibleDefinition = Result<Definition, Error>;

/// Error occurred during visualization definition.
#[derive(Clone, Debug)]
#[allow(missing_docs)]
pub enum Error {
    InvalidFunction(JsValue),
    InvalidClass(InvalidClass),
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Error::InvalidFunction(value) =>
                f.write_fmt(format_args!("Provided value is not a valid function: {:?}", value)),
            Error::InvalidClass(value) =>
                f.write_fmt(format_args!("Provided value is not a valid class: {:?}", value)),
        }
    }
}

/// Subset of `Error` related to invalid JavaScript class definition.
#[derive(Clone, Debug)]
#[allow(missing_docs, missing_copy_implementations)]
pub enum InvalidClass {
    MissingName,
}
