//! Definition of visualization JavaScript API.
//!
//! For details of the API please see:
//! * docstrings in the `visualization.js` file;
//! * [visualization documentation](https://enso.org/docs/developer/ide/product/visualizations.html).

use crate::prelude::*;
use ensogl::system::web::traits::*;

use crate::component::visualization;
use crate::component::visualization::InstantiationError;
use crate::component::visualization::InstantiationResult;
use crate::visualization::foreign::java_script::Sources;

use super::binding;
use super::instance::Instance;
use ensogl::application::Application;
use ensogl::system::web;
use ensogl::system::web::Function;
use ensogl::system::web::JsString;
use ensogl::system::web::JsValue;
use fmt::Formatter;



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
    pub fn new(project: visualization::path::Project, sources: Sources) -> Result<Self, Error> {
        let source = sources.to_string(&project);
        let context = JsValue::NULL;
        let function = Function::new_with_args_fixed(binding::JS_CLASS_NAME, &source)
            .map_err(Error::InvalidFunction)?;
        let js_class = binding::js_visualization_class();
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
    pub fn new_builtin(sources: Sources) -> Result<Self, Error> {
        Self::new(visualization::path::Project::Builtin, sources)
    }

    fn new_instance(&self, app: &Application) -> InstantiationResult {
        let instance =
            Instance::new(&self.class, app).map_err(InstantiationError::ConstructorError)?;
        Ok(instance.into())
    }
}

impl From<Definition> for visualization::Definition {
    fn from(t: Definition) -> Self {
        Self::new(t.signature.clone_ref(), move |app| t.new_instance(app))
    }
}


// === Utils ===

fn try_str_field(obj: &JsValue, field: &str) -> Option<String> {
    let field = web::Reflect::get(obj, &field.into()).ok()?;
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
                f.write_fmt(format_args!("Provided value is not a valid function: {value:?}")),
            Error::InvalidClass(value) =>
                f.write_fmt(format_args!("Provided value is not a valid class: {value:?}")),
        }
    }
}

/// Subset of `Error` related to invalid JavaScript class definition.
#[derive(Clone, Debug)]
#[allow(missing_docs, missing_copy_implementations)]
pub enum InvalidClass {
    MissingName,
}
