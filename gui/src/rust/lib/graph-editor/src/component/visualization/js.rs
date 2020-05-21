//! This module contains functionality to create a `Class` object from a JS source strings.

use crate::prelude::*;

use crate::component::visualization::JsVisualizationError;
use crate::component::visualization::InstantiationError;
use crate::component::visualization::JsRenderer;
use crate::component::visualization::JsResult;
use crate::component::visualization::InstantiationResult;
use crate::component::visualization::Class;
use crate::component::visualization::Visualization;
use crate::component::visualization::Signature;
use crate::component::visualization::EnsoType;

use ensogl::display::Scene;
use ensogl::system::web::JsValue;
use js_sys;



// ===================================
// === Visualization Class Wrapper ===
// ===================================

/// Internal wrapper for the a JS class that implements our visualization specification. Provides
/// convenience functions for accessing JS methods and signature.
#[derive(Clone,Debug)]
#[allow(missing_docs)]
struct VisualizationClassWrapper {
    class: JsValue,
}

impl VisualizationClassWrapper {
    fn instantiate_class(source:&str) -> JsResult<VisualizationClassWrapper> {
        let context     = JsValue::NULL;
        let constructor = js_sys::Function::new_no_args(source);
        let class       = constructor.call0(&context)?;
        Ok(VisualizationClassWrapper{class})
    }

    fn signature(&self) -> JsResult<Signature> {
        let input_types = self.input_types().unwrap_or_default();
        let name        = self.name()?;
        Ok(Signature {name,input_types})
    }

    fn constructor(&self) -> JsResult<js_sys::Function> {
        Ok(js_sys::Reflect::get(&self.prototype()?,&"constructor".into())?.into())
    }

    fn prototype(&self) -> JsResult<JsValue> {
        Ok(js_sys::Reflect::get(&self.class,&"prototype".into())?)
    }

    fn input_types(&self) -> JsResult<Vec<EnsoType>> {
        let input_types            = js_sys::Reflect::get(&self.class, &"inputTypes".into())?;
        let input_types            = js_sys::Array::from(&input_types);
        let js_string_to_enso_type = |value:JsValue| {Some(EnsoType::from(value.as_string()?))};
        Ok(input_types.iter().filter_map(js_string_to_enso_type).collect())
    }

    fn name(&self) -> JsResult<String> {
        let constructor = self.constructor()?;
        let name        = js_sys::Reflect::get(&constructor,&"name".into())?;
        Ok(name.as_string().unwrap_or_default())
    }

    fn instantiate(&self) -> JsResult<JsValue> {
        let fn_wrapper = js_sys::Function::new_with_args("cls", "return new cls()");
        let context    = JsValue::NULL;
        Ok(fn_wrapper.call1(&context, &self.class)?)
    }
}



// ========================
// === Js Source Class  ===
// ========================

/// Implements the `visualization::Class` for a JS source string.
///
/// Example
/// -------
/// ```no_run
///
/// # use graph_editor::component::visualization::JsSourceClass;
///
/// JsSourceClass::from_js_source_raw(r#"
///     class Visualization {
///         static inputTypes = ["[[Float,Float,Float]]"]
///         onDataReceived(root, data) {}
///         setSize(root, size) {}
///     }
///     return Visualizations;
/// "#.into()).unwrap();
/// ```
#[derive(CloneRef,Clone,Debug)]
#[allow(missing_docs)]
pub struct JsSourceClass {
    js_class   : Rc<VisualizationClassWrapper>,
    signature: Rc<Signature>,
}

impl JsSourceClass {
    /// Create a visualization source from piece of JS source code. Signature needs to be inferred.
    pub fn from_js_source_raw(source:&str) -> Result<Self,JsVisualizationError> {
        let js_class   = VisualizationClassWrapper::instantiate_class(&source)?;
        let signature  = js_class.signature()?;
        let js_class   = Rc::new(js_class);
        let signature  = Rc::new(signature);
        Ok(JsSourceClass{js_class,signature})
    }
}

impl Class for JsSourceClass {
    fn signature(&self) -> &Signature {
        &self.signature
    }

    fn instantiate(&self, scene:&Scene) -> InstantiationResult {
        let obj = match self.js_class.instantiate() {
            Ok(obj) => obj,
            Err(err) => return Err(InstantiationError::InvalidClass {inner:err.into()}),
        };
        let renderer = match JsRenderer::from_object(obj) {
            Ok(renderer) => renderer,
            Err(err) => return Err(InstantiationError::InvalidClass {inner:err.into()}),
        };
        renderer.set_dom_layer(&scene.dom.layers.front);
        Ok(Visualization::new(renderer))
    }
}
