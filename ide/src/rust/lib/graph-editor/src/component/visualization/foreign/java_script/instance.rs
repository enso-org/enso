//! This module contains functionality that allows the usage of JavaScript to define visualizations.
//!
//! The `Instance` defines a generic way to wrap JS function calls and allow interaction with
//! JS code and the visualization system.
//!
//! An `Instance` can be created via `Instance::from_object` where the a JS object is provided that
//! fullfills the spec described in `java_script/definition.rs
//!
//! All methods are optional and if they are not present, will be handled as no-op.

// FIXME: We should not mention in all docs that "all methods are optional etc" - if this changes,
//        we will have a lot of docs to be fixed. Lets just send the reader to the spec definition
//        instead!.

use crate::prelude::*;

use crate::component::visualization::*;
use crate::component::visualization;
use crate::frp;

use core::result;
use ensogl::display::DomScene;
use ensogl::display::DomSymbol;
use ensogl::display;
use ensogl::system::web::JsValue;
use ensogl::system::web;
use js_sys;
use std::fmt::Formatter;



// ==============
// === Errors ===
// ==============

/// Errors that can occur when transforming JS source to a visualization.
#[derive(Clone,Debug)]
#[allow(missing_docs)]
pub enum Error {
    // FIXME: this error is not specific enough. It should be named `InstanceIsNotAnObject` and used
    //        only to indicate that the value we used to create instance was not a valid object.
    //        The error messge below "Not an object: {:?}" is also not very specific about what
    //        really happened.
    /// The provided value was expected to be a JS object, but was not.
    NotAnObject        { object:JsValue },
    /// An error occurred when calling the class constructor.
    // FIXME: kets make it more specific - what exactly us constructor error?
    ConstructorError   { js_error:JsValue },
    // FIXME: can we remove it? We should handle each error explicitely. When something unknown happens?
    /// An unknown error occurred on the JS side. Inspect the content for more information.
    Unknown            { js_error:JsValue }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Error::NotAnObject { object: inner }  => {
                f.write_fmt(format_args!("Not an object: {:?}",inner))
            },
            Error::ConstructorError { js_error: inner }      => {
                f.write_fmt(format_args!("Error while constructing object: {:?}",inner))
            },
            Error::Unknown { js_error: inner }      => {
                f.write_fmt(format_args!("Unknown error: {:?}",inner))
            },
        }
    }
}

impl std::error::Error for Error {}

// FIXME: this impl encourages to use Unknown value. Using unknown should be always explicit.
//        do we really need Unknmown error at all?
impl From<JsValue> for Error {
    fn from(value:JsValue) -> Self {
        // TODO add differentiation if we encounter specific errors and return new variants.
        Error::Unknown { js_error:value}
    }
}

/// Internal helper type to propagate results that can fail due to `JsVisualizationError`s.
pub type Result<T> = result::Result<T, Error>;



// =====================
// === InstanceModel ===
// =====================

/// `JsVisualizationGeneric` allows the use of arbitrary javascript to create visualizations. It
/// takes function definitions as strings and proved those functions with data.
#[derive(Clone,CloneRef,Debug)]
#[allow(missing_docs)]
pub struct InstanceModel {
    pub root_node        : DomSymbol,
    pub logger           : Logger,
        on_data_received : Rc<Option<js_sys::Function>>,
        set_size         : Rc<Option<js_sys::Function>>,
}

impl InstanceModel {
    /// Internal helper that tries to convert a JS object into a `Instance`.
    fn from_object_js(object:js_sys::Object) -> result::Result<Self, Error> {
        let on_data_received = get_method(&object, "onDataReceived").ok(); // FIXME: literals eveywhere - move them to constants.
        let on_data_received = Rc::new(on_data_received);
        let set_size         = get_method(&object, "setSize").ok();
        let set_size         =  Rc::new(set_size);

        let logger    = Logger::new("Instance");
        let div       = web::create_div();
        let root_node = DomSymbol::new(&div);
        root_node.dom().set_attribute("id","vis")?;

        Ok(InstanceModel { on_data_received,set_size,root_node,logger })
    }

    /// Constructor from JavaScript object.
    pub fn from_object(object:JsValue) -> result::Result<Self, Error> {
        if !object.is_object() {
            return Err(Error::NotAnObject { object } )
        }
        Self::from_object_js(object.into())
    }

    /// Hooks the root node into the given scene.
    ///
    /// MUST be called to make this visualization visible.
    pub fn set_dom_layer(&self, scene:&DomScene) {
        scene.manage(&self.root_node);
    }

   fn receive_data(&self, data:&Data) -> result::Result<(),DataError> {
       if let Some (on_data_received) = &self.on_data_received.deref() {
           let context   = JsValue::NULL;
           let data_json = match data {
               Data::Json {content} => content,
               _ => todo!() // FIXME
           };
           let data_json:&serde_json::Value = data_json.deref();
           let data_js   = match JsValue::from_serde(data_json) {
               Ok(value) => value,
               Err(_)    => return Err(DataError::InvalidDataType),
           };
           if let Err(error) = on_data_received.call2(&context, &self.root_node.dom(), &data_js) {
               self.logger.warning( // FIXME Use `warning!` macro instead. Will be a lot shorter.
                   || format!("Failed to set data in {:?} with error: {:?}",self,error));
               return Err(DataError::InternalComputationError)
           }
       }
       Ok(())
   }

   fn set_size(&self, size:V2) {
       if let Some(set_size) = &self.set_size.deref() {
           let size          = Vector2::new(size.x,size.y);
           let context       = JsValue::NULL;
           let data_json     = JsValue::from_serde(&size).unwrap();
           if let Err(error) = set_size.call2(&context, &self.root_node.dom(), &data_json) {
               self.logger.warning(
                   || format!("Failed to set size in {:?} with error: {:?}", self, error));
           }
           self.root_node.set_size(size);
       }
   }
}



// ================
// === Instance ===
// ================

/// Sample visualization that renders the given data as text. Useful for debugging and testing.
#[derive(Clone,CloneRef,Debug,Shrinkwrap)]
#[allow(missing_docs)]
pub struct Instance {
    #[shrinkwrap(main_field)]
    model : InstanceModel,
    frp   : visualization::instance::Frp,
}

impl Instance {
    /// Constructor.
    pub fn new(object:JsValue) -> result::Result<Instance, Error>  {
        let model = InstanceModel::from_object(object)?;
        let frp   = default();
        Ok(Instance{model,frp}.init_frp())
    }

    fn init_frp(self) -> Self {
        let network = &self.frp.network;
        let model   = self.model.clone_ref();
        let frp     = self.frp.clone_ref();
        frp::extend! { network
            eval frp.set_size  ((size) model.set_size(*size));
            eval frp.send_data ([frp](data) { // FIXME this leaks memory!
                if let Err(e) = model.receive_data(data) {
                    frp.data_receive_error.emit(Some(e));
                }
             });
        }
        self
    }
}

impl From<Instance> for visualization::Instance {
    fn from(t:Instance) -> Self {
        Self::new(&t,&t.frp)
    }
}

impl display::Object for Instance {
    fn display_object(&self) -> &display::object::Instance {
        &self.model.root_node.display_object()
    }
}


// === Utils ===

/// Try to return the method specified by the given name on the given object as a
/// `js_sys::Function`.
fn get_method(object:&js_sys::Object, name:&str) -> Result<js_sys::Function> {
    let method_value                     = js_sys::Reflect::get(object,&name.into())?;
    let method_function:js_sys::Function = method_value.into();
    Ok(method_function)
}
