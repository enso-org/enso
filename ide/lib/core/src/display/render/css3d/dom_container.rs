#![allow(missing_docs)]

use crate::prelude::*;

use crate::system::web::get_element_by_id;
use crate::system::web::dyn_into;
use crate::system::web::Result;
use crate::system::web::StyleSetter;
use crate::system::web::resize_observer::ResizeObserver;

use wasm_bindgen::prelude::Closure;
use web_sys::HtmlElement;
use nalgebra::Vector2;
use std::cell::RefCell;
use std::rc::Rc;


// ======================
// === ResizeCallback ===
// ======================

type ResizeCallback = Box<dyn Fn(&Vector2<f32>)>;
pub trait ResizeCallbackFn = where Self: Fn(&Vector2<f32>) + 'static;

// ========================
// === DOMContainerData ===
// ========================

#[derive(Derivative)]
#[derivative(Debug)]
pub struct DOMContainerData {
    dimensions : Vector2<f32>,
    #[derivative(Debug="ignore")]
    resize_callbacks : Vec<ResizeCallback>
}

impl DOMContainerData {
    pub fn new(dimensions : Vector2<f32>) -> Self {
        let resize_callbacks = default();
        Self { dimensions, resize_callbacks }
    }
}


// ====================
// === DOMContainer ===
// ====================

/// A collection for holding 3D `Object`s.
#[derive(Debug)]
pub struct DOMContainer {
    pub dom          : HtmlElement,
    resize_observer  : Option<ResizeObserver>,
    data             : Rc<RefCell<DOMContainerData>>,
}

impl DOMContainer {
    pub fn new(dom_id:&str) -> Result<Self> {
        let dom : HtmlElement = dyn_into(get_element_by_id(dom_id)?)?;

        let width           = dom.client_width()  as f32;
        let height          = dom.client_height() as f32;
        let dimensions      = Vector2::new(width, height);
        let data            = Rc::new(RefCell::new(DOMContainerData::new(dimensions)));
        let resize_observer = None;
        let mut ret         = Self { dom, resize_observer, data };

        ret.init_listeners();
        Ok(ret)
    }

    fn init_listeners(&mut self) {
        let data = self.data.clone();
        let resize_closure = Closure::new(move |width, height| {
            let mut data = data.borrow_mut();
            data.dimensions = Vector2::new(width as f32, height as f32);
            for callback in &data.resize_callbacks {
                callback(&data.dimensions);
            }
        });
        self.resize_observer = Some(ResizeObserver::new(&self.dom, resize_closure));
    }

    /// Sets the Scene DOM's dimensions.
    pub fn set_dimensions(&mut self, dimensions:Vector2<f32>) {
        self.dom.set_property_or_panic("width" , format!("{}px", dimensions.x));
        self.dom.set_property_or_panic("height", format!("{}px", dimensions.y));
        self.data.borrow_mut().dimensions = dimensions;
    }

    /// Gets the Scene DOM's dimensions.
    pub fn dimensions(&self) -> Vector2<f32> {
        self.data.borrow().dimensions
    }

    /// Adds a ResizeCallback.
    pub fn add_resize_callback<T>(&mut self, callback:T)
        where T : ResizeCallbackFn {
        self.data.borrow_mut().resize_callbacks.push(Box::new(callback));
    }
}