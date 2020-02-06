//! This file contains the implementation of DOMContainer. A struct that aids us to handle html
//! elements, get its dimension avoiding style reflow.

use enso_prelude::*;

use crate::get_element_by_id;
use crate::dyn_into;
use crate::Result;
use crate::StyleSetter;
use crate::resize_observer::ResizeObserver;

use wasm_bindgen::prelude::Closure;
use web_sys::HtmlElement;
use nalgebra::Vector2;
use std::cell::RefCell;
use std::rc::Rc;



// ======================
// === ResizeCallback ===
// ======================

/// Resize callback used by `DOMContainer`.
pub trait ResizeCallback = Fn(&Vector2<f32>) + 'static;



// ==============================
// === DOMContainerProperties ===
// ==============================

#[derive(Derivative)]
#[derivative(Debug)]
struct DOMContainerProperties {
    dimensions : Vector2<f32>,
    #[derivative(Debug="ignore")]
    resize_callbacks   : Vec<Box<dyn ResizeCallback>>,
}

// ========================
// === DomContainerData ===
// ========================

#[derive(Debug)]
struct DomContainerData {
    properties : RefCell<DOMContainerProperties>
}

impl DomContainerData {
    pub fn new(dimensions:Vector2<f32>) -> Rc<Self> {
        let resize_callbacks   = Default::default();
        let properties         = RefCell::new(DOMContainerProperties {
            dimensions,
            resize_callbacks,
        });
        Rc::new(Self {properties})
    }

    fn on_resize(&self) {
        let dimensions = self.dimensions();
        for callback in &self.properties.borrow().resize_callbacks {
            (callback)(&dimensions)
        }
    }
}


// === Getters ===

impl DomContainerData {
    fn dimensions(&self) -> Vector2<f32> { self.properties.borrow().dimensions }
}


// === Setters ===

impl DomContainerData {
    fn set_dimensions(&self, dimensions:Vector2<f32>) {
        if dimensions != self.dimensions() {
            self.properties.borrow_mut().dimensions = dimensions;
            self.on_resize();
        }
    }

    fn add_resize_callback<T:ResizeCallback>(&self, callback:T) {
        self.properties.borrow_mut().resize_callbacks.push(Box::new(callback))
    }
}


// ====================
// === DomContainer ===
// ====================

/// A struct used to keep track of HtmlElement dimensions without worrying about style
/// reflow.
#[derive(Debug)]
pub struct DomContainer {
    pub dom               : HtmlElement,
    resize_observer       : Option<ResizeObserver>,
    data                  : Rc<DomContainerData>
}

impl Clone for DomContainer {
    fn clone(&self) -> Self {
        DomContainer::from_element(self.dom.clone())
    }
}

impl DomContainer {
    pub fn from_element(dom:HtmlElement) -> Self {
        let rect                  = dom.get_bounding_client_rect();
        let width                 = rect.width()  as f32;
        let height                = rect.height() as f32;
        let dimensions            = Vector2::new(width, height);
        let data                  = DomContainerData::new(dimensions);
        let resize_observer       = None;
        let mut ret = Self {dom,resize_observer,data};

        ret.init_listeners();
        ret
    }
    pub fn from_id(dom_id:&str) -> Result<Self> {
        let dom : HtmlElement = dyn_into(get_element_by_id(dom_id)?)?;
        Ok(Self::from_element(dom))
    }

    fn init_listeners(&mut self) {
        self.init_resize_listener();
    }

    fn init_resize_listener(&mut self) {
        let data = self.data.clone();
        let closure = Closure::new(move |width, height| {
            data.set_dimensions(Vector2::new(width as f32, height as f32));
        });
        let observer = ResizeObserver::new(&self.dom, closure);
        self.resize_observer = Some(observer);
    }

    /// Sets the Scene DOM's dimensions.
    pub fn set_dimensions(&mut self, dimensions:Vector2<f32>) {
        self.dom.set_style_or_panic("width" , format!("{}px", dimensions.x));
        self.dom.set_style_or_panic("height", format!("{}px", dimensions.y));
        self.data.set_dimensions(dimensions);
    }

    /// Gets the Scene DOM's position. Causes style reflow.
    pub fn position_with_style_reflow(&self) -> Vector2<f32> {
        let rect = self.dom.get_bounding_client_rect();
        Vector2::new(rect.x() as f32, rect.y() as f32)
    }

    /// Gets the Scene DOM's dimensions.
    pub fn dimensions(&self) -> Vector2<f32> {
        self.data.dimensions()
    }

    /// Adds a ResizeCallback.
    pub fn add_resize_callback<T:ResizeCallback>(&mut self, callback:T) {
        self.data.add_resize_callback(callback);
    }
}