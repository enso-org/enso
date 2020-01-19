// This file contains the implementation of DOMContainer. A struct that aids us to handle html
// elements, get its position and dimension avoiding style reflow.
//
// It relies on Resize Observer and Intersection Observer, which notifies us when the element's
// rect and visibility rect is updated.

use crate::prelude::*;

use crate::system::web::get_element_by_id;
use crate::system::web::dyn_into;
use crate::system::web::Result;
use crate::system::web::StyleSetter;
use crate::system::web::intersection_observer::IntersectionObserver;

use wasm_bindgen::prelude::Closure;
use web_sys::HtmlElement;
use nalgebra::Vector2;
use std::cell::RefCell;
use std::rc::Rc;



// ========================
// === PositionCallback ===
// ========================

/// Position callback used by `DOMContainer`.
pub trait PositionCallback = Fn(&Vector2<f32>) + 'static;



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
    position   : Vector2<f32>,
    dimensions : Vector2<f32>,
    #[derivative(Debug="ignore")]
    resize_callbacks   : Vec<Box<dyn ResizeCallback>>,
    #[derivative(Debug="ignore")]
    position_callbacks : Vec<Box<dyn PositionCallback>>
}

// ========================
// === DOMContainerData ===
// ========================

#[derive(Debug)]
struct DOMContainerData {
    properties : RefCell<DOMContainerProperties>
}

impl DOMContainerData {
    pub fn new(position:Vector2<f32>, dimensions:Vector2<f32>) -> Rc<Self> {
        let position_callbacks = default();
        let resize_callbacks   = default();
        let properties         = RefCell::new(DOMContainerProperties {
            position,
            dimensions,
            resize_callbacks,
            position_callbacks
        });
        Rc::new(Self {properties})
    }

    fn on_resize(&self) {
        let dimensions = self.dimensions();
        for callback in &self.properties.borrow().resize_callbacks {
            (callback)(&dimensions)
        }
    }

    fn on_position(&self) {
        let position   = self.position();
        for callback in &self.properties.borrow().position_callbacks {
            (callback)(&position)
        }
    }
}


// === Getters ===

impl DOMContainerData {
    fn position  (&self) -> Vector2<f32> { self.properties.borrow().position }
    fn dimensions(&self) -> Vector2<f32> { self.properties.borrow().dimensions }
}


// === Setters ===

impl DOMContainerData {
    fn set_position(&self, position:Vector2<f32>) {
        if position != self.position() {
            self.properties.borrow_mut().position = position;
            self.on_position()
        }
    }

    fn set_dimensions(&self, dimensions:Vector2<f32>) {
        if dimensions != self.dimensions() {
            self.properties.borrow_mut().dimensions = dimensions;
            self.on_resize();
        }
    }

    fn add_position_callback<T:PositionCallback>(&self, callback:T) {
        self.properties.borrow_mut().position_callbacks.push(Box::new(callback))
    }

    fn add_resize_callback<T:ResizeCallback>(&self, callback:T) {
        self.properties.borrow_mut().resize_callbacks.push(Box::new(callback))
    }
}


// ====================
// === DOMContainer ===
// ====================

/// A struct used to keep track of HtmlElement dimensions and position without worrying about style
/// reflow.
#[derive(Debug)]
pub struct DOMContainer {
    pub dom               : HtmlElement,
    intersection_observer : Option<IntersectionObserver>,
    data                  : Rc<DOMContainerData>
}

impl Clone for DOMContainer {
    fn clone(&self) -> Self {
        DOMContainer::from_element(self.dom.clone())
    }
}

impl DOMContainer {
    pub fn from_element(dom:HtmlElement) -> Self {
        let rect                  = dom.get_bounding_client_rect();
        let x                     = rect.x()      as f32;
        let y                     = rect.y()      as f32;
        let width                 = rect.width()  as f32;
        let height                = rect.height() as f32;
        let dimensions            = Vector2::new(width, height);
        let position              = Vector2::new(x    , y);
        let data                  = DOMContainerData::new(position, dimensions);
        let intersection_observer = None;
        let mut ret = Self {dom,intersection_observer,data};

        ret.init_listeners();
        ret
    }
    pub fn from_id(dom_id:&str) -> Result<Self> {
        let dom : HtmlElement = dyn_into(get_element_by_id(dom_id)?)?;
        Ok(Self::from_element(dom))
    }

    fn init_listeners(&mut self) {
        self.init_intersection_listener();
    }

    fn init_intersection_listener(&mut self) {
        let data = self.data.clone();
        let closure = Closure::new(move |x, y, width, height| {
            data.set_position(Vector2::new(x as f32, y as f32));
            data.set_dimensions(Vector2::new(width as f32, height as f32));
        });
        let observer = IntersectionObserver::new(&self.dom, closure);
        self.intersection_observer = Some(observer);
    }

    /// Sets the Scene DOM's dimensions.
    pub fn set_dimensions(&mut self, dimensions:Vector2<f32>) {
        self.dom.set_property_or_panic("width" , format!("{}px", dimensions.x));
        self.dom.set_property_or_panic("height", format!("{}px", dimensions.y));
        self.data.set_dimensions(dimensions);
    }

    /// Gets the Scene DOM's position.
    pub fn position(&self) -> Vector2<f32> {
        self.data.position()
    }

    /// Gets the Scene DOM's dimensions.
    pub fn dimensions(&self) -> Vector2<f32> {
        self.data.dimensions()
    }

    /// Adds a ResizeCallback.
    pub fn add_resize_callback<T:ResizeCallback>(&mut self, callback:T) {
        self.data.add_resize_callback(callback);
    }

    /// Adds a PositionCallback.
    pub fn add_position_callback<T:PositionCallback>(&mut self, callback:T) {
        self.data.add_position_callback(callback);
    }
}