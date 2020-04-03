//! This module defines an abstraction for DOM shapes and provides utility for efficient shape
//! change tracking which does not cause a reflow.
//! Learn more: https://gist.github.com/paulirish/5d52fb081b3570c81e3a

use crate::control::callback;
use crate::prelude::*;
use crate::system::web::resize_observer::ResizeObserver;
use crate::system::web;
use nalgebra::Vector2;
use wasm_bindgen::prelude::Closure;



// =============
// === Shape ===
// =============

// === Shape ===

use shapely::shared;

shared! { Shape
/// Contains information about DOM element shape and provides utils for querying the size both in
/// DOM pixel units as well as device pixel units.
#[derive(Debug)]
##[derive(Clone,Copy)]
pub struct ShapeData {
    width       : f32,
    height      : f32,
    pixel_ratio : f32
}

impl {
    /// Constructor.
    pub fn new() -> Self {
        let width       = 0.0;
        let height      = 0.0;
        let pixel_ratio = web::device_pixel_ratio() as f32;
        Self {width,height,pixel_ratio}
    }

    /// Getter.
    pub fn width(&self) -> f32 {
        self.width
    }

    /// Getter.
    pub fn height(&self) -> f32 {
        self.height
    }

    /// Getter.
    pub fn pixel_ratio(&self) -> f32 {
        self.pixel_ratio
    }

    /// Dimension setter in DOM pixel units. Use `device_pixels` to switch to device units.
    pub fn set(&mut self, width:f32, height:f32) {
        self.width  = width;
        self.height = height;
    }

    /// Sets the size to the size of the provided element. This operation is slow as it causes
    /// reflow.
    pub fn set_from_element_with_reflow(&mut self, element:&web::HtmlElement) {
        let bbox   = element.get_bounding_client_rect();
        let width  = bbox.width()  as f32;
        let height = bbox.height() as f32;
        self.set(width,height);
    }
}}

impl ShapeData {
    /// Switched to device pixel units. On low-dpi screens device pixels map 1:1 with DOM pixels.
    /// On high-dpi screens, a single device pixel is often mapped to 2 or 3 DOM pixels.
    pub fn device_pixels(&self) -> Self {
        let width  = self.width  * self.pixel_ratio;
        let height = self.height * self.pixel_ratio;
        Self {width,height,..*self}
    }
}

impl Shape {
    /// Constructor.
    pub fn from_element_with_reflow(element:&web::HtmlElement) -> Self {
        let this = Self::default();
        this.set_from_element_with_reflow(element);
        this
    }

    /// Current value of the shape.
    pub fn current(&self) -> ShapeData {
        *self.rc.borrow()
    }
}

impl Default for Shape {
    fn default() -> Self {
        Self::new()
    }
}

impl Default for ShapeData {
    fn default() -> Self {
        Self::new()
    }
}

impl Into<Vector2<f32>> for &Shape {
    fn into(self) -> Vector2<f32> {
        Vector2::new(self.width(),self.height())
    }
}



// ======================
// === WithKnownShape ===
// ======================

/// A wrapper for `HtmlElement` or anything which derefs to it. It tracks the element size without
/// causing browser reflow.
#[derive(Clone,CloneRef,Debug,Shrinkwrap)]
#[clone_ref(bound="T:CloneRef")]
pub struct WithKnownShape<T=web_sys::HtmlElement> {
    #[shrinkwrap(main_field)]
    dom       : T,
    shape     : Shape,
    observer  : Rc<ResizeObserver>,
    on_resize : Rc<RefCell<callback::Registry1<ShapeData>>>,
}

impl<T> WithKnownShape<T> {
    /// Constructor.
    pub fn new(dom:&T) -> Self
        where T : Clone + AsRef<web::JsValue> + Into<web_sys::HtmlElement> {
        let dom          = dom.clone();
        let html_element = dom.clone().into();
        let shape        = Shape::from_element_with_reflow(&html_element);
        let on_resize    = Rc::new(RefCell::new(callback::Registry1::default()));
        let callback     = Closure::new(enclose!((shape,on_resize) move |width,height| {
            shape.set(width,height);
            on_resize.borrow_mut().run_all(&shape.current())
        }));
        let observer = Rc::new(ResizeObserver::new(dom.as_ref(),callback));
        Self {dom,shape,observer,on_resize}
    }

    /// Attach a new callback which will fire whenever the object will be resized.
    pub fn on_resize<F:callback::CallbackMut1Fn<ShapeData>>(&self, callback:F) -> callback::Handle {
        self.on_resize.borrow_mut().add(callback)
    }

    /// Get the current shape of the object.
    pub fn shape(&self) -> &Shape {
        &self.shape
    }
}

impl From<WithKnownShape<web::HtmlDivElement>> for WithKnownShape<web::EventTarget> {
    fn from(t:WithKnownShape<web::HtmlDivElement>) -> Self {
        let dom       = t.dom.into();
        let shape     = t.shape;
        let observer  = t.observer;
        let on_resize = t.on_resize;
        Self {dom,shape,observer,on_resize}
    }
}

impl From<WithKnownShape<web::HtmlElement>> for WithKnownShape<web::EventTarget> {
    fn from(t:WithKnownShape<web::HtmlElement>) -> Self {
        let dom       = t.dom.into();
        let shape     = t.shape;
        let observer  = t.observer;
        let on_resize = t.on_resize;
        Self {dom,shape,observer,on_resize}
    }
}
