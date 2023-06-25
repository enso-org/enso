//! This module defines an abstraction for DOM shapes and provides utility for efficient shape
//! change tracking which does not cause a reflow.
//! Learn more: https://gist.github.com/paulirish/5d52fb081b3570c81e3a

use crate::prelude::*;

use crate::frp;
use crate::system::web;
use crate::system::web::resize_observer::ResizeObserver;

use nalgebra::Vector2;
use web::Closure;



// =============
// === Shape ===
// =============

/// Shape of the element. Includes information about pixel ratio of the screen and allows converting
/// the units to device pixel units.
#[derive(Clone, Copy, Debug)]
#[allow(missing_docs)]
pub struct Shape {
    pub width:       f32,
    pub height:      f32,
    pub pixel_ratio: f32,
}

impl Shape {
    /// Constructor.
    pub fn new(width: f32, height: f32, pixel_ratio: Option<f32>) -> Self {
        let pixel_ratio = pixel_ratio.unwrap_or_else(|| web::window.device_pixel_ratio() as f32);
        Self { width, height, pixel_ratio }
    }

    /// Compute shape of the provided element. Note that using it causes a reflow.
    pub fn new_from_element_with_reflow(element: &web::HtmlElement) -> Self {
        let mut shape = Self::default();
        shape.set_from_element_with_reflow(element);
        shape
    }

    /// Compute shape of the provided element. Note that using it causes a reflow.
    pub fn set_from_element_with_reflow(&mut self, element: &web::HtmlElement) {
        let bbox = element.get_bounding_client_rect();
        self.width = bbox.width() as f32;
        self.height = bbox.height() as f32;
    }

    /// Switched to device pixel units. On low-dpi screens device pixels map 1:1 with DOM pixels.
    /// On high-dpi screens, a single device pixel is often mapped to 2 or 3 DOM pixels.
    pub fn device_pixels(&self) -> Self {
        let width = self.width * self.pixel_ratio;
        let height = self.height * self.pixel_ratio;
        Self { width, height, ..*self }
    }

    /// Center of the shape.
    pub fn center(&self) -> Vector2<f32> {
        Vector2::new(self.width / 2.0, self.height / 2.0)
    }

    /// Create a new shape with the provided device pixel ratio. In case the provided value is
    /// [`None`], the device pixel ratio reported by the browser will be used.
    pub fn with_device_pixel_ratio(self, ratio: Option<f32>) -> Self {
        Self {
            pixel_ratio: ratio.unwrap_or_else(|| web::window.device_pixel_ratio() as f32),
            ..self
        }
    }
}

impl Default for Shape {
    fn default() -> Self {
        let width = 100.0;
        let height = 100.0;
        let pixel_ratio = web::window.device_pixel_ratio() as f32;
        Self { width, height, pixel_ratio }
    }
}

impl From<Shape> for Vector2<f32> {
    fn from(value: Shape) -> Self {
        Vector2::new(value.width, value.height)
    }
}

impl From<&Shape> for Vector2<f32> {
    fn from(value: &Shape) -> Self {
        Vector2::new(value.width, value.height)
    }
}



// ======================
// === WithKnownShape ===
// ======================

/// A wrapper for `HtmlElement` or anything which derefs to it. It tracks the element size without
/// causing browser reflow.
#[derive(Clone, CloneRef, Debug, Deref)]
#[clone_ref(bound = "T:CloneRef")]
#[allow(missing_docs)]
pub struct WithKnownShape<T = web::HtmlElement> {
    #[deref]
    dom:                    T,
    network:                frp::Network,
    pub shape:              frp::Sampler<Shape>,
    shape_source:           frp::Source<Shape>,
    observer:               Rc<ResizeObserver>,
    overridden_pixel_ratio: Rc<Cell<Option<f32>>>,
}

impl<T> WithKnownShape<T> {
    /// Constructor.
    pub fn new(dom: &T) -> Self
    where T: Clone + AsRef<web::JsValue> + Into<web::HtmlElement> {
        let dom = dom.clone();
        let element = dom.clone().into();
        let overridden_pixel_ratio: Rc<Cell<Option<f32>>> = default();
        frp::new_network! { network
            shape_source <- source();
            shape        <- shape_source.sampler();
        };
        let callback = Closure::new(f!([shape_source, overridden_pixel_ratio] (w,h)
            shape_source.emit(Shape::new(w, h, overridden_pixel_ratio.get()))));
        let observer = Rc::new(ResizeObserver::new(dom.as_ref(), callback));
        shape_source.emit(Shape::new_from_element_with_reflow(&element));
        Self { dom, network, shape, shape_source, observer, overridden_pixel_ratio }
    }

    /// Override the device pixel ratio. If the provided value is [`None`], the device pixel ratio
    /// provided by the browser will be used.
    pub fn override_device_pixel_ratio(&self, ratio: Option<f32>) {
        if ratio != self.overridden_pixel_ratio.get() {
            self.overridden_pixel_ratio.set(ratio);
            let shape = self.shape.value().with_device_pixel_ratio(ratio);
            self.shape_source.emit(shape);
        }
    }

    /// Treat this object as if id had the provided shape. Note that this function does not cause
    /// the actual DOM object to change its shape. Useful for testing.
    pub fn override_shape(&self, shape: Shape) {
        self.shape_source.emit(shape);
    }

    /// Get the current shape of the object.
    pub fn shape(&self) -> Shape {
        self.shape.value()
    }

    /// Recompute the shape. Note that this function causes reflow.
    pub fn recompute_shape_with_reflow(&self)
    where T: Clone + Into<web::HtmlElement> {
        self.shape_source.emit(Shape::new_from_element_with_reflow(&self.dom.clone().into()))
    }
}

impl From<WithKnownShape<web::HtmlDivElement>> for WithKnownShape<web::EventTarget> {
    fn from(t: WithKnownShape<web::HtmlDivElement>) -> Self {
        let dom = t.dom.into();
        let network = t.network;
        let shape = t.shape;
        let shape_source = t.shape_source;
        let observer = t.observer;
        let overridden_pixel_ratio = t.overridden_pixel_ratio;
        Self { dom, network, shape, shape_source, observer, overridden_pixel_ratio }
    }
}

impl From<WithKnownShape<web::HtmlElement>> for WithKnownShape<web::EventTarget> {
    fn from(t: WithKnownShape<web::HtmlElement>) -> Self {
        let dom = t.dom.into();
        let network = t.network;
        let shape = t.shape;
        let shape_source = t.shape_source;
        let observer = t.observer;
        let overridden_pixel_ratio = t.overridden_pixel_ratio;
        Self { dom, network, shape, shape_source, observer, overridden_pixel_ratio }
    }
}
