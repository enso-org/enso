// use super::Camera;
use crate::prelude::*;
use super::DOMContainer;
use super::ResizeCallbackFn;
use crate::system::web::Result;
use crate::system::web::StyleSetter;

use nalgebra::Vector2;

// ========================
// === GraphicsRenderer ===
// ========================

/// Base structure for our Renderers.
#[derive(Debug)]
pub struct GraphicsRenderer {
    pub container : DOMContainer
}

impl GraphicsRenderer {
    pub fn new(dom_id: &str) -> Result<Self> {
        let container = DOMContainer::new(dom_id)?;
        container.dom.set_property_or_panic("overflow", "hidden");
        Ok(Self { container })
    }

    /// Sets the Scene Renderer's dimensions.
    pub fn set_dimensions(&mut self, dimensions : Vector2<f32>) {
        self.container.set_dimensions(dimensions);
    }

    /// Gets the Scene Renderer's dimensions.
    pub fn dimensions(&self) -> Vector2<f32> {
        self.container.dimensions()
    }

    /// Adds a ResizeCallback.
    pub fn add_resize_callback<T>(&mut self, callback : T)
    where T : ResizeCallbackFn {
        self.container.add_resize_callback(callback);
    }
}
