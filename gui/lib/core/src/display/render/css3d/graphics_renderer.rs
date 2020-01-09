#![allow(missing_docs)]

// use super::Camera;
use crate::prelude::*;
use super::DOMContainer;
use super::ResizeCallback;
use crate::system::web::Result;

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
        let container = DOMContainer::from_id(dom_id)?;
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
    pub fn add_resize_callback<T:ResizeCallback>(&mut self, callback : T) {
        self.container.add_resize_callback(callback);
    }
}
