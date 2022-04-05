//! Display object abstraction and related utilities.

use crate::prelude::*;

use crate::display::camera::Camera2d;
use crate::display::scene::layer::Layer;


// ==============
// === Export ===
// ==============

pub mod class;
pub mod transform;

pub use class::Any;
pub use class::*;



// ==============
// === Traits ===
// ==============

/// Common traits.
pub mod traits {
    // Read the Rust Style Guide to learn more about the used naming.
    pub use super::Object as TRAIT_Object;
    pub use super::ObjectOps as TRAIT_ObjectOps;
}



// =============================
// === InstanceWithSublayers ===
// =============================

/// A wrapper around [`Instance`] with attached sublayers. It is used for
/// dynamic creation and managing of additional on-demand sublayers needed for various UI
/// components.
///
/// When the Instance is added to another layer, [`Sublayers::remove_from_layer`] and
/// [`Sublayers::add_to_layer`] methods of the `sublayers` field will be called with previous and
/// new layers respectively.
///
/// See [`MaskingSublayers`] docs for one of the use cases.
#[derive(Debug, Clone, CloneRef)]
pub struct InstanceWithSublayers<S: CloneRef> {
    inner:         Instance,
    /// Attached sublayers. Should implement [`Sublayers`] trait.
    pub sublayers: S,
}

impl<S: CloneRef> Deref for InstanceWithSublayers<S> {
    type Target = Instance;
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<S: CloneRef + Sublayers + 'static> InstanceWithSublayers<S> {
    /// Constructor. Sets a `on_scene_layers_changed` callback for `instance`.
    pub fn new(instance: Instance, sublayers: S) -> Self {
        instance.set_on_scene_layer_changed(f!([sublayers](_, from, to) {
            for layer in from {
                if let Some(layer) = layer.upgrade() {
                    sublayers.remove_from_layer(&layer);
                }
            }
            for layer in to {
                if let Some(layer) = layer.upgrade() {
                    sublayers.add_to_layer(&layer);
                }
            }
        }));
        Self { inner: instance, sublayers }
    }
}



// ========================
// === MaskingSublayers ===
// ========================

/// Sublayers for easier masking with arbitrary shapes.
///
/// One of the examples might be a `ScrollArea` component implementation. To clip the area's content
/// (so that it is displayed only inside its borders) we use `MaskingSublayers` with two additional
/// layers: one for the area's content and another one for the masking shape. These sublayers will
/// automatically follow the main [`Instance`] if it moves between different
/// layers and will be dropped together with the main object.
#[derive(Debug, Clone, CloneRef)]
pub struct MaskingSublayers {
    /// Contains the objects that needs to be masked.
    pub content: Layer,
    /// Contains a masking shape. See docs in [`crate::display::scene::layer`] for information on
    /// masking.
    pub mask:    Layer,
}

impl MaskingSublayers {
    /// Constructor. The passed `camera` will be used to render created sublayers.
    pub fn new(logger: &Logger, camera: &Camera2d) -> Self {
        let content = Layer::new_with_cam(logger.sub("ContentLayer"), camera);
        let mask = Layer::new_with_cam(logger.sub("MaskLayer"), camera);
        content.set_mask(&mask);
        Self { content, mask }
    }
}



// =================
// === Sublayers ===
// =================

/// The common API for all `sublayers` in [`InstanceWithSublayers`].
pub trait Sublayers {
    /// Called when the object is removed from a layer. Might be called multiple times in a row if
    /// the object is removed from multiple layers.
    fn remove_from_layer(&self, from: &Layer);
    /// Called when the object is added to a layer. Might be called multiple times in a row if the
    /// object is added to multiple layers.
    fn add_to_layer(&self, to: &Layer);
}

impl Sublayers for MaskingSublayers {
    fn remove_from_layer(&self, from: &Layer) {
        from.remove_sublayer(&self.content);
        from.remove_sublayer(&self.mask);
    }

    fn add_to_layer(&self, to: &Layer) {
        to.add_sublayer(&self.content);
        to.add_sublayer(&self.mask);
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    use crate::application::Application;

    #[test]
    fn test_that_sublayers_are_dropped() {
        let app = Application::new("root");
        let logger = &app.logger;
        let camera = &app.display.default_scene.layers.main.camera();
        let display_object = Instance::new(&logger);
        let sublayers = MaskingSublayers::new(logger, camera);
        let display_object = InstanceWithSublayers::new(display_object, sublayers);
        let content_layer = display_object.sublayers.content.downgrade();
        assert!(content_layer.upgrade().is_some());
        drop(display_object);
        assert!(content_layer.upgrade().is_none());
    }
}
