//! Display object abstraction and related utilities.

use crate::prelude::*;

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



// =================================
// === InstanceWithAttachedLayer ===
// =================================

/// When [`Instance`] is added to a new scene layer, the `layer` is attached to that new layer
/// using [Layer::add_sublayer]. Likewise, when the instance is removed from a layer, the
/// `layer` is detached using [Layer::remove_sublayer].
///
/// It is used for creation and managing of additional layers needed for various UI components.
/// See one of the use cases in [`crate::display::scene::layer::MaskedLayer`] docs.
///
/// [Layer::add_sublayer]: crate::display::scene::layer::LayerModel::add_sublayer
/// [Layer::remove_sublayer]: crate::display::scene::layer::LayerModel::remove_sublayer
#[derive(Debug, Clone, CloneRef, Deref)]
#[clone_ref(bound = "L:CloneRef")]
#[allow(missing_docs)]
pub struct InstanceWithAttachedLayer<L> {
    #[deref]
    instance:  Instance,
    pub layer: L,
}

impl<L: CloneRef + Deref<Target = Layer> + 'static> InstanceWithAttachedLayer<L> {
    /// Constructor. Sets a `on_scene_layers_changed` callback for `instance`.
    ///
    /// Calling this method does not immediately attaches `layer` to the `instance`'s layer, so it
    /// should be called before changing the layer of the `instance`.
    pub fn new(instance: Instance, layer: L) -> Self {
        instance.set_on_scene_layer_changed(f!([layer](_, source, destination) {
            for src_layer in source {
                src_layer.remove_sublayer(&*layer);
            }
            for dst_layer in destination {
                dst_layer.add_sublayer(&*layer);
            }
        }));
        Self { instance, layer }
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    use crate::application::Application;
    use crate::display::scene::layer::MaskedLayer;

    #[test]
    fn test_that_sublayers_are_dropped() {
        let app = Application::new("root");
        let logger = &app.logger;
        let camera = &app.display.default_scene.layers.main.camera();
        let display_object = Instance::new(&logger);
        let layer = MaskedLayer::new(logger, camera);
        let display_object = InstanceWithAttachedLayer::new(display_object, layer);
        let content_layer = display_object.layer.masked_object.downgrade();
        assert!(content_layer.upgrade().is_some());
        drop(display_object);
        assert!(content_layer.upgrade().is_none());
    }
}
