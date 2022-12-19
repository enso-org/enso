//! Display object abstraction and related utilities.

use crate::prelude::*;

use crate::display::scene::layer::Layer;


// ==============
// === Export ===
// ==============

pub mod event;
pub mod instance;
pub mod layout;
pub mod transformation;

pub use instance::Any;
pub use instance::*;



// ==============
// === Traits ===
// ==============

/// Common traits.
pub mod traits {
    // Read the Rust Style Guide to learn more about the used naming.
    pub use super::AutoLayoutOps as TRAIT_AutoLayoutOps;
    pub use super::LayoutOps as TRAIT_LayoutOps;
    pub use super::Object as TRAIT_Object;
    pub use super::ObjectOps as TRAIT_ObjectOps;
}



// =========================
// === InstanceWithLayer ===
// =========================

/// A display object with a layer. When [`Instance`] is added to a scene layer, the `layer`
/// is attached to that layer using [Layer::add_sublayer]. Likewise, when the instance is
/// removed from a layer, the `layer` is detached using [Layer::remove_sublayer].
///
/// Most UI components are relying on predefined set of scene layers in
/// [`crate::display::scene::HardcodedLayers`]. But some features, like [masking layers with
/// arbitrary shapes][masking], require a layer per component instance. This struct is used to
/// create and manage such additional layers. See one of the use cases in
/// [`crate::display::scene::layer::MaskedLayer`] docs.
///
/// [masking]: crate::display::scene::layer::Layer
/// [Layer::add_sublayer]: crate::display::scene::layer::LayerModel::add_sublayer
/// [Layer::remove_sublayer]: crate::display::scene::layer::LayerModel::remove_sublayer
#[derive(Debug, Clone, CloneRef, Deref)]
#[clone_ref(bound = "L:CloneRef")]
#[allow(missing_docs)]
pub struct InstanceWithLayer<L> {
    #[deref]
    instance:  Instance,
    pub layer: L,
}

impl<L: CloneRef + AsRef<Layer> + 'static> InstanceWithLayer<L> {
    /// Constructor. Sets a `on_scene_layers_changed` callback for `instance`.
    ///
    /// This function does not act on layers itself, it just sets a callback for future use. This
    /// callback ensures that the `layer` will always be attached to the same layer the
    /// `instance` is attached to.
    pub fn new(instance: Instance, layer: L) -> Self {
        let network = &instance.network;
        frp::extend! { network
            eval instance.on_layer_change ([layer] ((_, source, destination)) {
                if let Some(src_layer) = source {
                    src_layer.remove_sublayer(layer.as_ref());
                }
                if let Some(dst_layer) = destination {
                    dst_layer.add_sublayer(layer.as_ref());
                }
            });
        }
        Self { instance, layer }
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;
    use crate::display::scene::layer;

    #[test]
    fn test_that_sublayers_are_dropped() {
        let display_object = Instance::new();
        let layer = layer::Masked::new();
        let display_object = InstanceWithLayer::new(display_object, layer);
        let content_layer = display_object.layer.masked_layer.downgrade();
        assert!(content_layer.upgrade().is_some());
        drop(display_object);
        assert!(content_layer.upgrade().is_none());
    }
}
