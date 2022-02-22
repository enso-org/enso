//! Display object abstraction and related utilities.

pub mod class;
pub mod transform;

pub use class::*;

pub use class::Any;



// ==============
// === Traits ===
// ==============

/// Common traits.
pub mod traits {
    // Read the Rust Style Guide to learn more about the used naming.
    pub use super::Object as TRAIT_Object;
    pub use super::ObjectOps as TRAIT_ObjectOps;
}


pub mod frp {
    use crate::prelude::*;

    use crate::display::object::Instance;
    use crate::display::Scene;

    use enso_frp as frp;

    pub struct ObjectWithFrp<Host = Scene> {
        instance: Instance<Host>,
        network:  frp::Network,
    }

    impl<Host> ObjectWithFrp<Host> {
        fn new(parent: impl AnyLogger, label: impl Str) -> Self {
            let logger: Logger = parent.sub(&label);
            let instance = Instance::new(logger);
            let network = frp::Network::new(label);
            Self { instance, network }
        }

        fn new_from_network(network: frp::Neto)
    }

    impl<Host> Drop for ObjectWithFrp<Host> {
        fn drop(&mut self) {
            self.instance.unset_parent();
        }
    }
}

pub mod component {
    use crate::prelude::*;

    use crate::display::{object, Scene};

    struct Essence<Frp, Model> {
        scene:          Scene,
        frp:            Option<Box<Frp>>,
        model:          Option<Box<Model>>,
        display_object: object::Instance,
    }

    impl<Frp, Model, Host> Drop for Essence<Frp, Model> {
        fn drop(&mut self) {
            self.display_object.unset_parent();
            self.scene.delayed_death.borrow_mut().push(self.frp.take().unwrap());
            self.scene.delayed_death.borrow_mut().push(self.model.take().unwrap());
        }
    }

    pub struct Component<Frp, Model, Host = Scene> {

    }
}
