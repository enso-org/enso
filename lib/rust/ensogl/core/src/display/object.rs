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
    }

    impl<Host> Drop for ObjectWithFrp<Host> {
        fn drop(&mut self) {
            self.instance.unset_parent();
        }
    }
}

pub mod component {
    use crate::application::Application;
    use crate::prelude::*;

    use crate::display::object;
    use crate::display::object::Instance;
    use crate::display::object::WeakInstance;
    use crate::display::Object;
    use crate::display::Scene;

    #[derive(Debug)]
    pub struct Component<Frp: 'static, Model: 'static> {
        app:            Application,
        display_object: object::Instance,
        frp:            std::mem::ManuallyDrop<Frp>,
        model:          std::mem::ManuallyDrop<Model>,
    }

    impl<Frp: 'static, Model: 'static> Deref for Component<Frp, Model> {
        type Target = Frp;

        fn deref(&self) -> &Self::Target {
            &self.frp
        }
    }

    impl<Frp: 'static, Model: 'static> Component<Frp, Model> {
        pub fn new(
            app: &Application,
            frp: Frp,
            model: Model,
            display_object: object::Instance,
        ) -> Self {
            Self {
                app: app.clone_ref(),
                display_object,
                frp: std::mem::ManuallyDrop::new(frp),
                model: std::mem::ManuallyDrop::new(model),
            }
        }

        pub fn frp(&self) -> &Frp {
            &self.frp
        }

        pub fn model(&self) -> &Model {
            &self.model
        }
    }

    impl<Frp: 'static, Model: 'static> Drop for Component<Frp, Model> {
        fn drop(&mut self) {
            DEBUG!("DROPPING");
            self.display_object.unset_parent();
            unsafe {
                let frp = std::mem::ManuallyDrop::take(&mut self.frp);
                let model = std::mem::ManuallyDrop::take(&mut self.model);
                self.app.display.collect_garbage(frp);
                self.app.display.collect_garbage(model);
            }
        }
    }

    impl<Frp: 'static, Model: 'static> Object for Component<Frp, Model> {
        fn display_object(&self) -> &Instance<Scene> {
            &self.display_object
        }
    }
}
