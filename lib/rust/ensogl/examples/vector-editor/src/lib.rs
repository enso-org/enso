//! Example scene showing the usage of built-in vector editor component.
//!
//! TODO[WD]: This is work in progress and will be changed in the upcoming PRs.

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]

use ensogl_core::display::shape::compound::rectangle::*;
use ensogl_core::display::world::*;
use ensogl_core::prelude::*;

use ensogl_core::control::io::mouse;
use ensogl_core::data::color;
use ensogl_core::display;
use ensogl_core::display::navigation::navigator::Navigator;
use ensogl_core::display::object::ObjectOps;



// ==============
// === Events ===
// ==============

#[derive(Clone, CloneRef, Debug, Default)]
pub struct MouseOver;


// ============
// === Glob ===
// ============

pub mod glob {
    use super::*;
    ensogl_core::define_endpoints_2! {
        Input {
        }
        Output {
        }
    }
}

// ===========
// === FRP ===
// ===========

ensogl_core::define_endpoints_2! {
    Input {
    }
    Output {
    }
}


#[derive(Derivative, CloneRef, Debug, Deref)]
#[derivative(Clone(bound = ""))]
pub struct VectorEditor<T> {
    #[deref]
    pub frp:        Frp,
    display_object: display::object::Instance,
    model:          Rc<RefCell<Model<T>>>,
}

#[derive(Debug, Derivative)]
#[derivative(Default(bound = ""))]
pub struct Model<T> {
    items: Vec<T>,
}

impl<T> VectorEditor<T> {
    pub fn new() -> Self {
        let frp = Frp::new();
        let display_object = display::object::Instance::new();
        let model = default();
        display_object.use_auto_layout().set_gap((10.0, 10.0));
        Self { frp, display_object, model }.init()
    }

    fn init(self) -> Self {
        let network = self.frp.network();
        let event_handler = self.display_object.on_event::<mouse::Up>();
        frp::extend! { network
            eval_ event_handler ([] {
                warn!("Mouse up in parent");
            });
        }
        self
    }
}

impl<T: display::Object> VectorEditor<T> {
    fn append(&self, item: T) {
        self.add_child(&item);
        self.model.borrow_mut().items.push(item);
    }
}

impl<T> display::Object for VectorEditor<T> {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}

impl<T> Default for VectorEditor<T> {
    fn default() -> Self {
        Self::new()
    }
}


// ===================
// === Entry Point ===
// ===================

/// The example entry point.
#[entry_point]
#[allow(dead_code)]
pub fn main() {
    let world = World::new().displayed_in("root");
    let scene = &world.default_scene;
    let camera = scene.camera().clone_ref();
    let navigator = Navigator::new(scene, &camera);

    let vector_editor = VectorEditor::<Rectangle>::new();


    let shape1 = Circle().build(|t| {
        t.set_size(Vector2::new(100.0, 100.0))
            .set_color(color::Rgba::new(0.5, 0.0, 0.0, 0.3))
            .set_inset_border(5.0)
            .set_border_color(color::Rgba::new(0.0, 0.0, 1.0, 1.0))
            .keep_bottom_left_quarter();
    });
    let shape2 = RoundedRectangle(10.0).build(|t| {
        t.set_size(Vector2::new(100.0, 100.0))
            .set_color(color::Rgba::new(0.5, 0.0, 0.0, 0.3))
            .set_inset_border(5.0)
            .set_border_color(color::Rgba::new(0.0, 0.0, 1.0, 1.0));
    });


    let glob_frp = glob::Frp::new();
    let glob_frp_network = glob_frp.network();

    let shape1_over = shape1.on_event::<mouse::Over>();
    frp::extend! { glob_frp_network
        eval_ shape1_over ([] {
            warn!("Shape 1 over");
        });
    }

    vector_editor.append(shape1);
    vector_editor.append(shape2);

    let root = display::object::Instance::new();
    root.set_size(Vector2::new(300.0, 100.0));
    root.add_child(&vector_editor);
    world.add_child(&root);

    world.keep_alive_forever();
    mem::forget(glob_frp);
    mem::forget(navigator);
    mem::forget(root);
    mem::forget(vector_editor);
}
