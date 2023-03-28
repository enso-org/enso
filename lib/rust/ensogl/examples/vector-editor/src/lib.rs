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


const DRAG_THRESHOLD: i32 = 4;

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
        let on_down_capturing = self.display_object.on_event_capturing::<mouse::Down>();
        let on_up = self.display_object.on_event::<mouse::Up>();
        let on_move = scene().on_event::<mouse::Move>();
        frp::extend! { [TRACE_ALL] network
            // Do not pass events to children, as we don't know whether we are about to drag
            // them yet.
            eval on_down_capturing ([] (event) event.stop_propagation());
            is_down <- bool(&on_up, &on_down_capturing);
            pos_on_down <- on_down_capturing.map(|event| event.screen());
            pos_on_move <- on_move.gate(&is_down).map(|event| event.screen());
            pos_offset_on_move <- map2(&pos_on_move, &pos_on_down, |a, b| a - b);

            // Discover whether the elements are dragged. They need to be moved vertically by at
            // least the [`DRAG_THRESHOLD`].
            reorder_gate <- pos_offset_on_move.map(|t| t.y.abs() >= DRAG_THRESHOLD);
            reorder_start <- pos_on_move.gate(&reorder_gate);
            is_reordering <- bool(&on_up, &reorder_start).on_change();
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

    let shape1_down = shape1.on_event::<mouse::Down>();
    frp::extend! { glob_frp_network
        eval_ shape1_down ([] {
            warn!("Shape 1 down");
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
