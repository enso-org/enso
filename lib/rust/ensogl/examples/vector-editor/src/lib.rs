//! Example scene showing the usage of built-in vector editor component.
//!
//! TODO[WD]: This is work in progress and will be changed in the upcoming PRs.

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
#![recursion_limit = "256"]

use ensogl_core::display::shape::compound::rectangle::*;
use ensogl_core::display::world::*;
use ensogl_core::prelude::*;

use ensogl_core::control::io::mouse;
use ensogl_core::data::color;
use ensogl_core::display;
use ensogl_core::display::navigation::navigator::Navigator;
use ensogl_core::display::object::ObjectOps;
use ensogl_core::Animation;

const DRAG_THRESHOLD: f32 = 4.0;

// FIXME: to be parametrized
const GAP: f32 = 10.0;

/// If set to true, animations will be running slow. This is useful for debugging purposes.
pub const DEBUG_ANIMATION_SLOWDOWN: bool = true;

/// Spring factor for animations. If [`DEBUG_ANIMATION_SLOWDOWN`] is set to true, this value will be
/// used for animation simulators.
pub const DEBUG_ANIMATION_SPRING_FACTOR: f32 = if DEBUG_ANIMATION_SLOWDOWN { 0.05 } else { 1.0 };

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



// ==============
// === Spacer ===
// ==============

mod spacer {
    use super::*;
    ensogl_core::define_endpoints_2! {
        Input {
            set_init_size (f32),
            set_target_size (f32),
        }
        Output {
        }
    }

    #[derive(Debug, Clone, CloneRef, Deref)]
    pub struct Spacer {
        model: Rc<SpacerModel>,
    }

    #[derive(Debug, Default, Deref)]
    pub struct SpacerModel {
        #[deref]
        pub frp:  spacer::Frp,
        root:     display::object::Instance,
        self_ref: RefCell<Option<Rc<SpacerModel>>>,
    }

    impl Spacer {
        pub fn new() -> Self {
            let model = SpacerModel::default();
            let model = Rc::new(model);
            let root = model.root.clone_ref();

            let network = &model.frp.network();
            let size_animation = Animation::<f32>::new(network);
            size_animation.simulator.update_spring(|s| s * DEBUG_ANIMATION_SPRING_FACTOR);
            frp::extend! { network
                size_animation.target <+  model.frp.private.input.set_init_size;
                size_animation.skip <+ model.frp.private.input.set_init_size.constant(());

                trace model.frp.private.input.set_target_size;
                size_animation.target <+ model.frp.private.input.set_target_size;
                eval size_animation.value ((t) root.set_size_x(*t););

                trace size_animation.value;
            }
            Self { model }
        }

        pub fn downgrade(&self) -> WeakSpacer {
            WeakSpacer { model: Rc::downgrade(&self.model) }
        }

        pub fn collapse(&self) {
            *self.self_ref.borrow_mut() = Some(self.model.clone());
            self.set_target_size(0.0);
        }
    }

    impl display::Object for Spacer {
        fn display_object(&self) -> &display::object::Instance {
            &self.root
        }
    }

    #[derive(Debug, Clone, CloneRef, Deref)]
    pub struct WeakSpacer {
        model: Weak<SpacerModel>,
    }

    impl WeakSpacer {
        pub fn upgrade(&self) -> Option<Spacer> {
            self.model.upgrade().map(|model| Spacer { model })
        }
    }
}

use spacer::Spacer;
use spacer::WeakSpacer;


// ===========
// === FRP ===
// ===========

#[derive(Debug)]
pub enum Item<T> {
    Regular(T),
    Spacer(Spacer),
    WeakSpacer(WeakSpacer),
}

impl<T: display::Object> Item<T> {
    pub fn is_regular(&self) -> bool {
        matches!(self, Self::Regular(_))
    }
    pub fn is_spacer(&self) -> bool {
        matches!(self, Self::Spacer(_))
    }

    pub fn display_object(&self) -> Option<display::object::Instance> {
        match self {
            Item::Regular(t) => Some(t.display_object().clone_ref()),
            Item::Spacer(t) => Some(t.display_object().clone_ref()),
            Item::WeakSpacer(t) => t.upgrade().map(|t| t.display_object().clone_ref()),
        }
    }

    pub fn computed_size(&self) -> Vector2 {
        self.display_object().map(|t| t.computed_size()).unwrap_or_default()
    }

    pub fn set_margin_left(&self, margin: f32) {
        if let Some(display_object) = self.display_object() {
            display_object.set_margin_left(margin);
        }
    }
}


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
    root:           display::object::Instance,
    layouted_elems: display::object::Instance,
    dragged_elems:  display::object::Instance,
    model:          Rc<RefCell<Model<T>>>,
}

#[derive(Debug, Derivative)]
#[derivative(Default(bound = ""))]
pub struct Model<T> {
    dragged_item: Option<Item<T>>,
    items:        Vec<Item<T>>,
}

impl<T: display::Object + 'static> VectorEditor<T> {
    pub fn new() -> Self {
        let frp = Frp::new();
        let root = display::object::Instance::new();
        let layouted_elems = display::object::Instance::new();
        let dragged_elems = display::object::Instance::new();
        root.add_child(&layouted_elems);
        root.add_child(&dragged_elems);
        let model = default();
        layouted_elems.use_auto_layout(); //.set_gap((GAP, GAP));
        Self { frp, root, layouted_elems, dragged_elems, model }.init()
    }

    fn init(self) -> Self {
        let scene = scene();
        let network = self.frp.network();
        let root = &self.root;
        let layouted_elems = &self.layouted_elems;
        let dragged_elems = &self.dragged_elems;
        let model = &self.model;

        let on_down = self.layouted_elems.on_event_capturing::<mouse::Down>();
        let on_up = scene.on_event::<mouse::Up>();
        let on_move = scene.on_event::<mouse::Move>();
        frp::extend! { network
            // Do not pass events to children, as we don't know whether we are about to drag
            // them yet.
            eval on_down ([] (event) event.stop_propagation());
            target <= on_down.map(|event| event.target());

            target_pos_on_down <- target.map(|t| t.xy());
            pressed <- bool(&on_up, &on_down);
            glob_pos_on_down <- on_down.map(|event| event.client_centered());
            on_move_pressed <- on_move.gate(&pressed);
            glob_pos_on_move <- on_move_pressed.map(|event| event.client_centered());
            glob_pos_offset_on_move <- map2(&glob_pos_on_move, &glob_pos_on_down, |a, b| a - b);

            // Discover whether the elements are dragged. They need to be moved vertically by at
            // least the [`DRAG_THRESHOLD`].
            threshold_gate <- glob_pos_offset_on_move.map(|t| t.y.abs() >= DRAG_THRESHOLD);
            trigger <- on_move_pressed.gate(&threshold_gate);
            status <- bool(&on_up, &trigger).on_change();
            start <- status.on_true();
            target_on_start <- target.sample(&start);

            // Re-parent the dragged element.
            eval target_on_start ((t) dragged_elems.add_child(&t));
            eval target_on_start([model, layouted_elems] (t) model.borrow_mut().set_as_dragged_item(&layouted_elems, t));
            // center_points <- target_on_start.map(f_!(model.borrow().elems_center_points()));
            // trace center_points;

            // Move the dragged element.
            target_new_pos <- map2(&glob_pos_offset_on_move, &target_pos_on_down, |a, b| a + b);
            _eval <- target_new_pos.map2(&target, |pos, t| t.set_xy(*pos));

            local_pos_on_drop <- on_up.map(f!([root, model] (event) model.borrow().screen_to_object_space(&root, event.client_centered()) ));
            insert_index <- local_pos_on_drop.map(f!((pos) model.borrow().insert_index(pos.x)));
            trace insert_index;

            eval insert_index ([model, layouted_elems] (index) {
                let mut model = model.borrow_mut();
                model.unset_dragged_item(*index);
                model.redraw_items(&layouted_elems);
            });


        }
        self
    }
}

impl<T: display::Object> VectorEditor<T> {
    fn append(&self, item: T) {
        self.layouted_elems.add_child(&item);
        let mut model = self.model.borrow_mut();
        model.items.push(Item::Regular(item));
        model.reset_margins();
    }
}

impl<T: display::Object> Model<T> {
    fn redraw_items(&self, layouted_elems: &display::object::Instance) {
        for item in &self.items {
            if let Some(display_object) = item.display_object() {
                layouted_elems.add_child(&display_object);
            }
        }
    }

    fn reset_margins(&self) {
        warn!("=== reset_margins ===");
        let mut first_elem = true;
        for item in &self.items {
            match item {
                Item::WeakSpacer(_) => {
                    warn!("WeakSpacer");
                }
                Item::Spacer(_) => {
                    warn!("Spacer");
                    first_elem = false;
                }
                Item::Regular(_) => {
                    warn!("Regular");
                    if !first_elem {
                        item.set_margin_left(GAP);
                    } else {
                        item.set_margin_left(0.0);
                    }
                    first_elem = false;
                }
            }
        }
    }

    // FIXME: refactor and generalize
    fn screen_to_object_space(
        &self,
        display_object: &display::object::Instance,
        screen_pos: Vector2,
    ) -> Vector2 {
        let scene = scene();
        let camera = scene.camera();
        let origin_world_space = Vector4(0.0, 0.0, 0.0, 1.0);
        let origin_clip_space = camera.view_projection_matrix() * origin_world_space;
        let inv_object_matrix = display_object.transformation_matrix().try_inverse().unwrap();

        let shape = scene.frp.shape.value();
        let clip_space_z = origin_clip_space.z;
        let clip_space_x = origin_clip_space.w * 2.0 * screen_pos.x / shape.width;
        let clip_space_y = origin_clip_space.w * 2.0 * screen_pos.y / shape.height;
        let clip_space = Vector4(clip_space_x, clip_space_y, clip_space_z, origin_clip_space.w);
        let world_space = camera.inversed_view_projection_matrix() * clip_space;
        (inv_object_matrix * world_space).xy()
    }

    fn set_as_dragged_item(
        &mut self,
        layouted_elems: &display::object::Instance,
        target: &display::object::Instance,
    ) {
        let index = self
            .items
            .iter()
            .position(|item| item.display_object().as_ref() == Some(target))
            .expect("Item not found");
        let spacer = Spacer::new();
        let elem_ref = &self.items[index];
        let gap_size = elem_ref.margin_left();
        spacer.set_init_size(elem_ref.computed_size().x + gap_size);
        let elem = mem::replace(&mut self.items[index], Item::Spacer(spacer));
        self.dragged_item = Some(elem);
        self.reset_margins();
        self.redraw_items(layouted_elems);
    }

    fn unset_dragged_item(&mut self, index: usize) {
        let item = self.dragged_item.take().expect("No dragged item");
        if let Some(existing_item) = self.items.get_mut(index) {
            if existing_item.is_spacer() {
                *existing_item = item;
            } else {
                self.items.insert(index, item);
            }
        } else {
            self.items.push(item);
        }
        for item in &mut self.items {
            if let Item::Spacer(spacer) = item {
                spacer.collapse();
                *item = Item::WeakSpacer(spacer.downgrade());
            }
        }
        self.reset_margins();
    }

    fn insert_dragged_item(&mut self, index: usize) {
        let elem = self.dragged_item.take().expect("No dragged item");
        self.items.insert(index, elem);
    }

    fn elems_center_points(&self) -> Vec<f32> {
        let mut centers = Vec::new();
        let mut current = 0.0;
        for item in &self.items {
            let size = item.computed_size();
            current += size.x / 2.0;
            centers.push(current);
            current += size.x / 2.0 + GAP;
        }
        centers
    }

    fn insert_index(&self, x: f32) -> usize {
        let center_points = self.elems_center_points();
        let mut index = 0;
        for center in center_points {
            if x < center {
                break;
            }
            index += 1;
        }
        index
    }
}

impl<T> display::Object for VectorEditor<T> {
    fn display_object(&self) -> &display::object::Instance {
        &self.root
    }
}

impl<T: display::Object + 'static> Default for VectorEditor<T> {
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
    let shape3 = RoundedRectangle(10.0).build(|t| {
        t.set_size(Vector2::new(100.0, 100.0))
            .set_color(color::Rgba::new(0.5, 0.0, 0.0, 0.3))
            .set_inset_border(5.0)
            .set_border_color(color::Rgba::new(0.0, 1.0, 1.0, 1.0));
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
    vector_editor.append(shape3);

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
