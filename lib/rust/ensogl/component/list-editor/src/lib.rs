//! Example scene showing the usage of built-in vector editor component.
//!
//! TODO[WD]: This is work in progress and will be changed in the upcoming PRs.

#![recursion_limit = "256"]
// === Features ===
#![feature(let_chains)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]

use ensogl_core::display::shape::compound::rectangle::*;
use ensogl_core::display::world::*;
use ensogl_core::prelude::*;

use ensogl_core::application::Application;
use ensogl_core::control::io::mouse;
use ensogl_core::data::color;
use ensogl_core::display;
use ensogl_core::display::navigation::navigator::Navigator;
use ensogl_core::display::object::ObjectOps;
use ensogl_core::gui::cursor;
use ensogl_core::gui::cursor::Cursor;
use ensogl_core::Animation;



const DRAG_THRESHOLD: f32 = 4.0;

// FIXME: to be parametrized
const GAP: f32 = 10.0;

/// If set to true, animations will be running slow. This is useful for debugging purposes.
pub const DEBUG_ANIMATION_SLOWDOWN: bool = false;

pub const DEBUG_PLACEHOLDERS_VIZ: bool = true;

/// Spring factor for animations. If [`DEBUG_ANIMATION_SLOWDOWN`] is set to true, this value will be
/// used for animation simulators.
pub const DEBUG_ANIMATION_SPRING_FACTOR: f32 = if DEBUG_ANIMATION_SLOWDOWN { 0.1 } else { 1.0 };

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
// === Placeholder ===
// ==============

mod placeholder {
    use super::*;
    ensogl_core::define_endpoints_2! {
        Input {
            set_current_size(f32),
            set_target_size_no_animation (f32),
            set_target_size (f32),
            skip_animation(),
        }
        Output {
        }
    }

    #[derive(Debug, Clone, CloneRef, Deref)]
    pub struct Placeholder {
        model: Rc<SpacerModel>,
    }

    #[derive(Debug, Deref)]
    pub struct SpacerModel {
        #[deref]
        pub frp:        placeholder::Frp,
        root:           display::object::Instance,
        self_ref:       Rc<RefCell<Option<Rc<SpacerModel>>>>,
        collapsing:     Rc<Cell<bool>>,
        size_animation: Animation<f32>,
        _viz:           Option<Rectangle>,
    }

    impl SpacerModel {
        fn new() -> Self {
            let frp = placeholder::Frp::new();
            let root = display::object::Instance::new();
            let self_ref = default();
            let collapsing = default();
            let size_animation = Animation::<f32>::new(frp.network());
            let _viz = DEBUG_PLACEHOLDERS_VIZ.then(|| {
                let viz = RoundedRectangle(10.0).build(|t| {
                    t.set_size(Vector2::new(0.0, 20.0))
                        .allow_grow_x()
                        .set_color(color::Rgba::new(1.0, 0.0, 0.0, 1.0))
                        .set_inset_border(5.0)
                        .set_border_color(color::Rgba::new(0.0, 1.0, 1.0, 1.0));
                });
                root.add_child(&viz);
                viz
            });
            Self { frp, root, self_ref, collapsing, size_animation, _viz }
        }
    }

    impl Placeholder {
        pub fn new() -> Self {
            let model = SpacerModel::new();
            let model = Rc::new(model);
            let root = model.root.clone_ref();
            let collapsing = &model.collapsing;
            let self_ref = &model.self_ref;

            let network = &model.frp.network();
            let size_animation = &model.size_animation;
            size_animation.simulator.update_spring(|s| s * DEBUG_ANIMATION_SPRING_FACTOR);
            frp::extend! { network
                size_animation.target <+ model.frp.private.input.set_target_size_no_animation;
                size_animation.skip <+ model.frp.private.input.set_target_size_no_animation.constant(());
                size_animation.set_value <+ model.frp.private.input.set_current_size;
                size_animation.skip <+ model.frp.private.input.skip_animation;

                tg <- model.frp.private.input.set_target_size.on_change();
                size_animation.target <+ tg;
                eval size_animation.value ((t) { root.set_size_x(*t); });
                eval_ size_animation.on_end ([collapsing, self_ref] {
                    if collapsing.get() {
                        self_ref.borrow_mut().take();
                    }
                });
            }
            Self { model }
        }

        pub fn new_with_size(size: f32) -> Self {
            let placeholder = Self::new();
            placeholder.set_target_size_no_animation(size);
            placeholder
        }

        pub fn downgrade(&self) -> WeakPlaceholder {
            WeakPlaceholder { model: Rc::downgrade(&self.model) }
        }

        pub fn collapse(&self) {
            *self.self_ref.borrow_mut() = Some(self.model.clone());
            self.set_target_size(0.0);
            self.collapsing.set(true);
        }

        pub fn drop_self_ref(&self) {
            *self.self_ref.borrow_mut() = None;
        }

        pub fn reuse(&self) {
            self.drop_self_ref();
            self.collapsing.set(false);
        }

        pub fn add_to_size(&self, delta: f32) {
            self.size_animation.simulator.update_value(|v| v + delta);
        }
    }

    impl Default for Placeholder {
        fn default() -> Self {
            Self::new()
        }
    }

    impl display::Object for Placeholder {
        fn display_object(&self) -> &display::object::Instance {
            &self.root
        }
    }

    #[derive(Debug, Clone, CloneRef, Deref)]
    pub struct WeakPlaceholder {
        model: Weak<SpacerModel>,
    }

    impl WeakPlaceholder {
        pub fn upgrade(&self) -> Option<Placeholder> {
            self.model.upgrade().map(|model| Placeholder { model })
        }
    }
}

use placeholder::Placeholder;
use placeholder::WeakPlaceholder;


// ===========
// === FRP ===
// ===========

mod spot {
    use super::*;

    ensogl_core::define_endpoints_2! {
        Input {
            set_margin_left(f32),
            skip_anim(),
            skip_margin_anim(),
        }
        Output {
        }
    }

    #[derive(Debug)]
    pub struct DropSpot<T> {
        pub frp:         Frp,
        pub elem:        T,
        pub placeholder: Placeholder,
        pub debug:       Rc<Cell<bool>>,
    }

    impl<T: display::Object> DropSpot<T> {
        pub fn new(elem: T, init_width: f32) -> Self {
            let placeholder = Placeholder::new();
            let this = Self::new_from_placeholder(elem, placeholder);
            this.placeholder.set_target_size_no_animation(init_width);
            this.placeholder.frp.skip_animation();
            this
        }
        pub fn new_from_placeholder(elem: T, placeholder: Placeholder) -> Self {
            let frp = Frp::new();
            placeholder.add_child(&elem);
            let network = frp.network();
            let elem_display_object = elem.display_object();
            let margin_left = Animation::<f32>::new_with_init(network, 0.0);
            let elem_offset = Animation::<Vector2>::new_with_init(network, elem.position().xy());
            margin_left.simulator.update_spring(|s| s * DEBUG_ANIMATION_SPRING_FACTOR);
            elem_offset.simulator.update_spring(|s| s * DEBUG_ANIMATION_SPRING_FACTOR);
            let debug = Rc::new(Cell::new(false));

            frp::extend! { network
                margin_left.target <+ frp.set_margin_left;
                // fixme:
                elem_size <- elem_display_object.on_transformed.map(f!((_)
                    elem_display_object.computed_size().x
                ));
                target_size <= all_with(&elem_size, &margin_left.value, f!([debug](w, m) {
                    let out = w + m;
                    let out = if *w > 0.0 {
                        Some(out)
                    } else {
                        None
                    };
                    if debug.get() {
                        warn!("target_size: {} + {} = {:?}", w,m,out);
                    }
                    out
                }));
                placeholder.frp.set_target_size <+ target_size;
                _eval <- all_with(&margin_left.value, &elem_offset.value, f!((m, o) {
                    elem_display_object.set_xy(*o + Vector2(*m, 0.0));
                }));

                placeholder.frp.skip_animation <+ frp.skip_anim;
                margin_left.skip <+ frp.skip_anim;
                margin_left.skip <+ frp.skip_margin_anim;
                // eval margin_left.value((t) elem_display_object.set_x(*t));
                // eval elem_offset.value((t) elem_display_object.set_xy(*t));
            }
            elem_offset.target.emit(Vector2(0.0, 0.0));
            //fixme
            mem::forget(margin_left);
            mem::forget(elem_offset);


            Self { frp, elem, placeholder, debug }
        }

        pub fn set_margin_left(&self, margin: f32) {
            self.frp.set_margin_left.emit(margin)
        }
    }

    impl<T> display::Object for DropSpot<T> {
        fn display_object(&self) -> &display::object::Instance {
            self.placeholder.display_object()
        }
    }
}
use spot::DropSpot;

#[derive(Debug)]
pub enum Item<T> {
    Placeholder(Placeholder),
    WeakPlaceholder(WeakPlaceholder),
    DropSpot(DropSpot<T>),
}

impl<T: display::Object> Item<T> {
    pub fn is_placeholder(&self) -> bool {
        matches!(self, Self::Placeholder(_))
    }

    pub fn is_weak_placeholder(&self) -> bool {
        matches!(self, Self::WeakPlaceholder(_))
    }

    pub fn display_object(&self) -> Option<display::object::Instance> {
        match self {
            Item::Placeholder(t) => Some(t.display_object().clone_ref()),
            Item::WeakPlaceholder(t) => t.upgrade().map(|t| t.display_object().clone_ref()),
            Item::DropSpot(t) => Some(t.display_object().clone_ref()),
        }
    }

    pub fn computed_size(&self) -> Vector2 {
        self.display_object().map(|t| t.computed_size()).unwrap_or_default()
    }

    pub fn exists(&self) -> bool {
        match self {
            Item::Placeholder(_) => true,
            Item::DropSpot(_) => true,
            Item::WeakPlaceholder(t) => t.upgrade().is_some(),
        }
    }

    pub fn upgraded_weak_placeholder(&self) -> Option<Placeholder> {
        match self {
            Item::WeakPlaceholder(t) => t.upgrade(),
            _ => None,
        }
    }
}

impl<T> From<Placeholder> for Item<T> {
    fn from(placeholder: Placeholder) -> Self {
        Self::Placeholder(placeholder)
    }
}

pub type Index = usize;

#[derive(Clone, Copy, Debug, Default, Deref, DerefMut)]
pub struct Response<T> {
    #[deref]
    #[deref_mut]
    pub payload:         T,
    pub gui_interaction: bool,
}

impl<T> Response<T> {
    pub fn new(payload: T, gui_interaction: bool) -> Self {
        Self { payload, gui_interaction }
    }

    pub fn new_api_interaction(payload: T) -> Self {
        Self::new(payload, false)
    }

    pub fn new_gui_interaction(payload: T) -> Self {
        Self::new(payload, true)
    }
}

ensogl_core::define_endpoints_2! { <T: (frp::node::Data)>
    Input {
        /// Push a new element to the end of the list.
        push(Weak<T>),

        /// WARNING: not working yet, use `push` instead.
        insert((Index, Weak<T>)),
    }
    Output {
        /// Fires whenever a new element was added to the list.
        on_new_item(Response<(Index, Weak<T>)>),

        /// Request new item to be inserted at the provided index. In most cases, this happens after
        /// clicking a "plus" icon to add new element to the list. As a response, you should use the
        /// [`insert`] endpoint to provide the new item.
        request_new_item(Response<Index>),
    }
}


#[derive(Derivative, CloneRef, Debug, Deref)]
#[derivative(Clone(bound = ""))]
pub struct ListEditor<T: frp::node::Data> {
    #[deref]
    pub frp:        Frp<T>,
    root:           display::object::Instance,
    layouted_elems: display::object::Instance,
    dragged_elems:  display::object::Instance,
    model:          Rc<RefCell<Model<T>>>,
    add_elem_icon:  Rectangle,
}

#[derive(Debug, Derivative)]
#[derivative(Default(bound = ""))]
pub struct Model<T> {
    dragged_item: Option<T>,
    items:        Items<T>,
}

#[derive(Debug, Derivative, Deref, DerefMut)]
#[derivative(Default(bound = ""))]
pub struct Items<T> {
    items: Vec<Item<T>>,
}

impl<'t, T> IntoIterator for &'t Items<T> {
    type Item = &'t Item<T>;
    type IntoIter = std::slice::Iter<'t, Item<T>>;
    fn into_iter(self) -> Self::IntoIter {
        self.items.iter()
    }
}

impl<'t, T> IntoIterator for &'t mut Items<T> {
    type Item = &'t mut Item<T>;
    type IntoIter = std::slice::IterMut<'t, Item<T>>;
    fn into_iter(self) -> Self::IntoIter {
        self.items.iter_mut()
    }
}

impl<T> IntoIterator for Items<T> {
    type Item = Item<T>;
    type IntoIter = std::vec::IntoIter<Item<T>>;
    fn into_iter(self) -> Self::IntoIter {
        self.items.into_iter()
    }
}

impl<T> std::iter::FromIterator<Item<T>> for Items<T> {
    fn from_iter<I: IntoIterator<Item = Item<T>>>(iter: I) -> Self {
        let items = iter.into_iter().collect();
        Self { items }
    }
}

impl<T> Items<T> {
    fn collapse_all_placeholders(&mut self) {
        for item in self {
            if let Item::Placeholder(placeholder) = item {
                placeholder.collapse();
                *item = Item::WeakPlaceholder(placeholder.downgrade());
            }
        }
    }
}

impl<T: display::Object + frp::node::Data> ListEditor<T> {
    pub fn new(cursor: &Cursor) -> Self {
        let frp = Frp::new();
        let root = display::object::Instance::new();
        let layouted_elems = display::object::Instance::new();
        let dragged_elems = display::object::Instance::new();
        root.add_child(&layouted_elems);
        root.add_child(&dragged_elems);
        let model = default();
        layouted_elems.use_auto_layout(); //.set_gap((GAP, GAP));
        let add_elem_icon = Rectangle().build(|t| {
            t.set_size(Vector2::new(20.0, 20.0))
                .set_color(color::Rgba::new(0.0, 1.0, 0.0, 1.0))
                .set_inset_border(2.0)
                .set_border_color(color::Rgba::new(0.0, 0.0, 0.0, 0.5));
        });
        add_elem_icon.set_y(-30.0);
        root.add_child(&add_elem_icon);
        Self { frp, root, layouted_elems, dragged_elems, model, add_elem_icon }.init(cursor)
    }

    fn init(self, cursor: &Cursor) -> Self {
        let scene = scene();
        let frp = &self.frp;
        let network = self.frp.network();
        let root = &self.root;
        let layouted_elems = &self.layouted_elems;
        let dragged_elems = &self.dragged_elems;
        let model = &self.model;

        let add_elem_icon_down = self.add_elem_icon.on_event::<mouse::Down>();
        let on_down = self.layouted_elems.on_event_capturing::<mouse::Down>();
        let on_up = scene.on_event::<mouse::Up>();
        let on_move = scene.on_event::<mouse::Move>();
        frp::extend! { network

            frp.private.output.request_new_item <+ add_elem_icon_down.map(f!([model] (_) {
                let index = model.borrow_mut().items.len();
                Response::new_gui_interaction(index)
            }));

            push_item_index <= frp.push.map(f!([model, layouted_elems] (item)
                item.upgrade().map(|t| model.borrow_mut().push_item(&layouted_elems, (*t).clone()))
            ));
            on_item_pushed <- frp.push.map2(&push_item_index, |item, ix| Response::new_api_interaction((*ix, item.clone())));
            frp.private.output.on_new_item <+ on_item_pushed;

            // Do not pass events to children, as we don't know whether we are about to drag
            // them yet.
            eval on_down ([] (event) event.stop_propagation());
            target <= on_down.map(|event| event.target());

            target_pos_on_down <- target.map(|t| t.xy());
            pressed <- bool(&on_up, &on_down);
            glob_pos_on_down <- on_down.map(|event| event.client_centered());
            pos_on_down <- glob_pos_on_down.map(f!([root, model] (p) model.borrow().screen_to_object_space(&root, *p)));
            on_move_pressed <- on_move.gate(&pressed);
            glob_pos_on_move <- on_move_pressed.map(|event| event.client_centered());
            pos_on_move <- glob_pos_on_move.map(f!([root, model] (p) model.borrow().screen_to_object_space(&root, *p)));
            pos_offset_on_move <- map2(&pos_on_move, &pos_on_down, |a, b| a - b);

            // Discover whether the elements are dragged. They need to be moved vertically by at
            // least the [`DRAG_THRESHOLD`].
            drag_gate <- pos_offset_on_move.map(|t| t.y.abs() >= DRAG_THRESHOLD);
            pos_offset_on_drag <- pos_offset_on_move.gate(&drag_gate);
            status <- bool(&on_up, &pos_offset_on_drag).on_change();
            start <- status.on_true();
            target_on_start <- target.sample(&start);

            trashing <- pos_offset_on_drag.map(|t| t.y.abs() >= 100.0).on_change();
            on_trashing <- trashing.on_true();
            on_trash <- on_up.gate(&trashing);
            eval trashing ((t) cursor.frp.set_style(if *t { cursor::Style::trash() } else { cursor::Style::default() }));
            eval_ on_trashing (model.borrow_mut().items.collapse_all_placeholders());
            eval_ on_trash (model.borrow_mut().trash_dragged_item());

            // Re-parent the dragged element.
            eval target_on_start([model, layouted_elems, dragged_elems] (t) model.borrow_mut().set_as_dragged_item(&layouted_elems, &dragged_elems, t));
            // center_points <- target_on_start.map(f_!(model.borrow().elems_center_points()));
            // trace center_points;

            // Move the dragged element.
            target_new_pos <- map2(&pos_offset_on_drag, &target_pos_on_down, |a, b| a + b);
            _eval <- target_new_pos.map2(&target, |pos, t| t.set_xy(*pos));

            pos_non_trash <- pos_on_move.gate_not(&trashing);
            insert_index <- pos_non_trash.map(f!((pos) model.borrow().insert_index(pos.x))).on_change();
            insert_index_on_drop <- insert_index.sample(&on_up).gate_not(&trashing);

            eval insert_index ([model, layouted_elems] (index) {
                let mut model = model.borrow_mut();
                model.potential_insertion_point(*index);
                model.redraw_items(&layouted_elems);
            });

            eval insert_index_on_drop ([model, layouted_elems] (index) {
                let mut model = model.borrow_mut();
                model.place_dragged_item(*index);
                model.redraw_items(&layouted_elems);
            });


        }
        self
    }

    pub fn push(&self, item: T) {
        self.frp.push(Rc::new(item).downgrade());
    }
}

impl<T: display::Object + frp::node::Data> Model<T> {
    fn push_item(&mut self, layouted_elems: &display::object::Instance, item: T) -> usize {
        let index = self.items.len();
        let spot = DropSpot::new(item, 0.0);
        layouted_elems.add_child(&spot);
        self.items.push(Item::DropSpot(spot));
        self.recompute_margins();
        index
    }

    fn redraw_items(&mut self, layouted_elems: &display::object::Instance) {
        self.retain_non_collapsed_items();
        for item in &self.items {
            if let Some(display_object) = item.display_object() {
                layouted_elems.add_child(&display_object);
            }
        }
    }

    fn recompute_margins(&self) {
        warn!("recompute margins!");
        let mut first_elem = true;
        for item in &self.items {
            match item {
                Item::DropSpot(t) => {
                    let gap = if first_elem { 0.0 } else { GAP };
                    t.set_margin_left(gap);
                    first_elem = false;
                }
                Item::Placeholder(_) => {
                    first_elem = false;
                }
                _ => {}
            }
        }
    }

    fn first_non_placeholder_index(&self) -> Option<usize> {
        let mut index = 0;
        for item in &self.items {
            match item {
                Item::DropSpot(_) => return Some(index),
                _ => index += 1,
            }
        }
        None
    }

    fn retain_non_collapsed_items(&mut self) {
        warn!("retain_non_collapsed_items!");
        self.items.retain(|item| {
            let exists = item.exists();
            if !exists {
                warn!("removing 1 elem!")
            }
            exists
        });
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


    fn remove_item_by_display_object(
        &mut self,
        target: &display::object::Instance,
    ) -> Option<(usize, DropSpot<T>)> {
        let mut result: Option<(usize, DropSpot<T>)> = None;
        let items = mem::take(&mut self.items);
        self.items = items
            .into_iter()
            .enumerate()
            .filter_map(|(i, item)| match item {
                Item::DropSpot(t) =>
                    if t.elem.display_object() == target {
                        result = Some((i, t));
                        None
                    } else {
                        Some(Item::DropSpot(t))
                    },
                _ => Some(item),
            })
            .collect();
        result
    }

    fn get_upgraded_weak_placeholder(&self, index: usize) -> Option<(usize, Placeholder)> {
        self.items.get(index).and_then(|t| t.upgraded_weak_placeholder().map(|t| (index, t)))
    }

    /// Remove the selected item from the item list and mark it as an element being dragged. In the
    /// place where the element was a new placeholder will be created or existing placeholder will
    /// be reused and scaled to cover the size of the dragged element. The following cases are
    /// covered:
    ///
    /// 1. No placeholders to reuse. In this case, dragging an element marked with "X" results in
    /// the creation of a placeholder exactly where the dragged element was:   
    ///
    /// ```text
    ///                                                ╭──X──╮
    /// ╭─────╮ ╭─────╮ ╭─────╮    drag     ╭─────╮ ╭╌╌┼╌╌╮ ╭┼────╮
    /// │  A  │ │  X  │ │  B  │   ------>   │  A  │ ┆  ╰──┼─┼╯ B  │
    /// ╰─────╯ ╰─────╯ ╰─────╯             ╰─────╯ ╰╌╌╌╌╌╯ ╰─────╯
    /// ```
    ///
    /// 2. A single placeholder to reuse (either to the left or to the right of the dragged
    /// element). In this case, dragging the element marked with "X" results in the placeholder
    /// being scaled to cover the size of the dragged element. The placeholder will then be animated
    /// to finally get the same size as the dragged element.
    ///
    /// ```text                                                    
    ///                                                       ╭──X──╮      
    /// ╭─────╮ ╭─────╮ ╭╌╌╌╌╮ ╭─────╮    drag     ╭─────╮ ╭╌╌┼╌╌╌╌╌┼╮ ╭─────╮
    /// │  A  │ │  X  │ ┆    ┆ │  B  │   ------>   │  A  │ ┆  ╰─────╯┆ │  B  │
    /// ╰─────╯ ╰─────╯ ╰╌╌╌╌╯ ╰─────╯             ╰─────╯ ╰╌╌╌╌╌╌╌◀╌╯ ╰─────╯
    /// ```       
    ///      
    /// 3. The element is surrounded with placeholders. In this case, dragging the element marked
    /// with "X" results in the placeholders being merged into a single placeholder covering the
    /// size of both placeholders and the dragged element. The resulting placeholder will then be
    /// animated to finally get the same size as the dragged element.
    ///
    /// ```text                                                    
    ///                                                                 ╭──X──╮      
    /// ╭─────╮ ╭╌╌╌╌╮ ╭─────╮ ╭╌╌╌╌╮ ╭─────╮    drag     ╭─────╮ ╭╌╌╌╌╌┼╌╌╌╌╌┼╌╌╮ ╭─────╮
    /// │  A  │ ┆    ┆ │  X  │ ┆    ┆ │  B  │   ------>   │  A  │ ┆     ╰─────╯  ┆ │  B  │
    /// ╰─────╯ ╰╌╌╌╌╯ ╰─────╯ ╰╌╌╌╌╯ ╰─────╯             ╰─────╯ ╰╌╌╌╌╌╌╌╌╌╌╌╌◀╌╯ ╰─────╯
    /// ```   
    fn set_as_dragged_item(
        &mut self,
        layouted_elems: &display::object::Instance,
        dragged_elems: &display::object::Instance,
        target: &display::object::Instance,
    ) {
        if let Some((index, elem)) = self.remove_item_by_display_object(target) {
            self.items.collapse_all_placeholders();
            let prev_index = index.checked_sub(1);
            let prev_placeholder = prev_index.and_then(|i| self.get_upgraded_weak_placeholder(i));
            let next_placeholder = self.get_upgraded_weak_placeholder(index);
            let (placeholder, placeholder_to_merge) = match (prev_placeholder, next_placeholder) {
                (Some(p1), Some(p2)) => (Some(p1), Some(p2)),
                (p1, p2) => (p1.or(p2), None),
            };
            let size = elem.computed_size().x;
            if let Some((placeholder_index, placeholder)) = placeholder {
                placeholder.reuse();
                placeholder.add_to_size(size);
                placeholder.set_target_size(size);
                if let Some((placeholder2_index, placeholder2)) = placeholder_to_merge {
                    self.items.remove(placeholder2_index);
                    placeholder.add_to_size(placeholder2.computed_size().x);
                    placeholder2.drop_self_ref();
                }
                self.items[placeholder_index] = Item::Placeholder(placeholder);
            } else {
                self.items.insert(index, Placeholder::new_with_size(size).into());
            }
            dragged_elems.set_xy(elem.position().xy());
            dragged_elems.add_child(&elem.elem);
            self.dragged_item = Some(elem.elem);
            self.recompute_margins();
            self.redraw_items(layouted_elems);
        } else {
            warn!("Dragged item not found in the item list.")
        }
    }

    /// Prepare place for the dragged item by creating or reusing a placeholder and growing it to
    /// the dragged object size.
    fn potential_insertion_point(&mut self, index: usize) {
        warn!("Potential insertion point: {}.", index);
        if let Some(item) = self.dragged_item.as_ref() {
            self.items.collapse_all_placeholders();

            let gap =
                self.first_non_placeholder_index().map_or(
                    0.0,
                    |i| {
                        if index <= i {
                            0.0
                        } else {
                            GAP
                        }
                    },
                );
            warn!(
                "first_non_placeholder_index: {:?}, gap: {}",
                self.first_non_placeholder_index(),
                gap
            );
            let size = item.computed_size().x + gap;
            let prev_index = index.checked_sub(1);
            let old_placeholder = self
                .get_upgraded_weak_placeholder(index)
                .or_else(|| prev_index.and_then(|i| self.get_upgraded_weak_placeholder(i)));
            if let Some((index, placeholder)) = old_placeholder {
                // self.items.collapse_all_placeholders_but(index);
                placeholder.reuse();
                placeholder.set_target_size(size);
                self.items[index] = placeholder.into();
            } else {
                // self.items.collapse_all_placeholders_but(999);
                warn!("NEW PLACEHOLDER!");
                let placeholder = Placeholder::new_with_size(0.0);
                placeholder.set_target_size(size);
                self.items.insert(index, placeholder.into());
            }
            self.recompute_margins();
        } else {
            warn!("Called function to find insertion point while no element is being dragged.")
        }
    }

    /// Place the currently dragged element in the given index. The item will be enclosed in the
    /// [`DropSpot`] object, will handles its animation. See the documentation of [`Item`] to learn
    /// more.
    fn place_dragged_item(&mut self, index: usize) {
        if let Some(item) = self.dragged_item.take() {
            self.items.collapse_all_placeholders();
            let prev_index = index.checked_sub(1);
            let placeholder = self
                .get_upgraded_weak_placeholder(index)
                .or_else(|| prev_index.and_then(|i| self.get_upgraded_weak_placeholder(i)));
            if let Some((index, placeholder)) = placeholder {
                placeholder.reuse();
                // TODO: describe
                placeholder.set_target_size(placeholder.computed_size().x);
                let placeholder_position = placeholder.global_position().xy();
                let item_position = item.global_position().xy();
                item.set_xy(item_position - placeholder_position);
                let spot = DropSpot::new_from_placeholder(item, placeholder);
                let spot_frp = spot.frp.clone_ref();
                self.items[index] = Item::DropSpot(spot);
                self.recompute_margins();
                spot_frp.skip_margin_anim();
            } else {
                // This branch should never be reached, as when dragging an element we always create
                // a placeholder for it (see the [`potential_insertion_point`] function). However,
                // in case something breaks, we want it to still provide the user with the correct
                // outcome.
                // self.items.insert(index, Item::Regular(item));
                warn!("An element was inserted without a placeholder. This should not happen.");
                panic!();
            }
            // self.items.collapse_all_placeholders();
            // self.recompute_margins();
        } else {
            warn!("Called function to insert dragged element, but no element is being dragged.")
        }
    }

    pub fn trash_dragged_item(&mut self) {
        if let Some(item) = mem::take(&mut self.dragged_item) {
            Trash::new(item);
        }
    }


    // FIXME: does not work correctly with placeholders
    fn elems_center_points(&self) -> Vec<f32> {
        let mut centers = Vec::new();
        let mut current = 0.0;
        for item in &self.items {
            let size = item.computed_size();
            current += size.x / 2.0;
            centers.push(current);
            current += size.x / 2.0;
            // if item.is_regular() {
            //     current += GAP;
            // }
        }
        // warn!("centers: {:#?}", centers);
        centers
    }

    fn insert_index(&self, x: f32) -> usize {
        let center_points = self.elems_center_points();
        warn!("center_points: {:?}", center_points);
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

impl<T: frp::node::Data> display::Object for ListEditor<T> {
    fn display_object(&self) -> &display::object::Instance {
        &self.root
    }
}


mod trash {
    use super::*;
    ensogl_core::define_endpoints_2! {}

    #[derive(Debug, CloneRef, Derivative)]
    #[derivative(Clone(bound = ""))]
    pub struct Trash<T> {
        model: Rc<TrashModel<T>>,
    }

    #[derive(Debug)]
    pub struct TrashModel<T> {
        _frp: Frp,
        elem: T,
    }

    impl<T: display::Object + 'static> Trash<T> {
        pub fn new(elem: T) -> Self {
            let self_ref = Rc::new(RefCell::new(None));
            let _frp = Frp::new();
            let display_object = elem.display_object();
            let network = &_frp.network;
            let scale_animation = Animation::<f32>::new_with_init(network, 1.0);
            scale_animation.simulator.update_spring(|s| s * DEBUG_ANIMATION_SPRING_FACTOR);
            frp::extend! { network
                eval scale_animation.value ((t) display_object.set_scale_xy(Vector2(*t,*t)));
                eval_ scale_animation.on_end (self_ref.borrow_mut().take(););
            }
            scale_animation.target.emit(0.0);

            let model = TrashModel { _frp, elem };
            let model = Rc::new(model);
            *self_ref.borrow_mut() = Some(model.clone());
            Self { model }
        }
    }

    impl<T: display::Object> display::Object for Trash<T> {
        fn display_object(&self) -> &display::object::Instance {
            self.model.elem.display_object()
        }
    }
}
use trash::Trash;


// ===================
// === Entry Point ===
// ===================

/// The example entry point.
#[entry_point]
#[allow(dead_code)]
pub fn main() {
    let app = Application::new("root");
    let world = app.display.clone();
    let scene = &world.default_scene;

    let camera = scene.camera().clone_ref();
    let navigator = Navigator::new(scene, &camera);

    let vector_editor = ListEditor::<Rectangle>::new(&app.cursor);


    let shape1 = Circle().build(|t| {
        t.set_size(Vector2::new(100.0, 100.0))
            .set_color(color::Rgba::new(0.0, 0.0, 0.0, 0.1))
            .set_inset_border(2.0)
            .set_border_color(color::Rgba::new(0.0, 0.0, 0.0, 0.5))
            .keep_bottom_left_quarter();
    });
    let shape2 = RoundedRectangle(10.0).build(|t| {
        t.set_size(Vector2::new(200.0, 100.0))
            .set_color(color::Rgba::new(0.0, 0.0, 0.0, 0.1))
            .set_inset_border(2.0)
            .set_border_color(color::Rgba::new(0.0, 0.0, 0.0, 0.5));
    });
    let shape3 = RoundedRectangle(10.0).build(|t| {
        t.set_size(Vector2::new(100.0, 100.0))
            .set_color(color::Rgba::new(0.0, 0.0, 0.0, 0.1))
            .set_inset_border(2.0)
            .set_border_color(color::Rgba::new(0.0, 0.0, 0.0, 0.5));
    });


    let glob_frp = glob::Frp::new();
    let glob_frp_network = glob_frp.network();

    let shape1_down = shape1.on_event::<mouse::Down>();
    frp::extend! { glob_frp_network
        eval_ shape1_down ([] {
            warn!("Shape 1 down");
        });
        new_item <- vector_editor.request_new_item.map(|_| {
            let shape = RoundedRectangle(10.0).build(|t| {
                t.set_size(Vector2::new(100.0, 100.0))
                    .set_color(color::Rgba::new(0.0, 0.0, 0.0, 0.1))
                    .set_inset_border(2.0)
                    .set_border_color(color::Rgba::new(0.0, 0.0, 0.0, 0.5));
            });
            Rc::new(shape)
        });
        vector_editor.push <+ vector_editor.request_new_item.map2(&new_item, |_, item|
            item.downgrade()
        );
    }

    vector_editor.push(shape1);
    vector_editor.push(shape2);
    vector_editor.push(shape3);

    let root = display::object::Instance::new();
    root.set_size(Vector2::new(300.0, 100.0));
    root.add_child(&vector_editor);
    world.add_child(&root);

    world.keep_alive_forever();
    mem::forget(app);
    mem::forget(glob_frp);
    mem::forget(navigator);
    mem::forget(root);
    mem::forget(vector_editor);
}
