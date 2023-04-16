//! Example scene showing the usage of built-in vector editor component.
//!
//! TODO[WD]: This is work in progress and will be changed in the upcoming PRs.
//!   https://github.com/enso-org/enso/issues/6037

#![recursion_limit = "512"]
// === Features ===
#![feature(let_chains)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]

pub mod item;
pub mod placeholder;

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

use item::Item;
use placeholder::Placeholder;
use placeholder::StrongPlaceholder;



// =================
// === Constants ===
// =================

const DRAG_THRESHOLD: f32 = 4.0;

// FIXME: to be parametrized
const GAP: f32 = 20.0;

/// If set to true, animations will be running slow. This is useful for debugging purposes.
pub const DEBUG_ANIMATION_SLOWDOWN: bool = false;

pub const DEBUG_PLACEHOLDERS_VIZ: bool = true;

/// Spring factor for animations. If [`DEBUG_ANIMATION_SLOWDOWN`] is set to true, this value will be
/// used for animation simulators.
pub const DEBUG_ANIMATION_SPRING_FACTOR: f32 = if DEBUG_ANIMATION_SLOWDOWN { 0.1 } else { 1.0 };



// =============
// === Types ===
// =============

pub type Index = usize;



// ================
// === Response ===
// ================

#[derive(Clone, Copy, Debug, Default, Deref, DerefMut)]
pub struct Response<T> {
    #[deref]
    #[deref_mut]
    pub payload:         T,
    pub gui_interaction: bool,
}

impl<T> Response<T> {
    /// Constructor.
    pub fn new(payload: T, gui_interaction: bool) -> Self {
        Self { payload, gui_interaction }
    }

    /// Constructor indicating that the response was triggered by an API interaction.
    pub fn new_api_interaction(payload: T) -> Self {
        Self::new(payload, false)
    }

    /// Constructor indicating that the response was triggered by a GUI interaction.
    pub fn new_gui_interaction(payload: T) -> Self {
        Self::new(payload, true)
    }
}



// =========================
// === ItemOrPlaceholder ===
// =========================

newtype_prim! {
    /// Index of an item or a placeholder.
    ItemOrPlaceholderIndex(usize);
}

/// Enum for keeping either an item or a placeholder. Placeholder are used to display a place for
/// items while dragging or inserting them.
#[allow(missing_docs)]
#[derive(Debug, From)]
pub enum ItemOrPlaceholder<T> {
    Item(Item<T>),
    Placeholder(Placeholder),
}

impl<T: display::Object> ItemOrPlaceholder<T> {
    /// Check whether this is an item.
    pub fn is_item(&self) -> bool {
        matches!(self, Self::Item(_))
    }

    /// Check whether this is a placeholder.
    pub fn is_placeholder(&self) -> bool {
        matches!(self, Self::Placeholder(_))
    }

    /// Return a strong placeholder reference. In case it was a dropped weak placeholder or an item,
    /// return [`None`].
    pub fn as_strong_placeholder(&self) -> Option<StrongPlaceholder> {
        match self {
            ItemOrPlaceholder::Placeholder(p) => p.upgrade(),
            _ => None,
        }
    }

    /// Get the item reference if this is an item. Return [`None`] otherwise.
    pub fn as_item(&self) -> Option<&T> {
        match self {
            ItemOrPlaceholder::Item(t) => Some(&t.elem),
            _ => None,
        }
    }

    /// Get the display object of this item or placeholder. In case the placeholder is weak and does
    /// not exist anymore, it will return [`None`].
    pub fn display_object(&self) -> Option<display::object::Instance> {
        match self {
            ItemOrPlaceholder::Placeholder(t) => t.display_object(),
            ItemOrPlaceholder::Item(t) => Some(t.display_object().clone_ref()),
        }
    }

    /// Get the target size of this item or placeholder. The current size can be different if the
    /// size animation is running.
    pub fn target_size2(&self) -> f32 {
        match self {
            ItemOrPlaceholder::Placeholder(t) => t.target_size(),
            ItemOrPlaceholder::Item(t) =>
                t.placeholder.target_size.value() - t.frp.margin_left.value(),
        }
    }

    pub fn margin_left(&self) -> f32 {
        match self {
            ItemOrPlaceholder::Placeholder(t) => 0.0,
            ItemOrPlaceholder::Item(t) => t.frp.margin_left.value(),
        }
    }

    /// Return false if this is a weak placeholder with no strong references. Return true otherwise.
    pub fn exists(&self) -> bool {
        match self {
            ItemOrPlaceholder::Placeholder(p) => p.exists(),
            ItemOrPlaceholder::Item(_) => true,
        }
    }

    /// Compare the provided display object with the item's one if self is an item.
    pub fn cmp_item_display_object(&self, target: &display::object::Instance) -> bool {
        match self {
            ItemOrPlaceholder::Placeholder(_) => false,
            ItemOrPlaceholder::Item(item) => item.elem.display_object() == target,
        }
    }
}

impl<T> From<WeakPlaceholder> for ItemOrPlaceholder<T> {
    fn from(placeholder: WeakPlaceholder) -> Self {
        Self::Placeholder(placeholder.into())
    }
}

impl<T> From<StrongPlaceholder> for ItemOrPlaceholder<T> {
    fn from(placeholder: StrongPlaceholder) -> Self {
        Self::Placeholder(placeholder.into())
    }
}


// ==================
// === ListEditor ===
// ==================

ensogl_core::define_endpoints_2! { <T: ('static)>
    Input {
        /// Push a new element to the end of the list.
        push(Weak<T>),

        /// WARNING: not working yet, use `push` instead.
        insert((Index, Weak<T>)),

        /// Remove the element at the given index. If the index is invalid, nothing will happen.
        remove(Index),
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
    pub frp:          Frp<T>,
    root:             display::object::Instance,
    model:            Rc<RefCell<Model<T>>>,
    add_elem_icon:    Rectangle,
    remove_elem_icon: Rectangle,
}

#[derive(Debug)]
pub struct Model<T> {
    items:  VecIndexedBy<ItemOrPlaceholder<T>, ItemOrPlaceholderIndex>,
    root:   display::object::Instance,
    layout: display::object::Instance,
}

impl<T> Model<T> {
    /// Constructor.
    pub fn new() -> Self {
        let items = default();
        let root = display::object::Instance::new();
        let layout = display::object::Instance::new();
        layout.use_auto_layout();
        root.add_child(&layout);
        Self { items, root, layout }
    }
}

impl<T> Default for Model<T> {
    fn default() -> Self {
        Self::new()
    }
}


impl<T: display::Object + frp::node::Data> ListEditor<T> {
    pub fn new(cursor: &Cursor) -> Self {
        let frp = Frp::new();
        let model = Model::new();
        let root = model.root.clone_ref();
        let add_elem_icon = Rectangle().build(|t| {
            t.set_size(Vector2::new(20.0, 20.0))
                .set_color(color::Rgba::new(0.0, 1.0, 0.0, 1.0))
                .set_inset_border(2.0)
                .set_border_color(color::Rgba::new(0.0, 0.0, 0.0, 0.5));
        });

        let remove_elem_icon = Rectangle().build(|t| {
            t.set_size(Vector2::new(20.0, 20.0))
                .set_color(color::Rgba::new(1.0, 0.0, 0.0, 1.0))
                .set_inset_border(2.0)
                .set_border_color(color::Rgba::new(0.0, 0.0, 0.0, 0.5));
        });
        add_elem_icon.set_y(-30.0);
        root.add_child(&add_elem_icon);
        remove_elem_icon.set_y(-30.0);
        remove_elem_icon.set_x(30.0);
        root.add_child(&remove_elem_icon);
        let model = Rc::new(RefCell::new(model));
        Self { frp, root, model, add_elem_icon, remove_elem_icon }.init(cursor)
    }

    fn init(self, cursor: &Cursor) -> Self {
        let scene = scene();
        let frp = &self.frp;
        let network = self.frp.network();
        let model = &self.model;

        let add_elem_icon_down = self.add_elem_icon.on_event::<mouse::Down>();
        let remove_elem_icon_down = self.remove_elem_icon.on_event::<mouse::Down>();
        let on_down = model.borrow().layout.on_event_capturing::<mouse::Down>();
        let on_up = scene.on_event::<mouse::Up>();
        let on_move = scene.on_event::<mouse::Move>();
        frp::extend! { network

            frp.private.output.request_new_item <+ add_elem_icon_down.map(f!([model] (_) {
                let index = model.borrow_mut().items.len();
                Response::new_gui_interaction(index)
            }));

            frp.remove <+ remove_elem_icon_down.constant(0);

            push_item_index <= frp.push.map(f!([model] (item)
                item.upgrade().map(|t| model.borrow_mut().push_item((*t).clone()))
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
            pos_on_down <- glob_pos_on_down.map(f!([model] (p) model.borrow().screen_to_object_space(*p)));
            on_move_pressed <- on_move.gate(&pressed);
            glob_pos_on_move <- on_move_pressed.map(|event| event.client_centered());
            pos_on_move <- glob_pos_on_move.map(f!([model] (p) model.borrow().screen_to_object_space(*p)));
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
            eval_ on_trashing ([model] {
                model.borrow_mut().collapse_all_placeholders();
                model.borrow_mut().recompute_margins();
            });
            eval_ on_trash ([model, cursor] model.borrow_mut().trash_dragged_item(&cursor));

            eval frp.remove((index) model.borrow_mut().trash_item_at(*index));

            // Re-parent the dragged element.
            eval target_on_start([model, cursor] (t) model.borrow_mut().start_item_drag(&cursor, t));
            // center_points <- target_on_start.map(f_!(model.borrow().center_points()));
            // trace center_points;

            // Move the dragged element.
            // target_new_pos <- map2(&pos_offset_on_drag, &target_pos_on_down, |a, b| a + b);
            // _eval <- target_new_pos.map2(&target, |pos, t| t.set_xy(*pos));

            pos_non_trash <- pos_on_move.gate_not(&trashing);
            insert_index <- pos_non_trash.map(f!((pos) model.borrow().insert_index(pos.x))).on_change();
            insert_index_on_drop <- insert_index.sample(&on_up).gate_not(&trashing);

            eval insert_index ([model, cursor] (i) model.borrow_mut().add_insertion_point(&cursor, *i));

            eval insert_index_on_drop ([cursor, model] (index)
                model.borrow_mut().place_dragged_item(&cursor, *index)
            );
        }
        self
    }

    pub fn push(&self, item: T) {
        self.frp.push(Rc::new(item).downgrade());
    }

    pub fn items(&self) -> Vec<T> {
        self.model.borrow().items.iter().flat_map(|item| item.as_item().cloned()).collect()
    }
}

impl<T: display::Object + frp::node::Data> Model<T> {
    // FIXME: refactor and generalize
    fn screen_to_object_space(&self, screen_pos: Vector2) -> Vector2 {
        let scene = scene();
        let camera = scene.camera();
        let origin_world_space = Vector4(0.0, 0.0, 0.0, 1.0);
        let origin_clip_space = camera.view_projection_matrix() * origin_world_space;
        let inv_object_matrix = self.root.transformation_matrix().try_inverse().unwrap();

        let shape = scene.frp.shape.value();
        let clip_space_z = origin_clip_space.z;
        let clip_space_x = origin_clip_space.w * 2.0 * screen_pos.x / shape.width;
        let clip_space_y = origin_clip_space.w * 2.0 * screen_pos.y / shape.height;
        let clip_space = Vector4(clip_space_x, clip_space_y, clip_space_z, origin_clip_space.w);
        let world_space = camera.inversed_view_projection_matrix() * clip_space;
        (inv_object_matrix * world_space).xy()
    }

    fn push_item(&mut self, item: T) -> usize {
        let index = self.items.len();
        let item = Item::new(item);
        self.items.push(item.into());
        self.reposition_items();
        index
    }

    /// Remove all items and add them again, in order of their current position.
    fn reposition_items(&mut self) {
        self.retain_non_collapsed_items();
        for item in &self.items {
            if let Some(display_object) = item.display_object() {
                self.layout.add_child(&display_object);
            }
        }
        self.recompute_margins();
    }

    /// Recompute margins of all elements. The first element that is not a weak placeholder does not
    /// have a left margin. Every other element has. Weak placeholders are used for animation of the
    /// space leftover after removing an element, so they will collapse over time to zero-space.
    fn recompute_margins(&self) {
        let mut first_elem = true;
        for item in &self.items {
            match item {
                ItemOrPlaceholder::Placeholder(Placeholder::Weak(_)) => {}
                ItemOrPlaceholder::Placeholder(Placeholder::Strong(_)) => first_elem = false,
                ItemOrPlaceholder::Item(t) => {
                    t.set_margin_left(if first_elem { 0.0 } else { GAP });
                    first_elem = false;
                }
            }
        }
    }

    /// The index of the first element. In case there are no elements, [`None`] is returned.
    fn first_item_index(&self) -> Option<ItemOrPlaceholderIndex> {
        self.items.iter().enumerate().find(|(_, t)| t.is_item()).map(|(i, _)| i.into())
    }

    /// Get the margin at the given insertion point. If the insertion point is before the first
    /// item, the margin will be 0.
    fn margin_at(&self, index: ItemOrPlaceholderIndex) -> f32 {
        self.first_item_index().map_or(0.0, |i| if index <= i { 0.0 } else { GAP })
    }

    /// Retain only items and placeholders that did not collapse yet (both strong and weak ones).
    fn retain_non_collapsed_items(&mut self) {
        self.items.retain(|item| item.exists());
    }

    /// Find an element by the provided display object reference.
    fn item_index_of(&mut self, obj: &display::object::Instance) -> Option<ItemOrPlaceholderIndex> {
        self.items.iter().enumerate().find(|t| t.1.cmp_item_display_object(obj)).map(|t| t.0.into())
    }

    /// Convert the item index to item or placeholder index.
    fn index_to_item_or_placeholder_index(&mut self, ix: Index) -> Option<ItemOrPlaceholderIndex> {
        self.items.iter().enumerate().filter(|(_, item)| item.is_item()).nth(ix).map(|t| t.0.into())
    }

    /// Convert all placeholders to their weak versions and start the collapse animation.
    fn collapse_all_placeholders(&mut self) {
        for item in &mut self.items {
            if let ItemOrPlaceholder::Placeholder(placeholder) = item {
                placeholder.collapse();
            }
        }
    }

    /// Get the placeholder of the provided index if the index points to a strong placeholder or an
    /// existing weak one.
    fn get_placeholder_at(
        &self,
        index: ItemOrPlaceholderIndex,
    ) -> Option<(ItemOrPlaceholderIndex, StrongPlaceholder)> {
        self.items.get(index).and_then(|t| t.as_strong_placeholder().map(|t| (index, t)))
    }

    /// Get the placeholder next to the provided insertion point. In case the insertion point is
    /// between two placeholders, they will be merged into one. In case the placeholders are weak,
    /// the final placeholder will be upgraded to a strong one.
    fn get_indexed_merged_placeholder_at(
        &mut self,
        index: ItemOrPlaceholderIndex,
    ) -> Option<(ItemOrPlaceholderIndex, StrongPlaceholder)> {
        let prev_index = index.checked_sub(ItemOrPlaceholderIndex::from(1));
        let placeholder1 = prev_index.and_then(|i| self.get_placeholder_at(i));
        let placeholder2 = self.get_placeholder_at(index);
        let (placeholder, placeholder_to_merge) = match (placeholder1, placeholder2) {
            (Some(p1), Some(p2)) => (Some(p1), Some(p2)),
            (p1, p2) => (p1.or(p2), None),
        };
        placeholder.map(|(placeholder_index, placeholder)| {
            placeholder.reuse();
            if let Some((placeholder2_index, placeholder2)) = placeholder_to_merge {
                placeholder2.drop_self_ref();
                self.items.remove(placeholder2_index);
                placeholder.update_size(|t| t + placeholder2.computed_size().x);
            }
            self.items[placeholder_index] = placeholder.clone().into();
            (placeholder_index, placeholder)
        })
    }

    /// Non-indexed version of [`Self::get_indexed_merged_placeholder_at`].
    fn get_merged_placeholder_at(
        &mut self,
        index: ItemOrPlaceholderIndex,
    ) -> Option<StrongPlaceholder> {
        self.get_indexed_merged_placeholder_at(index).map(|t| t.1)
    }

    /// Remove the selected item from the item list and mark it as an element being dragged. In the
    /// place where the element was a new placeholder will be created or existing placeholder will
    /// be reused and scaled to cover the size of the dragged element.
    ///
    /// See docs of [`Self::start_item_drag_at`] for more information.
    fn start_item_drag(&mut self, cursor: &Cursor, target: &display::object::Instance) {
        if let Some(index) = self.item_index_of(target) {
            self.start_item_drag_at(cursor, index);
        } else {
            warn!("Requested to drag a non-existent item.")
        }
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
    /// element). This situation can happen when an element was previously dragged and it's
    /// placeholder did not collapse yet. In this case, dragging the element marked with "X" results
    /// in the placeholder being scaled to cover the size of the dragged element. The
    /// placeholder will then be animated to finally get the same size as the dragged element.
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
    fn start_item_drag_at(&mut self, cursor: &Cursor, index: ItemOrPlaceholderIndex) {
        if let Some(item) = self.replace_item_with_placeholder(index) {
            cursor.start_drag(item);
        }
    }

    fn replace_item_with_placeholder(&mut self, index: ItemOrPlaceholderIndex) -> Option<T> {
        match self.items.remove(index) {
            ItemOrPlaceholder::Item(item) => {
                self.collapse_all_placeholders();
                let item_size = item.computed_size().x;
                if let Some(placeholder) = self.get_merged_placeholder_at(index) {
                    placeholder.update_size(|t| t + item_size);
                    placeholder.set_target_size(item_size);
                } else {
                    self.items.insert(index, Placeholder::new_with_size(item_size).into())
                }
                self.reposition_items();
                Some(item.elem)
            }
            item => {
                warn!("Trying to use a placeholder index as an element index.");
                self.items.insert(index, item);
                None
            }
        }
    }

    /// Prepare place for the dragged item by creating or reusing a placeholder and growing it to
    /// the dragged object size.
    fn add_insertion_point(&mut self, cursor: &Cursor, index: ItemOrPlaceholderIndex) {
        if let Some(item) = cursor.with_dragged_item_if_is::<T, _>(|t| t.display_object().clone()) {
            self.collapse_all_placeholders();
            let item_size = item.computed_size().x + self.margin_at(index);
            let placeholder = self.get_merged_placeholder_at(index).unwrap_or_else(|| {
                let placeholder = StrongPlaceholder::new();
                self.items.insert(index, placeholder.clone().into());
                placeholder
            });
            placeholder.set_target_size(item_size);
            self.reposition_items();
        } else {
            warn!("Called function to find insertion point while no element is being dragged.")
        }
    }

    /// Place the currently dragged element in the given index. The item will be enclosed in the
    /// [`Item`] object, will handles its animation. See the documentation of
    /// [`ItemOrPlaceholder`] to learn more.
    fn place_dragged_item(&mut self, cursor: &Cursor, index: ItemOrPlaceholderIndex) {
        if let Some(element) = cursor.stop_drag_if_is::<T>() {
            self.collapse_all_placeholders();
            if let Some((index, placeholder)) = self.get_indexed_merged_placeholder_at(index) {
                placeholder.set_target_size(placeholder.computed_size().x);
                element.update_xy(|t| t - placeholder.global_position().xy());
                self.items[index] = Item::new_from_placeholder(element, placeholder).into();
            } else {
                // This branch should never be reached, as when dragging an element we always create
                // a placeholder for it (see the [`Self::add_insertion_point`] function). However,
                // in case something breaks, we want it to still provide the user with the correct
                // outcome.
                self.items.insert(index, Item::new(element).into());
                warn!("An element was inserted without a placeholder. This should not happen.");
            }
            self.reposition_items();
        } else {
            warn!("Called function to insert dragged element, but no element is being dragged.")
        }
    }

    pub fn trash_item(&mut self, item: T) {
        self.root.add_child(&Trash::new(item));
        self.reposition_items();
    }

    pub fn trash_dragged_item(&mut self, cursor: &Cursor) {
        if let Some(item) = cursor.stop_drag_if_is::<T>() {
            self.trash_item(item)
        }
    }

    pub fn trash_item_at(&mut self, index: Index) {
        if let Some(item_index) = self.index_to_item_or_placeholder_index(index) {
            if let Some(item) = self.replace_item_with_placeholder(item_index) {
                self.collapse_all_placeholders();
                self.trash_item(item);
            }
        } else {
            warn!("Wrong index.");
        }
    }


    /// Get the center points of items and placeholders. This is used to determine the index of the
    /// insertion point when a new item is being dragged.
    ///
    /// ```text                                                    
    ///   ┬      ┬         ┬         ┬
    /// ╭─┼─╮ ╭──┼──╮ ╭╌╌╌╌┼╌╌╌╌╮ ╭──┼──╮  
    /// │ ┆ │ │  ┆  │ ┆    ┆    ┆ │  ┆  │  
    /// ╰─┼─╯ ╰──┼──╯ ╰╌╌╌╌┼╌╌╌╌╯ ╰──┼──╯
    ///   ┴      ┴         ┴         ┴
    /// ```       
    fn center_points(&self) -> Vec<f32> {
        let mut centers = Vec::new();
        let mut current = 0.0;
        for item in &self.items {
            let size2 = item.target_size2() / 2.0;
            current += item.margin_left() + size2;
            centers.push(current);
            current += size2;
        }
        centers
    }

    /// The insertion point of the given vertical offset.
    fn insert_index(&self, x: f32) -> ItemOrPlaceholderIndex {
        self.center_points().iter().position(|t| x < *t).unwrap_or_else(|| self.items.len()).into()
    }
}

impl<T: frp::node::Data> display::Object for ListEditor<T> {
    fn display_object(&self) -> &display::object::Instance {
        &self.root
    }
}


// =============
// === Trash ===
// =============

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
use crate::placeholder::WeakPlaceholder;
use trash::Trash;


// ===================
// === Entry Point ===
// ===================

pub mod glob {
    use super::*;
    ensogl_core::define_endpoints_2! {
        Input {
        }
        Output {
        }
    }
}

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
        t.set_size(Vector2::new(60.0, 100.0))
            .set_color(color::Rgba::new(0.0, 0.0, 0.0, 0.1))
            .set_inset_border(2.0)
            .set_border_color(color::Rgba::new(0.0, 0.0, 0.0, 0.5))
            .keep_bottom_left_quarter();
    });
    let shape2 = RoundedRectangle(10.0).build(|t| {
        t.set_size(Vector2::new(120.0, 100.0))
            .set_color(color::Rgba::new(0.0, 0.0, 0.0, 0.1))
            .set_inset_border(2.0)
            .set_border_color(color::Rgba::new(0.0, 0.0, 0.0, 0.5));
    });
    let shape3 = RoundedRectangle(10.0).build(|t| {
        t.set_size(Vector2::new(240.0, 100.0))
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
