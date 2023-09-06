//! List Editor is a list component allowing adding, removing and reordering its items.
//!
//! # Primary and secondary axis
//! The implementation currently only supports horizontal layout, but it's designed to also support
//! vertical layout. It uses "primary" and "secondary" to describe the list's axis. The primary axis
//! is where the items are laid out, while the secondary axis is perpendicular to it.
//!
//! ```text
//!           ▲                          
//! secondary │ ╭─────╮ ╭─────╮ ╭─────╮
//!      axis │ ╰─────╯ ╰─────╯ ╰─────╯
//!           ┼────────────────────────▶
//!                 primary axis
//! ```
//!
//!
//! # Reordering items
//! To initialize item dragging, it must first be dragged along the secondary axis to detach it from
//! its current position. Then, it can be freely dragged and placed back in the list by dropping it
//! next to other items.
//!
//! This behavior is designed so because list items may be interactive widgets, such as sliders,
//! that need to receive mouse events like horizontal dragging. However, as mouse dragging is not
//! precise, the following algorithm is implemented:
//!
//! 1. When the mouse is pressed, the press position is stored.
//! 2. When the mouse is moved, the algorithm checks if the mouse primary axis offset is greater
//! than [`ListEditor::primary_axis_no_drag_threshold`]. If it is, the dragging is disabled until
//! the mouse is released and all following mouse events are passed to the item.
//! 3. If dragging is not disabled, when the mouse is moved, the algorithm checks if the mouse
//! secondary axis offset is greater than [ListEditor::secondary_axis_drag_threshold]. If it is, the
//! dragging is initialized and the item starts to follow the mouse.
//! The [`ListEditor::primary_axis_no_drag_threshold`] decays with time after the mouse is pressed
//! to provide the user with precise control over the widgets. The decay time is controlled by
//! [`ListEditor::primary_axis_no_drag_threshold_decay`].
//!
//! To better illustrate this behavior, the following illustration uses the "x" symbol to denote
//! mouse press position:
//!
//! ```text
//! ListEditor::primary_axis_no_drag_threshold
//!                ▶───◀
//!                ┆   ┆    
//! ╭─────────╮ ╭──┼───┼──╮ ╭─────────╮
//! │         │ │  ├╌╌╌┼╌╌┼╌┼╌╌╌╌╌╌╌╌╌┼╌╌▼
//! │         │ │  ┆ x ┆  │ │         │  │ ListEditor::secondary_axis_drag_threshold
//! │         │ │  └╌╌╌┴╌╌┼╌┼╌╌╌╌╌╌╌╌╌┼╌╌▲
//! ╰─────────╯ ╰─────────╯ ╰─────────╯
//! ```
//!
//!
//! # Removing items
//! In order to remove an item, it should be dragged out of the list along the secondary axis by at
//! least the size of the list along the secondary axis multiplied by
//! [`ListEditor::thrashing_offset_ratio`].
//!
//! ```text
//!             ╭─────────╮
//!             │         │   
//! ╭─────────╮ │    x    │ ╭─────────╮
//! │         │ │         │ │         │
//! │         │ ╰─────────╯╌┼╌╌╌╌╌╌╌╌╌┼╌╌▼ ListEditor::secondary_axis_drag_threshold
//! │         │             │         │  │ * ListEditor height
//! ╰─────────╯             ╰─────────╯╌╌▲
//! ```

#![recursion_limit = "512"]
// === Features ===
#![feature(let_chains)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]

use ensogl_core::display::world::*;
use ensogl_core::prelude::*;

use ensogl_core::control::io::mouse;
use ensogl_core::data::bounding_box::BoundingBox;
use ensogl_core::display;
use ensogl_core::display::object::Event;
use ensogl_core::display::object::ObjectOps;
use ensogl_core::gui::cursor;
use ensogl_core::gui::cursor::Cursor;
use ensogl_core::gui::cursor::Trash;
use ensogl_core::Easing;
use item::Item;
use placeholder::Placeholder;
use placeholder::StrongPlaceholder;
use placeholder::WeakPlaceholder;


// ==============
// === Export ===
// ==============

pub mod item;
pub mod placeholder;



// =================
// === Constants ===
// =================

/// If set to true, animations will be running slow. This is useful for debugging purposes.
pub const DEBUG_ANIMATION_SLOWDOWN: bool = false;

pub const DEBUG_PLACEHOLDERS_VIZ: bool = false;

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
    pub fn api(payload: T) -> Self {
        Self::new(payload, false)
    }

    /// Constructor indicating that the response was triggered by a GUI interaction.
    pub fn gui(payload: T) -> Self {
        Self::new(payload, true)
    }

    pub fn gui_interaction_payload(self) -> Option<T> {
        self.gui_interaction.then_some(self.payload)
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

    /// Replace the item element with a new one. Returns old element if it was replaced.
    pub fn replace_element(&mut self, element: T) -> Option<T> {
        match self {
            ItemOrPlaceholder::Item(t) =>
                if t.elem.display_object() == element.display_object() {
                    Some(mem::replace(&mut t.elem, element))
                } else {
                    let new_item = Item::new_from_placeholder(element, t.placeholder.clone_ref());
                    Some(mem::replace(t, new_item).elem)
                },
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
            ItemOrPlaceholder::Placeholder(_) => 0.0,
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

ensogl_core::define_endpoints_2! { <T: ('static + Debug)>
    Input {
        /// Push a new element to the end of the list.
        push(Rc<RefCell<Option<T>>>),

        /// Insert a new element in the given position. If the index is bigger than the list length,
        /// the item will be placed at the end of the list.
        insert((Index, Rc<RefCell<Option<T>>>)),

        /// Remove the element at the given index. If the index is invalid, nothing will happen.
        remove(Index),

        /// Set the spacing between elements.
        gap(f32),

        /// The distance the user needs to drag the element along secondary axis to start dragging
        /// the element. See docs of this module to learn more.
        secondary_axis_drag_threshold(f32),

        /// The distance the user needs to drag the element along primary axis to consider it not a
        /// drag movement and thus to pass mouse events to the item. See docs of this module to
        /// learn more.
        primary_axis_no_drag_threshold(f32),

        /// The time in which the `primary_axis_no_drag_threshold` drops to zero.
        primary_axis_no_drag_threshold_decay_time(f32),

        /// Controls the distance an item needs to be dragged out of the list for it to be trashed.
        /// See docs of this module to learn more.
        thrashing_offset_ratio(f32),

        /// Enable dragging of the list items. When disabled, the mouse events are always passed to
        /// the items immediately.
        enable_dragging(bool),

        /// Enable insertion points (plus icons) when moving mouse next to any of the list items.
        enable_all_insertion_points(bool),

        /// Enable insertion points (plus icons) when moving mouse after the last list item.
        enable_last_insertion_point(bool),

        /// A flag controlling this FRP debug mode. If enabled, additional logs can might be printed
        /// to console.
        debug(bool),
    }
    Output {
        /// Fires whenever a new element was added to the list.
        on_item_added(Response<Index>),

        /// Fires whenever an element was removed from the list. This can happen when dragging the
        /// element to switch its position.
        on_item_removed(Response<(Index, Rc<RefCell<Option<T>>>)>),

        /// Request new item to be inserted at the provided index. In most cases, this happens after
        /// clicking a "plus" icon to add new element to the list. As a response, you should use the
        /// [`insert`] endpoint to provide the new item.
        request_new_item(Response<Index>),
    }
}

#[derive(Derivative, CloneRef, Debug, Deref, display::Object)]
#[derivative(Clone(bound = ""))]
pub struct ListEditor<T: 'static + Debug> {
    #[deref]
    pub frp: Frp<T>,
    #[display_object]
    root:    display::object::Instance,
    model:   SharedModel<T>,
}

#[derive(Debug)]
pub struct Model<T> {
    cursor: Cursor,
    items:  VecIndexedBy<ItemOrPlaceholder<T>, ItemOrPlaceholderIndex>,
    root:   display::object::Instance,
    layout: display::object::Instance,
    gap:    f32,
}

impl<T> Model<T> {
    /// Constructor.
    pub fn new(cursor: &Cursor) -> Self {
        let cursor = cursor.clone_ref();
        let items = default();
        let root = display::object::Instance::new_named("ListEditor");
        let layout = display::object::Instance::new_named("layout");
        let gap = default();
        layout.use_auto_layout().allow_grow_y();
        root.add_child(&layout);
        Self { cursor, items, root, layout, gap }
    }
}

#[derive(Derivative, CloneRef, Debug, Deref)]
#[derivative(Clone(bound = ""))]
pub struct SharedModel<T> {
    rc: Rc<RefCell<Model<T>>>,
}

impl<T> From<Model<T>> for SharedModel<T> {
    fn from(model: Model<T>) -> Self {
        Self { rc: Rc::new(RefCell::new(model)) }
    }
}


impl<T: display::Object + CloneRef + Debug> ListEditor<T> {
    pub fn new(cursor: &Cursor) -> Self {
        let frp = Frp::new();
        let model = Model::new(cursor);
        let root = model.root.clone_ref();
        let model = model.into();
        Self { frp, root, model }.init(cursor).init_frp_values()
    }

    fn init(self, cursor: &Cursor) -> Self {
        let scene = scene();
        let frp = &self.frp;
        let network = self.frp.network();
        let model = &self.model;

        let on_down = model.borrow().root.on_event_capturing::<mouse::Down>();
        let on_up_source = scene.on_event::<mouse::Up>();
        let on_move = scene.on_event::<mouse::Move>();

        let dragged_item_network: Rc<RefCell<Option<frp::Network>>> = default();
        let on_resized = model.borrow().layout.on_resized.clone_ref();
        let drag_target = cursor::DragTarget::new();
        frp::extend! { network
            on_down <- on_down.gate(&frp.enable_dragging);
            target <= on_down.map(|event| event.target());

            on_up <- on_up_source.identity();
            on_up_cleaning_phase <- on_up_source.identity();

            is_down <- bool(&on_up, &on_down);
            on_move_down <- on_move.gate(&is_down);
            glob_pos_on_down <- on_down.map(|event| event.client_centered());
            glob_pos_on_move_down <- on_move_down.map(|event| event.client_centered());
            glob_pos_on_move <- on_move.map(|event| event.client_centered());
            pos_on_down <- glob_pos_on_down.map(f!((p) model.screen_to_object_space(*p)));
            pos_on_move_down <- glob_pos_on_move_down.map(f!((p) model.screen_to_object_space(*p)));
            pos_on_move <- glob_pos_on_move.map(f!((p) model.screen_to_object_space(*p)));
            pos_diff_on_move <- pos_on_move_down.map2(&pos_on_down, |a, b| a - b);
            pos_diff_on_down <- on_down.constant(Vector2(0.0, 0.0));
            pos_diff_on_up <- on_up_cleaning_phase.constant(Vector2(0.0, 0.0));
            pos_diff <- any3(&pos_diff_on_move, &pos_diff_on_down, &pos_diff_on_up);

            eval frp.gap((t) model.borrow_mut().set_gap(*t));

            // When an item is being dragged, we are connecting to it's `on_resized` endpoint to
            // watch for size changes while dragging. We want to disconnect from it as soon as the
            // drag ends, and thus we are storing a local FRP network here.
            dragged_item_offset <- source::<Vector2>();
            dragged_item_size <- any(...);
            eval_ cursor.frp.stop_drag([dragged_item_network]
                *dragged_item_network.borrow_mut() = None
            );
            eval_ cursor.frp.start_drag ([cursor, dragged_item_size, dragged_item_offset] {
                if let Some(obj) = cursor.dragged_display_object() {
                    let subnet = frp::Network::new("dragged_item_network");
                    frp::extend! { subnet
                        // Identity creates an explicit node in this network.
                        dragged_item_size <+ obj.on_resized.identity();
                    }
                    dragged_item_size.emit(obj.computed_size());
                    dragged_item_offset.emit(obj.position().xy());
                    *dragged_item_network.borrow_mut() = Some(subnet);
                }
            });

            this_bbox <- on_resized.map(|t| BoundingBox::from_size(*t));
            dragged_item_bbox <- all_with3(&dragged_item_size, &dragged_item_offset, &pos_on_move,
                |size, offset, pos| BoundingBox::from_bottom_left_position_and_size(*pos + *offset, *size)
            );
            is_close <- all_with(&this_bbox, &dragged_item_bbox, |a, b| a.intersects(b)).on_change();
            dragged_item_bbox_center <- dragged_item_bbox.map(|bbox| bbox.center());
            cursor.frp.switch_drag_target <+ is_close.map(f!([drag_target] (t) (drag_target.clone(), *t)));
        }

        self.init_add_and_remove();
        let no_drag =
            self.init_dragging(cursor, &on_up, &on_up_cleaning_phase, &on_down, &target, &pos_diff);
        frp::extend! { network
            on_up_close <- on_up.gate(&is_close);
        }
        self.init_dropping(&on_up_close, &dragged_item_bbox_center, &is_close);
        let insert_pointer_style = self.init_insertion_points(cursor, &on_up, &pos_on_move);

        frp::extend! { network
            cursor.frp.set_style_override <+ insert_pointer_style;
            on_down_drag <- on_down.gate_not(&no_drag);
            // Do not pass events to children, as we don't know whether we are about to drag
            // them yet.
            eval on_down_drag ([] (event) event.stop_propagation());
            resume_propagation <- no_drag.on_true().sync_gate(&on_down_drag);
            _eval <- resume_propagation.map2(&on_down, |_, event| event.resume_propagation());
        }
        self
    }

    fn init_insertion_points(
        &self,
        cursor: &Cursor,
        on_up: &frp::Stream<Event<mouse::Up>>,
        pos_on_move: &frp::Stream<Vector2>,
    ) -> frp::Stream<Option<cursor::Style>> {
        let on_up = on_up.clone_ref();
        let pos_on_move = pos_on_move.clone_ref();

        let frp = &self.frp;
        let network = self.frp.network();
        let model = &self.model;
        let model_borrowed = model.borrow();

        frp::extend! { network
            init <- source_();
            gaps <- all(&model_borrowed.layout.on_resized, &init)._0().map(f_!(model.gaps()));

            // We are debouncing the `is_dragging` stream to avoid double-borrow of list editor, as
            // this event is fired immediately after list-editor instructs cursor to stop dragging.
            is_dragging <- cursor.frp.is_dragging.debounce();

            opt_index <- all_with7(
                &frp.gap,
                &gaps,
                &pos_on_move,
                &model_borrowed.layout.on_resized,
                &is_dragging,
                &frp.enable_all_insertion_points,
                &frp.enable_last_insertion_point,
                f!([model] (gap, gaps, pos, size, is_dragging, enable_all, enable_last) {
                    let is_close_x = pos.x > -gap && pos.x < size.x + gap;
                    let is_close_y = pos.y > 0.0 && pos.y < size.y;
                    let is_close = is_close_x && is_close_y;
                    let enabled = is_close && !is_dragging && (*enable_all || *enable_last);
                    enabled
                        .and_option_from(|| gaps.find(pos.x))
                        .filter(|gap| *enable_all || (*enable_last && **gap == gaps.len() - 1))
                        .and_then(|gap| model.item_or_placeholder_index_to_index(gap))
                })
            ).on_change();
            index <= opt_index;
            enabled <- opt_index.is_some();
            pointer_style <- enabled.then_constant(cursor::Style::plus()).on_change();
            on_up_in_gap <- on_up.gate(&enabled);
            insert_in_gap <- index.sample(&on_up_in_gap);
            frp.private.output.request_new_item <+ insert_in_gap.map(|t| Response::gui(*t));
        }

        init.emit(());

        pointer_style
    }

    /// Implementation of adding and removing items logic.
    fn init_add_and_remove(&self) {
        let model = &self.model;
        let frp = &self.frp;
        let network = self.frp.network();

        frp::extend! { network
            push_ix <= frp.push.map(f!((item) model.push_cell(item)));
            on_pushed <- push_ix.map(|ix| Response::api(*ix));
            frp.private.output.on_item_added <+ on_pushed;

            insert_ix <= frp.insert.map(f!(((index, item)) model.insert_cell(*index, item)));
            on_inserted <- insert_ix.map(|ix| Response::api(*ix));
            frp.private.output.on_item_added <+ on_inserted;

            let on_item_removed = &frp.private.output.on_item_removed;
            eval frp.remove([model, on_item_removed] (index) {
                let item = model.borrow_mut().trash_item_at(*index);
                if let Some(item) = item {
                    on_item_removed.emit(Response::api((*index, Rc::new(RefCell::new(Some(item))))));
                }
            });
        }
    }

    /// Implementation of item dragging logic. See docs of this crate to learn more.
    fn init_dragging(
        &self,
        cursor: &Cursor,
        on_up: &frp::Stream<Event<mouse::Up>>,
        on_up_cleaning_phase: &frp::Stream<Event<mouse::Up>>,
        on_down: &frp::Stream<Event<mouse::Down>>,
        target: &frp::Stream<display::object::Instance>,
        pos_diff: &frp::Stream<Vector2>,
    ) -> frp::Stream<bool> {
        let model = &self.model;
        let on_up = on_up.clone_ref();
        let on_up_cleaning_phase = on_up_cleaning_phase.clone_ref();
        let on_down = on_down.clone_ref();
        let target = target.clone_ref();
        let pos_diff = pos_diff.clone_ref();
        let frp = &self.frp;
        let network = self.frp.network();
        let decay = Easing::new(network);
        frp::extend! { network
            let init_drag_threshold = &frp.secondary_axis_drag_threshold;
            let init_no_drag_threshold = &frp.primary_axis_no_drag_threshold;

            decay.set_duration <+ frp.primary_axis_no_drag_threshold_decay_time;
            decay.stop_and_rewind <+ any_(&on_up, &on_down).constant(1.0);
            decay.target <+ on_down.constant(0.0);
            no_drag_threshold <- all_with(&decay.value, init_no_drag_threshold, |d, t| d * t);

            init_no_drag <- all_with(&pos_diff, &no_drag_threshold, |p, t| p.x.abs() > *t).on_true();
            init_drag <- all_with(&pos_diff, init_drag_threshold, |p, t| p.y.abs() > *t).on_true();
            init_any <- any(init_no_drag, init_drag);
            click_was_handled <- bool(&on_up_cleaning_phase, &init_any).on_change();
            drag_disabled <- bool(&on_up, &init_no_drag).on_change();
            release_without_action <- on_up.gate_not(&click_was_handled);
            no_action <- bool(&on_up_cleaning_phase, &release_without_action).on_change();
            init_drag_not_disabled <- init_drag.gate_not(&drag_disabled);
            is_dragging <- bool(&on_up_cleaning_phase, &init_drag_not_disabled).on_change();
            drag_diff <- pos_diff.gate(&is_dragging);
            just_disabled <- drag_disabled.gate_not(&is_dragging).on_change();
            no_drag <- any(...);
            no_drag <+ no_action;
            no_drag <+ just_disabled;

            status <- bool(&on_up_cleaning_phase, &drag_diff).on_change();
            start <- status.on_true();
            target_on_start <- target.sample(&start);
            let on_item_removed = &frp.private.output.on_item_removed;
            eval target_on_start([model, cursor, on_item_removed, no_drag] (t) {
                let item = model.borrow_mut().start_item_drag(t);
                if let Some((index, item)) = item {
                    cursor.start_drag(item.clone_ref());
                    on_item_removed.emit(Response::gui((index, Rc::new(RefCell::new(Some(item))))));
                } else {
                    no_drag.emit(true);
                    no_drag.emit(false);
                }
            });
        }
        no_drag.into()
    }

    /// Implementation of dropping items logic, including showing empty placeholders when the item
    /// is dragged over a place where it could be dropped.
    fn init_dropping(
        &self,
        on_up: &frp::Stream<Event<mouse::Up>>,
        pos_on_move: &frp::Stream<Vector2>,
        is_close: &frp::Stream<bool>,
    ) {
        let pos_on_move = pos_on_move.clone_ref();
        let is_close = is_close.clone_ref();

        let model = &self.model;
        let frp = &self.frp;
        let network = self.frp.network();
        let model_borrowed = model.borrow();

        frp::extend! { network
            on_far <- is_close.on_false();
            center_points <- model_borrowed.layout.on_resized.map(f_!(model.center_points()));
            pos_close <- pos_on_move.sampled_gate(&is_close);
            insert_index <- pos_close.map2(&center_points, f!((p, c) model.insert_index(p.x, c)));
            insert_index <- insert_index.on_change();
            insert_index <- insert_index.sampled_gate(&is_close);

            eval_ on_far (model.collapse_all_placeholders());
            eval insert_index ((i) model.borrow_mut().add_insertion_point_if_type_match(*i));

            let on_item_added = &frp.private.output.on_item_added;
            insert_index_on_drop <- insert_index.sample(on_up).gate(&is_close);
            eval insert_index_on_drop ([model, on_item_added] (index)
                let index = model.borrow_mut().place_dragged_item(*index);
                if let Some(index) = index {
                    on_item_added.emit(Response::gui(index));
                }
            );
        }
    }

    /// Initializes default FRP values. See docs of this crate to learn more.
    fn init_frp_values(self) -> Self {
        self.frp.gap(10.0);
        self.frp.secondary_axis_drag_threshold(4.0);
        self.frp.primary_axis_no_drag_threshold(4.0);
        self.frp.primary_axis_no_drag_threshold_decay_time(1000.0);
        self.frp.thrashing_offset_ratio(1.0);
        self.frp.enable_dragging(true);
        self.frp.enable_all_insertion_points(true);
        self.frp.enable_last_insertion_point(true);
        self
    }

    pub fn push(&self, item: T) {
        self.frp.push(Rc::new(RefCell::new(Some(item))));
    }

    pub fn items(&self) -> Vec<T> {
        self.model.borrow().items.iter().flat_map(|item| item.as_item().cloned()).collect()
    }

    pub fn insert_item(&self, index: Index, item: T) {
        self.model.borrow_mut().insert(index, item);
    }

    pub fn insert_item_no_reposition(&self, index: Index, item: T) {
        self.model.borrow_mut().insert_no_reposition(index, item);
    }

    pub fn replace_item_no_reposition(&self, index: Index, new_item: T) -> Option<T> {
        let mut model = self.model.borrow_mut();
        let index = model.index_to_item_or_placeholder_index(index)?;
        let item = model.items.get_mut(index)?;
        item.replace_element(new_item)
    }

    pub fn trash_item_at(&self, index: Index) -> Option<T> {
        self.model.borrow_mut().trash_item_at(index)
    }

    pub fn clear(&self) {
        self.model.borrow_mut().clear();
    }

    pub fn len(&self) -> usize {
        self.model.borrow().len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn take_item_no_reposition(&self, index: Index) -> Option<T> {
        let mut model = self.model.borrow_mut();
        let index = model.index_to_item_or_placeholder_index(index)?;
        match model.items.remove(index) {
            ItemOrPlaceholder::Item(item) => Some(item.elem),
            ItemOrPlaceholder::Placeholder(_) => unreachable!(),
        }
    }

    pub fn reposition_items(&self) {
        self.model.borrow_mut().reposition_items();
    }

    pub fn item_at(&self, index: Index) -> Option<T> {
        let model = self.model.borrow();
        let index = model.index_to_item_or_placeholder_index(index)?;
        model.items.get(index).and_then(|item| item.as_item().cloned())
    }
}

impl<T: display::Object + CloneRef + 'static> SharedModel<T> {
    fn screen_to_object_space(&self, screen_pos: Vector2) -> Vector2 {
        self.borrow().screen_to_object_space(screen_pos)
    }

    fn collapse_all_placeholders(&self) {
        self.borrow_mut().collapse_all_placeholders()
    }

    fn push(&self, item: T) -> Index {
        self.borrow_mut().push(item)
    }

    fn push_cell(&self, item: &Rc<RefCell<Option<T>>>) -> Option<Index> {
        let item = mem::take(&mut *item.borrow_mut());
        item.map(|item| self.push(item))
    }

    fn insert(&self, index: Index, item: T) -> Index {
        self.borrow_mut().insert(index, item)
    }

    fn insert_cell(&self, index: Index, item: &Rc<RefCell<Option<T>>>) -> Option<Index> {
        let item = mem::take(&mut *item.borrow_mut());
        item.map(|item| self.insert(index, item))
    }

    fn insert_index(&self, x: f32, center_points: &[f32]) -> ItemOrPlaceholderIndex {
        self.borrow().insert_index(x, center_points)
    }

    fn gaps(&self) -> Gaps {
        self.borrow().gaps()
    }

    fn center_points(&self) -> Vec<f32> {
        self.borrow().center_points()
    }

    fn item_or_placeholder_index_to_index(&self, ix: ItemOrPlaceholderIndex) -> Option<Index> {
        self.borrow().item_or_placeholder_index_to_index(ix)
    }
}

#[derive(Clone, Debug, Default, Deref)]
pub struct Gaps {
    gaps: Vec<RangeInclusive<f32>>,
}

impl Gaps {
    pub fn find(&self, x: f32) -> Option<ItemOrPlaceholderIndex> {
        self.gaps.iter().position(|gap| gap.contains(&x)).map(|t| t.into())
    }
}

impl<T: display::Object + CloneRef + 'static> Model<T> {
    // FIXME: refactor and generalize
    fn screen_to_object_space(&self, screen_pos: Vector2) -> Vector2 {
        let scene = scene();
        let camera = scene.camera();
        let origin_world_space = Vector4(0.0, 0.0, 0.0, 1.0);
        let origin_clip_space = camera.view_projection_matrix() * origin_world_space;
        let inv_object_matrix = self.root.transformation_matrix().try_inverse().unwrap();

        let shape = scene.frp.shape.value();
        let clip_space_z = origin_clip_space.z;
        let clip_space_x = if shape.width.is_finite() && shape.width != 0.0 {
            origin_clip_space.w * 2.0 * screen_pos.x / shape.width
        } else {
            0.0
        };
        let clip_space_y = if shape.height.is_finite() && shape.height != 0.0 {
            origin_clip_space.w * 2.0 * screen_pos.y / shape.height
        } else {
            0.0
        };
        let clip_space = Vector4(clip_space_x, clip_space_y, clip_space_z, origin_clip_space.w);
        let world_space = camera.inversed_view_projection_matrix() * clip_space;
        (inv_object_matrix * world_space).xy()
    }

    fn len(&self) -> usize {
        self.items.iter().filter(|t| t.is_item()).count()
    }

    fn set_gap(&mut self, gap: f32) {
        self.gap = gap;
        self.recompute_margins();
    }

    /// Find an element by the provided display object reference.
    fn item_index_of(
        &self,
        obj: &display::object::Instance,
    ) -> Option<(Index, ItemOrPlaceholderIndex)> {
        self.items
            .iter()
            .enumerate()
            .map(|(i, t)| (ItemOrPlaceholderIndex::from(i), t))
            .filter(|(_, t)| t.is_item())
            .enumerate()
            .find(|(_, (_, t))| t.cmp_item_display_object(obj))
            .map(|(i1, (i2, _))| (i1, i2))
    }

    /// Convert the item index to item or placeholder index.
    fn index_to_item_or_placeholder_index(&self, ix: Index) -> Option<ItemOrPlaceholderIndex> {
        self.items.iter().enumerate().filter(|(_, item)| item.is_item()).nth(ix).map(|t| t.0.into())
    }

    fn item_or_placeholder_index_to_index(&self, ix: ItemOrPlaceholderIndex) -> Option<Index> {
        if *ix == self.items.len() {
            Some(self.len())
        } else {
            self.items
                .iter()
                .enumerate()
                .filter(|(_, item)| item.is_item())
                .position(|t| t.0 == *ix)
        }
    }

    fn push(&mut self, item: T) -> Index {
        let index = self.push_no_reposition(item);
        self.reposition_items();
        index
    }

    fn push_no_reposition(&mut self, item: T) -> Index {
        let index = self.len();
        let item = Item::new(item);
        self.items.push(item.into());
        index
    }


    fn insert(&mut self, index: Index, item: T) -> Index {
        self.insert_no_reposition(index, item);
        self.reposition_items();
        index
    }

    fn insert_no_reposition(&mut self, index: Index, item: T) -> Index {
        if let Some(index2) = self.index_to_item_or_placeholder_index(index) {
            let item = Item::new(item);
            self.items.insert(index2, item.into());
            index
        } else {
            self.push_no_reposition(item)
        }
    }

    /// Remove all items and add them again, in order of their current position.
    fn reposition_items(&mut self) {
        self.retain_non_collapsed_items();
        let children: SmallVec<[_; 16]> =
            self.items.iter().filter_map(|i| i.display_object()).collect();
        self.layout.replace_children(&children);
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
                    t.set_margin_left(if first_elem { 0.0 } else { self.gap });
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
        self.first_item_index().map_or(0.0, |i| if index <= i { 0.0 } else { self.gap })
    }

    /// Retain only items and placeholders that did not collapse yet (both strong and weak ones).
    fn retain_non_collapsed_items(&mut self) {
        self.items.retain(|item| item.exists());
    }

    /// Convert all placeholders to their weak versions and start the collapse animation. The
    /// margins will not be updated.
    fn collapse_all_placeholders_no_margin_update(&mut self) {
        for item in &mut self.items {
            if let ItemOrPlaceholder::Placeholder(placeholder) = item {
                placeholder.collapse();
            }
        }
    }

    /// Convert all placeholders to their weak versions and start the collapse animation. The
    /// margins of items will be updated as well.
    fn collapse_all_placeholders(&mut self) {
        self.collapse_all_placeholders_no_margin_update();
        self.recompute_margins();
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
    /// be reused and scaled to cover the size of the dragged element. Returns `None` if the target
    /// is not an item.
    ///
    /// See docs of [`Self::start_item_drag_at`] for more information.
    fn start_item_drag(&mut self, target: &display::object::Instance) -> Option<(Index, T)> {
        let objs = target.rev_parent_chain().reversed();
        let target_index = objs.into_iter().find_map(|t| self.item_index_of(&t));
        target_index.and_then(|(index, index_or_placeholder_index)| {
            self.start_item_drag_at(index_or_placeholder_index).map(|item| (index, item))
        })
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
    fn start_item_drag_at(&mut self, index: ItemOrPlaceholderIndex) -> Option<T> {
        self.replace_item_with_placeholder(index)
    }

    fn replace_item_with_placeholder(&mut self, index: ItemOrPlaceholderIndex) -> Option<T> {
        match self.items.remove(index) {
            ItemOrPlaceholder::Item(item) => {
                self.collapse_all_placeholders_no_margin_update();
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
    fn add_insertion_point_if_type_match(&mut self, index: ItemOrPlaceholderIndex) {
        if let Some(item) =
            self.cursor.with_dragged_item_if_is::<T, _>(|t| t.display_object().clone())
        {
            self.collapse_all_placeholders_no_margin_update();
            let item_size = item.computed_size().x + self.margin_at(index);
            let placeholder = self.get_merged_placeholder_at(index).unwrap_or_else(|| {
                let placeholder = StrongPlaceholder::new();
                if index >= ItemOrPlaceholderIndex::from(self.items.len()) {
                    self.items.push(placeholder.clone().into());
                } else {
                    self.items.insert(index, placeholder.clone().into());
                }
                placeholder
            });
            placeholder.set_target_size(item_size);
            self.reposition_items();
        }
    }

    /// Place the currently dragged element in the given index. The item will be enclosed in the
    /// [`Item`] object, will handles its animation. See the documentation of
    /// [`ItemOrPlaceholder`] to learn more.
    fn place_dragged_item(&mut self, index: ItemOrPlaceholderIndex) -> Option<Index>
    where T: Debug {
        if let Some(item) = self.cursor.stop_drag_if_is::<T>() {
            self.collapse_all_placeholders_no_margin_update();
            let actual_index = if let Some((index, placeholder)) =
                self.get_indexed_merged_placeholder_at(index)
            {
                placeholder.set_target_size(placeholder.computed_size().x);
                item.update_xy(|t| t - placeholder.global_position().xy());
                self.items[index] = Item::new_from_placeholder(item, placeholder).into();
                index
            } else {
                // This branch should never be reached, as when dragging an item we always create
                // a placeholder for it (see the [`Self::add_insertion_point_if_type_match`]
                // function). However, in case something breaks, we want it to still
                // provide the user with the correct outcome.
                self.items.insert(index, Item::new(item).into());
                warn!("An element was inserted without a placeholder. This should not happen.");
                index
            };
            let index = self.item_or_placeholder_index_to_index(actual_index);
            self.reposition_items();
            index
        } else {
            warn!("Called function to insert dragged element, but no element is being dragged.");
            None
        }
    }

    pub fn trash_item(&mut self, item: T) {
        self.root.add_child(&Trash::new(item));
        self.reposition_items();
    }

    pub fn trash_dragged_item(&mut self) {
        warn!("Trash dragged item.");
        if let Some(item) = self.cursor.stop_drag_if_is::<T>() {
            self.trash_item(item)
        }
    }

    pub fn trash_item_at(&mut self, index: Index) -> Option<T> {
        if let Some(item_index) = self.index_to_item_or_placeholder_index(index)
        && let Some(item) = self.replace_item_with_placeholder(item_index) {
            self.collapse_all_placeholders_no_margin_update();
            self.trash_item(item.clone_ref());
            Some(item)
        } else {
            warn!("Wrong index.");
            None
        }
    }

    pub fn clear(&mut self) {
        let mut removed_item = false;
        while let Some(item_index) = self.index_to_item_or_placeholder_index(0) {
            if let Some(item) = self.replace_item_with_placeholder(item_index) {
                removed_item = true;
                self.root.add_child(&Trash::new(item));
            }
        }
        if removed_item {
            self.collapse_all_placeholders_no_margin_update();
            self.reposition_items();
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

    fn gaps(&self) -> Gaps {
        if self.items.is_empty() {
            return Gaps { gaps: vec![f32::NEG_INFINITY..=f32::INFINITY] };
        }

        let mut gaps = Vec::new();
        gaps.push(f32::NEG_INFINITY..=0.0);
        let mut fist_gap = true;
        let mut current = 0.0;
        for item in &self.items {
            let start = current;
            current += item.margin_left();
            if !fist_gap {
                gaps.push(start..=current);
            }
            fist_gap = false;
            current += item.target_size2();
        }
        gaps.push(current..=f32::INFINITY);
        Gaps { gaps }
    }

    /// The insertion point of the given vertical offset.
    fn insert_index(&self, x: f32, center_points: &[f32]) -> ItemOrPlaceholderIndex {
        center_points.iter().position(|t| x < *t).unwrap_or(self.items.len()).into()
    }
}
