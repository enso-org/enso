//! This module provides a view for breadcrumbs, enabling us to know which node the graph being
//! edited belongs to and navigating through them.

use ensogl::display::shape::*;
use ensogl::prelude::*;

use engine_protocol::language_server::MethodPointer;
use enso_frp as frp;
use ensogl::application::Application;
use ensogl::display;
use ensogl::display::camera::Camera2d;
use ensogl::display::object::ObjectOps;
use ensogl::gui::cursor;
use ensogl_hardcoded_theme::application::top_bar as top_bar_theme;
use std::cmp::Ordering;


// ==============
// === Export ===
// ==============

pub mod breadcrumb;

pub use breadcrumb::Breadcrumb;



// ===========================
// === SharedMethodPointer ===
// ===========================

/// Information about target definition for node entering.
#[derive(Clone, Debug, Deref, PartialEq, Eq)]
pub struct SharedMethodPointer(pub Rc<MethodPointer>);

impl From<MethodPointer> for SharedMethodPointer {
    fn from(method_pointer: MethodPointer) -> Self {
        Self(Rc::new(method_pointer))
    }
}



// =================
// === LocalCall ===
// =================

/// A specific function call occurring within another function's definition body.
/// It's closely related to the `LocalCall` type defined in `Language Server` types, but uses the
/// new type `SharedMethodPointer`.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LocalCall {
    /// An expression being a call to a method.
    pub call:       engine_protocol::language_server::ExpressionId,
    /// A pointer to the called method.
    pub definition: SharedMethodPointer,
}


// =================
// === Constants ===
// =================

const BACKGROUND_HEIGHT: f32 = 32.0;
const TEXT_SIZE: f32 = 12.0;



// ===========
// === Frp ===
// ===========

ensogl::define_endpoints! {
    Input {
        /// Pushes new breadcrumbs after the selected breadcrumb. Any breadcrumbs that are already
        /// part of the stack are ignored. If the next breadcrumb isn't part of the breadcrumbs
        /// being pushed, any existing breadcrumb following the currently selected breadcrumb is
        /// removed from the panel.
        push_breadcrumbs            (Vec<LocalCall>),
        /// Pops the given number of breadcrumbs.
        pop_breadcrumbs             (usize),
        /// Signalizes a mouse press happened outside the breadcrumb panel. It's used to finish
        /// project renaming, committing the name in text field.
        outside_press               (),
        /// Select the breadcrumb by its index.
        select_breadcrumb           (usize),
        /// Same as `FrpInputs::push_breadcrumb`, but pushes a hardcoded breadcrumb if
        /// `Option<LocalCall>` is `None`.
        debug_push_breadcrumb       (Option<LocalCall>),
        /// Pops the breadcrumb view without sending signals to the controller.
        debug_pop_breadcrumb        (),
        /// Selects the breadcrumb by its index without sending signals to the controller.
        debug_select_breadcrumb     (usize),
        /// Indicates the IDE is in text edit mode.
        ide_text_edit_mode          (bool),
        /// The `gap_width` describes an empty space on the left of all the content. This space will
        /// be covered by the background and is intended to make room for windows control buttons.
        gap_width                   (f32),
        /// Set read-only mode for this component.
        set_read_only(bool),
        update_layout(),
    }
    Output {
        /// Signalizes new breadcrumbs to be pushed.
        breadcrumb_push (Vec<LocalCall>),
        /// Signalizes how many breadcrumbs to pop.
        breadcrumb_pop (usize),
        /// Signalizes when a breadcrumb is selected, returning a tuple with the amount of
        /// breadcrumbs to be popped, in case the selection happens on the left of the currently
        /// selected breadcrumb, or else a vector of existing breadcrumbs to be pushed.
        breadcrumb_select ((usize, Vec<LocalCall>)),
        /// Indicates the pointer style that should be shown based on the interactions with the
        /// breadcrumb.
        pointer_style      (cursor::Style),
        /// Indicates if the read-only mode is enabled.
        read_only(bool),
    }
}



// ========================
// === BreadcrumbsModel ===
// ========================

/// Breadcrumbs panel model.
#[derive(Debug, Clone, CloneRef)]
pub struct BreadcrumbsModel {
    /// The breadcrumbs panel display object.
    background:            Rectangle,
    /// A container for all the breadcrumbs after project name. This contained and all its
    /// breadcrumbs are moved when project name component is resized.
    breadcrumbs_container: display::object::Instance,
    // Required for creating new breadcrumbs
    app:                   Application,
    breadcrumbs:           Rc<RefCell<Vec<Breadcrumb>>>,
    frp_inputs:            FrpInputs,
    current_index:         Rc<Cell<usize>>,
    camera:                Camera2d,
    /// Describes an empty space on the left of all the content. This space will be covered by the
    /// background and is intended to make room for windows control buttons.
    gap_width:             Rc<Cell<f32>>,
}

impl BreadcrumbsModel {
    /// Constructor.
    /// The `gap_width` describes an empty space on the left of all the content. This space will be
    /// covered by the background and is intended to make room for windows control buttons.
    #[profile(Detail)]
    pub fn new(app: &Application, frp: &Frp) -> Self {
        let app = app.clone_ref();
        let scene = &app.display.default_scene;
        let breadcrumbs_container = display::object::Instance::new();
        let scene = scene.clone_ref();
        let breadcrumbs = default();
        let frp_inputs = frp.input.clone_ref();
        let current_index = default();
        let camera = scene.camera().clone_ref();
        let background: Rectangle = default();
        let gap_width = default();
        background
            .use_auto_layout()
            .set_padding_left(8.0)
            .set_padding_right(8.0)
            .set_children_alignment_left_center();

        let style = StyleWatchFrp::new(&app.display.default_scene.style_sheet);
        background.set_style(top_bar_theme::background::HERE, &style);

        scene.layers.panel_background_rect_level_0.add(&background);

        Self {
            background,
            breadcrumbs_container,
            app,
            breadcrumbs,
            frp_inputs,
            current_index,
            camera,
            gap_width,
        }
        .init()
    }

    fn init(self) -> Self {
        self.background.add_child(&self.breadcrumbs_container);
        self.add_root_breadcrumb();

        self.update_layout();

        self
    }

    /// Add a breadcrumb called "main" that represents the initial frame on stack.
    fn add_root_breadcrumb(&self) {
        let breadcrumb = Breadcrumb::new_empty(&self.app);
        breadcrumb.set_label("main");
        let network = &breadcrumb.frp.network;
        let frp_inputs = &self.frp_inputs;

        frp::extend! { network
            eval_ breadcrumb.frp.outputs.clicked(
                frp_inputs.select_breadcrumb.emit(0);
            );
            eval_ breadcrumb.frp.outputs.width(frp_inputs.update_layout.emit(()));
        }

        breadcrumb.set_x(self.breadcrumbs_container_width().round());
        self.breadcrumbs_container.add_child(&breadcrumb);
        self.breadcrumbs.borrow_mut().push(breadcrumb);
    }

    fn breadcrumbs_container_width(&self) -> f32 {
        self.breadcrumbs.borrow().iter().map(|breadcrumb| breadcrumb.width()).sum()
    }

    fn set_gap_width(&self, gap_width: f32) {
        self.gap_width.set(gap_width);
        self.update_layout();
    }

    fn update_layout(&self) {
        let width = self.breadcrumbs_container_width();
        self.breadcrumbs_container.set_size(Vector2(width, BACKGROUND_HEIGHT));
        self.background.set_padding_left(8.0);
        self.background.set_padding_right(8.0);
    }

    fn get_breadcrumb(&self, index: usize) -> Option<Breadcrumb> {
        self.breadcrumbs.borrow().get(index).map(|breadcrumb| breadcrumb.clone_ref())
    }

    /// Selects the breadcrumb identified by its `index` and returns `(popped_count,local_calls)`,
    /// where `popped_count` is the number of breadcrumbs in the right side of `index` that needs to
    /// be popped or a list of `LocalCall`s identifying the breadcrumbs we need to push.
    fn select_breadcrumb(&self, index: usize) -> (usize, Vec<LocalCall>) {
        debug!("Selecting breadcrumb #{index}.");
        let current_index = self.current_index.get();
        match index.cmp(&current_index) {
            Ordering::Less => (current_index - index, default()),
            Ordering::Equal => default(),
            Ordering::Greater => {
                let mut local_calls = Vec::new();
                for index in current_index..=index {
                    let info = self
                        .breadcrumbs
                        .borrow()
                        .get(index)
                        .and_then(|breadcrumb| breadcrumb.info.deref().clone())
                        .map(|info| {
                            let definition = info.method_pointer.clone();
                            let call = info.expression_id;
                            LocalCall { call, definition }
                        })
                        .as_ref()
                        .cloned();
                    if let Some(local_call) = info {
                        local_calls.push(local_call);
                    }
                }
                (default(), local_calls)
            }
        }
    }

    /// Pushes breadcrumbs and returns the index of the previously selected breadcrumb and the
    /// index of the newly selected one in the form of (old, new).
    fn push_breadcrumbs(&self, stack: &[LocalCall]) -> (usize, usize) {
        let old_index = self.current_index.get();
        for (substack_index, local_call) in stack.iter().enumerate() {
            let method_pointer = &local_call.definition;
            let expression_id = &local_call.call;
            let breadcrumb_index = old_index + substack_index + 1;

            let breadcrumb_exists =
                self.get_breadcrumb(breadcrumb_index).contains_if(|breadcrumb| {
                    breadcrumb
                        .info
                        .deref()
                        .as_ref()
                        .map_or(false, |i| i.expression_id == *expression_id)
                });

            if breadcrumb_exists {
                debug!("Entering an existing {} breadcrumb.", method_pointer.name);
            } else {
                debug!("Creating a new {} breadcrumb.", method_pointer.name);
                self.remove_breadcrumbs_history_beginning_from(breadcrumb_index);
                let breadcrumb = Breadcrumb::new(&self.app, method_pointer, *expression_id);
                breadcrumb.show_separator();
                let network = &breadcrumb.frp.network;
                let frp_inputs = &self.frp_inputs;

                frp::extend! { network
                    eval_ breadcrumb.frp.outputs.clicked(
                        frp_inputs.select_breadcrumb.emit(breadcrumb_index);
                    );
                    eval_ breadcrumb.frp.outputs.width(frp_inputs.update_layout.emit(()));
                }

                debug!("Pushing {} breadcrumb.", method_pointer.name);
                breadcrumb.set_x(self.breadcrumbs_container_width().round());
                self.breadcrumbs_container.add_child(&breadcrumb);
                self.breadcrumbs.borrow_mut().push(breadcrumb);
            }
        }
        let new_index = old_index + stack.len();
        self.current_index.set(new_index);
        self.update_layout();
        (old_index, new_index)
    }

    /// Selects the breadcrumb, without signalizing the controller, identified by its `index` and
    /// returns `(popped_count,local_calls)`, where `popped_count` is the number of breadcrumbs on
    /// the right side of `index` that needs to be popped, or a list of `LocalCall`s identifying the
    /// breadcrumbs we need to push.
    fn debug_select_breadcrumb(&self, index: usize) -> (usize, Vec<Option<LocalCall>>) {
        let (pop_count, stack_to_push) = self.select_breadcrumb(index);
        let stack_to_push = stack_to_push.into_iter().map(Some).collect();
        (pop_count, stack_to_push)
    }

    /// Pushes a breadcrumb, without signalizing the controller, and returns the index of the
    /// previously selected breadcrumb, and the index of the newly selected one in the form of
    /// `(old,new)`.
    fn debug_push_breadcrumb(&self, local_call: &Option<LocalCall>) -> (usize, usize) {
        let is_new_breadcrumb = local_call.is_none();
        let local_call = local_call.clone().unwrap_or_else(|| {
            let defined_on_type = default();
            let module = default();
            let name = "Hardcoded".to_string();
            let method_pointer = MethodPointer { module, defined_on_type, name };
            let definition = method_pointer.into();
            let call = uuid::Uuid::new_v4();
            LocalCall { call, definition }
        });
        let (old_index, new_index) = self.push_breadcrumbs(&[local_call]);
        if is_new_breadcrumb {
            if let Some(breadcrumb) = self.get_breadcrumb(new_index) {
                let network = &breadcrumb.frp.network;
                let frp_inputs = &self.frp_inputs;
                frp::extend! { network
                    eval_ breadcrumb.frp.outputs.clicked(
                        frp_inputs.debug_select_breadcrumb.emit(new_index);
                    );
                }
            }
        }
        (old_index, new_index)
    }

    /// Pops a breadcrumb, without signalizing the controller, and returns the index of the
    /// previously selected breadcrumb, and the index of the newly selected one in the form of
    /// `(old,new)`.
    fn debug_pop_breadcrumb(&self) -> Option<(usize, usize)> {
        self.pop_breadcrumbs(1)
    }

    /// Pops the given number of breadcrumbs and returns the index of the previously selected
    /// breadcrumb, and the index of the newly selected one in the form of (old, new).
    fn pop_breadcrumbs(&self, count: usize) -> Option<(usize, usize)> {
        (self.current_index.get() > 0).as_option().map(|_| {
            let old_index = self.current_index.get();
            let new_index = old_index - count;
            debug!("Popping {count} breadcrumbs, from {old_index} to {new_index} (excl.).");
            self.current_index.set(new_index);
            self.update_layout();
            (old_index, new_index)
        })
    }

    fn remove_breadcrumbs_history_beginning_from(&self, index: usize) {
        for breadcrumb in self.breadcrumbs.borrow_mut().split_off(index) {
            let name = breadcrumb.info.deref().as_ref().map(|i| i.method_pointer.name.as_str());
            debug!("Removing {:?}.", name);
            breadcrumb.unset_parent();
        }
        self.update_layout();
    }

    fn update_selection(&self, old_index: usize, new_index: usize) {
        // Deselect breadcrumbs between the old index (incl.) and the new index (excl.)
        let indices_to_deselect =
            if new_index > old_index { old_index..new_index } else { new_index + 1..old_index + 1 };
        for index_to_deselect in indices_to_deselect {
            if let Some(breadcrumb) = self.get_breadcrumb(index_to_deselect) {
                breadcrumb.frp.deselect.emit((index_to_deselect, new_index));
            }
        }
        // Select new breadcrumb
        if let Some(breadcrumb) = self.get_breadcrumb(new_index) {
            breadcrumb.frp.select.emit(());
        }
    }
}

impl display::Object for BreadcrumbsModel {
    fn display_object(&self) -> &display::object::Instance {
        self.background.display_object()
    }
}



// ===================
// === Breadcrumbs ===
// ===================

/// The breadcrumbs panel's view used for visualizing the breadcrumbs and navigating them.
#[derive(Debug, Clone, CloneRef)]
#[allow(missing_docs)]
pub struct Breadcrumbs {
    model: BreadcrumbsModel,
    frp:   Frp,
}

impl Breadcrumbs {
    /// Constructor.
    pub fn new(app: &Application) -> Self {
        let frp = Frp::new();
        let model = BreadcrumbsModel::new(app, &frp);
        let network = &frp.network;

        // === Breadcrumb selection ===

        frp::extend! { network

            // === Selecting ===

            frp.source.breadcrumb_select <+
                frp.select_breadcrumb.map(f!((index) model.select_breadcrumb(*index)));


            // === Stack Operations ===

            push_indices <- frp.push_breadcrumbs.map(f!((stack) model.push_breadcrumbs(stack)));
            pop_indices <= frp.pop_breadcrumbs.map(f!((count) model.pop_breadcrumbs(*count)));
            debug_push_indices <- frp.input.debug_push_breadcrumb.map(f!((local_call)
                model.debug_push_breadcrumb(local_call)
            ));
            debug_pop_indices <= frp.input.debug_pop_breadcrumb.map
                (f_!(model.debug_pop_breadcrumb()));
            indices <- any4(&push_indices,&pop_indices,&debug_push_indices,&debug_pop_indices);
            eval indices (((old_index, new_index)) model.update_selection(*old_index, *new_index));


            // === User Interaction ===

            breadcrumbs_to_pop_count <- frp.output.breadcrumb_select.filter_map(|(pop_count, _)|
                (*pop_count > 0).then_some(*pop_count)
            );
            breadcrumbs_to_push <- frp.output.breadcrumb_select.filter_map(|(_, breadcrumbs_to_push)|
                (!breadcrumbs_to_push.is_empty()).as_some_from(|| breadcrumbs_to_push.clone())
            );
            frp.source.breadcrumb_pop <+ breadcrumbs_to_pop_count;
            frp.source.breadcrumb_push <+ breadcrumbs_to_push;


            // === Debug Select ===

            debug_selected_breadcrumb <- frp.input.debug_select_breadcrumb.map(f!((index)
                model.debug_select_breadcrumb(*index))
            );
            popped_count <= debug_selected_breadcrumb.map(|selected| (0..selected.0).collect_vec());
            local_calls <= debug_selected_breadcrumb.map(|selected| selected.1.clone());
            frp.input.debug_pop_breadcrumb <+ popped_count.constant(());
            frp.input.debug_push_breadcrumb <+ local_calls;


            // === Relayout ===
            eval frp.input.gap_width((gap_width) model.set_gap_width(*gap_width));
            eval_ frp.input.update_layout(model.update_layout());


            // === Read-only mode ===

            frp.source.read_only <+ frp.input.set_read_only;
        }

        Self { model, frp }
    }
}

impl display::Object for Breadcrumbs {
    fn display_object(&self) -> &display::object::Instance {
        self.model.display_object()
    }
}

impl Deref for Breadcrumbs {
    type Target = Frp;
    fn deref(&self) -> &Self::Target {
        &self.frp
    }
}
