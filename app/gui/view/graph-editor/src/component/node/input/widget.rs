//! Definition of all hardcoded node widget variants and common widget FRP API.

use crate::prelude::*;

use enso_frp as frp;
use ensogl::application::Application;
use ensogl::data::color;
use ensogl::display;
use ensogl::display::object::event;
use ensogl_component::drop_down::Dropdown;



/// =================
/// === Constants ===
/// =================

const DOT_COLOR: color::Lch = color::Lch::new(0.56708, 0.23249, 0.71372);



// ===========
// === FRP ===
// ===========

ensogl::define_endpoints_2! {
    Input {
        set_metadata   (Option<Metadata>),
        set_node_data  (NodeData),
        set_current_value (Option<ImString>),
        set_focused       (bool),
        set_visible       (bool),
    }
    Output {
        value_changed(Option<ImString>),
    }
}

/// Widget metadata that comes from an asynchronous visualization. Defines which widget should be
/// used and a set of options that it should allow to choose from.
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct Metadata {
    pub kind:            Kind,
    pub display:         Display,
    pub dynamic_entries: Vec<ImString>,
}

/// Widget display mode. Determines when the widget should be expanded.
#[derive(serde::Deserialize, Debug, Clone, Copy, Default)]
pub enum Display {
    /// The widget should always be in its expanded mode.
    #[default]
    Always,
    /// The widget should only be in its expanded mode when it has non-default value.
    #[serde(rename = "When_Modified")]
    WhenModified,
    /// The widget should only be in its expanded mode whe the whole node is expanded.
    #[serde(rename = "Expanded_Only")]
    ExpandedOnly,
}

/// The data of node port that this widget is attached to. Available immediately after widget
/// creation. Can be updated later when the node data changes.
#[derive(Debug, Clone, Default)]
#[allow(missing_docs)]
pub struct NodeData {
    pub argument_info: span_tree::ArgumentInfo,
    pub node_height:   f32,
}



/// ==================
/// === SampledFrp ===
/// ==================

/// Sampled version of widget FRP endpoints that can be used by widget views that are initialized
/// on demand after first interaction. Without samplers, when a widget view would be initialized
/// after the endpoints were set, it would not receive previously set endpoint values.
#[derive(Debug, Clone, CloneRef)]
struct SampledFrp {
    set_current_value: frp::Sampler<Option<ImString>>,
    set_visible:       frp::Sampler<bool>,
    set_focused:       frp::Sampler<bool>,
    out_value_changed: frp::Any<Option<ImString>>,
}



// ==============
// === Widget ===
// ==============

/// The node widget view. Represents one widget of any kind on the node input area. Can change its
/// appearance and behavior depending on the widget metadata updates, without being recreated.
#[derive(Debug, Clone, CloneRef)]
pub struct View {
    frp:   Frp,
    model: Rc<Model>,
}

impl View {
    /// Create a new node widget. The widget is initialized to empty state, waiting for widget
    /// metadata to be provided using `set_node_data` and `set_metadata` FRP endpoints.
    #[profile(Task)]
    pub fn new(app: &Application) -> Self {
        let frp = Frp::new();
        let model = Rc::new(Model::new(app));
        Self { frp, model }.init()
    }

    /// Widget FRP API. Contains all endpoints that can be used to control the widget of any kind.
    pub fn frp(&self) -> &Frp {
        &self.frp
    }

    fn init(self) -> Self {
        let model = &self.model;
        let frp = &self.frp;
        let network = &frp.network;
        let input = &frp.input;

        frp::extend! { network
            widget_data <- all(&input.set_metadata, &input.set_node_data);

            set_current_value <- input.set_current_value.sampler();
            set_visible <- input.set_visible.sampler();
            set_focused <- input.set_focused.sampler();
            let out_value_changed = frp.private.output.value_changed.clone_ref();
            let sampled_frp = SampledFrp { set_current_value, set_visible, set_focused, out_value_changed };

            eval widget_data([model, sampled_frp]((meta, node_data)) {
                model.set_widget_data(&sampled_frp, meta, node_data);
            });
        }

        self
    }
}


/// =============
/// === Model ===
/// =============

#[derive(Debug)]
struct Model {
    app:            Application,
    display_object: display::object::Instance,
    kind_model:     RefCell<Option<KindModel>>,
}

impl Model {
    /// Create a new node widget, selecting the appropriate widget type based on the provided
    /// argument info.
    fn new(app: &Application) -> Self {
        let app = app.clone_ref();
        let display_object = display::object::Instance::new();
        let kind = default();
        Self { app, display_object, kind_model: kind }
    }

    #[profile(Task)]
    fn set_widget_data(&self, frp: &SampledFrp, meta: &Option<Metadata>, node_data: &NodeData) {
        trace!("Setting widget data: {:?} {:?}", meta, node_data);

        let has_tag_values = !node_data.argument_info.tag_values.is_empty();
        let kind_fallback = has_tag_values.then_some(Kind::SingleChoice);

        let desired_kind = meta.as_ref().map(|m| m.kind).or(kind_fallback);
        let current_kind = self.kind_model.borrow().as_ref().map(|m| m.kind());

        if current_kind != desired_kind {
            *self.kind_model.borrow_mut() = desired_kind.map(|desired_kind| {
                KindModel::new(&self.app, &self.display_object, desired_kind, frp, meta, node_data)
            });
        } else if let Some(model) = self.kind_model.borrow().as_ref() {
            model.update(meta, node_data);
        }
    }
}

impl Deref for View {
    type Target = Frp;
    fn deref(&self) -> &Self::Target {
        self.frp()
    }
}

impl display::Object for View {
    fn display_object(&self) -> &display::object::Instance {
        &self.model.display_object
    }
}



// ========================
// === KindModel / Kind ===
// ========================

/// Possible widgets for a node input.
///
/// Currently, all widget types are hardcoded. This is likely to be a temporary solution. In the
/// future the widget types might be user-defined, similar to visualizations.
#[derive(serde::Deserialize, Clone, Copy, Debug, PartialEq, Eq)]
pub enum Kind {
    /// A widget for selecting a single value from a list of available options.
    #[serde(rename = "Single_Choice")]
    SingleChoice,
}

/// A part of widget model that is dependant on the widget kind.
#[derive(Debug)]
pub enum KindModel {
    /// A widget for selecting a single value from a list of available options.
    SingleChoice(SingleChoiceModel),
}

impl KindModel {
    fn new(
        app: &Application,
        display_object: &display::object::Instance,
        kind: Kind,
        frp: &SampledFrp,
        meta: &Option<Metadata>,
        node_data: &NodeData,
    ) -> Self {
        let this = match kind {
            Kind::SingleChoice =>
                Self::SingleChoice(SingleChoiceModel::new(app, display_object, frp)),
        };

        this.update(meta, node_data);
        this
    }

    fn update(&self, meta: &Option<Metadata>, node_data: &NodeData) {
        match self {
            KindModel::SingleChoice(inner) => {
                let dynamic_entries = meta.as_ref().map(|meta| meta.dynamic_entries.clone());
                let tag_values = node_data.argument_info.tag_values.iter().map(Into::into);
                let entries = dynamic_entries.unwrap_or_else(|| tag_values.collect());

                inner.set_node_height(node_data.node_height);
                inner.set_entries(entries);
            }
        }
    }
    fn kind(&self) -> Kind {
        match self {
            Self::SingleChoice(_) => Kind::SingleChoice,
        }
    }
}



// =================
// === Dot Shape ===
// =================

/// Temporary dropdown activation shape definition.
pub mod dot {
    use super::*;
    ensogl::shape! {
        above = [
            crate::component::node::background,
            crate::component::node::input::port::hover
        ];
        (style:Style, color:Vector4) {
            let size   = Var::canvas_size();
            let radius = Min::min(size.x(),size.y()) / 2.0;
            let shape  = Rect(size).corners_radius(radius);
            shape.fill(color).into()
        }
    }
}



// ====================
// === SingleChoice ===
// ====================

/// A widget for selecting a single value from a list of available options. The options can be
/// provided as a static list of strings from argument `tag_values`, or as a dynamic expression.
#[derive(Debug)]
pub struct SingleChoiceModel {
    #[allow(dead_code)]
    network:        frp::Network,
    dropdown:       Rc<RefCell<LazyDropdown>>,
    /// temporary click handling
    activation_dot: dot::View,
}

impl SingleChoiceModel {
    fn new(
        app: &Application,
        display_object: &display::object::Instance,
        frp: &SampledFrp,
    ) -> Self {
        let activation_dot = dot::View::new();
        activation_dot.set_size((15.0, 15.0));
        display_object.add_child(&activation_dot);

        frp::new_network! { network
            init <- source_();
            let focus_in = display_object.on_event::<event::FocusIn>();
            let focus_out = display_object.on_event::<event::FocusOut>();
            is_focused <- bool(&focus_out, &focus_in);
            is_open <- frp.set_visible && is_focused;
            is_open <- is_open.sampler();
        };

        let set_current_value = frp.set_current_value.clone_ref();
        let dropdown_output = frp.out_value_changed.clone_ref();
        let dropdown =
            LazyDropdown::new(app, display_object, set_current_value, is_open, dropdown_output);
        let dropdown = Rc::new(RefCell::new(dropdown));

        frp::extend! { network
            let dot_clicked = activation_dot.events.mouse_down_primary.clone_ref();
            toggle_focus <- dot_clicked.map(f!([display_object](()) !display_object.is_focused()));
            set_focused <- any(toggle_focus, frp.set_focused);
            eval set_focused([display_object](focus) match focus {
                true => display_object.focus(),
                false => display_object.blur(),
            });

            set_visible <- all(&frp.set_visible, &init)._0();
            dot_alpha <- set_visible.map(|visible| if *visible { 1.0 } else { 0.0 });
            dot_color <- dot_alpha.map(|alpha| DOT_COLOR.with_alpha(*alpha));
            eval dot_color([activation_dot] (color) {
                activation_dot.color.set(color::Rgba::from(color).into());
            });

            eval focus_in((_) dropdown.borrow_mut().initialize_on_open());
        }

        init.emit(());

        Self { network, dropdown, activation_dot }
    }

    fn set_node_height(&self, node_height: f32) {
        self.activation_dot.set_y(-node_height / 2.0 - 1.0);
        self.dropdown.borrow_mut().set_node_height(node_height);
    }

    fn set_entries(&self, values: Vec<ImString>) {
        self.dropdown.borrow_mut().set_entries(values);
    }
}



// ====================
// === LazyDropdown ===
// ====================

/// A lazy dropdown that is only initialized when it is opened for the first time. This prevents
/// very long initialization time, as dropdown view creation is currently a very slow process.
///
/// FIXME [PG]: Improve grid-view creation performance, so that this is no longer needed.
/// https://www.pivotaltracker.com/story/show/184223891
///
/// Once grid-view creation is reasonably fast, this might be replaced by direct dropdown
/// initialization on widget creation.
#[derive(Debug)]
enum LazyDropdown {
    NotInitialized {
        app:               Application,
        display_object:    display::object::Instance,
        node_height:       f32,
        entries:           Vec<ImString>,
        set_current_value: frp::Sampler<Option<ImString>>,
        is_open:           frp::Sampler<bool>,
        output_value:      frp::Any<Option<ImString>>,
    },
    Initialized(Dropdown<ImString>, frp::Network),
}


impl LazyDropdown {
    fn new(
        app: &Application,
        display_object: &display::object::Instance,
        set_current_value: frp::Sampler<Option<ImString>>,
        is_open: frp::Sampler<bool>,
        output_value: frp::Any<Option<ImString>>,
    ) -> Self {
        let app = app.clone_ref();
        let display_object = display_object.clone_ref();
        let node_height = default();
        let entries = default();
        LazyDropdown::NotInitialized {
            app,
            display_object,
            node_height,
            entries,
            set_current_value,
            is_open,
            output_value,
        }
    }

    fn set_node_height(&mut self, new_node_height: f32) {
        match self {
            LazyDropdown::Initialized(dropdown, ..) => {
                dropdown.set_y(-new_node_height);
            }
            LazyDropdown::NotInitialized { node_height, .. } => {
                *node_height = new_node_height;
            }
        }
    }

    fn set_entries(&mut self, new_entries: Vec<ImString>) {
        match self {
            LazyDropdown::Initialized(dropdown, ..) => {
                dropdown.set_all_entries(new_entries);
            }
            LazyDropdown::NotInitialized { entries, .. } => {
                *entries = new_entries;
            }
        }
    }

    #[profile(Detail)]
    fn initialize_on_open(&mut self) {
        match self {
            LazyDropdown::Initialized(..) => {}
            LazyDropdown::NotInitialized {
                app,
                display_object,
                node_height,
                entries,
                is_open,
                set_current_value,
                output_value,
            } => {
                let dropdown = app.new_view::<Dropdown<ImString>>();
                display_object.add_child(&dropdown);
                app.display.default_scene.layers.above_nodes.add(&dropdown);
                dropdown.set_y(-*node_height);
                dropdown.set_max_open_size(Vector2(300.0, 500.0));
                dropdown.set_all_entries(std::mem::take(entries));
                dropdown.allow_deselect_all(true);

                frp::new_network! { network
                    init <- source_();
                    current_value <- all(set_current_value, &init)._0();
                    dropdown.set_selected_entries <+ current_value.map(|s| s.iter().cloned().collect());
                    first_selected_entry <- dropdown.selected_entries.map(|e| e.iter().next().cloned());

                    is_open <- all(is_open, &init)._0();
                    dropdown.set_open <+ is_open.on_change();

                    // Emit the output value only after actual user action. This prevents the
                    // dropdown from emitting its initial value when it is opened, which can
                    // represent slightly different version of code than actually written.
                    output_value <+ first_selected_entry.sample(&dropdown.user_select_action);

                }


                init.emit(());
                *self = LazyDropdown::Initialized(dropdown, network);
            }
        }
    }
}
