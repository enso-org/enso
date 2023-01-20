//! Definition of all hardcoded node widget variants and common widget FRP API.

use crate::prelude::*;

use enso_frp as frp;
use ensogl::application::Application;
use ensogl::data::color;
use ensogl::display;
use ensogl::display::object::event;
use ensogl_component::drop_down::Dropdown;



// ==================
// === NodeWidget ===
// ==================

ensogl::define_endpoints_2! {
    Input {
        set_metadata   (Option<Metadata>),
        set_node_data  (NodeData),
        set_current_value (Option<ImString>),
        set_focused       (bool),
    }
    Output {
        value_changed(Option<ImString>),
    }
}

/// Widget metadata that needs to be fetched from the graph asynchronously.
#[derive(Debug, Clone)]
pub struct Metadata {
    /// The kind of widget to use.
    pub kind:            Kind,
    /// The widget display mode.
    pub display:         Display,
    /// Dynamically fetched widget entries.
    pub dynamic_entries: Vec<ImString>,
}

/// Widget display mode. Determines when the widget should be expanded.
#[derive(serde::Deserialize, Debug, Clone, Copy)]
pub enum Display {
    /// The widget should always be in its expanded mode.
    Always,
    /// The widget should only be in its expanded mode when it has non-default value.
    #[serde(rename = "When_Modified")]
    WhenModified,
    /// The widget should only be in its expanded mode whe the whole node is expanded.
    #[serde(rename = "Expanded_Only")]
    ExpandedOnly,
}

impl Default for Display {
    fn default() -> Self {
        Self::Always
    }
}

/// The data of node port that this widget is attached to. Available immediately after widget
/// creation. Can be updated later when the node data changes.
#[derive(Debug, Clone, Default)]
pub struct NodeData {
    /// Argument info of the node port that this widget is attached to.
    pub argument_info: span_tree::ArgumentInfo,
    /// The current height of the node that the widget can use for layout.
    /// TODO [pg]: remove and use the automatic layout instead.
    pub node_height:   f32,
}

/// The node widget UI widget.
#[derive(Debug, Clone, CloneRef)]
pub struct Widget {
    frp:   Frp,
    model: Rc<Model>,
}


impl Widget {
    /// Create a new node widget. The widget is initialized into the `Unset` state, waiting for
    /// metadata to be set.
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

        let weak_frp = frp.downgrade();
        frp::extend! { network
            widget_data <- all(&input.set_metadata, &input.set_node_data);
            eval widget_data([model, weak_frp]((meta, node_data)) {
                model.set_widget_data(&weak_frp, meta, node_data);
            });
        }

        self
    }
}

#[derive(Debug)]
struct Model {
    common: ModelCommon,
    inner:  RefCell<ModelInner>,
}
impl Model {
    /// Create a new node widget, selecting the appropriate widget type based on the provided
    /// argument info.
    fn new(app: &Application) -> Self {
        let common = ModelCommon::new(app);
        let inner = RefCell::new(ModelInner::Unset);
        Self { common, inner }
    }

    fn set_widget_data(&self, frp: &WeakFrp, meta: &Option<Metadata>, node_data: &NodeData) {
        let kind_fallback = || {
            if !node_data.argument_info.tag_values.is_empty() {
                Kind::SingleChoice
            } else {
                Kind::Unset
            }
        };

        let desired_kind = meta.as_ref().map_or_else(kind_fallback, |m| m.kind);

        let kind_changed = desired_kind != self.inner.borrow().kind();
        if kind_changed {
            *self.inner.borrow_mut() =
                ModelInner::new(desired_kind, &self.common, frp, meta, node_data);
        } else {
            self.inner.borrow().update(&self.common, meta, node_data);
        }
    }
}

impl Deref for Widget {
    type Target = Frp;
    fn deref(&self) -> &Self::Target {
        self.frp()
    }
}

impl display::Object for Widget {
    fn display_object(&self) -> &display::object::Instance {
        &self.model.common.display_object
    }
}


#[derive(Debug)]
struct ModelCommon {
    app:            Application,
    display_object: display::object::Instance,
}

impl ModelCommon {
    fn new(app: &Application) -> Self {
        let app = app.clone_ref();
        let display_object = display::object::Instance::new();
        Self { app, display_object }
    }
}

// =========================
// === ModelInner / Kind ===
// =========================

/// Possible widgets for a node input.
///
/// Currently all widget types are hardcoded. This is likely to be a temporary solution. In the
/// future the set of widget types might be dynamic, similar to visualizations.
#[derive(serde::Deserialize, Clone, Copy, Debug, PartialEq, Eq)]
pub enum Kind {
    /// Placeholder widget data when no data is available.
    #[serde(skip)]
    Unset,
    /// A widget for selecting a single value from a list of available options.
    #[serde(rename = "Single_Choice")]
    SingleChoice,
}

/// The widget model that is dependant on the widget kind.
#[derive(Debug)]
pub enum ModelInner {
    /// Placeholder widget data when no data is available.
    Unset,
    /// A widget for selecting a single value from a list of available options.
    SingleChoice(SingleChoiceModel),
}

impl ModelInner {
    fn new(
        kind: Kind,
        common: &ModelCommon,
        frp: &WeakFrp,
        meta: &Option<Metadata>,
        node_data: &NodeData,
    ) -> Self {
        match kind {
            Kind::SingleChoice => {
                let entries =
                    meta.as_ref().map(|meta| meta.dynamic_entries.clone()).unwrap_or_else(|| {
                        node_data.argument_info.tag_values.iter().map(Into::into).collect()
                    });
                Self::SingleChoice(SingleChoiceModel::new(
                    common,
                    frp,
                    node_data.node_height,
                    entries,
                ))
            }
            _ => Self::Unset,
        }
    }
    fn update(&self, _common: &ModelCommon, meta: &Option<Metadata>, node_data: &NodeData) {
        match self {
            ModelInner::Unset => {}
            ModelInner::SingleChoice(inner) => {
                let entries =
                    meta.as_ref().map(|meta| meta.dynamic_entries.clone()).unwrap_or_else(|| {
                        node_data.argument_info.tag_values.iter().map(Into::into).collect()
                    });
                warn!("New entries: {entries:?}");
                inner.set_node_height(node_data.node_height);
                inner.set_entries(entries);
            }
        }
    }
    fn kind(&self) -> Kind {
        match self {
            Self::Unset => Kind::Unset,
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
    fn new(common: &ModelCommon, frp: &WeakFrp, node_height: f32, entries: Vec<ImString>) -> Self {
        let display_object = &common.display_object;
        let input = &frp.input;
        let output = &frp.private.output;

        let activation_dot = dot::View::new();
        let color: color::Rgba = color::Lcha::new(0.56708, 0.23249, 0.71372, 1.0).into();
        activation_dot.color.set(color.into());
        activation_dot.set_size((15.0, 15.0));
        activation_dot.set_y(-node_height / 2.0);
        display_object.add_child(&activation_dot);

        frp::new_network! { network
            set_current_value <- input.set_current_value.sampler();
        };

        let dropdown = LazyDropdown::NotInitialized {
            app: common.app.clone_ref(),
            display_object: display_object.clone_ref(),
            node_height,
            entries,
            network: network.clone_ref(),
            set_current_value,
            output_value_changed: output.value_changed.clone_ref(),
        };
        let dropdown = Rc::new(RefCell::new(dropdown));

        frp::extend! { network
            let dot_clicked = activation_dot.events.mouse_down_primary.clone_ref();
            toggle_focus <- dot_clicked.map(f!([display_object](()) !display_object.is_focused()));
            set_focused <- any(toggle_focus, input.set_focused);
            eval set_focused([display_object](focus) match focus {
                true => display_object.focus(),
                false => display_object.blur(),
            });

            let focus_in = display_object.on_event::<event::FocusIn>();
            eval focus_in((_) dropdown.borrow_mut().initialize_on_open());
        }

        Self { network, dropdown, activation_dot }
    }

    fn set_node_height(&self, node_height: f32) {
        self.activation_dot.set_y(-node_height / 2.0);
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
        app:                  Application,
        display_object:       display::object::Instance,
        node_height:          f32,
        entries:              Vec<ImString>,
        network:              frp::Network,
        set_current_value:    frp::Sampler<Option<ImString>>,
        output_value_changed: frp::Any<Option<ImString>>,
    },
    Initialized(Dropdown<ImString>),
}


impl LazyDropdown {
    fn set_node_height(&mut self, new_node_height: f32) {
        match self {
            LazyDropdown::Initialized(dropdown) => {
                dropdown.set_y(-new_node_height);
            }
            LazyDropdown::NotInitialized { node_height, .. } => {
                *node_height = new_node_height;
            }
        }
    }

    fn set_entries(&mut self, new_entries: Vec<ImString>) {
        match self {
            LazyDropdown::Initialized(dropdown) => {
                dropdown.set_all_entries(new_entries);
            }
            LazyDropdown::NotInitialized { entries, .. } => {
                *entries = new_entries;
            }
        }
    }

    fn initialize_on_open(&mut self) {
        match self {
            LazyDropdown::Initialized(_) => {}
            LazyDropdown::NotInitialized {
                app,
                display_object,
                node_height,
                entries,
                network,
                set_current_value,
                output_value_changed,
            } => {
                let dropdown = app.new_view::<Dropdown<ImString>>();
                display_object.add_child(&dropdown);
                app.display.default_scene.layers.above_nodes.add(&dropdown);
                dropdown.set_y(-*node_height);
                dropdown.set_max_open_size(Vector2(300.0, 500.0));
                dropdown.set_all_entries(std::mem::take(entries));
                dropdown.allow_deselect_all(true);


                frp::extend! { network
                    init <- source_();
                    current_value <- all(set_current_value, &init)._0();
                    dropdown.set_selected_entries <+ current_value.map(|s| s.iter().cloned().collect());
                    first_selected_entry <- dropdown.selected_entries.map(|e| e.iter().next().cloned());
                    output_value_changed <+ first_selected_entry.on_change();

                    let focus_in = display_object.on_event::<event::FocusIn>();
                    let focus_out = display_object.on_event::<event::FocusOut>();
                    is_focused <- bool(&focus_out, &focus_in);

                    dropdown.set_open <+ is_focused;
                }

                init.emit(());
                dropdown.set_open(true);

                *self = LazyDropdown::Initialized(dropdown);
            }
        }
    }
}
