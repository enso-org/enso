//! Definition of all hardcoded node widget variants and common widget FRP API.

use crate::prelude::*;

use enso_config::ARGS;
use enso_frp as frp;
use ensogl::application::Application;
use ensogl::data::color;
use ensogl::display;
use ensogl::display::object::event;
use ensogl_component::drop_down::Dropdown;
use ensogl_component::drop_down::DropdownValue;


// ==============
// === Export ===
// ==============

pub mod vector_editor;



/// =================
/// === Constants ===
/// =================

const ACTIVATION_SHAPE_COLOR: color::Lch = color::Lch::new(0.56708, 0.23249, 0.71372);
const ACTIVATION_SHAPE_Y_OFFSET: f32 = -5.0;
const ACTIVATION_SHAPE_SIZE: Vector2 = Vector2(15.0, 11.0);
/// Distance between the dropdown and the bottom of the port.
const DROPDOWN_Y_OFFSET: f32 = 5.0;


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
        request_import(ImString),
    }
}

/// Widget metadata that comes from an asynchronous visualization. Defines which widget should be
/// used and a set of options that it should allow to choose from.
#[derive(Debug, Clone, PartialEq)]
#[allow(missing_docs)]
pub struct Metadata {
    pub kind:            Kind,
    pub display:         Display,
    /// Entries that should be displayed by the widget, as proposed by language server. This list
    /// is not exhaustive. The widget implementation might present additional options or allow
    /// arbitrary user input.
    pub dynamic_entries: Vec<Entry>,
}

/// Widget display mode. Determines when the widget should be expanded.
#[derive(serde::Deserialize, Debug, Clone, Copy, Default, PartialEq, Eq)]
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

/// Widget entry. Represents a possible value choice on the widget, as proposed by the language
/// server.
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct Entry {
    /// The expression that should be inserted by the widget. Note that  this expression can still
    /// be preprocessed by the widget before being inserted into the node.
    pub value:           ImString,
    /// The import that must be present in the module when the widget entry is selected.
    pub required_import: Option<ImString>,
    /// The text that should be displayed by the widget to represent this option. The exact
    /// appearance of the label is up to the widget implementation.
    pub label:           ImString,
}

impl From<&span_tree::TagValue> for Entry {
    fn from(tag_value: &span_tree::TagValue) -> Self {
        let value: ImString = (&tag_value.expression).into();
        let label: ImString = tag_value.label.as_ref().map_or_else(|| value.clone(), Into::into);
        let required_import = tag_value.required_import.clone().map(Into::into);
        Entry { value, required_import, label }
    }
}

impl Entry {
    /// Create an entry with the same value and label.
    pub fn from_value(value: ImString) -> Self {
        Self { label: value.clone(), required_import: None, value }
    }

    /// Cloning entry value getter.
    pub fn value(&self) -> ImString {
        self.value.clone()
    }

    /// Cloning entry getter of import that must be present for value insertion to be valid.
    pub fn required_import(&self) -> Option<ImString> {
        self.required_import.clone()
    }
}

impl DropdownValue for Entry {
    fn label(&self) -> ImString {
        self.label.clone()
    }
}

/// The data of node port that this widget is attached to. Available immediately after widget
/// creation. Can be updated later when the node data changes.
#[derive(Debug, Clone, Default, PartialEq)]
#[allow(missing_docs)]
pub struct NodeData {
    pub tag_values: Vec<span_tree::TagValue>,
    pub port_size:  Vector2,
    pub tp:         Option<String>,
}



/// ==================
/// === SampledFrp ===
/// ==================

/// Sampled version of widget FRP endpoints that can be used by widget views that are initialized
/// on demand after first interaction. Without samplers, when a widget view would be initialized
/// after the endpoints were set, it would not receive previously set endpoint values.
#[derive(Debug, Clone, CloneRef)]
pub struct SampledFrp {
    set_current_value:  frp::Sampler<Option<ImString>>,
    set_visible:        frp::Sampler<bool>,
    set_focused:        frp::Sampler<bool>,
    out_value_changed:  frp::Any<Option<ImString>>,
    out_request_import: frp::Any<ImString>,
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
            metadata_change <- input.set_metadata.on_change();
            node_data_change <- input.set_node_data.on_change();
            widget_data <- all(&metadata_change, &node_data_change).debounce();

            set_current_value <- input.set_current_value.sampler();
            set_visible <- input.set_visible.sampler();
            set_focused <- input.set_focused.sampler();
            let out_value_changed = frp.private.output.value_changed.clone_ref();
            let out_request_import = frp.private.output.request_import.clone_ref();
            let sampled_frp = SampledFrp { set_current_value, set_visible, set_focused, out_value_changed, out_request_import };

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
        const VECTOR_TYPE: &str = "Standard.Base.Data.Vector.Vector";
        let is_array_enabled = ARGS.groups.feature_preview.options.vector_editor.value;
        let is_array_type = node_data.tp.as_ref().map_or(false, |tp| tp.contains(VECTOR_TYPE));
        let has_tag_values = !node_data.tag_values.is_empty();
        let kind_fallback = (is_array_enabled && is_array_type)
            .then_some(Kind::VectorEditor)
            .or(has_tag_values.then_some(Kind::SingleChoice));

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
    /// A widget for constructing and modifying vector of various types.
    #[serde(rename = "Vector_Editor")]
    VectorEditor,
}

/// A part of widget model that is dependant on the widget kind.
#[derive(Debug)]
pub enum KindModel {
    /// A widget for selecting a single value from a list of available options.
    SingleChoice(SingleChoiceModel),
    /// A widget for constructing and modifying vector of various types.
    VectorEditor(vector_editor::Model),
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
            Kind::VectorEditor =>
                Self::VectorEditor(vector_editor::Model::new(app, display_object, frp)),
        };

        this.update(meta, node_data);
        this
    }

    fn update(&self, meta: &Option<Metadata>, node_data: &NodeData) {
        match self {
            KindModel::SingleChoice(inner) => {
                let dynamic_entries = meta.as_ref().map(|meta| meta.dynamic_entries.clone());
                let entries = dynamic_entries
                    .unwrap_or_else(|| node_data.tag_values.iter().map(Into::into).collect());

                inner.set_port_size(node_data.port_size);
                inner.set_entries(entries);
            }
            KindModel::VectorEditor(inner) => {
                warn!("VectorEditor updated with metadata {meta:#?} and node data {node_data:#?}.");
                inner.set_port_size.emit(node_data.port_size);
            }
        }
    }

    fn kind(&self) -> Kind {
        match self {
            Self::SingleChoice(_) => Kind::SingleChoice,
            Self::VectorEditor(_) => Kind::VectorEditor,
        }
    }
}



// ======================
// === Triangle Shape ===
// ======================

/// Temporary dropdown activation shape definition.
pub mod triangle {
    use super::*;
    ensogl::shape! {
        above = [
            crate::component::node::background,
            crate::component::node::input::port::hover
        ];
        alignment = center;
        (style:Style, color:Vector4) {
            let size   = Var::canvas_size();
            let radius = 1.0.px();
            let shrink = &radius * 2.0;
            let shape  = Triangle(size.x() - &shrink, size.y() - &shrink)
                .flip_y()
                .grow(radius);
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
    network:          frp::Network,
    dropdown:         Rc<RefCell<LazyDropdown>>,
    /// temporary click handling
    activation_shape: triangle::View,
}

impl SingleChoiceModel {
    fn new(
        app: &Application,
        display_object: &display::object::Instance,
        frp: &SampledFrp,
    ) -> Self {
        let activation_shape = triangle::View::new();
        activation_shape.set_size(ACTIVATION_SHAPE_SIZE);
        display_object.add_child(&activation_shape);

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
        let request_import = frp.out_request_import.clone_ref();
        let dropdown = LazyDropdown::new(
            app,
            display_object,
            set_current_value,
            is_open,
            dropdown_output,
            request_import,
        );
        let dropdown = Rc::new(RefCell::new(dropdown));

        frp::extend! { network
            let dot_clicked = activation_shape.events_deprecated.mouse_down_primary.clone_ref();
            toggle_focus <- dot_clicked.map(f!([display_object](()) !display_object.is_focused()));
            set_focused <- any(toggle_focus, frp.set_focused);
            eval set_focused([display_object](focus) match focus {
                true => display_object.focus(),
                false => display_object.blur(),
            });

            set_visible <- all(&frp.set_visible, &init)._0();
            shape_alpha <- set_visible.map(|visible| if *visible { 1.0 } else { 0.0 });
            shape_color <- shape_alpha.map(|a| ACTIVATION_SHAPE_COLOR.with_alpha(*a));
            eval shape_color([activation_shape] (color) {
                activation_shape.color.set(color::Rgba::from(color).into());
            });

            eval focus_in((_) dropdown.borrow_mut().initialize_on_open());
        }

        init.emit(());

        Self { network, dropdown, activation_shape }
    }

    fn set_port_size(&self, port_size: Vector2) {
        self.activation_shape.set_x(port_size.x() / 2.0);
        self.activation_shape
            .set_y(-port_size.y() / 2.0 - ACTIVATION_SHAPE_SIZE.y() - ACTIVATION_SHAPE_Y_OFFSET);
        self.dropdown.borrow_mut().set_port_size(port_size);
    }

    fn set_entries(&self, entries: Vec<Entry>) {
        self.dropdown.borrow_mut().set_entries(entries);
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
        dropdown_y:        f32,
        entries:           Vec<Entry>,
        set_current_value: frp::Sampler<Option<ImString>>,
        is_open:           frp::Sampler<bool>,
        output_value:      frp::Any<Option<ImString>>,
        request_import:    frp::Any<ImString>,
    },
    Initialized {
        _network:    frp::Network,
        dropdown:    Dropdown<Entry>,
        set_entries: frp::Any<Vec<Entry>>,
    },
}

impl LazyDropdown {
    fn new(
        app: &Application,
        display_object: &display::object::Instance,
        set_current_value: frp::Sampler<Option<ImString>>,
        is_open: frp::Sampler<bool>,
        output_value: frp::Any<Option<ImString>>,
        request_import: frp::Any<ImString>,
    ) -> Self {
        let app = app.clone_ref();
        let display_object = display_object.clone_ref();
        let dropdown_y = default();
        let entries = default();
        LazyDropdown::NotInitialized {
            app,
            display_object,
            dropdown_y,
            entries,
            set_current_value,
            is_open,
            output_value,
            request_import,
        }
    }

    fn set_port_size(&mut self, new_port_size: Vector2) {
        let y = -new_port_size.y() - DROPDOWN_Y_OFFSET;
        match self {
            LazyDropdown::Initialized { dropdown, .. } => {
                dropdown.set_y(y);
            }
            LazyDropdown::NotInitialized { dropdown_y, .. } => {
                *dropdown_y = y;
            }
        }
    }

    fn set_entries(&mut self, new_entries: Vec<Entry>) {
        match self {
            LazyDropdown::Initialized { set_entries, .. } => {
                set_entries.emit(new_entries);
            }
            LazyDropdown::NotInitialized { entries, .. } => {
                *entries = new_entries;
            }
        }
    }

    #[profile(Detail)]
    fn initialize_on_open(&mut self) {
        match self {
            LazyDropdown::Initialized { .. } => {}
            LazyDropdown::NotInitialized {
                app,
                display_object,
                dropdown_y,
                entries,
                is_open,
                set_current_value,
                output_value,
                request_import,
            } => {
                let dropdown = app.new_view::<Dropdown<Entry>>();
                display_object.add_child(&dropdown);
                app.display.default_scene.layers.above_nodes.add(&dropdown);
                dropdown.set_y(*dropdown_y);
                dropdown.set_max_open_size(Vector2(300.0, 500.0));
                dropdown.allow_deselect_all(true);

                frp::new_network! { network
                    init <- source_();
                    set_entries <- any(...);

                    dropdown.set_all_entries <+ set_entries;
                    entries_and_value <- all(&set_entries, set_current_value);
                    entries_and_value <- entries_and_value.debounce();

                    selected_entry <- entries_and_value.map(|(e, v)| entry_for_current_value(e, v));
                    dropdown.set_selected_entries <+ selected_entry.map(|e| e.iter().cloned().collect());

                    dropdown_entry <- dropdown.selected_entries.map(|e| e.iter().next().cloned());
                    // Emit the output value only after actual user action. This prevents the
                    // dropdown from emitting its initial value when it is opened, which can
                    // represent slightly different version of code than actually written.
                    submitted_entry <- dropdown_entry.sample(&dropdown.user_select_action);
                    dropdown_out_value <- submitted_entry.map(|e| e.as_ref().map(Entry::value));
                    dropdown_out_import <- submitted_entry.map(|e| e.as_ref().and_then(Entry::required_import));
                    request_import <+ dropdown_out_import.unwrap();
                    output_value <+ dropdown_out_value.sample(&dropdown.user_select_action);

                    is_open <- all(is_open, &init)._0();
                    dropdown.set_open <+ is_open.on_change();

                    // Close the dropdown after a short delay after selection. Because the dropdown
                    // value application triggers operations that can introduce a few dropped frames,
                    // we want to delay the dropdown closing animation after that is handled.
                    // Otherwise the animation finishes within single frame, which looks bad.
                    let close_after_selection_timer = frp::io::timer::Timeout::new(&network);
                    close_after_selection_timer.restart <+ dropdown.user_select_action.constant(1);
                    eval close_after_selection_timer.on_expired((()) display_object.blur());
                }

                set_entries.emit(std::mem::take(entries));
                init.emit(());
                *self = LazyDropdown::Initialized { _network: network, dropdown, set_entries };
            }
        }
    }
}

fn entry_for_current_value(
    all_entries: &[Entry],
    current_value: &Option<ImString>,
) -> Option<Entry> {
    let current_value = current_value.clone()?;
    let found_entry = all_entries.iter().find(|entry| entry.value.as_ref() == current_value);
    let with_partial_match = found_entry.or_else(|| {
        // Handle parentheses in current value. Entries with parenthesized expressions will match if
        // they start with the same expression as the current value. That way it is still matched
        // once extra arguments are added to the nested function call.
        if current_value.starts_with('(') {
            let current_value = current_value.trim_start_matches('(').trim_end_matches(')');
            all_entries.iter().find(|entry| {
                let trimmed_value = entry.value.trim_start_matches('(').trim_end_matches(')');
                current_value.starts_with(trimmed_value)
            })
        } else {
            None
        }
    });

    let with_fallback =
        with_partial_match.cloned().unwrap_or_else(|| Entry::from_value(current_value.clone()));
    Some(with_fallback)
}
