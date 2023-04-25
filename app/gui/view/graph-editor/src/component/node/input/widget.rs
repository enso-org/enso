//! Node widgets hierarchy. This module defines a widget [`Tree`] view, which manages all widgets
//! and edge ports for a given node. The widgets are organized in a tree structure, where each
//! widget can create multiple child widgets and organize their display objects according to its
//! needs. When node's expression is changed, the widget tree is rebuilt, attempting to preserve
//! as many widgets as possible, which allows widgets to maintain internal state.
//!
//!
//! # Widget Lifecycle
//!
//! The widget lifecycle is managed using [`SpanWidget`] trait.
//!
//! When a widget tree is built for the first time (or the expression has been completely changed),
//! all widgets are created from scratch, using the [`SpanWidget::new`] method. During this phase,
//! each widget can initialize its own view structure and create an FRP network. Immediately after
//! the widget is created, it is configured for the first time using [`SpanWidget::configure`]
//! method.
//!
//! During configuration, the widget should declare its child widgets and place them in its
//! view structure, as well as emit its own internal FRP events to update its view's state.
//!
//! For each subsequent expression change or configuration update, the widget tree is rebuilt,
//! reusing the same widgets for nodes that maintained their identity (see [`WidgetIdentity`]).
//! When a widget is reused, the [`SpanWidget::configure`] method is called, allowing the widget to
//! update its view and declare its child widgets again. Usually, the same children are declared,
//! allowing the build process to propagate down the tree and reuse existing child widgets as well.
//!
//! Whenever a configuration change causes a widget to change its kind (e.g. from a simple label to
//! a single choice dropdown), the widget is removed and a new one is created in its place.
//!
//!
//! # Widget Configuration
//!
//! Each widget kind has its own configuration type, which is used to pass additional data to the
//! widget, as inferred from the expression, or provided by external source as an override. The
//! configuration source is determined in order:
//! 1. If a parent widget has directly provided a configuration for its child, it is always used.
//!    Parent widget can provide it by using [`TreeBuilder::child_widget_of_type`] method.
//! 2. If there is a configuration override that matches given span, it is used. The configuration
//!    overrides are defined at the whole tree level, and can be provided using
//!    [`Tree::set_config_override`] method.
//! 3. The default configuration for the node is created using [`Configuration::from_node`] method.
//!    It uses the combination of span tree node kind data and type information to decide which
//!    widget is the best fit for the node.

use crate::prelude::*;

use crate::component::node::input::area::NODE_HEIGHT;
use crate::component::node::input::area::TEXT_OFFSET;
use crate::component::node::input::port::Port;

use enso_config::ARGS;
use enso_frp as frp;
use enso_text as text;
use ensogl::application::Application;
use ensogl::data::color;
use ensogl::display;
use ensogl::display::shape::StyleWatch;
use ensogl::gui::cursor;
use ensogl_component::drop_down::DropdownValue;
use span_tree::node::Ref as SpanRef;
use text::index::Byte;



// =================
// === Constants ===
// =================

/// Spacing between sibling widgets per each code character that separates them. Current value is
/// based on the space glyph width at node's default font size, so that entering node edit mode
/// introduces the least amount of visual changes. The value can be adjusted once we implement
/// granular edit mode that works with widgets.
pub const WIDGET_SPACING_PER_OFFSET: f32 = 7.224_609_4;

/// The maximum depth of the widget port that is still considered primary. This is used to determine
/// the hover area of the port.
pub const PRIMARY_PORT_MAX_NESTING_LEVEL: usize = 0;



// ===========
// === FRP ===
// ===========

ensogl::define_endpoints_2! {
    Input {
        set_ports_visible    (bool),
        set_read_only        (bool),
        set_view_mode        (crate::view::Mode),
        set_profiling_status (crate::node::profiling::Status),
        set_disabled         (bool),
    }
    Output {
        value_changed    (span_tree::Crumbs, Option<ImString>),
        request_import   (ImString),
        on_port_hover    (Switch<span_tree::Crumbs>),
        on_port_press    (span_tree::Crumbs),
        pointer_style    (cursor::Style),
        /// Any of the connected port's display object within the widget tree has been updated. This
        /// signal is generated using the `on_updated` signal of the `display_object` of the widget,
        /// all caveats of that signal apply here as well.
        connected_port_updated (),
        /// Tree data update recently caused it to be marked as dirty. Rebuild is required.
        rebuild_required (),
        /// Dirty flag has been marked. This signal is fired immediately after the update that
        /// caused it. Prefer using `rebuild_required` signal instead, which is debounced.
        marked_dirty_sync (),
    }
}

/// A key used for overriding widget configuration. Allows locating the widget that should be
/// configured using provided external data.
#[derive(Debug, Default, Clone, Hash, PartialEq, Eq)]
pub struct OverrideKey {
    /// The function call associated with the widget.
    pub call_id:       ast::Id,
    /// The name of function argument at which the widget is located.
    pub argument_name: ImString,
}


// ======================
// === Widget modules ===
// ======================

/// Common trait for constructing and reconfiguring all widget variants. See "Widget Lifecycle"
/// section of the module documentation for more details.
pub trait SpanWidget {
    /// Configuration associated with specific widget variant.
    type Config: Debug + Clone + PartialEq;
    /// Root display object of a widget. It is returned to the parent widget for positioning.
    fn root_object(&self) -> &display::object::Instance;
    /// Create a new widget with given configuration.
    fn new(config: &Self::Config, ctx: &ConfigContext) -> Self;
    /// Update configuration for existing widget.
    fn configure(&mut self, config: &Self::Config, ctx: ConfigContext);
}


/// Generate implementation for [`DynWidget`] enum and its associated [`Config`] enum. Those enums
/// are used to represent any possible widget kind and its configuration.
macro_rules! define_widget_modules(
    ($(
        $(#[$meta:meta])*
        $name:ident $module:ident,
    )*) => {
        $(pub mod $module;)*

        /// A widget configuration that determines the widget kind.
        #[derive(Debug, Clone, PartialEq)]
        #[allow(missing_docs)]
        pub enum DynConfig {
            $($name(<$module::Widget as SpanWidget>::Config),)*
        }

        /// The node widget view. Represents one widget of any kind on the node input area. Can
        /// change its appearance and behavior depending on the widget configuration updates,
        /// without being recreated. New widget can be created using the `new` method, while the
        /// existing widget can be reconfigured using the `configure` method.
        ///
        /// When a new configuration is applied, the existing widget will handle the update using
        /// its `configure` method. If the new configuration requires a different widget kind, the
        /// widget of new kind will be created and the old one will be dropped.
        #[derive(Debug)]
        #[allow(missing_docs)]
        pub enum DynWidget {
            $(
                $(#[$meta])*
                $name($module::Widget)
            ),*
        }

        $(
            impl const From<<$module::Widget as SpanWidget>::Config> for DynConfig {
                fn from(config: <$module::Widget as SpanWidget>::Config) -> Self {
                    Self::$name(config)
                }
            }

            impl const From<$module::Widget> for DynWidget {
                fn from(config: $module::Widget) -> Self {
                    Self::$name(config)
                }
            }
        )*

        impl SpanWidget for DynWidget {
            type Config = DynConfig;
            fn root_object(&self) -> &display::object::Instance {
                match self {
                    $(DynWidget::$name(inner) => inner.root_object(),)*
                }
            }

            fn new(config: &DynConfig, ctx: &ConfigContext) -> Self {
                match config {
                    $(DynConfig::$name(config) => DynWidget::$name(SpanWidget::new(config, ctx)),)*
                }
            }

            fn configure(&mut self, config: &DynConfig, ctx: ConfigContext) {
                match (self, config) {
                    $((DynWidget::$name(model), DynConfig::$name(config)) => {
                        SpanWidget::configure(model, config, ctx);
                    },)*
                    (this, _) => {
                        *this = SpanWidget::new(config, &ctx);
                        this.configure(config, ctx)
                    },
                }
            }
        }
    };
);

define_widget_modules! {
    /// Default widget that only displays text.
    Label label,
    /// Empty widget that does not display anything, used for empty insertion points.
    InsertionPoint insertion_point,
    /// A widget for selecting a single value from a list of available options.
    SingleChoice single_choice,
    /// A widget for managing a list of values - adding, removing or reordering them.
    ListEditor list_editor,
    /// Default span tree traversal widget.
    Hierarchy hierarchy,
}

// =====================
// === Configuration ===
// =====================

/// The configuration of a widget and its display properties. Defines how the widget should be
/// displayed, if it should be displayed at all, and whether or not it should have a port. Widgets
/// that declare themselves as having a port will be able to handle edge connections and visually
/// indicate that they are connected.
#[derive(Debug, Clone, PartialEq)]
#[allow(missing_docs)]
pub struct Configuration {
    /// Display mode of the widget: determines whether or not the widget should be displayed
    /// depending on current tree display mode.
    pub display:  Display,
    /// Whether or not the widget can receive a port. If `true`, the widget can be wrapped in a
    /// [`Port`] struct, but it is not guaranteed. If multiple widgets created at single span node
    /// declare themselves as wanting a port, only one of them will actually have one.
    pub has_port: bool,
    /// Configuration specific to given widget kind.
    pub kind:     DynConfig,
}

impl Configuration {
    /// Derive widget configuration from Enso expression, node data in span tree and inferred value
    /// type. When no configuration is provided with an override, this function will be used to
    /// create a default configuration.
    fn from_node(span_node: &SpanRef, usage_type: Option<crate::Type>, expression: &str) -> Self {
        use span_tree::node::Kind;

        let kind = &span_node.kind;
        let has_children = !span_node.children.is_empty();

        const VECTOR_TYPE: &str = "Standard.Base.Data.Vector.Vector";
        let is_list_editor_enabled = ARGS.groups.feature_preview.options.vector_editor.value;
        let is_vector = |arg: &span_tree::node::Argument| {
            let type_matches = usage_type
                .as_ref()
                .map(|t| t.as_str())
                .or(arg.tp.as_deref())
                .map_or(false, |tp| tp.contains(VECTOR_TYPE));
            if type_matches {
                let node_expr = &expression[span_node.span()];
                node_expr.starts_with('[') && node_expr.ends_with(']')
            } else {
                false
            }
        };

        match kind {
            Kind::Argument(arg) if !arg.tag_values.is_empty() =>
                Self::static_dropdown(arg.name.as_ref().map(Into::into), &arg.tag_values),
            Kind::Argument(arg) if is_list_editor_enabled && is_vector(arg) => Self::list_editor(),
            Kind::InsertionPoint(arg) if arg.kind.is_expected_argument() =>
                if !arg.tag_values.is_empty() {
                    Self::static_dropdown(arg.name.as_ref().map(Into::into), &arg.tag_values)
                } else {
                    Self::always(label::Config::default())
                },
            Kind::Token | Kind::Operation if !has_children => Self::inert(label::Config::default()),
            Kind::NamedArgument => Self::inert(hierarchy::Config),
            Kind::InsertionPoint(_) => Self::inert(insertion_point::Config),
            _ if has_children => Self::always(hierarchy::Config),
            _ => Self::always(label::Config::default()),
        }
    }

    const fn always<C>(kind: C) -> Self
    where C: ~const Into<DynConfig> {
        Self { display: Display::Always, kind: kind.into(), has_port: true }
    }

    const fn inert<C>(kind: C) -> Self
    where C: ~const Into<DynConfig> {
        Self { display: Display::Always, kind: kind.into(), has_port: false }
    }

    /// Widget configuration for static dropdown, based on the tag values provided by suggestion
    /// database.
    fn static_dropdown(
        label: Option<ImString>,
        tag_values: &[span_tree::TagValue],
    ) -> Configuration {
        let entries = Rc::new(tag_values.iter().map(Entry::from).collect());
        Self::always(single_choice::Config { label, entries })
    }

    fn list_editor() -> Configuration {
        Self::always(list_editor::Config { item_widget: None, item_default: "_".into() })
    }
}

/// Widget display mode. Determines when the widget should be expanded.
#[derive(serde::Deserialize, Debug, Clone, Copy, Default, PartialEq, Eq)]
#[serde(tag = "constructor")]
pub enum Display {
    /// The widget should always be in its expanded mode.
    #[default]
    Always,
    /// The widget should only be in its expanded mode when it has non-default value.
    #[serde(rename = "When_Modified")]
    WhenModified,
    /// The widget should only be in its expanded mode when the whole node is expanded.
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



// ==================
// === WidgetsFrp ===
// ==================

/// Widget FRP endpoints that can be used by widget views, and go straight to the root.
#[derive(Debug, Clone, CloneRef)]
pub struct WidgetsFrp {
    pub(super) set_ports_visible:      frp::Sampler<bool>,
    pub(super) set_read_only:          frp::Sampler<bool>,
    pub(super) set_view_mode:          frp::Sampler<crate::view::Mode>,
    pub(super) set_profiling_status:   frp::Sampler<crate::node::profiling::Status>,
    pub(super) value_changed:          frp::Any<(span_tree::Crumbs, Option<ImString>)>,
    pub(super) request_import:         frp::Any<ImString>,
    pub(super) on_port_hover:          frp::Any<Switch<span_tree::Crumbs>>,
    pub(super) on_port_press:          frp::Any<span_tree::Crumbs>,
    pub(super) pointer_style:          frp::Any<cursor::Style>,
    pub(super) connected_port_updated: frp::Any<()>,
}



// ============
// === Tree ===
// ============

/// The node widget tree view. Contains all widgets created from the node's span tree, as well as
/// all input ports of a node. The tree is initialized to empty state, waiting for first
/// `rebuild_tree` call to build appropriate view hierarchy.
#[derive(Debug, Deref, Clone, CloneRef)]
pub struct Tree {
    #[deref]
    frp:         Frp,
    widgets_frp: WidgetsFrp,
    model:       Rc<TreeModel>,
}

impl display::Object for Tree {
    fn display_object(&self) -> &display::object::Instance {
        &self.model.display_object
    }
}

impl Tree {
    /// Create a new node widget. The widget is initialized to empty state, waiting for first
    /// `rebuild_tree` call to build appropriate view hierarchy.
    #[profile(Task)]
    pub fn new(app: &Application) -> Self {
        let frp = Frp::new();
        let model = Rc::new(TreeModel::new(app));

        let network = &frp.network;

        frp::extend! { network
            frp.private.output.rebuild_required <+ frp.marked_dirty_sync.debounce();

            set_ports_visible <- frp.set_ports_visible.sampler();
            set_read_only <- frp.set_read_only.sampler();
            set_view_mode <- frp.set_view_mode.sampler();
            set_profiling_status <- frp.set_profiling_status.sampler();

            on_port_hover <- any(...);
            on_port_press <- any(...);
            frp.private.output.on_port_hover <+ on_port_hover;
            frp.private.output.on_port_press <+ on_port_press;
        }

        let value_changed = frp.private.output.value_changed.clone_ref();
        let request_import = frp.private.output.request_import.clone_ref();
        let pointer_style = frp.private.output.pointer_style.clone_ref();
        let connected_port_updated = frp.private.output.connected_port_updated.clone_ref();
        let widgets_frp = WidgetsFrp {
            set_ports_visible,
            set_read_only,
            set_view_mode,
            set_profiling_status,
            value_changed,
            request_import,
            on_port_hover,
            on_port_press,
            pointer_style,
            connected_port_updated,
        };

        Self { frp, widgets_frp, model }
    }

    /// Override widget configuration. The configuration is used to determine the widget appearance
    /// and behavior. By default, the widget configuration will be inferred from its span tree kind
    /// and type. However, in some cases, we want to change the selected widget for a given span
    /// tree node, and it can be done by calling this method. The set configuration is persistent,
    /// and will be applied to any future widget of this node that matches given pointer.
    pub fn set_config_override(&self, pointer: OverrideKey, config: Option<Configuration>) {
        self.notify_dirty(self.model.set_config_override(pointer, config));
    }

    /// Set the inferred type of the expression for given ast ID. On rebuild, the type will be
    /// linked with any widget created on any span with matching AST ID. It is used to determine the
    /// widget appearance and default inferred widget configuration.
    pub fn set_usage_type(&self, ast_id: ast::Id, usage_type: Option<crate::Type>) {
        self.notify_dirty(self.model.set_usage_type(ast_id, usage_type));
    }

    /// Set connection status for given span crumbs. The connected nodes will be highlighted with a
    /// different color, and the widgets might change behavior depending on the connection
    /// status.
    pub fn set_connected(&self, crumbs: &span_tree::Crumbs, status: Option<color::Lcha>) {
        self.notify_dirty(self.model.set_connected(crumbs, status));
    }

    /// Set disabled status for given span tree node. The disabled nodes will be grayed out.
    /// The widgets might change behavior depending on the disabled status.
    pub fn set_disabled(&self, disabled: bool) {
        self.notify_dirty(self.model.set_disabled(disabled));
    }


    /// Rebuild tree if it has been marked as dirty. The dirty flag is marked whenever more data
    /// external to the span-tree is provided, using `set_config_override`, `set_usage_type`,
    /// `set_connected` or `set_disabled` methods of the widget tree.
    pub fn rebuild_tree_if_dirty(
        &self,
        tree: &span_tree::SpanTree,
        node_expression: &str,
        styles: &StyleWatch,
    ) {
        if self.model.tree_dirty.get() {
            self.rebuild_tree(tree, node_expression, styles);
        }
    }

    /// Rebuild the widget tree using given span tree expression. All widgets necessary for the
    /// provided expression will be created and added to the view hierarchy. If the tree has been
    /// already built, existing widgets will be reused in the parts of the expression that did not
    /// change since then.
    pub fn rebuild_tree(
        &self,
        tree: &span_tree::SpanTree,
        node_expression: &str,
        styles: &StyleWatch,
    ) {
        self.model.rebuild_tree(self.widgets_frp.clone_ref(), tree, node_expression, styles)
    }

    /// Get the root display object of the widget port for given span tree node. Not all nodes must
    /// have a distinct widget, so the returned value might be [`None`].
    pub fn get_port_display_object(
        &self,
        span_node: &SpanRef,
    ) -> Option<display::object::Instance> {
        let pointer = self.model.get_node_widget_pointer(span_node);
        self.model.with_port(pointer, |w| w.display_object().clone())
    }

    /// Get hover shapes for all ports in the tree. Used in tests to manually dispatch mouse events.
    pub fn port_hover_shapes(&self) -> Vec<super::port::HoverShape> {
        let nodes = self.model.nodes_map.borrow();
        self.model
            .hierarchy
            .borrow()
            .iter()
            .filter_map(|n| nodes.get(&n.identity))
            .filter_map(|e| Some(e.node.port()?.hover_shape().clone_ref()))
            .collect_vec()
    }

    fn notify_dirty(&self, dirty_flag_just_set: bool) {
        if dirty_flag_just_set {
            self.frp.private.output.marked_dirty_sync.emit(());
        }
    }
}


// ================
// === TreeNode ===
// ================

/// A single entry in the widget tree. If the widget has an attached port, it will be wrapped in
/// `Port` struct and stored in `Port` variant. Otherwise, the widget will be stored directly using
/// the `Widget` node variant.
#[derive(Debug)]
pub(super) enum TreeNode {
    /// A tree node that contains a port. The port wraps a widget.
    Port(Port),
    /// A tree node without a port, directly containing a widget.
    Widget(DynWidget),
}

impl TreeNode {
    fn port(&self) -> Option<&Port> {
        match self {
            TreeNode::Port(port) => Some(port),
            TreeNode::Widget(_) => None,
        }
    }
}

impl display::Object for TreeNode {
    fn display_object(&self) -> &display::object::Instance {
        match self {
            TreeNode::Port(port) => port.display_object(),
            TreeNode::Widget(widget) => widget.root_object(),
        }
    }
}

/// Hierarchy structure that can be used to quickly navigate the tree.
#[derive(Debug, Clone, Copy)]
struct NodeHierarchy {
    identity:          WidgetIdentity,
    parent_index:      Option<usize>,
    total_descendants: usize,
}

/// Single entry in the tree.
#[derive(Debug)]
struct TreeEntry {
    node:  TreeNode,
    /// Index in the `hierarchy` vector.
    index: usize,
}



// ================
// === EdgeData ===
// ================

/// Data associated with an edge connected to a port in the tree. It is accessible to the connected
/// port, its widget and all its descendants through `connection` and `subtree_connection` fields
/// of  [`NodeState`].
#[derive(Debug, Clone, Copy, PartialEq)]
pub(super) struct EdgeData {
    /// Color of an edge connected to the port.
    pub color: color::Lcha,
    /// Span tree depth at which the connection is made.
    pub depth: usize,
}



// =================
// === TreeModel ===
// =================

#[derive(Debug)]
struct TreeModel {
    app:            Application,
    display_object: display::object::Instance,
    /// A map from widget identity to the tree node and its index in the `hierarchy` vector.
    nodes_map:      RefCell<HashMap<WidgetIdentity, TreeEntry>>,
    /// Hierarchy data for nodes, stored in node insertion order (effectively depth-first). It can
    /// be used to quickly find the parent of a node, or iterate over all children or descendants
    /// of a node.
    hierarchy:      RefCell<Vec<NodeHierarchy>>,
    ports_map:      RefCell<HashMap<StableSpanIdentity, usize>>,
    override_map:   Rc<RefCell<HashMap<OverrideKey, Configuration>>>,
    connected_map:  Rc<RefCell<HashMap<span_tree::Crumbs, color::Lcha>>>,
    usage_type_map: Rc<RefCell<HashMap<ast::Id, crate::Type>>>,
    node_disabled:  Cell<bool>,
    tree_dirty:     Cell<bool>,
}

impl TreeModel {
    /// Create a new node widget, selecting the appropriate widget type based on the provided
    /// argument info.
    fn new(app: &Application) -> Self {
        let app = app.clone_ref();
        let display_object = display::object::Instance::new();
        display_object.use_auto_layout();
        display_object.set_children_alignment_left_center().justify_content_center_y();
        display_object.set_size_y(NODE_HEIGHT);
        display_object.set_padding_left(TEXT_OFFSET);
        display_object.set_padding_right(TEXT_OFFSET);

        Self {
            app,
            display_object,
            node_disabled: default(),
            nodes_map: default(),
            hierarchy: default(),
            ports_map: default(),
            override_map: default(),
            connected_map: default(),
            usage_type_map: default(),
            tree_dirty: default(),
        }
    }

    /// Mark dirty flag if the tree has been modified. Return true if the flag has been changed.
    fn mark_dirty_flag(&self, modified: bool) -> bool {
        if modified && !self.tree_dirty.get() {
            self.tree_dirty.set(true);
            true
        } else {
            false
        }
    }

    /// Set the configuration under given key. It may cause the tree to be marked as dirty.
    fn set_config_override(&self, pointer: OverrideKey, config: Option<Configuration>) -> bool {
        let mut map = self.override_map.borrow_mut();
        let dirty = map.synchronize_entry(pointer, config);
        self.mark_dirty_flag(dirty)
    }

    /// Set the connection status under given widget. It may cause the tree to be marked as dirty.
    fn set_connected(&self, crumbs: &span_tree::Crumbs, status: Option<color::Lcha>) -> bool {
        let mut map = self.connected_map.borrow_mut();
        let dirty = map.synchronize_entry(crumbs.clone(), status);
        self.mark_dirty_flag(dirty)
    }

    /// Set the usage type of an expression. It may cause the tree to be marked as dirty.
    fn set_usage_type(&self, ast_id: ast::Id, usage_type: Option<crate::Type>) -> bool {
        let mut map = self.usage_type_map.borrow_mut();
        let dirty = map.synchronize_entry(ast_id, usage_type);
        self.mark_dirty_flag(dirty)
    }

    /// Set the connection status under given widget. It may cause the tree to be marked as dirty.
    fn set_disabled(&self, disabled: bool) -> bool {
        let prev_disabled = self.node_disabled.replace(disabled);
        self.mark_dirty_flag(prev_disabled != disabled)
    }

    /// Get parent of a node under given pointer, if exists.
    #[allow(dead_code)]
    pub fn parent(&self, pointer: WidgetIdentity) -> Option<WidgetIdentity> {
        let hierarchy = self.hierarchy.borrow();
        let nodes = self.nodes_map.borrow();
        let index = nodes.get(&pointer).map(|entry| entry.index)?;
        let parent_index = hierarchy[index].parent_index?;
        Some(hierarchy[parent_index].identity)
    }

    /// Iterate children of a node under given pointer, if any exist.
    #[allow(dead_code)]
    pub fn iter_children(
        &self,
        pointer: WidgetIdentity,
    ) -> impl Iterator<Item = WidgetIdentity> + '_ {
        let hierarchy = self.hierarchy.borrow();
        let nodes = self.nodes_map.borrow();
        let mut total_range = nodes.get(&pointer).map_or(0..0, |entry| {
            let start = entry.index + 1;
            let total_descendants = hierarchy[entry.index].total_descendants;
            start..start + total_descendants
        });

        std::iter::from_fn(move || {
            let index = total_range.next()?;
            let entry = hierarchy[index];
            // Skip all descendants of the child. The range is now at the next direct child.
            if entry.total_descendants > 0 {
                total_range.nth(entry.total_descendants - 1);
            }
            Some(entry.identity)
        })
    }

    #[profile(Task)]
    fn rebuild_tree(
        &self,
        frp: WidgetsFrp,
        tree: &span_tree::SpanTree,
        node_expression: &str,
        styles: &StyleWatch,
    ) {
        self.tree_dirty.set(false);
        let app = self.app.clone();
        let override_map = self.override_map.borrow();
        let connected_map = self.connected_map.borrow();
        let usage_type_map = self.usage_type_map.borrow();
        let old_nodes = self.nodes_map.take();
        let node_disabled = self.node_disabled.get();

        // Old hierarchy is not used during the rebuild, so we might as well reuse the allocation.
        let mut hierarchy = self.hierarchy.take();
        hierarchy.clear();

        let mut builder = TreeBuilder {
            app,
            frp,
            node_disabled,
            node_expression,
            styles,
            override_map: &override_map,
            connected_map: &connected_map,
            usage_type_map: &usage_type_map,
            old_nodes,
            hierarchy,
            pointer_usage: default(),
            new_nodes: default(),
            parent_info: default(),
            last_ast_depth: default(),
            extensions: default(),
        };

        let child = builder.child_widget(tree.root_ref(), default());
        self.display_object.replace_children(&[child]);

        self.nodes_map.replace(builder.new_nodes);
        self.hierarchy.replace(builder.hierarchy);
        let mut ports_map_borrow = self.ports_map.borrow_mut();
        ports_map_borrow.clear();
        ports_map_borrow.extend(
            builder.pointer_usage.into_iter().filter_map(|(k, v)| Some((k, v.port_index?))),
        );
    }

    /// Convert span tree node to a representation with stable identity across rebuilds. Every node
    /// in the span tree has a unique representation in the form of a [`StableSpanIdentity`], which
    /// is more stable across changes in the span tree than [`span_tree::Crumbs`]. The pointer is
    /// used to identify the widgets or ports in the widget tree.
    pub fn get_node_widget_pointer(&self, span_node: &SpanRef) -> StableSpanIdentity {
        if let Some(id) = span_node.ast_id {
            // This span represents an AST node, return a pointer directly to it.
            StableSpanIdentity::new(Some(id), &[])
        } else {
            let root = span_node.span_tree.root_ref();
            let root_ast_data = root.ast_id.map(|id| (id, 0));

            // When the node does not represent an AST node, its widget will be identified by the
            // closest parent AST node, if it exists. We have to find the closest parent node with
            // AST ID, and then calculate the relative crumbs from it to the current node.
            let (_, ast_parent_data) = span_node.crumbs.into_iter().enumerate().fold(
                (root, root_ast_data),
                |(node, last_seen), (index, crumb)| {
                    let ast_data = node.node.ast_id.map(|id| (id, index)).or(last_seen);
                    (node.child(*crumb).expect("Node ref must be valid"), ast_data)
                },
            );

            match ast_parent_data {
                // Parent AST node found, return a pointer relative to it.
                Some((ast_id, ast_parent_index)) => {
                    let crumb_slice = &span_node.crumbs[ast_parent_index..];
                    StableSpanIdentity::new(Some(ast_id), crumb_slice)
                }
                // No parent AST node found. Return a pointer from root.
                None => StableSpanIdentity::new(None, &span_node.crumbs),
            }
        }
    }

    /// Perform an operation on a shared reference to a tree port under given pointer. When there is
    /// no port under provided pointer, the operation will not be performed and `None` will be
    /// returned.
    pub fn with_port<T>(
        &self,
        pointer: StableSpanIdentity,
        f: impl FnOnce(&Port) -> T,
    ) -> Option<T> {
        let index = *self.ports_map.borrow().get(&pointer)?;
        let unique_ptr = WidgetIdentity { main: pointer, index };
        self.nodes_map.borrow().get(&unique_ptr).and_then(|n| n.node.port()).map(f)
    }
}

/// State of a node in the widget tree. Provides additional information about the node's current
/// state, such as its depth in the widget tree, if it's connected, disabled, etc.
#[derive(Debug, Clone, PartialEq)]
pub(super) struct NodeInfo {
    /// Unique identifier of this node within this widget tree.
    pub identity:           WidgetIdentity,
    /// Index of node in the widget tree, in insertion order.
    pub insertion_index:    usize,
    /// Logical nesting level of this widget, which was specified by the parent node during its
    /// creation. Determines the mouse hover area size and widget indentation.
    pub nesting_level:      NestingLevel,
    /// Data associated with an edge connected to this node's span. Only present at the exact node
    /// that is connected, not at any of its children.
    pub connection:         Option<EdgeData>,
    /// Data associated with an edge connected to this subtree. Contains the status of this node's
    /// connection, or its first parent that is connected. It is the same as `connection` for nodes
    /// that are directly connected.
    pub subtree_connection: Option<EdgeData>,
    /// Whether the node is disabled, i.e. its expression is not currently used in the computation.
    /// Widgets of disabled nodes are usually grayed out.
    pub disabled:           bool,
    /// Inferred type of Enso expression at this node's span. May differ from the definition type
    /// stored in the span tree.
    pub usage_type:         Option<crate::Type>,
}

/// A collection of common data used by all widgets and ports in the widget tree during
/// configuration. Provides the main widget's interface to the tree builder, allowing for creating
/// child widgets.
#[derive(Debug)]
pub struct ConfigContext<'a, 'b> {
    builder:               &'a mut TreeBuilder<'b>,
    /// The span tree node corresponding to the widget being configured.
    pub(super) span_node:  span_tree::node::Ref<'a>,
    /// Additional state associated with configured widget tree node, such as its depth, connection
    /// status or parent node information.
    pub(super) info:       NodeInfo,
    /// The length of tree extensions vector before the widget was configured. Used to determine
    /// which extensions were added by the widget parents, and which are new.
    parent_extensions_len: usize,
}

impl<'a, 'b> ConfigContext<'a, 'b> {
    /// Get the application instance, in which the widget tree is being built.
    pub fn app(&self) -> &Application {
        &self.builder.app
    }

    /// Get the FRP endpoints shared by all widgets and ports in this tree.
    pub fn frp(&self) -> &WidgetsFrp {
        &self.builder.frp
    }

    /// Get the code expression fragment represented by the given byte range. Can be combined with
    /// [`span_tree::node::Ref`]'s `span` method to get the expression of a given span tree node.
    pub fn expression_at(&self, range: text::Range<Byte>) -> &str {
        &self.builder.node_expression[range]
    }

    /// Get the `StyleWatch` used by this node.
    pub fn styles(&self) -> &StyleWatch {
        self.builder.styles
    }

    /// Set an extension object of specified type at the current tree position. Any descendant
    /// widget will be able to access it, as long as it can name its type. This allows for
    /// configure-time communication between any widgets inside the widget tree.
    pub fn set_extension<T: Any>(&mut self, val: T) {
        let id = std::any::TypeId::of::<T>();
        match self.self_extension_index_by_type(id) {
            Some(idx) => *self.builder.extensions[idx].downcast_mut().unwrap() = val,
            None => {
                self.builder.extensions.push(Box::new(val));
            }
        }
    }

    /// Get an extension object of specified type at the current tree position. The extension object
    /// must have been created by any parent widget up in the hierarchy. If it does not exist, this
    /// method will return `None`.
    ///
    /// See also: [`ConfigContext::get_extension_or_default`], [`ConfigContext::modify_extension`].
    pub fn get_extension<T: Any>(&self) -> Option<&T> {
        self.any_extension_index_by_type(std::any::TypeId::of::<T>())
            .map(|idx| self.builder.extensions[idx].downcast_ref().unwrap())
    }

    /// Get a clone of provided extension value, or a default value if it was not provided.
    ///
    /// See also: [`ConfigContext::get_extension`].
    pub fn get_extension_or_default<T: Any + Clone + Default>(&self) -> T {
        self.get_extension().map_or_default(Clone::clone)
    }

    /// Modify an extension object of specified type at the current tree position. The modification
    /// will only be visible to the descendants of this widget, even if the extension was added
    /// by one of its parents.
    ///
    /// See also: [`ConfigContext::get_extension`].
    pub fn modify_extension<T>(&mut self, f: impl FnOnce(&mut T))
    where T: Any + Default + Clone {
        match self.any_extension_index_by_type(std::any::TypeId::of::<T>()) {
            // This extension has been created by this widget, so we can modify it directly.
            Some(idx) if idx >= self.parent_extensions_len => {
                f(self.builder.extensions[idx].downcast_mut().unwrap());
            }
            // The extension exist, but has been created by one of the parents. We need to clone it.
            Some(idx) => {
                let mut val: T = self.builder.extensions[idx].downcast_mut::<T>().unwrap().clone();
                f(&mut val);
                self.builder.extensions.push(Box::new(val));
            }
            // The extension does not exist yet, so we need to create it from scratch.
            None => {
                let mut val = T::default();
                f(&mut val);
                self.builder.extensions.push(Box::new(val));
            }
        }
    }

    fn any_extension_index_by_type(&self, id: std::any::TypeId) -> Option<usize> {
        self.builder.extensions.iter().rposition(|ext| ext.deref().type_id() == id)
    }

    fn self_extension_index_by_type(&self, id: std::any::TypeId) -> Option<usize> {
        let self_extensions = &self.builder.extensions[self.parent_extensions_len..];
        self_extensions.iter().rposition(|ext| ext.deref().type_id() == id)
    }
}



// ====================
// === NestingLevel ===
// ====================

/// A logical nesting level associated with a widget which determines the mouse hover area size and
/// widget indentation. It is specified by the parent widget when creating a child widget, as an
/// argument to the '[`ConfigContext`]' method.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NestingLevel {
    level: usize,
}

impl NestingLevel {
    /// Create a deeper nesting level. The depth of the new level will be one greater than the
    /// current one.
    pub fn next(self) -> Self {
        Self { level: self.level + 1 }
    }

    /// Create an optionally deeper nesting level. When `condition` is `false`, the nesting level
    /// will remain the same.
    pub fn next_if(self, condition: bool) -> Self {
        condition.as_some(self.next()).unwrap_or(self)
    }

    /// Check if a port at this nesting level is still considered primary. Primary ports have wider
    /// hover areas and are indented more.
    #[allow(clippy::absurd_extreme_comparisons)]
    pub fn is_primary(self) -> bool {
        self.level <= PRIMARY_PORT_MAX_NESTING_LEVEL
    }
}


// ===========================================
// === StableSpanIdentity / WidgetIdentity ===
// ===========================================

/// A stable identifier to a span tree node. Uniquely determines a main widget of specific node in
/// the span tree. It is a base of a widget stable identity, and allows widgets to be reused when
/// rebuilding the tree.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StableSpanIdentity {
    /// AST ID of either the node itself, or the closest ancestor node which has one. Is [`None`]
    /// when there is no such parent with assigned AST id.
    ast_id:        Option<ast::Id>,
    /// A hash of remaining data used to distinguish between tree nodes. We store a hash instead of
    /// the data directly, so the type can be trivially copied. The collision is extremely unlikely
    /// due to u64 being extremely large hash space, compared to the size of the used data. Many
    /// nodes are also already fully distinguished by the AST ID alone.
    ///
    /// Currently we are hashing a portion of span-tree crumbs, starting from the closest node with
    /// assigned AST id up to this node. The widgets should not rely on the exact kind of data
    /// used, as it may be extended to include more information in the future.
    identity_hash: u64,
}

impl StableSpanIdentity {
    fn new(ast_id: Option<ast::Id>, crumbs_since_ast: &[span_tree::Crumb]) -> Self {
        let mut hasher = DefaultHasher::new();
        crumbs_since_ast.hash(&mut hasher);
        let identity_hash = hasher.finish();
        Self { ast_id, identity_hash }
    }

    /// Convert this pointer to a stable identity of a widget, making it unique among all widgets.
    fn to_identity(self, usage: &mut PointerUsage) -> WidgetIdentity {
        WidgetIdentity { main: self, index: usage.next_index() }
    }
}

/// An unique identity of a widget in the widget tree. It is a combination of a [`SpanIdentity`] and
/// a sequential index of the widget assigned to the same span tree node. Any widget is allowed to
/// create a child widget on the same span tree node, so we need to be able to distinguish between
/// them. Note that only one widget created for a given span tree node will be able to receive a
/// port. The port is assigned to the first widget at given span that wants to receive it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Deref)]
pub struct WidgetIdentity {
    /// The pointer to the main widget of this widget's node.
    #[deref]
    main:  StableSpanIdentity,
    /// The sequential index of a widget assigned to the same span tree node.
    index: usize,
}

impl WidgetIdentity {
    /// Whether this widget pointer represents first created widget for its span tree node.
    fn is_first_widget_of_span(&self) -> bool {
        self.index == 0
    }
}

/// Additional information about the usage of a widget pointer while building a tree. This is used
/// to determine which widget should receive a port, and to assign sequential indices to widgets
/// created for the same span tree node. Used to transform ambiguous [`SpanIdentity`] into
/// unique [`WidgetIdentity`].
#[derive(Debug, Default)]
struct PointerUsage {
    /// Next sequence index that will be assigned to a widget created for the same span tree node.
    next_index: usize,
    /// The pointer index of a widget on this span tree that received a port, if any exist already.
    port_index: Option<usize>,
}

impl PointerUsage {
    fn next_index(&mut self) -> usize {
        self.next_index += 1;
        self.next_index - 1
    }

    fn request_port(&mut self, identity: &WidgetIdentity, wants_port: bool) -> bool {
        let will_receive_port = wants_port && self.port_index.is_none();
        will_receive_port.then(|| self.port_index = Some(identity.index));
        will_receive_port
    }
}



// ===================
// === TreeBuilder ===
// ===================

/// A builder for the widget tree. Maintains transient state necessary during the tree construction,
/// and provides methods for creating child nodes of the tree. Maintains a map of all widgets
/// created so far, and is able to reuse existing widgets under the same location in the tree, only
/// updating their configuration as necessary.
#[derive(Debug)]
struct TreeBuilder<'a> {
    app:             Application,
    frp:             WidgetsFrp,
    node_disabled:   bool,
    node_expression: &'a str,
    styles:          &'a StyleWatch,
    override_map:    &'a HashMap<OverrideKey, Configuration>,
    connected_map:   &'a HashMap<span_tree::Crumbs, color::Lcha>,
    usage_type_map:  &'a HashMap<ast::Id, crate::Type>,
    old_nodes:       HashMap<WidgetIdentity, TreeEntry>,
    new_nodes:       HashMap<WidgetIdentity, TreeEntry>,
    hierarchy:       Vec<NodeHierarchy>,
    pointer_usage:   HashMap<StableSpanIdentity, PointerUsage>,
    parent_info:     Option<NodeInfo>,
    last_ast_depth:  usize,
    extensions:      Vec<Box<dyn Any>>,
}

impl<'a> TreeBuilder<'a> {
    /// Create a new child widget, along with its whole subtree. The widget type will be
    /// automatically inferred, either based on the node kind, or on the configuration provided
    /// from the language server. If possible, an existing widget will be reused under the same
    /// location in the tree, only updating its configuration as necessary. If no widget can be
    /// reused, a new one will be created.
    ///
    /// The root display object of the created widget will be returned, and it must be inserted into
    /// the display object hierarchy by the caller. It will be common that the returned display
    /// object will frequently not change between subsequent widget `configure` calls, but it will
    /// eventually happen if the child widget is relocated or changed its type. The caller must be
    /// prepared to handle that situation, and never rely on it not changing. In order to handle
    /// that efficiently, the caller can use the `replace_children` method of
    /// [`display::object::InstanceDef`], which will only perform hierarchy updates if the children
    /// list has been actually modified.
    #[must_use]
    pub fn child_widget(
        &mut self,
        span_node: span_tree::node::Ref<'_>,
        nesting_level: NestingLevel,
    ) -> display::object::Instance {
        self.child_widget_of_type(span_node, nesting_level, None)
    }

    /// Create a new widget for given span tree node, recursively building a subtree of its
    /// children. When a widget configuration is not provided, it is inferred automatically from the
    /// span tree and expression value type.
    ///
    /// The returned value contains a root display object of created widget child, and it must be
    /// inserted into the display hierarchy by the caller. The returned display object will
    /// frequently not change between subsequent widget `configure` calls, as long as it can be
    /// reused by the tree. The caller must not rely on it not changing. In order to handle that
    /// efficiently, the caller can use the `replace_children` method of
    /// [`display::object::InstanceDef`], which will only perform hierarchy updates if the children
    /// list has been actually modified.
    pub fn child_widget_of_type(
        &mut self,
        span_node: span_tree::node::Ref<'_>,
        nesting_level: NestingLevel,
        configuration: Option<&Configuration>,
    ) -> display::object::Instance {
        // This call can recurse into itself within the widget configuration logic. We need to save
        // the current layer's state, so it can be restored later after visiting the child node.
        let parent_last_ast_depth = self.last_ast_depth;
        let depth = span_node.crumbs.len();

        // Figure out the widget tree pointer for the current node. That pointer determines the
        // widget identity, allowing it to maintain internal state. If the previous tree already
        // contained a widget for this pointer, we have to reuse it.
        let main_ptr = match span_node.ast_id {
            Some(ast_id) => {
                self.last_ast_depth = depth;
                StableSpanIdentity::new(Some(ast_id), &[])
            }
            None => {
                let ast_id = self.parent_info.as_ref().and_then(|st| st.identity.main.ast_id);
                let this_crumbs = &span_node.crumbs;
                let crumbs_since_id = &this_crumbs[parent_last_ast_depth..];
                StableSpanIdentity::new(ast_id, crumbs_since_id)
            }
        };

        let ptr_usage = self.pointer_usage.entry(main_ptr).or_default();
        let widget_id = main_ptr.to_identity(ptr_usage);

        let is_placeholder = span_node.is_expected_argument();
        let sibling_offset = span_node.sibling_offset.as_usize();
        let usage_type = main_ptr.ast_id.and_then(|id| self.usage_type_map.get(&id)).cloned();

        // Get widget configuration. There are three potential sources for configuration, that are
        // used in order, whichever is available first:
        // 1. The `config_override` argument, which can be set by the parent widget if it wants to
        //    override the configuration for its child.
        // 2. The override stored in the span tree node, located using `OverrideKey`. This can be
        //    set by an external source, e.g. based on language server.
        // 3. The default configuration for the widget, which is determined based on the node kind,
        // usage type and whether it has children.
        let kind = &span_node.kind;
        let config_override = || {
            self.override_map.get(&OverrideKey {
                call_id:       kind.call_id()?,
                argument_name: kind.argument_name()?.into(),
            })
        };
        let inferred_config;
        let configuration = match configuration.or_else(config_override) {
            Some(config) => config,
            None => {
                let ty = usage_type.clone();
                inferred_config = Configuration::from_node(&span_node, ty, self.node_expression);
                &inferred_config
            }
        };

        let widget_has_port = ptr_usage.request_port(&widget_id, configuration.has_port);

        let insertion_index = self.hierarchy.len();
        self.hierarchy.push(NodeHierarchy {
            identity:          widget_id,
            parent_index:      self.parent_info.as_ref().map(|info| info.insertion_index),
            // This will be updated later, after the child widgets are created.
            total_descendants: 0,
        });

        let old_node = self.old_nodes.remove(&widget_id).map(|e| e.node);

        // Prepare the widget node info and build context.
        let connection_color = self.connected_map.get(&span_node.crumbs);
        let connection = connection_color.map(|&color| EdgeData { color, depth });
        let parent_connection = self.parent_info.as_ref().and_then(|info| info.connection);
        let subtree_connection = connection.or(parent_connection);

        let disabled = self.node_disabled;
        let info = NodeInfo {
            identity: widget_id,
            insertion_index,
            nesting_level,
            connection,
            subtree_connection,
            disabled,
            usage_type,
        };

        let parent_info = std::mem::replace(&mut self.parent_info, Some(info.clone()));
        let parent_extensions_len = self.extensions.len();

        let ctx = ConfigContext { builder: &mut *self, span_node, info, parent_extensions_len };
        let app = ctx.app();
        let frp = ctx.frp();

        // Widget creation/update can recurse into the builder. All borrows must be dropped
        // at this point. The `configure` calls on the widgets are allowed to call back into the
        // tree builder in order to create their child widgets. Those calls will change builder's
        // state to reflect the correct parent node. We need to restore the state after the
        // `configure` call has been done, so that the next sibling node will receive correct parent
        // data.
        let child_node = if widget_has_port {
            let mut port = match old_node {
                Some(TreeNode::Port(port)) => port,
                Some(TreeNode::Widget(widget)) => Port::new(widget, app, frp),
                None => Port::new(DynWidget::new(&configuration.kind, &ctx), app, frp),
            };
            port.configure(&configuration.kind, ctx);
            TreeNode::Port(port)
        } else {
            let mut widget = match old_node {
                Some(TreeNode::Port(port)) => port.into_widget(),
                Some(TreeNode::Widget(widget)) => widget,
                None => DynWidget::new(&configuration.kind, &ctx),
            };
            widget.configure(&configuration.kind, ctx);
            TreeNode::Widget(widget)
        };

        // Once the node has been configured and all its children have been created, we can update
        // the hierarchy data.
        self.hierarchy[insertion_index].total_descendants =
            self.hierarchy.len() - insertion_index - 1;

        // After visiting child node, restore previous layer's parent data.
        self.parent_info = parent_info;
        self.last_ast_depth = parent_last_ast_depth;
        self.extensions.truncate(parent_extensions_len);

        // Apply left margin to the widget, based on its offset relative to the previous sibling.
        let child_root = child_node.display_object().clone();
        let offset = match () {
            _ if !widget_id.is_first_widget_of_span() => 0,
            _ if is_placeholder => 1,
            _ => sibling_offset,
        };

        let left_margin = offset as f32 * WIDGET_SPACING_PER_OFFSET;
        if child_root.margin().x.start.as_pixels().map_or(true, |px| px != left_margin) {
            child_root.set_margin_left(left_margin);
        }

        let entry = TreeEntry { node: child_node, index: insertion_index };
        self.new_nodes.insert(widget_id, entry);
        child_root
    }
}



// =============
// === Child ===
// =============

/// A child structure returned from the tree builder. Contains information about just built widget,
/// which might be useful for the parent widget in order to correctly place it in its view
/// hierarchy.
#[derive(Debug, Clone, Deref)]
struct Child {
    /// The widget identity that is stable across rebuilds. The parent might use it to associate
    /// internal state with any particular child. When a new child is inserted between two existing
    /// children, their identities will be maintained.
    #[allow(dead_code)]
    pub id:          WidgetIdentity,
    /// The root object of the widget. In order to make the widget visible, it must be added to the
    /// parent's view hierarchy. Every time a widget is [`configure`d], its root object may change.
    /// The parent must not assume ownership over a root object of a removed child. The widget
    /// [`Tree`] is allowed to reuse any widgets and insert them into different branches.
    ///
    /// [`configure`d]: SpanWidget::configure
    #[deref]
    pub root_object: display::object::Instance,
}
