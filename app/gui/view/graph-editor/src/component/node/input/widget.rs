//! Definition of all hardcoded node widget variants and common widget FRP API.

use crate::prelude::*;

use crate::component::node::input::area::NODE_HEIGHT;
use crate::component::node::input::area::TEXT_OFFSET;
use crate::component::node::input::port::Port;
use crate::component::node::ConnectionData;
use crate::component::node::ConnectionStatus;
use enso_config::ARGS;
use enso_frp as frp;
use enso_text as text;
use ensogl::application::Application;
use ensogl::display;
use ensogl::display::shape::StyleWatch;
use ensogl::gui::cursor;
use ensogl_component::drop_down::DropdownValue;
use text::index::Byte;



// =================
// === Constants ===
// =================

/// Spacing between sibling widgets per each code character that separates them. Current value is
/// based on the space glyph width at node's default font size, so that entering node edit mode
/// introduces the least amount of visual changes. The value can be adjusted once we implement
/// granular edit mode that works with widgets.
pub const WIDGET_SPACING_PER_OFFSET: f32 = 7.224_609_4;



// ===========
// === FRP ===
// ===========

ensogl::define_endpoints_2! {
    Input {
        set_ports_visible    (bool),
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
    }
}

/// Information associated with widget metadata which describes which uniquely identifies the
/// widget to reconfigure.
#[derive(Debug, Default, Clone, Hash, PartialEq, Eq)]
pub struct MetadataPointer {
    /// The function call associated with the widget.
    pub call_id:       ast::Id,
    /// The name of function argument at which the widget is located.
    pub argument_name: ImString,
}


/// ======================
/// === Widget modules ===
/// ======================

/// Common trait for constructing and reconfiguring all widget variants.
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

macro_rules! define_widget_modules(
    ($(
        $(#[$meta:meta])*
        $name:ident $module:ident,
    )*) => {
        $(pub mod $module;)*

        /// A widget configuration that determines the widget kind.
        #[derive(Debug, Clone, PartialEq)]
        #[allow(missing_docs)]
        pub enum Config {
            $($name(<$module::Widget as SpanWidget>::Config),)*
        }

        /// The node widget view. Represents one widget of any kind on the node input area. Can
        /// change its appearance and behavior depending on the widget metadata updates, without
        /// being recreated. New widget can be created using the `new` method, while the existing
        /// widget can be reconfigured using the `configure` method.
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
            impl const From<<$module::Widget as SpanWidget>::Config> for Config {
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
            type Config = Config;
            fn root_object(&self) -> &display::object::Instance {
                match self {
                    $(DynWidget::$name(inner) => inner.root_object(),)*
                }
            }

            fn new(config: &Config, ctx: &ConfigContext) -> Self {
                match config {
                    $(
                        Config::$name(config) => DynWidget::$name(SpanWidget::new(config, ctx)),
                    )*
                }
            }

            fn configure(&mut self, config: &Config, ctx: ConfigContext) {
                match (self, config) {
                    $((DynWidget::$name(model), Config::$name(config)) => {
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
    VectorEditor vector_editor,
    /// Default span tree traversal widget.
    Hierarchy hierarchy,
}

/// ================
/// === Metadata ===
/// ================

/// Widget metadata that comes from an asynchronous visualization. Defines which widget should be
/// used and a set of options that it should allow to choose from.
#[derive(Debug, Clone, PartialEq)]
#[allow(missing_docs)]
pub struct Metadata {
    /// The placeholder text value. By default, the parameter name is used.
    pub display:  Display,
    pub config:   Config,
    pub has_port: bool,
}

impl Metadata {
    const fn always<C>(config: C) -> Self
    where C: ~const Into<Config> {
        Self { display: Display::Always, config: config.into(), has_port: true }
    }

    const fn inert<C>(config: C) -> Self
    where C: ~const Into<Config> {
        Self { display: Display::Always, config: config.into(), has_port: false }
    }

    /// Widget metadata for static dropdown, based on the tag values provided by suggestion
    /// database.
    fn static_dropdown(label: Option<ImString>, tag_values: &[span_tree::TagValue]) -> Metadata {
        let entries = Rc::new(tag_values.iter().map(Entry::from).collect());
        Self::always(single_choice::Config { label, entries })
    }

    fn vector_editor() -> Metadata {
        Self::always(vector_editor::Config { item_editor: None, item_default: "_".into() })
    }

    fn from_node(
        node: &span_tree::node::Ref,
        usage_type: Option<crate::Type>,
        expression: &str,
    ) -> Self {
        use span_tree::node::Kind;

        let kind = &node.kind;
        let has_children = !node.children.is_empty();

        const VECTOR_TYPE: &str = "Standard.Base.Data.Vector.Vector";
        let is_array_enabled = ARGS.groups.feature_preview.options.vector_editor.value;
        let is_vector = |arg: &span_tree::node::Argument| {
            let type_matches = usage_type
                .as_ref()
                .map(|t| t.as_str())
                .or(arg.tp.as_deref())
                .map_or(false, |tp| tp.contains(VECTOR_TYPE));
            if type_matches {
                let node_expr = &expression[node.span()];
                node_expr.starts_with('[') && node_expr.ends_with(']')
            } else {
                false
            }
        };

        match kind {
            Kind::Argument(arg) if !arg.tag_values.is_empty() =>
                Self::static_dropdown(arg.name.as_ref().map(Into::into), &arg.tag_values),
            Kind::Argument(arg) if is_array_enabled && is_vector(arg) => Self::vector_editor(),
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



/// ==================
/// === WidgetsFrp ===
/// ==================

/// Widget FRP endpoints that can be used by widget views, and go straight to the root.
#[derive(Debug, Clone, CloneRef)]
pub struct WidgetsFrp {
    pub(super) set_ports_visible:      frp::Sampler<bool>,
    pub(super) set_view_mode:          frp::Sampler<crate::view::Mode>,
    pub(super) set_profiling_status:   frp::Sampler<crate::node::profiling::Status>,
    pub(super) value_changed:          frp::Any<(span_tree::Crumbs, Option<ImString>)>,
    pub(super) request_import:         frp::Any<ImString>,
    pub(super) on_port_hover:          frp::Any<Switch<span_tree::Crumbs>>,
    pub(super) on_port_press:          frp::Any<span_tree::Crumbs>,
    pub(super) pointer_style:          frp::Any<cursor::Style>,
    pub(super) connected_port_updated: frp::Any<()>,
}



// ==============
// === Widget ===
// ==============

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
            set_ports_visible    <- frp.set_ports_visible.sampler();
            set_view_mode        <- frp.set_view_mode.sampler();
            set_profiling_status <- frp.set_profiling_status.sampler();

            on_port_hover        <- any(...);
            on_port_press        <- any(...);
            trace on_port_hover;
            frp.private.output.on_port_hover <+ on_port_hover;
            frp.private.output.on_port_press <+ on_port_press;
        }

        let value_changed = frp.private.output.value_changed.clone_ref();
        let request_import = frp.private.output.request_import.clone_ref();
        let pointer_style = frp.private.output.pointer_style.clone_ref();
        let connected_port_updated = frp.private.output.connected_port_updated.clone_ref();
        let widgets_frp = WidgetsFrp {
            set_ports_visible,
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

    /// Override widget metadata. The metadata is used to determine the widget appearance and
    /// behavior. By default, the widget metadata will be inferred from its span tree kind and type.
    /// However, in some cases, we want to change the selected widget for a given span tree node,
    /// and it can be done by calling this method. The set metadata is persistent, and will be
    /// applied to any future widget of this node that matches given pointer.
    pub fn set_metadata(&self, pointer: MetadataPointer, meta: Option<Metadata>) {
        self.model.set_metadata(pointer, meta);
    }

    /// Set usage type for given span tree node. The usage type is used to determine the widget
    /// appearance and default inferred widget metadata.
    pub fn set_usage_type(&self, ast_id: ast::Id, usage_type: Option<crate::Type>) {
        self.model.set_usage_type(ast_id, usage_type);
    }

    /// Set connection status for given span tree node. The connected nodes will be highlighted
    /// with a different color, and the widgets might change behavior depending on the connection
    /// status.
    pub fn set_connected(&self, tree_node: &span_tree::node::Ref, status: ConnectionStatus) {
        if let Some(pointer) = self.model.get_node_widget_pointer(tree_node) {
            self.model.set_connected(pointer, status);
        }
    }

    /// Set disabled status for given span tree node. The disabled nodes will be grayed out.
    /// The widgets might change behavior depending on the disabled status.
    pub fn set_disabled(&self, disabled: bool) {
        self.model.set_disabled(disabled);
    }


    /// Rebuild tree if it has been marked as dirty. The dirty flag is marked whenever more data
    /// external to the span-tree is provided, using `set_metadata`, `set_usage_type`,
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
    /// have a distinct widget, so the returned value might be `None`.
    pub fn get_port_display_object(
        &self,
        tree_node: &span_tree::node::Ref,
    ) -> Option<display::object::Instance> {
        let pointer = self.model.get_node_widget_pointer(tree_node)?;
        self.model.with_port(pointer, |w| w.display_object().clone())
    }

    /// Get hover shapes for all ports in the tree. Used for testing to simulate mouse events.
    pub fn port_hover_shapes(&self) -> Vec<super::port::hover_shape::View> {
        let nodes = self.model.nodes_map.borrow();
        self.model
            .hierarchy
            .borrow()
            .iter()
            .filter_map(|n| nodes.get(&n.identity))
            .filter_map(|e| Some(e.node.port()?.hover_shape().clone_ref()))
            .collect_vec()
    }
}


/// ================
/// === TreeNode ===
/// ================

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
    #[allow(dead_code)]
    parent_index:      Option<usize>,
    total_descendants: usize,
}

/// Single entry in the tree.
#[derive(Debug)]
struct TreeEntry {
    node:  TreeNode,
    /// Index in the `hierarchy` vector.
    #[allow(dead_code)]
    index: usize,
}

/// =================
/// === TreeModel ===
/// =================

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
    ports_map:      RefCell<HashMap<MainWidgetPointer, usize>>,
    metadata_map:   Rc<RefCell<HashMap<MetadataPointer, Metadata>>>,
    connected_map:  Rc<RefCell<HashMap<MainWidgetPointer, ConnectionData>>>,
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
            metadata_map: default(),
            connected_map: default(),
            usage_type_map: default(),
            tree_dirty: default(),
        }
    }

    /// Set the metadata under given pointer. It may cause the tree to be marked as dirty.
    fn set_metadata(&self, pointer: MetadataPointer, meta: Option<Metadata>) {
        let mut map = self.metadata_map.borrow_mut();
        let dirty = map.synchronize_entry(pointer, meta);
        if dirty {
            self.tree_dirty.set(true);
        }
    }

    /// Set the connection status under given widget. It may cause the tree to be marked as dirty.
    fn set_connected(&self, pointer: MainWidgetPointer, status: ConnectionStatus) {
        let mut map = self.connected_map.borrow_mut();
        let dirty = map.synchronize_entry(pointer, status.data());
        if dirty {
            self.tree_dirty.set(true);
        }
    }

    /// Set the usage type of an expression. It may cause the tree to be marked as dirty.
    fn set_usage_type(&self, ast_id: ast::Id, usage_type: Option<crate::Type>) {
        let mut map = self.usage_type_map.borrow_mut();
        let dirty = map.synchronize_entry(ast_id, usage_type);
        if dirty {
            self.tree_dirty.set(true);
        }
    }

    /// Set the connection status under given widget. It may cause the tree to be marked as dirty.
    fn set_disabled(&self, disabled: bool) {
        let prev_disabled = self.node_disabled.replace(disabled);
        if prev_disabled != disabled {
            self.tree_dirty.set(true);
        }
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
        let metadata_map = self.metadata_map.borrow();
        let connected_map = self.connected_map.borrow();
        let usage_type_map = self.usage_type_map.borrow();
        let old_nodes = self.nodes_map.take();
        let node_disabled = self.node_disabled.get();

        // Old hierarchy is not used during the rebuild, so we might as well reuse the allocation.
        let mut hierarchy = self.hierarchy.take();
        hierarchy.clear();

        let mut builder = WidgetTreeBuilder {
            app,
            frp,
            node_disabled,
            node_expression,
            styles,
            metadata_map: &metadata_map,
            connected_map: &connected_map,
            usage_type_map: &usage_type_map,
            old_nodes,
            hierarchy,
            pointer_usage: default(),
            new_nodes: default(),
            parent_state: default(),
            last_ast_depth: default(),
            extensions: default(),
        };

        let child = builder.child_widget(tree.root_ref(), 0);
        self.display_object.replace_children(&[child]);

        self.nodes_map.replace(builder.new_nodes);
        self.hierarchy.replace(builder.hierarchy);
        let mut ports_map_borrow = self.ports_map.borrow_mut();
        ports_map_borrow.clear();
        ports_map_borrow.extend(
            builder.pointer_usage.into_iter().filter_map(|(k, v)| Some((k, v.port_index?))),
        );
    }

    /// Convert span tree node to a corresponding widget tree pointer. Every node in the span tree
    /// has a unique representation in the form of a widget tree pointer, which is more stable
    /// across changes in the span tree than [`span_tree::Crumbs`]. The pointer is used to identify
    /// the widgets or ports in the widget tree.
    pub fn get_node_widget_pointer(
        &self,
        tree_node: &span_tree::node::Ref,
    ) -> Option<MainWidgetPointer> {
        if let Some(id) = tree_node.node.ast_id {
            // This span represents an AST node, return a pointer directly to it.
            Some(MainWidgetPointer::new(Some(id), &[]))
        } else {
            let root = tree_node.span_tree.root_ref();
            let root_ast_data = root.ast_id.map(|id| (id, 0));

            // When the node does not represent an AST node, its widget will be identified by the
            // closest parent AST node, if it exists. We have to find the closest parent node with
            // AST ID, and then calculate the relative crumbs from it to the current node.
            let (_, ast_parent_data) = tree_node.crumbs.into_iter().enumerate().fold(
                (root, root_ast_data),
                |(node, last_seen), (index, crumb)| {
                    let ast_data = node.node.ast_id.map(|id| (id, index)).or(last_seen);
                    (node.child(*crumb).expect("Node ref must be valid"), ast_data)
                },
            );

            match ast_parent_data {
                // Parent AST node found, return a pointer relative to it.
                Some((ast_id, ast_parent_index)) => {
                    let crumb_slice = &tree_node.crumbs[ast_parent_index..];
                    Some(MainWidgetPointer::new(Some(ast_id), crumb_slice))
                }
                // No parent AST node found. Return a pointer from root.
                None => Some(MainWidgetPointer::new(None, &tree_node.crumbs)),
            }
        }
    }

    /// Perform an operation on a shared reference to a tree port under given pointer. When there is
    /// no port under provided pointer, the operation will not be performed and `None` will be
    /// returned.
    pub fn with_port<T>(
        &self,
        pointer: MainWidgetPointer,
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
pub(super) struct NodeState {
    /// Identity of this node.
    pub identity:           WidgetIdentity,
    /// Index of node in the widget tree, in insertion order.
    pub insertion_index:    usize,
    /// Widget tree node depth, as provided by the parent node. This does not necessarily
    /// correspond to the depth in the view hierarchy or span tree, but instead is treated as a
    /// logical nesting level in the expressions. It is fully determined by the chain of parent
    /// widgets, and thus is more or less independent from the true depth of the widget tree data
    /// structure.
    pub depth:              usize,
    /// Connection status of the node. Only present at the exact node that is connected, not at
    /// any of its parents.
    pub connection:         ConnectionStatus,
    /// Connection status of the node's subtree. Contains the status of this node's connection, or
    /// it's first parent that is connected. This is the same as `connection` for nodes that
    /// are directly connected.
    pub subtree_connection: ConnectionStatus,
    /// Whether the node is disabled, i.e. its expression is not currently used in the computation.
    /// Widgets of disabled nodes are usually grayed out.
    pub disabled:           bool,
    /// Expression's usage type, as opposed to its definition type stored in the span tree. The
    /// usage type represents the type of the expression as it is used in the computation, which
    /// may differ from the definition type. For example, the definition type of a variable may be
    /// `Number`, but its usage type may be `Vector`, if it is used as a vector component.
    pub usage_type:         Option<crate::Type>,
}

/// A collection of common data used by all widgets and ports in the widget tree during
/// configuration. Provides the main widget's interface to the tree builder, allowing for creating
/// child widgets.
#[derive(Debug)]
pub struct ConfigContext<'a, 'b> {
    builder:                   &'a mut WidgetTreeBuilder<'b>,
    /// Display mode of the widget.
    /// TODO [PG]: Consider handling display modes in the widget tree builder directly, instead of
    /// passing it to the widgets. Right now it has not effect at all.
    #[allow(dead_code)]
    pub(super) display:        Display,
    /// The span tree node corresponding to the widget being configured.
    pub(super) span_tree_node: span_tree::node::Ref<'a>,
    /// Additional state associated with configured widget tree node, such as its depth, connection
    /// status or parent node information.
    pub(super) state:          NodeState,
    /// The length of tree extensions vector before the widget was configured. Used to determine
    /// which extensions were added by the widget parents, and which are new.
    parent_extensions_len:     usize,
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
    pub fn get_extension<T: Any>(&self) -> Option<&T> {
        self.any_extension_index_by_type(std::any::TypeId::of::<T>())
            .map(|idx| self.builder.extensions[idx].downcast_ref().unwrap())
    }

    /// Modify an extension object of specified type at the current tree position. The modification
    /// will only be visible to the descendants of this widget, even if the extension was added
    /// by one of its parents.
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

/// A pointer to main widget of specific node in the span tree. Determines the base of a widget
/// stable identity, and allows widgets to be reused when rebuilding the tree. The pointer is
/// composed of two parts:
/// - `id` is the AST ID of either the node itself, or the closest ancestor node which has one. It
///   can be `None` if there is no such parent, e.g. for a tree Root node.
/// - `crumbs_hash` is a hash of remaining crumbs since last node with stable AST ID, or since the
///   root node if there is no such parent.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MainWidgetPointer {
    /// The latest set ast::Id in the span tree.
    id:          Option<ast::Id>,
    /// A hash of remaining crumbs to the widget, starting from the node with the latest set
    /// ast::Id. We store a hash instead of the crumbs directly, so the type can be trivially
    /// copied. The collision is extremely unlikely due to commonly having very short lists of
    /// crumbs to store here and u64 being comparatively extremely large hash space.
    crumbs_hash: u64,
}

/// An unique identity of a widget in the widget tree. It is a combination of a `MainWidgetPointer`
/// and a sequential index of the widget assigned to the same span tree node. Any widget is allowed
/// to create a child widget on the same span tree node, so we need to be able to distinguish
/// between them. Note that only the first widget created for a given span tree node will be able to
/// receive a port and thus be directly connected.
///
/// For all widgets with identity that shares the same `MainWidgetPointer`, at most one of them
/// will be able to receive a port. The port is assigned to the first widget created for a given
/// node that wants to receive it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct WidgetIdentity {
    /// The pointer to the main widget of this widget's node.
    main:  MainWidgetPointer,
    /// The sequential index of a widget assigned to the same span tree node.
    index: usize,
}

impl MainWidgetPointer {
    fn new(id: Option<ast::Id>, crumbs: &[span_tree::Crumb]) -> Self {
        let mut hasher = DefaultHasher::new();
        crumbs.hash(&mut hasher);
        let crumbs_hash = hasher.finish();
        Self { id, crumbs_hash }
    }
}



/// =========================
/// === WidgetTreeBuilder ===
/// =========================

/// A builder for the widget tree. Maintains transient state necessary during the tree construction,
/// and provides methods for creating child nodes of the tree. Maintains a map of all widgets
/// created so far, and is able to reuse existing widgets under the same location in the tree, only
/// updating their configuration as necessary.
#[derive(Debug)]
struct WidgetTreeBuilder<'a> {
    app:             Application,
    frp:             WidgetsFrp,
    node_disabled:   bool,
    node_expression: &'a str,
    styles:          &'a StyleWatch,
    metadata_map:    &'a HashMap<MetadataPointer, Metadata>,
    connected_map:   &'a HashMap<MainWidgetPointer, ConnectionData>,
    usage_type_map:  &'a HashMap<ast::Id, crate::Type>,
    old_nodes:       HashMap<WidgetIdentity, TreeEntry>,
    new_nodes:       HashMap<WidgetIdentity, TreeEntry>,
    hierarchy:       Vec<NodeHierarchy>,
    pointer_usage:   HashMap<MainWidgetPointer, PointerUsage>,
    parent_state:    Option<NodeState>,
    last_ast_depth:  usize,
    extensions:      Vec<Box<dyn Any>>,
}

/// Additional information about the usage of a widget pointer while building a tree. This is used
/// to determine which widget should receive a port, and to assign sequential indices to widgets
/// created for the same span tree node.
#[derive(Debug, Default)]
struct PointerUsage {
    /// Next sequence index that will be assigned to a widget created for the same span tree node.
    next_index: usize,
    /// The pointer index of a widget on this span tree that received a port, if any exist already.
    port_index: Option<usize>,
}

impl<'a> WidgetTreeBuilder<'a> {
    /// Create a new child widget. The widget type will be automatically inferred, either based on
    /// the node kind, or on the metadata provided from the language server. If possible, an
    /// existing widget will be reused under the same location in the tree, only updating its
    /// configuration as necessary. If no widget can be reused, a new one will be created.
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
        span_tree_node: span_tree::node::Ref<'_>,
        depth: usize,
    ) -> display::object::Instance {
        self.create_child_widget(span_tree_node, depth, None)
    }

    /// Create a new child widget. Does not infer the widget type, but uses the provided metadata
    /// instead. That way, the parent widget can explicitly control the type of the child widget.
    ///
    /// See [`child_widget`] method for more details about widget creation.
    #[must_use]
    #[allow(dead_code)] // Currently unused, but will be used in the future in VectorEditor.
    pub fn child_widget_of_type(
        &mut self,
        span_tree_node: span_tree::node::Ref<'_>,
        depth: usize,
        meta: Metadata,
    ) -> display::object::Instance {
        self.create_child_widget(span_tree_node, depth, Some(meta))
    }

    /// Create a new widget for given span tree node. This function recursively builds a subtree
    /// of widgets, starting from the given node. The built subtree's root display object is
    /// returned, so that it can be added to the parent's display hierarchy.
    fn create_child_widget(
        &mut self,
        span_tree_node: span_tree::node::Ref<'_>,
        depth: usize,
        set_metadata: Option<Metadata>,
    ) -> display::object::Instance {
        // This call can recurse into itself within the widget configuration logic. We need to save
        // the current layer's state, so it can be restored later after visiting the child node.
        let parent_last_ast_depth = self.last_ast_depth;

        // Figure out the widget tree pointer for the current node. That pointer determines the
        // widget identity, allowing it to maintain internal state. If the previous tree already
        // contained a widget for this pointer, we have to reuse it.
        let main_ptr = match span_tree_node.ast_id {
            Some(ast_id) => {
                self.last_ast_depth = span_tree_node.crumbs.len();
                MainWidgetPointer::new(Some(ast_id), &[])
            }
            None => {
                let ast_id = self.parent_state.as_ref().and_then(|st| st.identity.main.id);
                let this_crumbs = &span_tree_node.crumbs;
                let crumbs_since_id = &this_crumbs[parent_last_ast_depth..];
                MainWidgetPointer::new(ast_id, crumbs_since_id)
            }
        };

        let ptr_usage = self.pointer_usage.entry(main_ptr).or_default();
        let unique_ptr = WidgetIdentity { main: main_ptr, index: ptr_usage.next_index };

        let is_placeholder = span_tree_node.is_expected_argument();
        let sibling_offset = span_tree_node.sibling_offset.as_usize();
        let usage_type = main_ptr.id.and_then(|id| self.usage_type_map.get(&id)).cloned();

        // Get widget metadata. There are three potential sources for metadata, that are used in
        // order, whichever is available first:
        // 1. The `set_metadata` argument, which can be set by the parent widget if it wants to
        //   override the metadata for its child.
        // 2. The `MetadataPointer` stored in the span tree node. This can be set by an external
        //  source (e.g. based on language server) to override the default metadata for the node.
        // 3. The default metadata for the node, which is determined based on the node kind, usage
        // type and whether it has children.
        let mut meta_fallback = None;
        let kind = &span_tree_node.kind;
        let meta = set_metadata
            .as_ref()
            .or_else(|| {
                let pointer_data = kind.call_id().zip(kind.argument_name());
                let meta_pointer = pointer_data.map(|(call_id, argument_name)| MetadataPointer {
                    call_id,
                    argument_name: argument_name.into(),
                });

                meta_pointer.and_then(|ptr| self.metadata_map.get(&ptr))
            })
            .unwrap_or_else(|| {
                meta_fallback.get_or_insert_with(|| {
                    Metadata::from_node(&span_tree_node, usage_type.clone(), self.node_expression)
                })
            });

        let widget_has_port = match (meta.has_port, &mut ptr_usage.port_index) {
            (true, port_index @ None) => {
                *port_index = Some(unique_ptr.index);
                true
            }
            _ => false,
        };

        let self_insertion_index = self.hierarchy.len();
        self.hierarchy.push(NodeHierarchy {
            identity:          unique_ptr,
            parent_index:      self.parent_state.as_ref().map(|st| st.insertion_index),
            // This will be updated later, after the child widgets are created.
            total_descendants: 0,
        });

        let old_node = self.old_nodes.remove(&unique_ptr).map(|e| e.node);

        // Once we have the metadata and potential old widget to reuse, we have to apply the
        // configuration to the widget.
        let connection: ConnectionStatus = self.connected_map.get(&main_ptr).copied().into();
        let subtree_connection = match self.parent_state.as_ref().map(|s| s.subtree_connection) {
            Some(parent_connection) => connection.or(parent_connection),
            None => connection,
        };

        let disabled = self.node_disabled;
        let state = NodeState {
            identity: unique_ptr,
            insertion_index: self_insertion_index,
            depth,
            connection,
            subtree_connection,
            disabled,
            usage_type,
        };

        let state_to_restore = std::mem::replace(&mut self.parent_state, Some(state.clone()));

        let parent_extensions_len = self.extensions.len();

        let ctx = ConfigContext {
            builder: &mut *self,
            display: meta.display,
            span_tree_node,
            state,
            parent_extensions_len,
        };
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
                None => Port::new(DynWidget::new(&meta.config, &ctx), app, frp),
            };
            port.configure(&meta.config, ctx);
            TreeNode::Port(port)
        } else {
            let mut widget = match old_node {
                Some(TreeNode::Port(port)) => port.into_widget(),
                Some(TreeNode::Widget(widget)) => widget,
                None => DynWidget::new(&meta.config, &ctx),
            };
            widget.configure(&meta.config, ctx);
            TreeNode::Widget(widget)
        };

        // Once the node has been configured and all its children have been created, we can update
        // the hierarchy data.
        self.hierarchy[self_insertion_index].total_descendants =
            self.hierarchy.len() - self_insertion_index - 1;

        // After visiting child node, restore previous layer's parent data.
        self.parent_state = state_to_restore;
        self.last_ast_depth = parent_last_ast_depth;
        self.extensions.truncate(parent_extensions_len);

        // Apply left margin to the widget, based on its offset relative to the previous sibling.
        let child_root = child_node.display_object().clone();
        let offset = sibling_offset.max(if is_placeholder { 1 } else { 0 });
        let left_margin = offset as f32 * WIDGET_SPACING_PER_OFFSET;
        if child_root.margin().x.start.as_pixels().map_or(true, |px| px != left_margin) {
            child_root.set_margin_left(left_margin);
        }

        let entry = TreeEntry { node: child_node, index: self_insertion_index };
        self.new_nodes.insert(unique_ptr, entry);
        child_root
    }
}
