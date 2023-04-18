//! Definition of all hardcoded node widget variants and common widget FRP API.

mod debug;

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
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;
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
        set_ports_visible (bool),
        set_view_mode     (crate::view::Mode),
        set_disabled      (bool),
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
    fn new(config: &Self::Config, app: &Application, frp: &WidgetsFrp) -> Self;
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
        pub(super) enum DynWidget {
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

            fn new(config: &Config, app: &Application, frp: &WidgetsFrp) -> Self {
                match config {
                    $(
                        Config::$name(config) => DynWidget::$name(SpanWidget::new(config, app, frp)),
                    )*
                }
            }

            fn configure(&mut self, config: &Config, ctx: ConfigContext) {
                match (self, config) {
                    $((DynWidget::$name(model), Config::$name(config)) => {
                        SpanWidget::configure(model, config, ctx);
                    },)*
                    (this, _) => {
                        *this = SpanWidget::new(config, ctx.app(), ctx.frp());
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
        let entries = Rc::new(tag_values.into_iter().map(Entry::from).collect());
        Self::always(single_choice::Config { label, entries })
    }

    fn vector_editor() -> Metadata {
        Self::always(vector_editor::Config::default())
    }

    fn from_kind(
        kind: &span_tree::node::Kind,
        _usage_type: Option<crate::Type>,
        has_children: bool,
    ) -> Self {
        use span_tree::node::Kind;

        const VECTOR_TYPE: &str = "Standard.Base.Data.Vector.Vector";
        let is_array_enabled = ARGS.groups.feature_preview.options.vector_editor.value;

        match kind {
            Kind::Argument(arg) if !arg.tag_values.is_empty() =>
                Self::static_dropdown(arg.name.as_ref().map(Into::into), &arg.tag_values),
            Kind::Argument(arg)
                if is_array_enabled
                    && arg.tp.as_ref().map_or(false, |tp| tp.contains(VECTOR_TYPE)) =>
                Self::vector_editor(),
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

/// The node widget tree view. Contains all widgets created from the node's span tree.
#[derive(Debug, Deref, Clone, CloneRef)]
pub struct Tree {
    #[deref]
    frp:         Frp,
    widgets_frp: WidgetsFrp,
    model:       Rc<TreeModel>,
}

impl display::Object for Tree {
    fn display_object(&self) -> &display::object::Instance {
        &self.model.display_object.outer
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
            set_ports_visible <- frp.set_ports_visible.sampler();
            set_view_mode     <- frp.set_view_mode.sampler();
        }

        let value_changed = frp.private.output.value_changed.clone_ref();
        let request_import = frp.private.output.request_import.clone_ref();
        let on_port_hover = frp.private.output.on_port_hover.clone_ref();
        let on_port_press = frp.private.output.on_port_press.clone_ref();
        let pointer_style = frp.private.output.pointer_style.clone_ref();
        let connected_port_updated = frp.private.output.connected_port_updated.clone_ref();
        let widgets_frp = WidgetsFrp {
            set_ports_visible,
            set_view_mode,
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
        if let Some(pointer) = self.model.get_port_widget_pointer(tree_node) {
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
        if self.model.tree_dirty.load(Ordering::Acquire) {
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

    /// Get the root display object of the widget port for given span tree node. Not all widgets
    /// have a port, so the returned value might be `None` even if a widget exist for given span
    /// tree node.
    pub fn get_port_display_object(
        &self,
        tree_node: &span_tree::node::Ref,
    ) -> Option<display::object::Instance> {
        let pointer = self.model.get_port_widget_pointer(tree_node)?;
        self.model.with_node(&pointer, |w| w.display_object().clone())
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
    fn port_mut(&mut self) -> Option<&mut Port> {
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



/// =================
/// === TreeModel ===
/// =================

#[derive(Debug)]
struct TreeModel {
    app:            Application,
    display_object: debug::InstanceWithBg,
    widgets_map:    RefCell<HashMap<WidgetTreePointer, TreeNode>>,
    metadata_map:   Rc<RefCell<HashMap<MetadataPointer, Metadata>>>,
    connected_map:  Rc<RefCell<HashMap<WidgetTreePointer, ConnectionData>>>,
    usage_type_map: Rc<RefCell<HashMap<ast::Id, crate::Type>>>,
    disabled:       AtomicBool,
    tree_dirty:     AtomicBool,
}

impl TreeModel {
    /// Create a new node widget, selecting the appropriate widget type based on the provided
    /// argument info.
    fn new(app: &Application) -> Self {
        let app = app.clone_ref();
        let display_object = debug::InstanceWithBg::gray();
        display_object.inner.use_auto_layout();
        display_object.inner.set_children_alignment_left_center().justify_content_center_y();
        display_object.inner.set_size_y(NODE_HEIGHT);
        display_object.inner.set_padding_left(TEXT_OFFSET);
        display_object.inner.set_padding_right(TEXT_OFFSET);

        Self {
            app,
            display_object,
            disabled: default(),
            widgets_map: default(),
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
            self.tree_dirty.store(true, Ordering::Release);
        }
    }

    /// Set the connection status under given widget. It may cause the tree to be marked as dirty.
    fn set_connected(&self, pointer: WidgetTreePointer, status: ConnectionStatus) {
        let mut map = self.connected_map.borrow_mut();
        let dirty = map.synchronize_entry(pointer, status.data());
        if dirty {
            self.tree_dirty.store(true, Ordering::Release);
        }
    }

    /// Set the usage type of an expression. It may cause the tree to be marked as dirty.
    fn set_usage_type(&self, ast_id: ast::Id, usage_type: Option<crate::Type>) {
        let mut map = self.usage_type_map.borrow_mut();
        let dirty = map.synchronize_entry(ast_id, usage_type);
        if dirty {
            self.tree_dirty.store(true, Ordering::Release);
        }
    }

    /// Set the connection status under given widget. It may cause the tree to be marked as dirty.
    fn set_disabled(&self, disabled: bool) {
        let prev_disabled = self.disabled.swap(disabled, Ordering::AcqRel);
        if prev_disabled != disabled {
            self.tree_dirty.store(true, Ordering::Release);
        }
    }

    #[profile(Task)]
    fn rebuild_tree(
        &self,
        frp: WidgetsFrp,
        tree: &span_tree::SpanTree,
        node_expression: &str,
        styles: &StyleWatch,
    ) {
        self.tree_dirty.store(false, Ordering::Release);
        let app = self.app.clone();
        let metadata_map = self.metadata_map.borrow();
        let connected_map = self.connected_map.borrow();
        let usage_type_map = self.usage_type_map.borrow();
        let old_widgets = self.widgets_map.take();
        let node_disabled = self.disabled.load(Ordering::Acquire);
        let mut builder = WidgetTreeBuilder {
            app,
            frp,
            node_disabled,
            node_expression,
            styles,
            metadata_map: &metadata_map,
            connected_map: &connected_map,
            usage_type_map: &usage_type_map,
            old_widgets,
            new_widgets: default(),
            parent_ast_id: default(),
            parent_crumbs: default(),
            parent_state: default(),
        };

        let child = builder.child_widget(tree.root_ref(), 0);
        self.display_object.inner.replace_children(&[child]);
        self.widgets_map.replace(builder.new_widgets);
    }

    /// Convert span tree node to a corresponding widget tree pointer. Every node in the span tree
    /// has a unique representation in the form of a widget tree pointer, which is more stable
    /// across changes in the span tree than [`span_tree::Crumbs`]. The pointer is used to identify
    /// the widgets or ports in the widget tree.
    pub fn get_port_widget_pointer(
        &self,
        tree_node: &span_tree::node::Ref,
    ) -> Option<WidgetTreePointer> {
        if let Some(id) = tree_node.node.ast_id {
            // This span represents an AST node, return a pointer directly to it.
            Some(WidgetTreePointer { id: Some(id), crumbs: default() })
        } else {
            let root = tree_node.span_tree.root_ref();
            let root_ast_data = root.ast_id.map(|id| (id, 0));

            // When the node does not represent an AST node, its widget will be identified by the
            // closest parent AST node, if it exists. We have to find the closest parent node with
            // AST ID, and then calculate the relative crumbs from it to the current node.
            let (_, ast_parent_data) = tree_node.crumbs.into_iter().enumerate().fold(
                (root, root_ast_data),
                |(node, last_seen), (index, crumb)| {
                    let ast_data = node.node.ast_id.map(|id| (id, index + 1)).or(last_seen);
                    (node.child(*crumb).expect("Node ref must be valid"), ast_data)
                },
            );

            match ast_parent_data {
                // Parent AST node found, return a pointer relative to it.
                Some((ast_id, ast_parent_index)) => {
                    let crumb_slice = &tree_node.crumbs[ast_parent_index..];
                    let crumbs = span_tree::Crumbs::new(crumb_slice.to_vec());
                    Some(WidgetTreePointer { id: Some(ast_id), crumbs })
                }
                // No parent AST node found. Return a pointer from root.
                None => Some(WidgetTreePointer { id: None, crumbs: tree_node.crumbs.clone() }),
            }
        }
    }

    /// Perform an operation on a shared reference to a tree node under given pointer. When there is
    /// no node under provided pointer, the operation will not be performed and `None` will be
    /// returned.
    pub fn with_node<T>(
        &self,
        pointer: &WidgetTreePointer,
        f: impl FnOnce(&TreeNode) -> T,
    ) -> Option<T> {
        self.widgets_map.borrow().get(pointer).map(f)
    }

    /// Perform an operation on a mutable reference to a tree node under given pointer. When there
    /// is no node under provided pointer, the operation will not be performed and `None` will be
    /// returned.
    pub fn with_node_mut<T>(
        &self,
        pointer: &WidgetTreePointer,
        f: impl FnOnce(&mut TreeNode) -> T,
    ) -> Option<T> {
        self.widgets_map.borrow_mut().get_mut(pointer).map(f)
    }

    /// Perform an operation on a mutable reference to a widget port under given pointer. When there
    /// is no node under provided pointer, or when the found node has no port, the operation will
    /// not be performed and `None` will be returned.
    pub fn with_port_mut<T>(
        &self,
        pointer: &WidgetTreePointer,
        f: impl FnOnce(&mut Port) -> T,
    ) -> Option<T> {
        self.widgets_map.borrow_mut().get_mut(pointer).and_then(TreeNode::port_mut).map(f)
    }
}

/// State of a node in the widget tree. Provides additional information about the node's current
/// state, such as its depth in the widget tree, if it's connected, disabled, etc.
#[derive(Debug, Default, Clone, PartialEq)]
pub(super) struct NodeState {
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
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct WidgetTreePointer {
    /// The latest set ast::Id in the span tree.
    id:     Option<ast::Id>,
    /// Remaining crumbs to the widget, starting from the node with the latest set ast::Id.
    crumbs: span_tree::Crumbs,
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
    connected_map:   &'a HashMap<WidgetTreePointer, ConnectionData>,
    usage_type_map:  &'a HashMap<ast::Id, crate::Type>,
    old_widgets:     HashMap<WidgetTreePointer, TreeNode>,
    new_widgets:     HashMap<WidgetTreePointer, TreeNode>,
    parent_ast_id:   Option<ast::Id>,
    parent_crumbs:   span_tree::Crumbs,
    parent_state:    NodeState,
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
        let mut ast_data_to_restore = None;

        // Figure out the widget tree pointer for the current node. That pointer determines the
        // widget identity, allowing it to maintain internal state. If the previous tree already
        // contained a widget for this pointer, we have to reuse it.
        let tree_ptr = match span_tree_node.ast_id {
            Some(ast_id) => {
                let parent_ast_id = self.parent_ast_id.replace(ast_id);
                let parent_tail_crumbs =
                    std::mem::replace(&mut self.parent_crumbs, span_tree_node.crumbs.clone());
                ast_data_to_restore = Some((parent_ast_id, parent_tail_crumbs));
                WidgetTreePointer { id: Some(ast_id), crumbs: default() }
            }
            None => {
                let this_crumbs = &span_tree_node.crumbs;
                // We should always be in a child node since last ast ID. Verify that.
                let is_in_ast_subtree = this_crumbs.starts_with(&self.parent_crumbs);
                assert!(is_in_ast_subtree, "Not in AST child node.");
                let id = self.parent_ast_id;
                let crumbs_since_id = &this_crumbs[self.parent_crumbs.len()..];
                let crumbs = span_tree::Crumbs::new(crumbs_since_id.to_vec());
                WidgetTreePointer { id, crumbs }
            }
        };
        let old_node = self.old_widgets.remove(&tree_ptr);
        let is_placeholder = span_tree_node.is_expected_argument();
        let sibling_offset = span_tree_node.sibling_offset.as_usize();
        let usage_type = tree_ptr.id.and_then(|id| self.usage_type_map.get(&id)).cloned();

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
        let has_children = !span_tree_node.children.is_empty();
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
                    Metadata::from_kind(kind, usage_type.clone(), has_children)
                })
            });


        // Once we have the metadata and potential old widget to reuse, we have to apply the
        // configuration to the widget.
        let connection: ConnectionStatus = self.connected_map.get(&tree_ptr).copied().into();
        let subtree_connection = connection.or(self.parent_state.subtree_connection);
        let disabled = self.node_disabled;
        let state = NodeState { depth, connection, subtree_connection, disabled, usage_type };
        let state_to_restore = std::mem::replace(&mut self.parent_state, state.clone());

        let ctx =
            ConfigContext { builder: &mut *self, display: meta.display, span_tree_node, state };
        let app = ctx.app();
        let frp = ctx.frp();

        // Widget creation/update can recurse into the builder. All borrows must be dropped
        // at this point. The `configure` calls on the widgets are allowed to call back into the
        // tree builder in order to create their child widgets. Those calls will change builder's
        // state to reflect the correct parent node. We need to restore the state after the
        // `configure` call has been done, so that the next sibling node will receive correct parent
        // data.
        let child_node = if meta.has_port {
            let mut port = match old_node {
                Some(TreeNode::Port(port)) => port,
                Some(TreeNode::Widget(widget)) => Port::new(widget, app, frp),
                None => Port::new(DynWidget::new(&meta.config, app, frp), app, frp),
            };
            port.configure(&meta.config, ctx);
            TreeNode::Port(port)
        } else {
            let mut widget = match old_node {
                Some(TreeNode::Port(port)) => port.into_widget(),
                Some(TreeNode::Widget(widget)) => widget,
                None => DynWidget::new(&meta.config, app, frp),
            };
            widget.configure(&meta.config, ctx);
            TreeNode::Widget(widget)
        };

        // After visiting child node, restore previous layer's parent data.
        self.parent_state = state_to_restore;
        if let Some((id, crumbs)) = ast_data_to_restore {
            self.parent_ast_id = id;
            self.parent_crumbs = crumbs;
        }


        // Apply left margin to the widget, based on its offset relative to the previous sibling.
        let child_root = child_node.display_object().clone();
        let offset = sibling_offset.max(if is_placeholder { 1 } else { 0 });
        let left_margin = offset as f32 * WIDGET_SPACING_PER_OFFSET;
        if child_root.margin().x.start.as_pixels().map_or(true, |px| px != left_margin) {
            child_root.set_margin_left(left_margin);
        }

        self.new_widgets.insert(tree_ptr.clone(), child_node);
        child_root
    }
}
