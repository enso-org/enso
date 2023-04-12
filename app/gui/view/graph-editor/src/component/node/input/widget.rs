//! Definition of all hardcoded node widget variants and common widget FRP API.

mod debug;

use crate::prelude::*;

use crate::component::node::input::area::NODE_HEIGHT;
use crate::component::node::input::area::TEXT_OFFSET;
use crate::component::node::ConnectionStatus;
use enso_config::ARGS;
use enso_frp as frp;
use enso_text as text;
use ensogl::application::Application;
use ensogl::data::color;
use ensogl::display;
use ensogl_component::drop_down::DropdownValue;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;
use text::index::Byte;

// ===========
// === FRP ===
// ===========

ensogl::define_endpoints_2! {
    Input {
        // /// Set the widget's metadata that was received from the language server. It overrides
        // /// widget's configuration, even allowing the widget type to be completely changed. When
        // /// the metadata is set to `Some` value, the corresponding widget will ignore its span-tree
        // /// type.
        // set_metadata   (MetadataPointer, Option<Metadata>),

        // /// Set the connection status of the port indicated by the breadcrumbs. The optional type
        // /// is the type of the edge that was connected or disconnected if the edge was typed.
        // set_connected  (span_tree::Crumbs, ConnectionStatus),
    }
    Output {
        value_changed  (span_tree::Crumbs, Option<ImString>),
        request_import (ImString),
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

        /// A part of widget model that is dependant on the widget kind.
        #[derive(Debug)]
        #[allow(missing_docs)]
        enum DynWidget {
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
    pub display: Display,
    pub config:  Config,
}

impl Metadata {
    const fn always<C>(config: C) -> Self
    where C: ~const Into<Config> {
        Self { display: Display::Always, config: config.into() }
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

    fn from_kind(kind: &span_tree::node::Kind, has_children: bool) -> Self {
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
            Kind::Root | Kind::NamedArgument | Kind::Chained(_) if has_children =>
                Self::always(hierarchy::Config),
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
    pub(self) value_changed:  frp::Any<(span_tree::Crumbs, Option<ImString>)>,
    pub(self) request_import: frp::Any<ImString>,
}



// ==============
// === Widget ===
// ==============

/// The node widget view. Represents one widget of any kind on the node input area. Can change its
/// appearance and behavior depending on the widget metadata updates, without being recreated.
#[derive(Debug, Deref, Clone, CloneRef)]
pub struct Root {
    #[deref]
    frp:         Frp,
    sampled_frp: WidgetsFrp,
    model:       Rc<RootModel>,
}

impl display::Object for Root {
    fn display_object(&self) -> &display::object::Instance {
        &self.model.display_object.outer
    }
}

impl Root {
    /// Create a new node widget. The widget is initialized to empty state, waiting for first
    /// `rebuild_tree` call to build appropriate view hierarchy.
    #[profile(Task)]
    pub fn new(app: &Application) -> Self {
        let frp = Frp::new();
        let model = Rc::new(RootModel::new(app));

        let value_changed = frp.private.output.value_changed.clone_ref();
        let request_import = frp.private.output.request_import.clone_ref();
        let sampled_frp = WidgetsFrp { value_changed, request_import };

        Self { frp, sampled_frp, model }
    }

    pub fn set_metadata(&self, pointer: MetadataPointer, meta: Option<Metadata>) {
        self.model.set_metadata(pointer, meta);
    }

    pub fn set_connected(&self, tree_node: &span_tree::node::Ref, status: ConnectionStatus) {
        self.model
            .get_port_widget_pointer(tree_node)
            .map(|pointer| self.model.with_port_mut(&pointer, |p| p.set_connected(status)));
    }

    pub fn rebuild_tree_on_metadata_change(
        &self,
        tree: &span_tree::SpanTree,
        node_expression: &str,
    ) {
        if self.model.metadata_dirty.load(Ordering::Acquire) {
            self.rebuild_tree(tree, node_expression);
        }
    }

    pub fn rebuild_tree(&self, tree: &span_tree::SpanTree, node_expression: &str) {
        self.model.rebuild_tree(self.sampled_frp.clone_ref(), tree, node_expression)
    }

    pub fn get_port_display_object(
        &self,
        tree_node: &span_tree::node::Ref,
    ) -> Option<display::object::Instance> {
        let pointer = self.model.get_port_widget_pointer(tree_node)?;
        self.model.with_port(&pointer, |w| w.display_object().clone())
    }
}

/// =============
/// === Model ===
/// =============

#[derive(Debug)]
struct RootModel {
    app:            Application,
    display_object: debug::InstanceWithBg,
    widgets_map:    RefCell<HashMap<WidgetTreePointer, Port>>,
    metadata_map:   Rc<RefCell<HashMap<MetadataPointer, Metadata>>>,
    metadata_dirty: AtomicBool,
}

mod rectangle {
    use super::*;
    ensogl::shape! {
        alignment = left_bottom;
        (style: Style, color: Vector4<f32>) {
            let color = Var::<color::Rgba>::from(color);
            let width = Var::<Pixels>::from("input_size.x");
            let height = Var::<Pixels>::from("input_size.y");
            let rect = Rect((&width, &height)).corners_radius(5.0.px());
            let shape = rect.fill(color);
            shape.into()
        }
    }
}



impl RootModel {
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

        let widgets_map = default();
        let metadata_map = default();
        let metadata_dirty = default();
        Self { app, display_object, widgets_map, metadata_map, metadata_dirty }
    }

    /// Set the metadata for the given node data.
    fn set_metadata(&self, pointer: MetadataPointer, meta: Option<Metadata>) {
        // TODO: rebuild tree partially after batch updating metadata.
        // For now, the tree will be rebuilt completely.

        use std::collections::hash_map::Entry;
        let mut map = self.metadata_map.borrow_mut();
        let entry = map.entry(pointer);
        let dirty = match (entry, meta) {
            (Entry::Occupied(e), None) => {
                e.remove();
                true
            }
            (Entry::Occupied(mut e), Some(meta)) if e.get() != &meta => {
                e.insert(meta);
                true
            }
            (Entry::Vacant(e), Some(meta)) => {
                e.insert(meta);
                true
            }
            _ => false,
        };
        self.metadata_dirty.store(dirty, Ordering::Release);
    }

    #[profile(Task)]
    fn rebuild_tree(&self, frp: WidgetsFrp, tree: &span_tree::SpanTree, node_expression: &str) {
        self.metadata_dirty.store(false, Ordering::Release);
        let app = self.app.clone();
        let metadata_map = self.metadata_map.borrow();
        let widgets_map = self.widgets_map.take();
        let mut builder =
            WidgetTreeBuilder::new(node_expression, app, frp, &*metadata_map, widgets_map);
        let child = builder.child_widget(tree.root_ref(), 0);
        self.display_object.inner.replace_children(&[child]);
        self.widgets_map.replace(builder.new_widgets);
    }

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

    pub fn with_port<T>(
        &self,
        pointer: &WidgetTreePointer,
        f: impl FnOnce(&Port) -> T,
    ) -> Option<T> {
        self.widgets_map.borrow().get(pointer).map(f)
    }

    pub fn with_port_mut<T>(
        &self,
        pointer: &WidgetTreePointer,
        f: impl FnOnce(&mut Port) -> T,
    ) -> Option<T> {
        self.widgets_map.borrow_mut().get_mut(pointer).map(f)
    }
}


#[derive(Debug)]
pub struct ConfigContext<'a, 'b> {
    builder:        &'a mut WidgetTreeBuilder<'b>,
    display:        Display,
    span_tree_node: span_tree::node::Ref<'a>,
    depth:          usize,
}

impl<'a, 'b> ConfigContext<'a, 'b> {
    fn app(&self) -> &Application {
        &self.builder.app
    }

    fn frp(&self) -> &WidgetsFrp {
        &self.builder.frp
    }

    fn expression_at(&self, range: text::Range<Byte>) -> &str {
        &self.builder.node_expression[range]
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct WidgetTreePointer {
    /// The latest set ast::Id in the span tree.
    id:     Option<ast::Id>,
    /// Remaining crumbs to the widget, starting from the node with the latest set ast::Id.
    crumbs: span_tree::Crumbs,
}

#[derive(Debug)]
struct WidgetTreeBuilder<'a> {
    app:                Application,
    frp:                WidgetsFrp,
    metadata_map:       &'a HashMap<MetadataPointer, Metadata>,
    old_widgets:        HashMap<WidgetTreePointer, Port>,
    new_widgets:        HashMap<WidgetTreePointer, Port>,
    last_ast_id:        Option<ast::Id>,
    last_ast_id_crumbs: span_tree::Crumbs,
    node_expression:    &'a str,
}

impl<'a> WidgetTreeBuilder<'a> {
    fn new(
        node_expression: &'a str,
        app: Application,
        frp: WidgetsFrp,
        metadata_map: &'a HashMap<MetadataPointer, Metadata>,
        old_widgets: HashMap<WidgetTreePointer, Port>,
    ) -> Self {
        Self {
            app,
            frp,
            metadata_map,
            old_widgets,
            new_widgets: default(),
            last_ast_id: default(),
            last_ast_id_crumbs: default(),
            node_expression,
        }
    }

    #[must_use]
    pub(self) fn child_widget(
        &mut self,
        span_tree_node: span_tree::node::Ref<'_>,
        depth: usize,
    ) -> display::object::Instance {
        self.create_child_widget(span_tree_node, depth, None)
    }

    #[must_use]
    pub(self) fn child_widget_of_type(
        &mut self,
        span_tree_node: span_tree::node::Ref<'_>,
        depth: usize,
        meta: Metadata,
    ) -> display::object::Instance {
        self.create_child_widget(span_tree_node, depth, Some(meta))
    }


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
                let prev_ast_id = self.last_ast_id.replace(ast_id);
                let prev_crumbs =
                    std::mem::replace(&mut self.last_ast_id_crumbs, span_tree_node.crumbs.clone());
                ast_data_to_restore = Some((prev_ast_id, prev_crumbs));
                WidgetTreePointer { id: Some(ast_id), crumbs: default() }
            }
            None => {
                let this_crumbs = &span_tree_node.crumbs;
                // We should always be in a child node since last ast ID. Verify that.
                let is_in_ast_subtree = this_crumbs.starts_with(&self.last_ast_id_crumbs);
                assert!(is_in_ast_subtree, "Not in AST child node.");
                let id = self.last_ast_id;
                let crumbs_since_id = &this_crumbs[self.last_ast_id_crumbs.len()..];
                let crumbs = span_tree::Crumbs::new(crumbs_since_id.to_vec());
                WidgetTreePointer { id, crumbs }
            }
        };


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
                meta_fallback.get_or_insert_with(|| Metadata::from_kind(kind, has_children))
            });

        let old_widget = self.old_widgets.remove(&tree_ptr);
        let port = {
            let ctx =
                ConfigContext { builder: &mut *self, display: meta.display, span_tree_node, depth };
            // Widget creation/update can recurse into the builder. All borrows must be dropped
            // at this point.

            match old_widget {
                Some(mut port) => {
                    port.widget.configure(&meta.config, ctx);
                    port.update_root();
                    port
                }
                None => {
                    let app = ctx.app();
                    let frp = ctx.frp();
                    let mut widget = DynWidget::new(&meta.config, app, frp);
                    widget.configure(&meta.config, ctx);
                    Port::new(widget)
                }
            }
        };
        let root = port.display_object().clone();
        self.new_widgets.insert(tree_ptr.clone(), port);

        // After visiting child node, restore previous layer's data.
        if let Some((id, crumbs)) = ast_data_to_restore {
            self.last_ast_id = id;
            self.last_ast_id_crumbs = crumbs;
        }
        root
    }
}


// =================
// === Constants ===
// =================

/// The horizontal padding of ports. It affects how the port hover should extend the target text
/// boundary on both sides.
pub const PADDING_X: f32 = 4.0;


// ============
// === Port ===
// ============

/// Port shape definition.
pub mod port {
    use super::*;
    ensogl::shape! {
        (style:Style, color:Vector4) {
            let size = Var::canvas_size();
            let transparent = Var::<color::Rgba>::from("srgba(1.0,1.0,1.0,0.00001)");
            let shape_color = Var::<color::Rgba>::from(color);
            let hover_shape  = Rect(&size).fill(transparent);
            let visual_shape  = Rect(&size).corners_radius(size.y() / 2.0).fill(shape_color);
            hover_shape.union(visual_shape).into()
        }
    }
}

#[derive(Debug)]
pub(super) struct Port {
    shape:  port::View,
    widget: DynWidget,
    inner:  display::object::Instance,
}

impl Port {
    fn new(widget: DynWidget) -> Self {
        let shape = port::View::new();
        shape.color.set(color::Rgba::transparent().into());
        let inner = widget.root_object().clone_ref();
        shape.add_child(&inner);
        Self { shape, widget, inner }
    }

    fn update_root(&mut self) {
        let new_root = self.widget.root_object();
        if new_root != &self.inner {
            self.shape.remove_child(&self.inner);
            self.shape.add_child(new_root);
            self.inner = new_root.clone_ref();
        }
    }

    fn set_connected(&self, status: ConnectionStatus) {
        match status {
            ConnectionStatus::Connected { color } =>
                self.shape.color.set(color::Rgba::from(color).into()),
            ConnectionStatus::Disconnected => self.shape.color.modify(|c| c.xyz().push(0.0)),
        };
    }
}

impl display::Object for Port {
    fn display_object(&self) -> &display::object::Instance {
        self.shape.display_object()
    }
}
