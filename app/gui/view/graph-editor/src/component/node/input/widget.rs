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
//!
//!
//! ## Priority over override
//!
//! Whenever a configuration is selected using an override source (either point 1. or 2.), its
//! application can still be rejected if there are other applicable widgets that define
//! [`SpanWidget::PRIORITY_OVER_OVERRIDE`] as `true` in their implementation. In that case, the
//! override will be applied again on their children that use the same span-tree node.

use crate::prelude::*;

use crate::component::node::input::area::NODE_HEIGHT;
use crate::component::node::input::area::TEXT_OFFSET;
use crate::component::node::input::port::Port;
use crate::display::shape::Rectangle;
use crate::layers::CommonLayers;
use crate::GraphLayers;

use enso_frp as frp;
use enso_text as text;
use ensogl::application::Application;
use ensogl::data::color;
use ensogl::display;
use ensogl::display::scene;
use ensogl::display::shape::StyleWatchFrp;
use ensogl::display::style::FromTheme;
use ensogl::display::Scene;
use ensogl::gui::cursor;
use ensogl_component::drop_down::DropdownValue;
use span_tree::node::Ref as SpanRef;
use span_tree::PortId;
use span_tree::TagValue;
use std::any::TypeId;
use text::index::Byte;



/// A prelude module imported in all widget modules.
pub(super) mod prelude {
    pub use super::Choice;
    pub use super::ConfigContext;
    pub use super::Configuration;
    pub use super::IdentityBase;
    pub use super::KindFlags;
    pub use super::NodeInfo;
    pub use super::OverrideKey;
    pub use super::Score;
    pub use super::SpanWidget;
    pub use super::TransferRequest;
    pub use super::TreeNode;
    pub use super::WidgetIdentity;
    pub use super::WidgetsFrp;

    pub use ensogl::control::io::mouse;
    pub use ensogl::data::color;
    pub use ensogl::display;
    pub use ensogl::display::object;
    pub use ensogl::display::shape::Rectangle;
    pub use ensogl::display::shape::StyleWatchFrp;
    pub use ensogl::display::style::FromTheme;
    pub use ensogl_hardcoded_theme as theme;
    pub use span_tree::node::Ref as SpanRef;
}

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
        set_edit_ready_mode  (bool),
        set_read_only        (bool),
        set_view_mode        (crate::view::Mode),
        set_disabled         (bool),
        set_pending          (bool),
        node_base_color      (color::Lcha),
        node_port_color      (color::Lcha),
    }
    Output {
        value_changed    (span_tree::Crumbs, Option<ImString>),
        request_import   (ImString),
        on_port_hover    (Switch<PortId>),
        on_port_press    (PortId),
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
        /// The widget tree has been rebuilt. Its port structure has potentially been updated.
        on_rebuild_finished (),
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
pub trait SpanWidget: display::Object {
    /// Configuration associated with specific widget variant.
    type Config: Debug + Clone + PartialEq;
    /// Declare whether this widget should be considered for creation despite a config override
    /// being present. Widgets that declare `true` here must create a child widget on the same
    /// span-tree node, so that the override can be applied to this child instead.
    ///
    /// Only widgets declared above the overridden widget will take priority over it. That
    /// declaration order is defined in the usage of [`define_widget_modules!`] macro.
    const PRIORITY_OVER_OVERRIDE: bool = false;
    /// Score how well a widget kind matches current [`ConfigContext`], e.g. checking if the span
    /// node or declaration type match specific patterns. When this method returns
    /// [`Score::Mismatch`], this widget kind will not be used, even if it was requested by an
    /// override. The override will be ignored and another best scoring widget with default
    /// configuration will be used.
    fn match_node(ctx: &ConfigContext) -> Score;
    /// After a widget has been matched to a node, this method is used to determine its
    /// automatically derived configuration. It is not called for widgets that have a configuration
    /// provided externally or by a parent widget.
    fn default_config(ctx: &ConfigContext) -> Configuration<Self::Config>;
    /// Create a new widget with given configuration.
    fn new(config: &Self::Config, ctx: &ConfigContext) -> Self;
    /// Update configuration for existing widget.
    fn configure(&mut self, config: &Self::Config, ctx: ConfigContext);
    /// Receive a reference of the tree items for which the ownership transfer was requested. Called
    /// by the tree when [`WidgetsFrp::transfer_ownership`] signal is used.
    fn receive_ownership(
        &mut self,
        original_request: TransferRequest,
        nodes: Vec<(WidgetIdentity, TreeNode)>,
    ) {
        _ = (original_request, nodes);
    }
}

/// Description of how well a widget matches given node. Used to determine which widget should be
/// used, or whether the applied widget override is valid in given context.
#[derive(Debug, Default, Clone, Copy, PartialOrd, Ord, PartialEq, Eq)]
pub enum Score {
    /// This widget kind cannot accept the node. It will never be used, even if it was explicitly
    /// requested using an override.
    Mismatch,
    /// A bad, but syntactically valid match. Matching widget kind will only be used if it was
    /// explicitly requested using an override. Should be the default choice for cases where
    /// the node is syntactically valid in this widget's context, but no sensible defaults can
    /// be inferred from context.
    #[default]
    OnlyOverride,
    /// A good match, but there might be a better one. one. This widget will be used if there is no
    /// better option.
    Good,
    /// Widget matches perfectly and can be used outright, without checking other kinds.
    Perfect,
}

impl Score {
    /// A score from hard boolean condition. Either the widget matches perfectly, or it is a
    /// complete mismatch. Useful for more "technical" widgets that are mostly implementation
    /// details of the IDE.
    pub fn only_if(condition: bool) -> Self {
        if condition {
            Score::Perfect
        } else {
            Score::Mismatch
        }
    }

    /// Conditionally declare that the widget can be used if the condition is true, but only if it
    /// is explicitly requested using an override.
    pub fn allow_override_if(condition: bool) -> Self {
        if condition {
            Score::OnlyOverride
        } else {
            Score::Mismatch
        }
    }
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

        bitflags::bitflags!{
            /// A set of flags that determine the widget kind.
            #[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
            pub struct KindFlags: u32 {
                $(
                    #[allow(missing_docs, non_upper_case_globals)]
                    const $name = 1 << ${index()};
                )*
                /// A combination of flags for all widget kinds that has to be matched first before
                /// accepting an overridden configuration.
                const PRIORITY_OVER_OVERRIDE = $(
                    (<$module::Widget as SpanWidget>::PRIORITY_OVER_OVERRIDE as u32) << ${index()}
                )|*;
            }
        }

        impl KindFlags {
            /// Check whether the widget kind matching this flag is able to receive given span node.
            /// When more than one flag is set, [`Score::Mismatch`] will be returned.
            fn match_node(&self, ctx: &ConfigContext) -> Score {
                match self {
                    $(&KindFlags::$name => $module::Widget::match_node(ctx),)*
                    _ => Score::Mismatch,
                }
            }

            /// Create default configuration of the widget kind contained within this flag.
            fn default_config(&self, ctx: &ConfigContext) -> Configuration {
                match self {
                    $(&KindFlags::$name => $module::Widget::default_config(ctx).into_dyn(),)*
                    _ => panic!("No widget kind specified.")
                }
            }
        }

        impl DynConfig {
            /// Return a single flag that determines the used widget kind.
            fn flag(&self) -> KindFlags {
                match self {
                    $(DynConfig::$name(_) => KindFlags::$name,)*
                }
            }

            /// Return a bitfield that determines which widget kinds need to be checked for a match
            /// before applying this configuration as override.
            fn flags_with_priority_before_override(&self) -> KindFlags {
                // All bits that are set before the current one. Since we know that the `self.flag`
                // always has only a single bit set, we can just subtract 1 from it.
                let all_defined_before = KindFlags::from_bits_retain(self.flag().bits() - 1);
                all_defined_before & KindFlags::PRIORITY_OVER_OVERRIDE
            }
        }

        $(
            impl const From<<$module::Widget as SpanWidget>::Config> for DynConfig {
                fn from(config: <$module::Widget as SpanWidget>::Config) -> Self {
                    Self::$name(config)
                }
            }
        )*

        impl DynWidget {
            fn new(config: &DynConfig, ctx: &ConfigContext) -> Self {
                match config {
                    $(DynConfig::$name(config) => DynWidget::$name(SpanWidget::new(config, ctx)),)*
                }
            }

            fn name(&self) -> &'static str {
                match self {
                    $(DynWidget::$name(_) => stringify!($name),)*
                }
            }

            pub(super) fn configure(&mut self, config: &DynConfig, ctx: ConfigContext) {
                match (self, config) {
                    $((DynWidget::$name(model), DynConfig::$name(config)) => {
                        SpanWidget::configure(model, config, ctx);
                    },)*
                    (this, _) => {
                        *this = Self::new(config, &ctx);
                        this.configure(config, ctx)
                    },
                }
            }

            fn receive_ownership(&mut self,
                req: TransferRequest,
                nodes: Vec<(WidgetIdentity, TreeNode)>,
            ) {
                match (self) {
                    $(DynWidget::$name(model) => model.receive_ownership(req, nodes),)*
                }
            }
        }

        impl display::Object for DynWidget {
            fn display_object(&self) -> &display::object::Instance {
                match self {
                    $(DynWidget::$name(inner) => inner.display_object(),)*
                }
            }

            fn focus_receiver(&self) -> &display::object::Instance {
                match self {
                    $(DynWidget::$name(inner) => inner.focus_receiver(),)*
                }
            }
        }
    };
);

// Definition of implemented widget kinds. The order of the definitions determines the order in
// which the widgets are checked for a match with every span-tree node.
define_widget_modules! {
    /// A widget for top-level Enso method calls. Displays an icon.
    Method method,
    /// Separating line between top-level argument widgets. Can only be assigned through override,
    /// which is currently done in hierarchy widget.
    Separator separator,
    /// A widget for selecting a single value from a list of available options.
    SingleChoice single_choice,
    /// Displays the argument name next to its value widget. Can only be assigned through override,
    /// which is currently done in separator widget.
    ArgumentName argument_name,
    /// A widget for managing a list of values - adding, removing or reordering them.
    ListEditor list_editor,
    /// Empty widget that does not display anything, used for empty insertion points.
    InsertionPoint insertion_point,
    /// Default span tree traversal widget.
    Hierarchy hierarchy,
    /// Dedicated widget for '_' symbol in argument position.
    Blank blank,
    /// Default widget that only displays text.
    Label label,
}

// =====================
// === Configuration ===
// =====================

/// The configuration of a widget and its display properties. Defines how the widget should be
/// displayed, if it should be displayed at all, and whether or not it should have a port. Widgets
/// that declare themselves as having a port will be able to handle edge connections and visually
/// indicate that they are connected.
#[derive(Debug, Clone, PartialEq)]
pub struct Configuration<KindConfig = DynConfig> {
    /// Display mode of the widget: determines whether or not the widget should be displayed
    /// depending on current tree display mode.
    pub display:  Display,
    /// Whether or not the widget can receive a port. If `true`, the widget can be wrapped in a
    /// [`Port`] struct, but it is not guaranteed. If multiple widgets created at single span node
    /// declare themselves as wanting a port, only one of them will actually have one.
    pub has_port: bool,
    /// Configuration specific to given widget kind.
    pub kind:     KindConfig,
}

impl Configuration {
    /// Derive widget configuration from Enso expression, node data in span tree and inferred node
    /// info, like value type. When no configuration is provided with an override, this function
    /// will be used to create a default configuration.
    ///
    /// Will never return any configuration kind specified in `disallow` parameter, except for
    /// [`DynConfig::Label`] as an option of last resort.
    fn infer_from_context(
        ctx: &ConfigContext,
        allowed: KindFlags,
        score_better_than: Score,
    ) -> Option<Self> {
        let mut best_match = None;
        for kind in allowed {
            let score = kind.match_node(ctx);
            let current_score = best_match.map(|(_, score)| score).unwrap_or(score_better_than);
            if score > current_score {
                best_match = Some((kind, score));
                if score >= Score::Perfect {
                    break;
                }
            }
        }

        best_match.map(|(kind, _score)| kind.default_config(ctx))
    }

    /// An insertion point that always has a port.
    pub fn active_insertion_point() -> Self {
        Self::always(insertion_point::Config.into())
    }
}


impl<KindConfig> Configuration<KindConfig> {
    fn maybe_with_port(kind: KindConfig, has_port: bool) -> Self {
        Self { display: Display::Always, kind, has_port }
    }

    fn always(kind: KindConfig) -> Self {
        Self::maybe_with_port(kind, true)
    }

    fn inert(kind: KindConfig) -> Self {
        Self::maybe_with_port(kind, false)
    }

    /// Convert this configuration into a dynamic version.
    pub fn into_dyn(self) -> Configuration
    where KindConfig: Into<DynConfig> {
        Configuration {
            display:  self.display,
            has_port: self.has_port,
            kind:     self.kind.into(),
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

/// A possible value to choose in the widget (e.g. a single- or multi-choice widget). Can either be
/// derived from a `TagValue`, or from a widget configuration received from the language server.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Choice {
    /// The expression that should be inserted by the widget. Note that  this expression can still
    /// be preprocessed by the widget before being inserted into the node.
    pub value:           ImString,
    /// The import that must be present in the module or added when the widget entry is selected.
    pub required_import: Option<ImString>,
    /// The text that should be displayed by the widget to represent this option. The exact
    /// appearance of the label is up to the widget implementation.
    pub label:           ImString,
}

impl From<&TagValue> for Choice {
    fn from(tag_value: &TagValue) -> Self {
        let value: ImString = (&tag_value.expression).into();
        let label: ImString = tag_value.label.as_ref().map_or_else(|| value.clone(), Into::into);
        let required_import = tag_value.required_import.clone().map(Into::into);
        Choice { value, required_import, label }
    }
}

impl Choice {
    /// Create a choice with the same value and label.
    pub fn from_value(value: ImString) -> Self {
        Self { label: value.clone(), required_import: None, value }
    }

    /// Cloning choice value getter.
    pub fn value(&self) -> ImString {
        self.value.clone()
    }

    /// Cloning choice getter of import that must be present for value insertion to be valid.
    pub fn required_import(&self) -> Option<ImString> {
        self.required_import.clone()
    }
}

impl DropdownValue for Choice {
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
    pub(super) node_base_color:        frp::Sampler<color::Lcha>,
    pub(super) node_port_color:        frp::Sampler<color::Lcha>,
    pub(super) set_ports_visible:      frp::Sampler<bool>,
    pub(super) set_edit_ready_mode:    frp::Sampler<bool>,
    pub(super) set_read_only:          frp::Sampler<bool>,
    /// Whether the widget should allow user interaction, either mouse hover or click. A
    /// combination of `set_read_only`, `set_edit_ready_mode` and `set_ports_visible` signals.
    pub(super) allow_interaction:      frp::Sampler<bool>,
    pub(super) set_view_mode:          frp::Sampler<crate::view::Mode>,
    pub(super) hovered_port_children:  frp::Sampler<HashSet<WidgetIdentity>>,
    /// Remove given tree node's reference from the widget tree, and send its only remaining strong
    /// reference to a new widget owner using [`SpanWidget::receive_ownership`] method. This will
    /// effectively give up tree's ownership of that node, and will prevent its view from being
    /// reused.
    ///
    /// NOTE: Calling this during rebuild will have no effect.
    pub(super) transfer_ownership:     frp::Any<TransferRequest>,
    pub(super) value_changed:          frp::Any<(span_tree::Crumbs, Option<ImString>)>,
    pub(super) request_import:         frp::Any<ImString>,
    pub(super) on_port_hover:          frp::Any<Switch<PortId>>,
    pub(super) on_port_press:          frp::Any<PortId>,
    pub(super) pointer_style:          frp::Any<cursor::Style>,
    pub(super) connected_port_updated: frp::Any<()>,
}

/// A request for widget tree item ownership transfer. See [`WidgetsFrp::transfer_ownership`].
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Default)]
pub struct TransferRequest {
    /// The widget ID that will receive the ownership of the node. Usually this is the ID of the
    /// widget that sends the request, which can be obtained from [`NodeInfo::identity`], which is
    /// provided on [`ConfigContext`].
    pub new_owner:     WidgetIdentity,
    /// The ID of the node that should be transferred. Usually one of the node's children, which
    /// can be obtained from [`Child`] instance returned from [`TreeBuilder::child_widget`].
    pub to_transfer:   WidgetIdentity,
    /// Whether the whole subtree should be transferred, or just the node itself.
    pub whole_subtree: bool,
}



// ============
// === Tree ===
// ============

/// The node widget tree view. Contains all widgets created from the node's span tree, as well as
/// all input ports of a node. The tree is initialized to empty state, waiting for first
/// `rebuild_tree` call to build appropriate view hierarchy.
#[derive(Debug, Deref, Clone, CloneRef, display::Object)]
pub struct Tree {
    #[deref]
    frp:         Frp,
    widgets_frp: WidgetsFrp,
    #[display_object]
    model:       Rc<TreeModel>,
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
            transfer_ownership <- any(...);
            eval transfer_ownership((request) model.transfer_ownership(*request));

            set_ports_visible <- frp.set_ports_visible.sampler();
            set_edit_ready_mode <- frp.set_edit_ready_mode.sampler();
            set_read_only <- frp.set_read_only.sampler();
            set_view_mode <- frp.set_view_mode.sampler();
            node_base_color <- frp.node_base_color.sampler();
            node_port_color <- frp.node_port_color.sampler();
            on_port_hover <- any(...);
            on_port_press <- any(...);
            frp.private.output.on_port_hover <+ on_port_hover;
            frp.private.output.on_port_press <+ on_port_press;

            allow_interaction <- all_with3(
                &set_edit_ready_mode, &set_read_only, &set_ports_visible,
                |edit, read_only, ports_visible| !(*edit || *read_only || *ports_visible)
            ).sampler();

            port_hover_chain_dirty <- all(&on_port_hover, &frp.on_rebuild_finished)._0().debounce();
            hovered_port_children <- port_hover_chain_dirty.map(
                f!([model] (port) port.into_on().map_or_default(|id| model.port_child_widgets(id)))
            ).sampler();

        }

        let value_changed = frp.private.output.value_changed.clone_ref();
        let request_import = frp.private.output.request_import.clone_ref();
        let pointer_style = frp.private.output.pointer_style.clone_ref();
        let connected_port_updated = frp.private.output.connected_port_updated.clone_ref();
        let widgets_frp = WidgetsFrp {
            node_base_color,
            node_port_color,
            set_ports_visible,
            set_edit_ready_mode,
            set_read_only,
            allow_interaction,
            set_view_mode,
            transfer_ownership,
            value_changed,
            request_import,
            on_port_hover,
            on_port_press,
            pointer_style,
            hovered_port_children,
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

    /// Set all currently active connections. The connected nodes will be highlighted with a
    /// different color, and the widgets might change behavior depending on the connection status.
    pub fn set_connections(&self, map: &HashMap<PortId, color::Lcha>) {
        self.notify_dirty(self.model.set_connections(map));
    }

    /// Set disabled status for given span tree node. The disabled nodes will be grayed out.
    /// The widgets might change behavior depending on the disabled status.
    pub fn set_disabled(&self, disabled: bool) {
        self.notify_dirty(self.model.set_disabled(disabled));
    }

    /// Set pending status for given span tree node. The pending nodes will be semi-transparent.
    /// The widgets might change behavior depending on the pending status.
    pub fn set_pending(&self, pending: bool) {
        self.notify_dirty(self.model.set_pending(pending));
    }

    /// Rebuild tree if it has been marked as dirty. The dirty flag is marked whenever more data
    /// external to the span-tree is provided, using `set_config_override`, `set_usage_type`,
    /// `set_connections`, `set_disabled`, or `set_pending` methods of the widget tree.
    pub fn rebuild_tree_if_dirty(
        &self,
        tree: &span_tree::SpanTree,
        node_expression: &str,
        layers: &GraphLayers,
        styles: &StyleWatchFrp,
    ) {
        if self.model.tree_dirty.get() {
            self.rebuild_tree(tree, node_expression, layers, styles);
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
        layers: &GraphLayers,
        styles: &StyleWatchFrp,
    ) {
        self.model.rebuild_tree(
            self.widgets_frp.clone_ref(),
            tree,
            node_expression,
            layers,
            styles,
        );
        self.frp.private.output.on_rebuild_finished.emit(());
        debug!("Widget tree:\n{:?}", self.pretty_printer());
    }

    /// Get the root display object of the widget port for given span tree node. Not all nodes must
    /// have a distinct widget, so the returned value might be [`None`].
    pub fn get_port_display_object(&self, port_id: PortId) -> Option<display::object::Instance> {
        self.model.with_port(port_id, |w| w.display_object().clone())
    }

    /// Get hover shapes for all ports in the tree. Used in tests to manually dispatch mouse events.
    pub fn port_hover_shapes(&self) -> Vec<Rectangle> {
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

    /// Wrap the widget into a pretty-printing adapter that implements `Debug` and prints the widget
    /// hierarchy. See [`Tree::debug_print`] method for more details.
    ///
    /// Note that this printer emits multi-line output. In order for those lines to be properly
    /// aligned, it should be always printed on a new line.
    pub fn pretty_printer(&self) -> WidgetTreePrettyPrint<'_> {
        WidgetTreePrettyPrint { tree: self }
    }

    /// Get pretty-printed representation of this widget tree for debugging purposes.
    fn debug_print(&self) -> String {
        let mut result = String::new();
        let hierarchy = self.model.hierarchy.borrow();
        let Some(root) = hierarchy.first() else { return "<EMPTY>".to_string() };
        self.print_subtree(&mut result, "", root.identity);
        result
    }

    fn print_subtree(&self, dst: &mut String, indent: &str, pointer: WidgetIdentity) {
        dst.push_str(indent);
        match self.model.nodes_map.borrow().get(&pointer) {
            Some(entry) => {
                dst.push_str(entry.node.widget().name());
                if entry.node.port().is_some() {
                    dst.push('*');
                }
            }
            None => {
                dst.push_str("<MISSING>");
            }
        }
        dst.push('\n');
        let indent = format!("{indent}  ");
        self.model.iter_children(pointer).for_each(|child| self.print_subtree(dst, &indent, child));
    }
}

// === Pretty printing debug adapter ===

/// Debug adapter used for pretty-printing the widget tree. Can be used to print the widget tree
/// hierarchy. This printer is normally too verbose to be a default `Debug` implementation of
/// `Tree`, so it is hidden behind a separate adapter and can be chosen by calling
/// `tree.pretty_printer()`.
pub struct WidgetTreePrettyPrint<'a> {
    tree: &'a Tree,
}

impl<'a> Debug for WidgetTreePrettyPrint<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> fmt::Result {
        let printed = self.tree.debug_print();
        f.write_str(&printed)
    }
}


// ================
// === TreeNode ===
// ================

/// A single entry in the widget tree. If the widget has an attached port, it will be wrapped in
/// `Port` struct and stored in `Port` variant. Otherwise, the widget will be stored directly using
/// the `Widget` node variant.
#[derive(Debug)]
pub enum TreeNode {
    /// A tree node that contains a port. The port wraps a widget.
    Port(Port),
    /// A tree node without a port, directly containing a widget.
    Widget(DynWidget),
}

impl TreeNode {
    #[allow(missing_docs)]
    pub fn port(&self) -> Option<&Port> {
        match self {
            TreeNode::Port(port) => Some(port),
            TreeNode::Widget(_) => None,
        }
    }

    #[allow(missing_docs)]
    pub fn widget(&self) -> &DynWidget {
        match self {
            TreeNode::Port(port) => port.widget(),
            TreeNode::Widget(widget) => widget,
        }
    }

    #[allow(missing_docs)]
    pub fn widget_mut(&mut self) -> &mut DynWidget {
        match self {
            TreeNode::Port(port) => port.widget_mut(),
            TreeNode::Widget(widget) => widget,
        }
    }
}

impl display::Object for TreeNode {
    fn display_object(&self) -> &display::object::Instance {
        match self {
            TreeNode::Port(port) => port.display_object(),
            TreeNode::Widget(widget) => widget.display_object(),
        }
    }

    fn focus_receiver(&self) -> &display::object::Instance {
        match self {
            TreeNode::Port(port) => port.focus_receiver(),
            TreeNode::Widget(widget) => widget.focus_receiver(),
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
pub struct EdgeData {
    /// Color of an edge connected to the port.
    pub color: color::Lcha,
    /// Span tree depth at which the connection is made.
    pub depth: usize,
}



// =================
// === TreeModel ===
// =================

#[derive(Debug, display::Object)]
struct TreeModel {
    app:            Application,
    display_object: display::object::Instance,
    /// A map from widget identity to the tree node and its index in the `hierarchy` vector.
    nodes_map:      RefCell<HashMap<WidgetIdentity, TreeEntry>>,
    /// Hierarchy data for nodes, stored in node insertion order (effectively depth-first). It can
    /// be used to quickly find the parent of a node, or iterate over all children or descendants
    /// of a node.
    hierarchy:      RefCell<Vec<NodeHierarchy>>,
    ports_map:      RefCell<HashMap<PortId, WidgetIdentity>>,
    override_map:   Rc<RefCell<HashMap<OverrideKey, Configuration>>>,
    connected_map:  Rc<RefCell<HashMap<PortId, color::Lcha>>>,
    usage_type_map: Rc<RefCell<HashMap<ast::Id, crate::Type>>>,
    node_disabled:  Cell<bool>,
    node_pending:   Cell<bool>,
    tree_dirty:     Cell<bool>,
}

impl TreeModel {
    /// Create a new node widget, selecting the appropriate widget type based on the provided
    /// argument info.
    fn new(app: &Application) -> Self {
        let app = app.clone_ref();
        let display_object = display::object::Instance::new_named("Tree");
        display_object.use_auto_layout();
        display_object.set_children_alignment_left_center().justify_content_center_y();
        display_object.set_size_y(NODE_HEIGHT);
        display_object.set_padding_left(TEXT_OFFSET);
        display_object.set_padding_right(TEXT_OFFSET);

        Self {
            app,
            display_object,
            node_disabled: default(),
            node_pending: default(),
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
    fn set_connections(&self, map: &HashMap<PortId, color::Lcha>) -> bool {
        let mut prev_map = self.connected_map.borrow_mut();
        let modified = &*prev_map != map;
        if modified {
            *prev_map = map.clone();
        }
        self.mark_dirty_flag(modified)
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

    /// Set the execution status under given widget. It may cause the tree to be marked as dirty.
    fn set_pending(&self, pending: bool) -> bool {
        let prev_pending = self.node_pending.replace(pending);
        self.mark_dirty_flag(prev_pending != pending)
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

    /// Iterate over a node with given pointer and all its descendants, if it exists in the tree.
    #[allow(dead_code)]
    pub fn iter_subtree(
        &self,
        pointer: WidgetIdentity,
    ) -> impl Iterator<Item = WidgetIdentity> + '_ {
        let hierarchy = self.hierarchy.borrow();
        let nodes = self.nodes_map.borrow();
        let total_range = nodes.get(&pointer).map_or(0..0, |entry| {
            let start = entry.index;
            let total_descendants = hierarchy[entry.index].total_descendants;
            start..start + 1 + total_descendants
        });
        total_range.map(move |index| hierarchy[index].identity)
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

        iter::from_fn(move || {
            let index = total_range.next()?;
            let entry = hierarchy[index];
            // Skip all descendants of the child. The range is now at the next direct child.
            if entry.total_descendants > 0 {
                total_range.nth(entry.total_descendants - 1);
            }
            Some(entry.identity)
        })
    }

    /// Prevent a widget from being reused in future rebuild. Send its only remaining strong
    /// reference to the new owner.
    fn transfer_ownership(&self, request: TransferRequest) {
        let mut nodes = Vec::new();
        if request.whole_subtree {
            let iter = self.iter_subtree(request.to_transfer);
            let mut nodes_map = self.nodes_map.borrow_mut();
            nodes.extend(iter.filter_map(move |id| Some((id, nodes_map.remove(&id)?.node))));
        } else {
            let mut nodes_map = self.nodes_map.borrow_mut();
            if let Some(entry) = nodes_map.remove(&request.to_transfer) {
                nodes.push((request.to_transfer, entry.node));
            }
        }

        let mut nodes_map = self.nodes_map.borrow_mut();
        if let Some(owner) = nodes_map.get_mut(&request.new_owner) {
            owner.node.widget_mut().receive_ownership(request, nodes);
        }
    }

    #[profile(Task)]
    fn rebuild_tree(
        &self,
        frp: WidgetsFrp,
        tree: &span_tree::SpanTree,
        node_expression: &str,
        layers: &GraphLayers,
        styles: &StyleWatchFrp,
    ) {
        self.tree_dirty.set(false);
        let app = self.app.clone();
        let override_map = self.override_map.borrow();
        let connected_map = self.connected_map.borrow();
        let usage_type_map = self.usage_type_map.borrow();
        let old_nodes = self.nodes_map.take();
        let node_disabled = self.node_disabled.get();
        let node_pending = self.node_pending.get();

        // Old hierarchy is not used during the rebuild, so we might as well reuse the allocation.
        let mut hierarchy = self.hierarchy.take();
        hierarchy.clear();

        let shared_extension = scene().extension::<SceneSharedExtension>();

        let mut builder = TreeBuilder {
            app,
            frp,
            node_disabled,
            node_pending,
            node_expression,
            layers,
            styles,
            override_map: &override_map,
            connected_map: &connected_map,
            usage_type_map: &usage_type_map,
            old_nodes,
            hierarchy,
            local_overrides: default(),
            pointer_usage: default(),
            new_nodes: default(),
            parent_info: default(),
            last_ast_depth: default(),
            extensions: default(),
            node_settings: default(),
            shared: &shared_extension.shared,
        };

        let child = builder.child_widget(tree.root_ref(), default());
        self.display_object.replace_children(&[child.root_object]);

        self.nodes_map.replace(builder.new_nodes);
        self.hierarchy.replace(builder.hierarchy);
        let mut ports_map_borrow = self.ports_map.borrow_mut();
        ports_map_borrow.clear();
        ports_map_borrow.extend(builder.pointer_usage.into_iter().filter_map(|(k, v)| {
            let (port_id, index) = v.assigned_port?;
            Some((port_id, WidgetIdentity { main: k, index }))
        }));
    }

    /// Perform an operation on a shared reference to a tree port under given pointer. When there is
    /// no port under provided pointer, the operation will not be performed and `None` will be
    /// returned.
    pub fn with_port<T>(&self, port: PortId, f: impl FnOnce(&Port) -> T) -> Option<T> {
        let identity = *self.ports_map.borrow().get(&port)?;
        self.nodes_map.borrow().get(&identity).and_then(|n| n.node.port()).map(f)
    }

    /// Compute a set of descendant widgets of a given port.
    fn port_child_widgets(&self, port: PortId) -> HashSet<WidgetIdentity> {
        let identity = self.ports_map.borrow().get(&port).copied();
        identity.map_or_default(|id| self.iter_subtree(id).collect())
    }
}

/// State of a node in the widget tree. Provides additional information about the node's current
/// state, such as its depth in the widget tree, if it's connected, disabled, etc.
#[derive(Debug, Clone, PartialEq)]
pub struct NodeInfo {
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
    /// Whether the node is awaiting execution completion.
    pub pending:            bool,
    /// Inferred type of Enso expression at this node's span. May differ from the definition type
    /// stored in the span tree.
    pub usage_type:         Option<crate::Type>,
}

/// Settings that can be manipulated by the widget during its own configuration, and will impact
/// how the builder will treat the widget's children.
#[derive(Debug, Clone, Default)]
struct NodeSettings {
    /// Whether the widget is manipulating the child margins itself. Will prevent the builder from
    /// automatically adding margins calculated from span tree offsets.
    manage_margins:                  bool,
    /// Whether the widget wants to not have the automatic margin applied.
    skip_self_margin:                bool,
    /// Override the padding of port hover area, which is used during edge dragging to determine
    /// which port is being hovered.
    custom_child_port_hover_padding: Option<f32>,
}

/// A collection of common data used by all widgets and ports in the widget tree during
/// configuration. Provides the main widget's interface to the tree builder, allowing for creating
/// child widgets.
#[derive(Debug)]
pub struct ConfigContext<'a, 'b> {
    builder:               &'a mut TreeBuilder<'b>,
    /// The span tree node corresponding to the widget being configured.
    pub(super) span_node:  SpanRef<'a>,
    /// Additional state associated with configured widget tree node, such as its depth, connection
    /// status or parent node information.
    pub(super) info:       NodeInfo,
    /// The layer partitions that should be used for all of the widget's visual and hover areas. By
    /// default, the widget root object is automatically placed on the visual partition. The hover
    /// partition needs to be manually assigned to suitable display objects within the widget
    /// implementation.
    pub layers:            CommonLayers,
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

    /// Get the code expression fragment represented by current span node.
    pub fn span_expression(&self) -> &str {
        self.expression_at(self.span_node.span())
    }

    /// Get the code expression fragment represented by the given byte range.
    pub fn expression_at(&self, range: text::Range<Byte>) -> &str {
        &self.builder.node_expression[range]
    }

    /// Get the `StyleWatchFrp` used by this node.
    pub fn styles(&self) -> &StyleWatchFrp {
        self.builder.styles
    }

    /// Get a style sampler of typed style object. The core FRP nodes of this style object are
    /// shared across all widget instances in the scene. Using this method is preferred over
    /// manually calling [`FromTheme::from_theme`] in combination with [`ConfigContext::styles`].
    pub fn cached_style<T: FromTheme + Any>(&self, network: &frp::Network) -> frp::Stream<T> {
        let sampler = self.builder.shared.cached_style::<T>();
        // For each widget, create a dedicated FRP node that will be initialized after its FRP
        // network has been fully connected.
        let per_widget = network.any_mut("cached_style");
        per_widget.attach(&sampler);

        // Initialize and re-emit the last sampler value in the next tick. Using direct on_event and
        // emit to avoid creating unnecessary additional FRP nodes.
        use frp::stream::EventEmitter;
        network.store(&frp::microtasks::next_microtask(
            f!([sampler, per_widget] per_widget.emit_event(&default(), &sampler.value())),
        ));

        per_widget.into()
    }

    /// Set an extension object of specified type at the current tree position. Any descendant
    /// widget will be able to access it, as long as it can name its type. This allows for
    /// configure-time communication between any widgets inside the widget tree.
    pub fn set_extension<T: Any>(&mut self, val: T) {
        let id = TypeId::of::<T>();
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
        self.any_extension_index_by_type(TypeId::of::<T>())
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
        match self.any_extension_index_by_type(TypeId::of::<T>()) {
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

    fn any_extension_index_by_type(&self, id: TypeId) -> Option<usize> {
        self.builder.extensions.iter().rposition(|ext| ext.deref().type_id() == id)
    }

    fn self_extension_index_by_type(&self, id: TypeId) -> Option<usize> {
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct StableSpanIdentity {
    /// Identity base of either the node itself, or the closest ancestor node which has one.
    pub base:          IdentityBase,
    /// A hash of remaining data used to distinguish between tree nodes. We store a hash instead of
    /// the data directly, so the type can be trivially copied. The collision is extremely unlikely
    /// due to u64 being extremely large hash space, compared to the size of the used data. Many
    /// nodes are also already fully distinguished by the AST ID alone.
    ///
    /// Currently we are hashing a portion of span-tree crumbs, starting from the closest node with
    /// assigned identity base up to this node. The widgets should not rely on the exact kind of
    /// data used, as it may be extended to include more information in the future.
    pub identity_hash: u64,
}

/// Data that uniquely identifies some span nodes. Not all nodes have unique identity base. To
/// disambiguate nodes that don't have their own stable base, use [`StableSpanIdentity`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum IdentityBase {
    /// There isn't any parent with assigned AST id.
    #[default]
    FromRoot,
    /// AST ID of either the node itself, or the closest ancestor node which has one.
    AstNode(ast::Id),
    /// AST ID of the node that this span is an extension of. Only present if this span doesn't
    /// have an unique AST ID assigned.
    ExtNode(ast::Id),
}

impl StableSpanIdentity {
    fn from_node(node: &SpanRef) -> Self {
        let (base, base_idx) = if let Some(ast_id) = node.ast_id {
            (IdentityBase::AstNode(ast_id), node.crumbs.len())
        } else if let Some(ext_id) = node.extended_ast_id {
            let base_idx = node
                .span_tree
                .find_map_in_chain(&node.crumbs, |idx, node| {
                    (node.extended_ast_id == Some(ext_id)).then_some(idx)
                })
                .unwrap_or(node.crumbs.len());
            (IdentityBase::ExtNode(ext_id), base_idx)
        } else {
            let mut found_base = (IdentityBase::FromRoot, node.crumbs.len());
            let mut current_ext = None;
            node.span_tree.find_map_in_chain(&node.crumbs, |idx, node| {
                if let Some(ast) = node.ast_id {
                    found_base = (IdentityBase::AstNode(ast), idx);
                } else if let Some(ext) = node.extended_ast_id {
                    if current_ext != Some(ext) {
                        found_base = (IdentityBase::ExtNode(ext), idx);
                    }
                }
                current_ext = node.extended_ast_id;
                None::<()>
            });
            found_base
        };
        let identity_hash = Self::hash_crumbs_from_base(&node.crumbs[..], base_idx);
        Self { base, identity_hash }
    }

    fn hash_crumbs_from_base(crumbs: &[usize], base_idx: usize) -> u64 {
        let remaining_crumbs = &crumbs[base_idx..];
        let mut hasher = DefaultHasher::new();
        remaining_crumbs.hash(&mut hasher);
        hasher.finish()
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Deref, Default)]
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
    next_index:       usize,
    /// The pointer index of a widget on this span tree that received a port, if any exist already.
    assigned_port:    Option<(PortId, usize)>,
    /// The widget kinds that are no longer allowed to be created for this widget pointer. Most
    /// often used for widgets that were already created on this node.
    disallowed_kinds: KindFlags,
}

impl PointerUsage {
    fn next_index(&mut self) -> usize {
        self.next_index += 1;
        self.next_index - 1
    }

    fn request_port(
        &mut self,
        identity: &WidgetIdentity,
        port_id: Option<PortId>,
        wants_port: bool,
    ) -> bool {
        let Some(port_id) = port_id else { return false; };
        let will_receive_port = wants_port && self.assigned_port.is_none();
        will_receive_port.then(|| self.assigned_port = Some((port_id, identity.index)));
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
    node_pending:    bool,
    node_expression: &'a str,
    layers:          &'a GraphLayers,
    styles:          &'a StyleWatchFrp,
    /// A list of widget overrides configured on the widget tree. It is persistent between tree
    /// builds, and cannot be modified during the tree building process.
    override_map:    &'a HashMap<OverrideKey, Configuration>,
    /// A list of additional overrides specified by the widgets during the tree building process.
    /// Useful for applying overrides conditionally, e.g. only when a specific dropdown choice is
    /// selected. This is a temporary map that is cleared and created from scratch for
    /// each tree building process.
    local_overrides: HashMap<OverrideKey, Configuration>,
    connected_map:   &'a HashMap<PortId, color::Lcha>,
    usage_type_map:  &'a HashMap<ast::Id, crate::Type>,
    old_nodes:       HashMap<WidgetIdentity, TreeEntry>,
    new_nodes:       HashMap<WidgetIdentity, TreeEntry>,
    hierarchy:       Vec<NodeHierarchy>,
    pointer_usage:   HashMap<StableSpanIdentity, PointerUsage>,
    parent_info:     Option<NodeInfo>,
    node_settings:   NodeSettings,
    last_ast_depth:  usize,
    extensions:      Vec<Box<dyn Any>>,
    shared:          &'a SceneShared,
}

impl<'a> TreeBuilder<'a> {
    /// Signal to the builder that this widget manages child margins on its own. This will prevent
    /// the builder from automatically adding margins to the widget's children based on the offset
    /// from previous span.
    pub fn manage_child_margins(&mut self) {
        self.node_settings.manage_margins = true;
    }

    /// Signal to the builder that this widget manages its own margin. This will prevent the
    /// builder from automatically adding margins to the widget based on the offset from previous
    /// span.
    pub fn manage_margin(&mut self) {
        self.node_settings.skip_self_margin = true;
    }

    /// Set an additional config override for widgets that might be built in the future within the
    /// same tree build process. Takes precedence over overrides specified externally. This is
    /// useful for applying overrides conditionally, e.g. only when a specific dropdown choice is
    /// selected.
    pub fn set_local_override(&mut self, key: OverrideKey, config: Configuration) {
        self.local_overrides.insert(key, config);
    }

    /// Override horizontal port hover area margin for ports of this children. The margin is used
    /// during edge dragging to determine which port is being hovered.
    pub fn override_port_hover_padding(&mut self, padding: Option<f32>) {
        self.node_settings.custom_child_port_hover_padding = padding;
    }

    /// Mark certain widget kinds as forbidden for particular span-tree node. When the widgets for
    /// that node are being built, the builder will not evaluate those widget's matching rules.
    /// This method only has an effect for future widgets created for the same node, and does not
    /// affect widgets that were already created. Make sure to call it before using
    /// [`TreeBuilder::create_widget`] on that node.
    pub fn forbid_widget_kind(&mut self, span_node: &SpanRef<'_>, widget_kinds: KindFlags) {
        let main_ptr = StableSpanIdentity::from_node(span_node);
        let ptr_usage = self.pointer_usage.entry(main_ptr).or_default();
        ptr_usage.disallowed_kinds |= widget_kinds;
    }

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
    pub fn child_widget(&mut self, span_node: SpanRef<'_>, nesting_level: NestingLevel) -> Child {
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
        span_node: SpanRef<'_>,
        nesting_level: NestingLevel,
        configuration: Option<&Configuration>,
    ) -> Child {
        // This call can recurse into itself within the widget configuration logic. We need to save
        // the current layer's state, so it can be restored later after visiting the child node.
        let parent_last_ast_depth = self.last_ast_depth;
        let depth = span_node.crumbs.len();
        let is_extended_ast = span_node.ast_id.is_none() && span_node.extended_ast_id.is_some();

        // Figure out the widget tree pointer for the current node. That pointer determines the
        // widget identity, allowing it to maintain internal state. If the previous tree already
        // contained a widget for this pointer, we have to reuse it.
        let main_ptr = StableSpanIdentity::from_node(&span_node);

        let ptr_usage = self.pointer_usage.entry(main_ptr).or_default();
        let widget_id = main_ptr.to_identity(ptr_usage);

        let is_placeholder = span_node.is_expected_argument();
        let sibling_offset = span_node.sibling_offset.as_usize();
        let usage_type = span_node.ast_id.and_then(|id| self.usage_type_map.get(&id)).cloned();

        // Prepare the widget node info and build context.
        let connection_color = span_node.port_id.as_ref().and_then(|p| self.connected_map.get(p));
        let connection = connection_color.map(|&color| EdgeData { color, depth });
        let parent_connection = self.parent_info.as_ref().and_then(|info| info.connection);
        let subtree_connection = connection.or(parent_connection);

        let insertion_index = self.hierarchy.len();
        self.hierarchy.push(NodeHierarchy {
            identity:          widget_id,
            parent_index:      self.parent_info.as_ref().map(|info| info.insertion_index),
            // This will be updated later, after the child widgets are created.
            total_descendants: 0,
        });

        let disabled = self.node_disabled;
        let pending = self.node_pending;

        let info = NodeInfo {
            identity: widget_id,
            insertion_index,
            nesting_level,
            connection,
            subtree_connection,
            disabled,
            pending,
            usage_type,
        };

        // == Determine widget configuration ==

        // There are three potential sources for configuration, that are
        // used in order, whichever is available and allowed first:
        // 1. The `configuration` argument, which can be set by the parent widget if it wants to
        //    override the configuration for its child.
        // 2. The override associated with a the span tree node, located using `OverrideKey`. This
        // can be set by an external source, e.g. based on language server.
        // 3. The default configuration for the widget, which is determined based on the node kind,
        // usage type and whether it has children.
        //
        // Note that the override can still be rejected if there is an available higher priority
        // applicable widget that defines [`SpanWidget::PRIORITY_OVER_OVERRIDE`] as `true` in its
        // implementation. In that case, it is expected that the forcing widget will declare a child
        // using the same span-tree node, so that the override can eventually be applied anyway.

        let allowed_configs = !ptr_usage.disallowed_kinds;
        let parent_extensions_len = self.extensions.len();

        // TODO: We always use `main_nodes` here right now, as widgets are never visible when the
        // node is in edit the mode. Once this changes, we will need to use `edited_nodes`
        // conditionally.
        let layers = self.layers.main_nodes.layers_for_widgets_at_depth(depth);

        let ctx = ConfigContext {
            builder: &mut *self,
            span_node,
            info: info.clone(),
            parent_extensions_len,
            layers: layers.clone(),
        };


        let config_is_applicable = |ctx: &ConfigContext, cfg: &Configuration| {
            let flag = cfg.kind.flag();
            allowed_configs.contains(flag) && flag.match_node(ctx) > Score::Mismatch
        };

        let mut local_override_key = None;
        let arg_override =
            configuration.filter(|c| config_is_applicable(&ctx, c)).map(Cow::Borrowed);
        let applicable_override = arg_override.or_else(|| {
            let key = OverrideKey {
                call_id:       ctx.span_node.kind.call_id()?,
                argument_name: ctx.span_node.kind.argument_name()?.into(),
            };
            let from_local_map = ctx.builder.local_overrides.remove(&key);
            if let Some(local_override) = from_local_map.filter(|c| config_is_applicable(&ctx, c)) {
                local_override_key = Some(key);
                Some(Cow::Owned(local_override))
            } else {
                let external_override =
                    ctx.builder.override_map.get(&key).filter(|c| config_is_applicable(&ctx, c));
                external_override.map(Cow::Borrowed)
            }
        });

        let configuration = match applicable_override {
            Some(override_config) => {
                // Once an override is determined, check if there are other higher-priority widgets
                // that would like to force their configuration on this node, despite the override.
                let with_priority =
                    override_config.kind.flags_with_priority_before_override() & allowed_configs;

                let matched_before_override =
                    Configuration::infer_from_context(&ctx, with_priority, Score::OnlyOverride);
                if let Some(matched) = matched_before_override {
                    // When an applicable local override ends up not being applied, we need to put
                    // it back into the local overrides map. It may still be used by a child widget
                    // defined on the same span-tree node.
                    if let Some(key) = local_override_key {
                        ctx.builder.local_overrides.insert(key, override_config.into_owned());
                    }
                    Cow::Owned(matched)
                } else {
                    override_config
                }

                // If the override config was rejected, put it back into the local overrides map.
            }
            None => Cow::Owned(
                Configuration::infer_from_context(&ctx, allowed_configs, Score::Mismatch)
                    .unwrap_or_else(|| {
                        // When no applicable widget was found, fallback to the label.
                        KindFlags::Label.default_config(&ctx)
                    }),
            ),
        };
        let configuration = configuration.as_ref();

        // == Apply selected configuration. ==

        let this = &mut *ctx.builder;
        let ptr_usage = this.pointer_usage.entry(main_ptr).or_default();
        ptr_usage.disallowed_kinds |= configuration.kind.flag();
        let can_assign_port = configuration.has_port && !is_extended_ast;
        let widget_has_port =
            ptr_usage.request_port(&widget_id, ctx.span_node.port_id, can_assign_port);

        let port_pad = this.node_settings.custom_child_port_hover_padding;
        let old_node = this.old_nodes.remove(&widget_id).map(|e| e.node);
        let parent_info = mem::replace(&mut this.parent_info, Some(info.clone()));
        let saved_node_settings = mem::take(&mut this.node_settings);

        // === Create the widget ===

        // Widget creation/update can recurse into the builder. All borrows must be dropped
        // at this point. The `configure` calls on the widgets are allowed to call back into the
        // tree builder in order to create their child widgets. Those calls will change builder's
        // state to reflect the correct parent node. We need to restore the state after the
        // `configure` call has been done, so that the next sibling node will receive correct parent
        // data.
        let child_node = if widget_has_port {
            let mut port = match old_node {
                Some(TreeNode::Port(port)) => port,
                Some(TreeNode::Widget(widget)) => Port::new(widget, &ctx),
                None => Port::new(DynWidget::new(&configuration.kind, &ctx), &ctx),
            };
            let port_hover_layer = ctx.builder.layers.main_nodes.port_hover_layer(depth);
            port.configure(&configuration.kind, ctx, port_pad, &port_hover_layer);
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

        // === Maintain hierarchy ===

        // Once the node has been configured and all its children have been created, we can update
        // the hierarchy data.
        self.hierarchy[insertion_index].total_descendants =
            self.hierarchy.len() - insertion_index - 1;

        // After visiting child node, restore previous layer's parent data.
        self.parent_info = parent_info;
        self.last_ast_depth = parent_last_ast_depth;
        self.extensions.truncate(parent_extensions_len);

        // === Apply layout ===

        let skip_self_margin = self.node_settings.skip_self_margin;
        self.node_settings = saved_node_settings;

        let child_root = child_node.display_object().clone();
        layers.visual.add(&child_root);

        if !(skip_self_margin || self.node_settings.manage_margins) {
            // Apply left margin to the widget, based on its offset relative to the previous
            // sibling.
            let offset = match () {
                _ if !widget_id.is_first_widget_of_span() => 0,
                _ if is_placeholder => 1,
                _ => sibling_offset,
            };

            let left_margin = offset as f32 * WIDGET_SPACING_PER_OFFSET;
            if child_root.margin().x.start.as_pixels().map_or(true, |px| px != left_margin) {
                child_root.set_margin_left(left_margin);
            }
        }

        let entry = TreeEntry { node: child_node, index: insertion_index };
        self.new_nodes.insert(widget_id, entry);
        Child { info, root_object: child_root }
    }

    /// Access a tree node widget at given span ID that were created during this build process. Only
    /// widgets that were built before calling this method will be returned.
    pub fn iter_built_widgets_of_span(
        &self,
        span_id: StableSpanIdentity,
    ) -> impl DoubleEndedIterator<Item = &TreeNode> + '_ {
        let last_index = self.pointer_usage.get(&span_id).map_or(0, |usage| usage.next_index);
        (0..last_index).filter_map(move |index| {
            let id = WidgetIdentity { main: span_id, index };
            self.new_nodes.get(&id).map(|e| &e.node)
        })
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
    /// The node info used during building of the child widget. The parent might use it to
    /// associate internal state with any particular child. When a new child is inserted between
    /// two existing children, their identities will be maintained.
    pub info:        NodeInfo,
    /// The root object of the widget. In order to make the widget visible, it must be added to the
    /// parent's view hierarchy. Every time a widget is [`configure`d], its root object may change.
    /// The parent must not assume ownership over a root object of a removed child. The widget
    /// [`Tree`] is allowed to reuse any widgets and insert them into different branches.
    ///
    /// [`configure`d]: SpanWidget::configure
    #[deref]
    pub root_object: display::object::Instance,
}



// ===================
// === SceneShared ===
// ===================

/// A set of cached objects shared between all widget trees within a given scene. It is currently
/// used to cache all style objects of common types created from within widget instances. That way
/// we can avoid a cost of creating dedicated style FRP nodes for each widget instance.
#[derive(Debug)]
struct SceneShared {
    /// An `StyleWatchFrp` instance used by all style objects inside all widget trees within the
    /// scene. Using the same instance for all style objects allows it to cache all internal FRP
    /// nodes, which would otherwise be created for each style object separately.
    cached_styles_frp: StyleWatchFrp,
    /// FRP Network shared by all cached style FRP nodes.
    ///
    /// NOTE: Any FRP nodes created within this network will have a lifetime of the whole scene. It
    /// is very important to not create any per-widget-instance nodes in it, as that would lead to
    /// a memory leak. Use this network only for shared, intentionally long-lived nodes.
    network:           frp::Network,
    /// All shared state used by widgets, including shared style objects. Indexable by type.
    objects:           RefCell<HashMap<TypeId, Box<dyn Any>>>,
}

/// A scene extension holding [`SceneShared`] object.
#[derive(Debug, Clone, CloneRef)]
struct SceneSharedExtension {
    shared: Rc<SceneShared>,
}

impl scene::Extension for SceneSharedExtension {
    fn init(scene: &Scene) -> Self {
        let shared = SceneShared {
            cached_styles_frp: StyleWatchFrp::new(&scene.style_sheet),
            network:           frp::Network::new("widget::SceneShared"),
            objects:           default(),
        };
        Self { shared: Rc::new(shared) }
    }
}

impl SceneShared {
    /// Retrieve a shared style object that implements `FromTheme`. All calls using the same type
    /// will receive a `clone_ref` of the same object instance.
    ///
    /// NOTE: When an existing style instance is returned, its `update` stream is not guaranteed
    /// to emit an event in next microtask. The caller must not rely on it.
    fn cached_style<T: FromTheme>(&self) -> frp::Sampler<T> {
        self.get_or_insert_with(|| T::from_theme(&self.network, &self.cached_styles_frp))
            .clone_ref()
    }

    fn get_or_insert_with<T: Any>(&self, with: impl FnOnce() -> T) -> RefMut<T> {
        RefMut::map(self.objects.borrow_mut(), |objects| {
            objects
                .entry(TypeId::of::<T>())
                .or_insert_with(|| Box::new(with()))
                .downcast_mut::<T>()
                .expect("TypeId key should always be matching stored object type.")
        })
    }
}
