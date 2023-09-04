//! Definition of the node input port component.

use crate::prelude::*;
use enso_text::index::*;
use ensogl::display::shape::*;
use ensogl::display::traits::*;

use crate::node;
use crate::node::input::widget;
use crate::node::input::widget::OverrideKey;
use crate::view;
use crate::CallWidgetsConfig;
use crate::GraphLayers;
use crate::Type;

use enso_frp as frp;
use enso_frp;
use ensogl::application::Application;
use ensogl::data::color;
use ensogl::display;
use ensogl::gui::cursor;
use ensogl_component::text;
use ensogl_component::text::buffer::selection::Selection;
use ensogl_component::text::FromInContextSnapped;
use ensogl_hardcoded_theme as theme;



// =================
// === Constants ===
// =================

/// An offset from the port area position to the text position.
pub const TEXT_OFFSET: f32 = 10.0;

/// Total height of the node input area.
pub const NODE_HEIGHT: f32 = 18.0;

/// Text size used for input area text.
pub const TEXT_SIZE: f32 = 11.5;



// ================
// === SpanTree ===
// ================

pub use span_tree::Crumb;
pub use span_tree::Crumbs;
pub use span_tree::PortId;
pub use span_tree::SpanTree;



// ==================
// === Expression ===
// ==================

/// Specialized version of `node::Expression`.
#[derive(Clone, Default)]
#[allow(missing_docs)]
pub struct Expression {
    pub code:       ImString,
    pub span_tree:  SpanTree,
    /// The map containing either first or "this" argument for each unique `call_id` in the tree.
    pub target_map: HashMap<ast::Id, ast::Id>,
    /// Map from Port ID to its span-tree node crumbs.
    pub ports_map:  HashMap<PortId, Crumbs>,
}

impl Deref for Expression {
    type Target = SpanTree;
    fn deref(&self) -> &Self::Target {
        &self.span_tree
    }
}

impl DerefMut for Expression {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.span_tree
    }
}

impl Debug for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Expression({})", self.code)
    }
}

impl Expression {
    fn port_node(&self, port: PortId) -> Option<span_tree::node::Ref> {
        let crumbs = self.ports_map.get(&port)?;
        self.span_tree.get_node(crumbs).ok()
    }
}


// === Pretty printing debug adapter ===

/// Debug adapter used for pretty-printing the `Expression` span tree. Can be used to print the
/// expression with detailed span-tree information. This printer is normally too verbose to be
/// a default `Debug` implementation of `Expression`, so it is hidden behind a separate adapter
/// and can be chosen by calling `expression.tree_pretty_printer()`.
pub struct ExpressionTreePrettyPrint<'a> {
    expression: &'a Expression,
}

impl<'a> Debug for ExpressionTreePrettyPrint<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> fmt::Result {
        let printed = self.expression.span_tree.debug_print(&self.expression.code);
        f.write_str(&printed)
    }
}

impl Expression {
    /// Wrap the expression into a pretty-printing adapter that implements `Debug` and prints
    /// detailed span-tree information. See [`SpanTree::debug_print`] method for more details.
    ///
    /// Note that this printer emits multi-line output. In order for those lines to be properly
    /// aligned, it should be always printed on a new line.
    pub fn tree_pretty_printer(&self) -> ExpressionTreePrettyPrint<'_> {
        ExpressionTreePrettyPrint { expression: self }
    }
}


// === Conversions ===

impl From<node::Expression> for Expression {
    #[profile(Debug)]
    fn from(t: node::Expression) -> Self {
        let span_tree = t.input_span_tree;
        let mut target_map: HashMap<ast::Id, ast::Id> = HashMap::new();
        let mut ports_map: HashMap<PortId, Crumbs> = HashMap::new();
        span_tree.root_ref().dfs(|node| {
            if let Some((ast_id, call_id)) = node.ast_id.zip(node.kind.call_id()) {
                let entry = target_map.entry(call_id);
                let is_target_argument = node.kind.is_this();
                entry
                    .and_modify(|target_id| {
                        if is_target_argument {
                            *target_id = ast_id;
                        }
                    })
                    .or_insert(ast_id);
            }
            if let Some(port_id) = node.port_id {
                ports_map.insert(port_id, node.crumbs.clone());
            }
        });
        Self { code: t.code, span_tree, target_map, ports_map }
    }
}



// =============
// === Model ===
// =============

/// Internal model of the port area.
#[derive(Debug, display::Object)]
pub struct Model {
    layers:          GraphLayers,
    display_object:  display::object::Instance,
    edit_mode_label: text::Text,
    expression:      RefCell<Expression>,
    styles:          StyleWatch,
    styles_frp:      StyleWatchFrp,
    widget_tree:     widget::Tree,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
struct WidgetBind {
    call_id:       ast::Id,
    argument_name: String,
}

impl Model {
    /// Constructor.
    #[profile(Debug)]
    pub fn new(app: &Application, layers: &GraphLayers) -> Self {
        let display_object = display::object::Instance::new_named("input");

        let edit_mode_label = app.new_view::<text::Text>();
        let expression = default();
        let styles = StyleWatch::new(&app.display.default_scene.style_sheet);
        let styles_frp = StyleWatchFrp::new(&app.display.default_scene.style_sheet);
        display_object.add_child(&edit_mode_label);
        let widget_tree = widget::Tree::new(app);
        let layers = layers.clone_ref();
        Self {
            layers,
            display_object,
            edit_mode_label,
            expression,
            styles,
            styles_frp,
            widget_tree,
        }
        .init(app)
    }

    /// React to edit mode change. Shows and hides appropriate child views according to current
    /// mode. Sets cursor position when entering edit mode.
    pub fn set_edit_mode(&self, edit_mode_active: bool) {
        if edit_mode_active {
            let expression = self.expression.borrow();
            self.edit_mode_label.set_content(expression.code.clone());
            self.display_object.remove_child(&self.widget_tree);
            self.display_object.add_child(&self.edit_mode_label);

            // A workaround to fix the cursor position calculation when clicking into the node:
            // Since the object is not updated immediately after `add_child`, we need to force
            // update the node layout for label to be able to calculate cursor position properly.
            self.display_object.update(&scene());
            self.edit_mode_label.set_cursor_at_mouse_position();
            // [`ensogl_text`] has not been ported to the new focus API yet.
            self.edit_mode_label.deprecated_focus();
        } else {
            self.display_object.remove_child(&self.edit_mode_label);
            self.display_object.add_child(&self.widget_tree);
            self.edit_mode_label.set_content("");
        }
        self.edit_mode_label.deprecated_set_focus(edit_mode_active);
    }

    #[profile(Debug)]
    fn init(self, app: &Application) -> Self {
        let text_color = self.styles.get_color(theme::graph_editor::node::text);
        let text_cursor_color: color::Lch = text_color.into();

        self.edit_mode_label.set_single_line_mode(true);
        app.commands.set_command_enabled(
            &self.edit_mode_label,
            "add_cursor_at_mouse_position",
            false,
        );
        self.edit_mode_label.set_property_default(text_color);
        self.edit_mode_label.set_selection_color(text_cursor_color);
        self.edit_mode_label.set_property_default(text::Size(TEXT_SIZE));
        self.edit_mode_label.remove_all_cursors();

        let widgets_origin = Vector2(0.0, -NODE_HEIGHT / 2.0);
        let label_origin = Vector2(TEXT_OFFSET, TEXT_SIZE / 2.0);
        self.widget_tree.set_xy(widgets_origin);
        self.edit_mode_label.set_xy(label_origin);
        self.set_edit_mode(false);
        self
    }

    fn set_connections(&self, map: &HashMap<PortId, color::Lcha>) {
        self.widget_tree.set_connections(map);
    }

    fn set_expression_usage_type(&self, id: ast::Id, usage_type: Option<Type>) {
        self.widget_tree.set_usage_type(id, usage_type);
    }

    fn body_hover_pointer_style(&self, hovered: &bool) -> cursor::Style {
        hovered.then(cursor::Style::cursor).unwrap_or_default()
    }

    fn port_hover_pointer_style(&self, hovered: &Switch<PortId>) -> Option<cursor::Style> {
        let display_object = self.widget_tree.get_port_display_object(hovered.into_on()?)?;
        let pad_x = node::input::port::PORT_PADDING_X * 2.0;
        let min_y = node::input::port::BASE_PORT_HEIGHT;
        let computed_size = display_object.computed_size();
        let size = Vector2(computed_size.x + pad_x, computed_size.y.max(min_y));
        Some(cursor::Style::new_highlight(display_object, size, min_y / 2.0))
    }

    fn port_type(&self, port: PortId) -> Option<Type> {
        let expression = self.expression.borrow();
        Some(Type::from(expression.port_node(port)?.kind.tp()?))
    }

    /// Configure widgets associated with single Enso call expression, overriding default widgets
    /// generated from span tree. The provided widget configuration is merged with configurations
    /// already present in the widget tree. Setting a widget configuration to `None` will remove
    /// an override, and a default widget will be used.
    fn apply_widget_configuration(&self, config: &CallWidgetsConfig) {
        let CallWidgetsConfig { call_id, definitions } = config;
        for definition in definitions.iter() {
            let argument_name = definition.argument_name.clone().into();
            let override_key = OverrideKey { call_id: *call_id, argument_name };
            self.widget_tree.set_config_override(override_key, definition.config.clone());
        }
    }

    /// If the widget tree was marked as dirty since its last update, rebuild it.
    fn rebuild_widget_tree_if_dirty(&self) {
        let expr = self.expression.borrow();
        self.widget_tree.rebuild_tree_if_dirty(
            &expr.span_tree,
            &expr.code,
            &self.layers,
            &self.styles_frp,
        );
    }

    /// Scan node expressions for all known method calls, for which the language server can provide
    /// widget configuration overrides. Emit a request for each such detected call, allowing the
    /// controller to request the overrides and provide them.
    ///
    /// See also: [`controller::graph::widget`] module of `enso-gui` crate.
    #[profile(Debug)]
    fn request_widget_config_overrides(&self, expression: &Expression, area_frp: &api::Private) {
        for (call_id, target_id) in expression.target_map.iter() {
            area_frp.output.requested_widgets.emit((*call_id, *target_id));
        }
    }

    /// Set a displayed expression, updating the input ports. `is_editing` indicates whether the
    /// expression is being edited by the user.
    #[profile(Debug)]
    fn set_expression(&self, new_expression: impl Into<node::Expression>, area_frp: &api::Private) {
        let new_expression = Expression::from(new_expression.into());
        debug!("Set expression: \n{:?}", new_expression.tree_pretty_printer());

        // Request widget configuration before rebuilding the widget tree, so that in case there are
        // any widget responses already cached, they can be immediately used during the first build.
        // Otherwise the tree would often be rebuilt twice immediately after setting the expression.
        self.request_widget_config_overrides(&new_expression, area_frp);
        self.widget_tree.rebuild_tree(
            &new_expression.span_tree,
            &new_expression.code,
            &self.layers,
            &self.styles_frp,
        );

        *self.expression.borrow_mut() = new_expression;
    }

    /// Get hover shapes for all input ports of a node. Mainly used in tests to manually dispatch
    /// mouse events.
    pub fn port_hover_shapes(&self) -> Vec<Rectangle> {
        self.widget_tree.port_hover_shapes()
    }
}



// ===========
// === FRP ===
// ===========

ensogl::define_endpoints_2! {
    Input {
        /// Set the node expression.
        set_expression (node::Expression),

        /// Edit the node expression: if the node is currently edited, the given range will be
        /// replaced with the string, and the text cursor will be placed after the inserted string.
        ///
        /// If the node is **not** edited, nothing changes.
        edit_expression (text::Range<Byte>, ImString),

        /// Set the mode in which the cursor will indicate that editing of the node is possible.
        set_edit_ready_mode (bool),

        /// Enable or disable node editing.
        set_editing (bool),

        /// Set or unset hover over the node. Port area is unable to determine hover by itself, as
        /// the hover may sometimes happen on the node background and the area still needs to be
        /// notified about it, for example in order to display the right cursor style in edit ready
        /// mode.
        set_hover (bool),

        /// Disable the node (aka "skip mode").
        set_disabled (bool),

        /// Set the node pending (awaiting execution completion).
        set_pending (bool),

        /// Set read-only mode for input ports.
        set_read_only (bool),

        /// Provide a map of edge colors for all connected ports.
        set_connections (HashMap<PortId, color::Lcha>),

        /// Update widget configuration for widgets already present in this input area.
        update_widgets   (CallWidgetsConfig),

        /// Enable / disable port hovering
        set_ports_active (bool),

        set_view_mode        (view::Mode),

        /// Set the expression USAGE type. This is not the definition type, which can be set with
        /// `set_expression` instead. In case the usage type is set to None, ports still may be
        /// colored if the definition type was present.
        set_expression_usage_type (ast::Id,Option<Type>),

        /// Set the primary (background) and secondary (port) node colors.
        set_node_colors ((color::Lcha, color::Lcha)),
    }

    Output {
        pointer_style       (cursor::Style),
        width               (f32),
        /// Changes done when nodes is in edit mode.
        expression_edit     (ImString, Vec<Selection<Byte>>),

        editing             (bool),
        ports_visible       (bool),
        body_hover          (bool),
        on_port_press       (PortId),
        on_port_hover       (Switch<PortId>),
        on_port_code_update (Crumbs,ImString),
        view_mode           (view::Mode),
        /// A set of widgets attached to a method requests their definitions to be queried from an
        /// external source. The tuple contains the ID of the call expression the widget is attached
        /// to, and the ID of that call's target expression (`self` or first argument).
        requested_widgets    (ast::Id, ast::Id),
        request_import       (ImString),
        /// A connected port within the node has been moved. Some edges might need to be updated.
        input_edges_need_refresh (),
        /// The widget tree has been rebuilt. Some ports might have been added or removed.
        widget_tree_rebuilt (),
    }
}



// ============
// === Area ===
// ============

/// Input ports area.
///
/// ## Origin
/// Please note that the origin of the node is on its left side, centered vertically. To learn more
/// about this design decision, please read the docs for the [`node::Node`].
#[derive(Clone, Deref, CloneRef, Debug, display::Object)]
pub struct Area {
    #[allow(missing_docs)]
    #[deref]
    pub frp:          Frp,
    #[display_object]
    pub(crate) model: Rc<Model>,
}

impl Area {
    /// Constructor.
    #[profile(Debug)]
    pub fn new(app: &Application, layers: &GraphLayers) -> Self {
        let model = Rc::new(Model::new(app, layers));
        let frp = Frp::new();
        let network = &frp.network;

        frp::extend! { network
            init <- source::<()>();
            set_editing <- all(frp.set_editing, init)._0();

            // === Body Hover ===
            // This is meant to be on top of FRP network. Read more about `Node` docs to
            // learn more about the architecture and the importance of the hover
            // functionality.
            frp.private.output.on_port_hover <+ model.widget_tree.on_port_hover;
            frp.private.output.on_port_press <+ model.widget_tree.on_port_press;
            port_hover <- frp.on_port_hover.map(|t| t.is_on());
            frp.private.output.body_hover <+ frp.set_hover || port_hover;


            // === Cursor setup ===

            eval set_editing((is_editing) model.set_edit_mode(*is_editing));


            // === Show / Hide Phantom Ports ===

            let ports_active = &frp.set_ports_active;
            edit_or_ready <- frp.set_edit_ready_mode || set_editing;
            reacts_to_hover <- all_with(&edit_or_ready, ports_active, |e, a| *e && !a);
            port_vis <- all_with(&set_editing, ports_active, |e, a| !e && *a);
            frp.private.output.ports_visible <+ port_vis;
            frp.private.output.editing <+ set_editing;
            model.widget_tree.set_ports_visible <+ frp.ports_visible;
            model.widget_tree.set_edit_ready_mode <+ frp.set_edit_ready_mode;
            refresh_edges <- model.widget_tree.connected_port_updated.debounce();
            frp.private.output.input_edges_need_refresh <+ refresh_edges;

            // === Label Hover ===

            label_hovered <- reacts_to_hover && frp.output.body_hover;
            model.edit_mode_label.set_hover <+ label_hovered && set_editing;
            hovered_body_pointer <- label_hovered.map(f!((t) model.body_hover_pointer_style(t)));

            // === Port Hover ===

            hovered_port_pointer <- model.widget_tree.on_port_hover.map(
                f!((t) model.port_hover_pointer_style(t).unwrap_or_default())
            );
            pointer_style <- all[
                hovered_body_pointer,
                model.widget_tree.pointer_style,
                hovered_port_pointer
            ].fold();
            frp.private.output.pointer_style <+ pointer_style;

            // === Properties ===
            let widget_tree_object = model.widget_tree.display_object();
            widget_tree_width <- widget_tree_object.on_resized.map(|size| size.x());
            edit_label_width <- all(model.edit_mode_label.width, init)._0();
            padded_edit_label_width <- edit_label_width.map(|t| t + 2.0 * TEXT_OFFSET);
            frp.private.output.width <+ set_editing.switch(
                &widget_tree_width,
                &padded_edit_label_width
            );

            // === Expression ===

            let frp_endpoints = &frp.private;
            eval frp.set_expression([frp_endpoints, model](expr) model.set_expression(expr, &frp_endpoints));
            legit_edit <- frp.input.edit_expression.gate(&set_editing);
            model.edit_mode_label.select <+ legit_edit.map(|(range, _)| (range.start.into(), range.end.into()));
            model.edit_mode_label.insert <+ legit_edit._1();
            expression_edited <- model.edit_mode_label.content.gate(&set_editing);
            selections_edited <- model.edit_mode_label.selections.gate(&set_editing);
            frp.private.output.expression_edit <+ selections_edited.gate(&set_editing).map2(
                &model.edit_mode_label.content,
                f!([model](selection, full_content) {
                    let full_content = full_content.into();
                    let to_byte = |loc| text::Byte::from_in_context_snapped(&model.edit_mode_label, loc);
                    let selections = selection.iter().map(|sel| sel.map(to_byte)).collect_vec();
                    (full_content, selections)
                })
            );
            frp.private.output.on_port_code_update <+ expression_edited.map(|e| {
                // Treat edit mode update as a code modification at the span tree root.
                (default(), e.into())
            });

            widget_code_update <- model.widget_tree.value_changed.map(|(crumbs, value)| {
                let expression = value.clone().unwrap_or_default();
                (crumbs.clone(), expression)
            });

            frp.private.output.on_port_code_update <+ widget_code_update;
            frp.private.output.request_import <+ model.widget_tree.request_import;

            // === Widgets ===

            eval frp.update_widgets((a) model.apply_widget_configuration(a));
            eval frp.set_connections((conn) model.set_connections(conn));
            eval frp.set_expression_usage_type(((id,tp)) model.set_expression_usage_type(*id,tp.clone()));
            eval frp.set_disabled ((disabled) model.widget_tree.set_disabled(*disabled));
            eval frp.set_pending ((pending) model.widget_tree.set_pending(*pending));
            eval_ model.widget_tree.rebuild_required(model.rebuild_widget_tree_if_dirty());
            frp.private.output.widget_tree_rebuilt <+ model.widget_tree.on_rebuild_finished;


            // === View Mode ===

            frp.private.output.view_mode <+ frp.set_view_mode;
            model.widget_tree.set_read_only <+ frp.set_read_only;
            model.widget_tree.set_view_mode <+ frp.set_view_mode;
            model.widget_tree.node_base_color <+ frp.set_node_colors._0();
            model.widget_tree.node_port_color <+ frp.set_node_colors._1();
        }

        init.emit(());

        Self { frp, model }
    }


    /// Check if the node currently contains a port of given ID.
    pub fn has_port(&self, port: PortId) -> bool {
        self.model.widget_tree.get_port_display_object(port).is_some()
    }

    /// An offset from node position to a specific port.
    pub fn port_offset(&self, port: PortId) -> Vector2<f32> {
        let object = self.model.widget_tree.get_port_display_object(port);
        let initial_position = Vector2(TEXT_OFFSET, NODE_HEIGHT / 2.0);
        object.map_or(initial_position, |object| {
            let pos = object.global_position();
            let node_pos = self.model.display_object.global_position();
            let size = object.computed_size();
            pos.xy() - node_pos.xy() + size * 0.5
        })
    }

    /// An the computed layout size of a specific port.
    pub fn port_size(&self, port: PortId) -> Vector2<f32> {
        self.model.port_hover_pointer_style(&Switch::On(port)).map_or_default(|s| s.get_size())
    }

    /// A type of the specified port.
    pub fn port_type(&self, port: PortId) -> Option<Type> {
        self.model.port_type(port)
    }

    /// Get the span-tree crumbs for the specified port.
    pub fn port_crumbs(&self, port: PortId) -> Option<Crumbs> {
        self.model.expression.borrow().ports_map.get(&port).cloned()
    }
}
