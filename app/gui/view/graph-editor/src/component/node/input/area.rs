//! Definition of the node input port component.

use crate::prelude::*;
use enso_text::index::*;
use ensogl::display::shape::*;
use ensogl::display::traits::*;

use crate::component::type_coloring;
use crate::node;
use crate::node::input::widget;
use crate::node::input::widget::MetadataPointer;
use crate::node::profiling;
use crate::view;
use crate::Type;
use crate::WidgetUpdates;

use enso_frp as frp;
use enso_frp;
use ensogl::application::Application;
use ensogl::data::color;
use ensogl::display;
use ensogl::gui::cursor;
use ensogl::Animation;
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

/// Width of a single glyph
// TODO: avoid using hardcoded value. See https://www.pivotaltracker.com/story/show/183567623.
pub const GLYPH_WIDTH: f32 = 7.224_609_4;

/// Enable visual port debug mode and additional port creation logging.
pub const DEBUG: bool = false;

/// Visual port offset for debugging purposes. Applied hierarchically. Applied only when `DEBUG` is
/// set to `true`.
pub const DEBUG_PORT_OFFSET: f32 = 5.0;

/// Text size used for input area text.
pub const TEXT_SIZE: f32 = 12.0;



// ================
// === SpanTree ===
// ================

pub use span_tree::Crumb;
pub use span_tree::Crumbs;
pub use span_tree::SpanTree;



// ==================
// === Expression ===
// ==================

/// Specialized version of `node::Expression`.
#[derive(Clone, Default)]
#[allow(missing_docs)]
pub struct InputExpression {
    pub code:      ImString,
    pub span_tree: SpanTree,
}

impl Deref for InputExpression {
    type Target = SpanTree;
    fn deref(&self) -> &Self::Target {
        &self.span_tree
    }
}

impl DerefMut for InputExpression {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.span_tree
    }
}

impl Debug for InputExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Expression({})", self.code)
    }
}


// === Pretty printing debug adapter ===

/// Debug adapter used for pretty-printing the `Expression` span tree. Can be used to print the
/// expression with detailed span-tree information. This printer is normally too verbose to be
/// a default `Debug` implementation of `Expression`, so it is hidden behind a separate adapter
/// and can be chosen by calling `expression.tree_pretty_printer()`.
pub struct ExpressionTreePrettyPrint<'a> {
    expression: &'a InputExpression,
}

impl<'a> Debug for ExpressionTreePrettyPrint<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> fmt::Result {
        let printed = self.expression.span_tree.debug_print(&self.expression.code);
        f.write_str(&printed)
    }
}

impl InputExpression {
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

impl From<node::Expression> for InputExpression {
    #[profile(Debug)]
    fn from(t: node::Expression) -> Self {
        Self { code: t.code, span_tree: t.input_span_tree }
    }
}



// =============
// === Model ===
// =============

/// Internal model of the port area.
#[derive(Debug)]
pub struct Model {
    app:             Application,
    display_object:  display::object::Instance,
    edit_mode_label: text::Text,
    expression:      RefCell<InputExpression>,
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
    pub fn new(app: &Application) -> Self {
        let app = app.clone_ref();
        let display_object = display::object::Instance::new();

        let edit_mode_label = app.new_view::<text::Text>();
        let expression = default();
        let styles = StyleWatch::new(&app.display.default_scene.style_sheet);
        let styles_frp = StyleWatchFrp::new(&app.display.default_scene.style_sheet);
        let widget_tree = widget::Tree::new(&app);
        Self { app, display_object, edit_mode_label, expression, styles, styles_frp, widget_tree }
            .init()
    }

    /// React to edit mode change. Shows and hides appropriate child views according to current
    /// mode. Sets cursor position when entering edit mode.
    pub fn set_edit_mode(&self, edit_mode_active: bool) {
        if edit_mode_active {
            let expression = self.expression.borrow();
            self.edit_mode_label.set_content(expression.code.clone());
            self.display_object.remove_child(&self.widget_tree);
            self.display_object.add_child(&self.edit_mode_label);
            self.edit_mode_label.set_cursor_at_mouse_position();
        } else {
            self.display_object.remove_child(&self.edit_mode_label);
            self.display_object.add_child(&self.widget_tree);
            self.edit_mode_label.set_content("");
        }
        self.edit_mode_label.deprecated_set_focus(edit_mode_active);
    }

    #[profile(Debug)]
    fn init(self) -> Self {
        // TODO: Depth sorting of labels to in front of the mouse pointer. Temporary solution.
        //   It needs to be more flexible once we have proper depth management.
        //   See https://www.pivotaltracker.com/story/show/183567632.
        let scene = &self.app.display.default_scene;
        self.set_label_layer(&scene.layers.label);

        let text_color = self.styles.get_color(theme::graph_editor::node::text);

        self.edit_mode_label.set_single_line_mode(true);
        self.edit_mode_label.disable_command("cursor_move_up");
        self.edit_mode_label.disable_command("cursor_move_down");
        self.edit_mode_label.disable_command("add_cursor_at_mouse_position");
        self.edit_mode_label.set_property_default(text_color);
        self.edit_mode_label.set_property_default(text::Size(TEXT_SIZE));
        self.edit_mode_label.remove_all_cursors();

        let widgets_origin = Vector2(0.0, -NODE_HEIGHT / 2.0);
        let label_origin = Vector2(TEXT_OFFSET, TEXT_SIZE / 2.0);
        self.widget_tree.set_xy(widgets_origin);
        self.edit_mode_label.set_xy(label_origin);
        self.set_edit_mode(false);

        self
    }

    fn set_label_layer(&self, layer: &display::scene::Layer) {
        self.edit_mode_label.add_to_scene_layer(layer);
    }

    /// Set connection status of the given port.
    fn set_connected(&self, crumbs: &Crumbs, status: node::ConnectionStatus) {
        let expr = self.expression.borrow();
        let port = expr.span_tree.get_node(crumbs).ok();
        port.map(|port| self.widget_tree.set_connected(&port, status));
    }

    /// Set usage type of the given port.
    fn set_expression_usage_type(&self, id: ast::Id, usage_type: Option<Type>) {
        self.widget_tree.set_usage_type(id, usage_type);
    }

    fn hover_pointer_style(&self, hovered: &Switch<Crumbs>) -> Option<cursor::Style> {
        let crumbs = hovered.on()?;
        let expr = self.expression.borrow();
        let port = expr.span_tree.get_node(crumbs).ok()?;
        let display_object = self.widget_tree.get_widget_display_object(&port)?;
        let tp = port.tp().map(|t| Type(t.into()));
        let color = tp.as_ref().map(|tp| type_coloring::compute(tp, &self.styles));
        let pad_x = node::input::port::PORT_PADDING_X * 2.0;
        let min_y = node::input::port::BASE_PORT_HEIGHT;
        let computed_size = display_object.computed_size();
        let size = Vector2(computed_size.x + pad_x, computed_size.y.max(min_y));
        let radius = size.y / 2.0;
        Some(cursor::Style::new_highlight(display_object, size, radius, color))
    }

    /// Apply widget updates to widgets in this input area.
    fn apply_widget_updates(&self, updates: &WidgetUpdates) {
        let WidgetUpdates { call_id, updates } = updates;
        for update in updates.iter() {
            let argument_name = update.argument_name.clone().into();
            let meta_pointer = MetadataPointer { call_id: *call_id, argument_name };
            self.widget_tree.set_metadata(meta_pointer, update.meta.clone());
        }
    }

    /// If the widget tree was marked as dirty since its last update, rebuild it.
    fn rebuild_widget_tree_if_dirty(&self) {
        let expr = self.expression.borrow();
        self.widget_tree.rebuild_tree_if_dirty(&expr.span_tree, &expr.code, &self.styles);
    }

    /// Request widgets metadata for all method calls within the expression.
    #[profile(Debug)]
    fn request_widgets_metadata(&self, expression: &InputExpression, area_frp: &FrpEndpoints) {
        let call_info = CallInfoMap::scan_expression(&expression.span_tree);
        for (call_id, info) in call_info.iter() {
            if let Some(target_id) = info.target_id {
                area_frp.source.requested_widgets.emit((*call_id, target_id));
            }
        }
    }

    /// Set a displayed expression, updating the input ports. `is_editing` indicates whether the
    /// expression is being edited by the user.
    #[profile(Debug)]
    fn set_expression(&self, new_expression: impl Into<node::Expression>, area_frp: &FrpEndpoints) {
        let new_expression = InputExpression::from(new_expression.into());
        debug!("set expression: \n{:?}", new_expression.tree_pretty_printer());

        self.widget_tree.rebuild_tree(
            &new_expression.span_tree,
            &new_expression.code,
            &self.styles,
        );

        // TODO streams to handle:
        // pointer_style        (cursor::Style), <- handle edit mode cursor change
        // view_mode            (view::Mode), <- frp into widgets to change label color

        self.request_widgets_metadata(&new_expression, area_frp);
        *self.expression.borrow_mut() = new_expression;
    }
}



// ===========
// === FRP ===
// ===========

ensogl::define_endpoints! {
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

        /// Set read-only mode for input ports.
        set_read_only (bool),

        /// Set the connection status of the port indicated by the breadcrumbs. For connected ports,
        /// contains the color of connected edge.
        set_connected (Crumbs, node::ConnectionStatus),

        /// Update widget metadata for widgets already present in this input area.
        update_widgets   (WidgetUpdates),

        /// Enable / disable port hovering. The optional type indicates the type of the active edge
        /// if any. It is used to highlight ports if they are missing type information or if their
        /// types are polymorphic.
        set_ports_active (bool,Option<Type>),

        set_view_mode        (view::Mode),
        set_profiling_status (profiling::Status),

        /// Set the expression USAGE type. This is not the definition type, which can be set with
        /// `set_expression` instead. In case the usage type is set to None, ports still may be
        /// colored if the definition type was present.
        set_expression_usage_type (ast::Id,Option<Type>),
    }

    Output {
        pointer_style       (cursor::Style),
        width               (f32),
        expression_edit     (ImString, Vec<Selection<Byte>>),

        editing             (bool),
        ports_visible       (bool),
        body_hover          (bool),
        on_port_press       (Crumbs),
        on_port_hover       (Switch<Crumbs>),
        on_port_code_update (Crumbs,ImString),
        on_background_press (),
        view_mode           (view::Mode),
        /// A set of widgets attached to a method requires metadata to be queried. The tuple
        /// contains the ID of the call expression the widget is attached to, and the ID of that
        /// call's target expression (`self` or first argument).
        requested_widgets    (ast::Id, ast::Id),
        request_import       (ImString),
        /// A connected port within the node has been moved. Some edges might need to be updated.
        /// This event is already debounced.
        input_edges_need_refresh (),
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
#[derive(Clone, Deref, CloneRef, Debug)]
pub struct Area {
    #[allow(missing_docs)]
    #[deref]
    pub frp:          Frp,
    pub(crate) model: Rc<Model>,
}

impl display::Object for Area {
    fn display_object(&self) -> &display::object::Instance {
        &self.model.display_object
    }
}

impl Area {
    /// Constructor.
    #[profile(Debug)]
    pub fn new(app: &Application) -> Self {
        let model = Rc::new(Model::new(app));
        let frp = Frp::new();
        let network = &frp.network;
        let selection_color = Animation::new(network);

        frp::extend! { network
            init <- source::<()>();
            set_editing <- all(frp.set_editing, init)._0();

            // === Body Hover ===
            // This is meant to be on top of FRP network. Read more about `Node` docs to
            // learn more about the architecture and the importance of the hover
            // functionality.
            frp.output.source.on_port_hover <+ model.widget_tree.on_port_hover;
            frp.output.source.on_port_press <+ model.widget_tree.on_port_press;
            port_hover <- frp.on_port_hover.map(|t| t.is_on());
            frp.output.source.body_hover <+ frp.set_hover || port_hover;


            // === Cursor setup ===

            eval set_editing((is_editing) model.set_edit_mode(*is_editing));

            // Prevent text selection from being created right after entering edit mode. Otherwise,
            // a selection would be created between the current mouse position (the position at
            // which we clicked) and initial cursor position within edit mode label (the code
            // position corresponding to clicked port).
            start_editing <- set_editing.on_true();
            stop_editing  <- set_editing.on_false();
            start_editing_delayed     <- start_editing.debounce();
            reenable_selection_update <- any(&start_editing_delayed, &stop_editing);
            selection_update_enabled  <- bool(&start_editing, &reenable_selection_update);
            eval selection_update_enabled([model] (enabled) {
                let cmd_start = "start_newest_selection_end_follow_mouse";
                let cmd_stop = "stop_newest_selection_end_follow_mouse";
                model.edit_mode_label.set_command_enabled(cmd_start, *enabled);
                model.edit_mode_label.set_command_enabled(cmd_stop, *enabled);
            });


            // === Show / Hide Phantom Ports ===

            let ports_active = &frp.set_ports_active;
            edit_or_ready   <- frp.set_edit_ready_mode || set_editing;
            reacts_to_hover <- all_with(&edit_or_ready, ports_active, |e, (a, _)| *e && !a);
            port_vis        <- all_with(&edit_or_ready, ports_active, |e, (a, _)| !e && *a);
            frp.output.source.ports_visible     <+ port_vis;
            frp.output.source.editing           <+ set_editing;
            model.widget_tree.set_ports_visible <+ frp.ports_visible;
            refresh_edges <- model.widget_tree.connected_port_updated.debounce();
            frp.output.source.input_edges_need_refresh <+ refresh_edges;

            // === Label Hover ===

            label_hovered <- reacts_to_hover && frp.output.body_hover;
            model.edit_mode_label.set_hover <+ label_hovered && set_editing;

            // === Port Hover ===

            hovered_port_pointer <- model.widget_tree.on_port_hover.map(
                f!((t) model.hover_pointer_style(t).unwrap_or_default())
            );
            pointer_style <- all[
                model.widget_tree.pointer_style,
                hovered_port_pointer
            ].fold();
            frp.output.source.pointer_style <+ pointer_style;

            // === Properties ===
            let layout_refresh = ensogl::animation::on_before_animations();
            widget_tree_width <- layout_refresh.map(f!([model](_) {
                let instance = model.widget_tree.display_object();
                instance.computed_size().x()
            })).on_change();

            padded_edit_label_width <- model.edit_mode_label.width.map(|t| t + 2.0 * TEXT_OFFSET);

            frp.output.source.width <+ set_editing.switch(
                &widget_tree_width,
                &padded_edit_label_width
            );


            // === Expression ===

            let frp_endpoints = &frp.output;
            eval frp.set_expression([frp_endpoints, model](expr) model.set_expression(expr, &frp_endpoints));
            legit_edit <- frp.input.edit_expression.gate(&set_editing);
            model.edit_mode_label.select <+ legit_edit.map(|(range, _)| (range.start.into(), range.end.into()));
            model.edit_mode_label.insert <+ legit_edit._1();
            expression_changed_by_user <- model.edit_mode_label.content.gate(&set_editing);
            frp.output.source.expression_edit <+ model.edit_mode_label.selections.map2(
                &expression_changed_by_user,
                f!([model](selection, full_content) {
                    let full_content = full_content.into();
                    let to_byte = |loc| text::Byte::from_in_context_snapped(&model.edit_mode_label, loc);
                    let selections = selection.iter().map(|sel| sel.map(to_byte)).collect_vec();
                    (full_content, selections)
                })
            );
            frp.output.source.on_port_code_update <+ expression_changed_by_user.map(|e| {
                // Treat edit mode update as a code modification at the span tree root.
                (default(), e.into())
            });

            model.widget_tree.set_view_mode <+ frp.set_view_mode;
            widget_code_update <- model.widget_tree.value_changed.map(|(crumbs, value)| {
                let expression = value.clone().unwrap_or_default();
                (crumbs.clone(), expression)
            });

            frp.output.source.on_port_code_update <+ widget_code_update;
            frp.output.source.request_import <+ model.widget_tree.request_import;

            // === Widgets ===

            eval frp.update_widgets((a) model.apply_widget_updates(a));
            eval frp.set_connected(((crumbs,status)) model.set_connected(crumbs,*status));
            eval frp.set_expression_usage_type(((id,tp)) model.set_expression_usage_type(*id,tp.clone()));
            eval frp.set_disabled ((disabled) model.widget_tree.set_disabled(*disabled));
            widget_tree_invalidated <- any_(...);
            widget_tree_invalidated <+ frp.update_widgets;
            widget_tree_invalidated <+ frp.set_connected;
            widget_tree_invalidated <+ frp.set_disabled;
            widget_tree_invalidated <+ frp.set_expression_usage_type;
            rebuild_widget_tree <- widget_tree_invalidated.debounce();
            eval rebuild_widget_tree((_) model.rebuild_widget_tree_if_dirty());

            // === View Mode ===

            frp.output.source.view_mode <+ frp.set_view_mode;

            in_profiling_mode <- frp.view_mode.map(|m| m.is_profiling());
            finished          <- frp.set_profiling_status.map(|s| s.is_finished());
            profiled          <- in_profiling_mode && finished;

            use theme::code::syntax;
            let std_selection_color      = model.styles_frp.get_color(syntax::selection);
            let profiled_selection_color = model.styles_frp.get_color(syntax::profiling::selection);
            selection_color_rgba <- profiled.switch(&std_selection_color,&profiled_selection_color);

            selection_color.target          <+ selection_color_rgba.map(|c| color::Lcha::from(c));
            model.edit_mode_label.set_selection_color <+ selection_color.value.map(|c| color::Lch::from(c));
        }

        init.emit(());

        Self { frp, model }
    }

    /// An offset from node position to a specific port.
    pub fn port_offset(&self, crumbs: &[Crumb]) -> Option<Vector2<f32>> {
        let expr = self.model.expression.borrow();
        let node = expr.get_node(crumbs).ok()?;
        let instance = self.model.widget_tree.get_widget_display_object(&node)?;
        let pos = instance.global_position();
        let node_pos = self.model.display_object.global_position();
        let size = instance.computed_size();
        Some(pos.xy() - node_pos.xy() + size * 0.5)
    }

    /// A type of the specified port.
    pub fn port_type(&self, crumbs: &Crumbs) -> Option<Type> {
        let expression = self.model.expression.borrow();
        expression
            .span_tree
            .root_ref()
            .get_descendant(crumbs)
            .ok()
            .and_then(|t| t.tp().map(|t| Type(t.into())))
    }

    /// Set a scene layer for text rendering.
    pub fn set_label_layer(&self, layer: &display::scene::Layer) {
        self.model.set_label_layer(layer);
    }
}



/// ===================
/// === CallInfoMap ===
/// ===================

#[derive(Debug, Deref)]
struct CallInfoMap {
    /// The map from node's call_id to call information.
    call_info: HashMap<ast::Id, CallInfo>,
}

/// Information about the call expression, which are derived from the span tree.
#[derive(Debug, Default)]
struct CallInfo {
    /// The AST ID associated with `self` argument span of the call expression.
    target_id: Option<ast::Id>,
}

impl CallInfoMap {
    fn scan_expression(expression: &SpanTree) -> Self {
        let mut call_info: HashMap<ast::Id, CallInfo> = HashMap::new();
        expression.root_ref().dfs(|node| {
            if let Some(call_id) = node.kind.call_id() {
                let mut entry = call_info.entry(call_id).or_default();
                if entry.target_id.is_none() || node.kind.is_this() {
                    entry.target_id = node.ast_id;
                }
            }
        });

        Self { call_info }
    }
}
