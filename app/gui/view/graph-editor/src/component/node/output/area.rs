//! Implements the segmented output port area.

use crate::prelude::*;
use ensogl::display::traits::*;

use crate::component::node;
use crate::component::node::input;
use crate::component::node::output::port;
use crate::tooltip;
use crate::view;
use crate::Type;

use enso_config::ARGS;
use enso_frp as frp;
use enso_frp;
use ensogl::animation::hysteretic::HystereticAnimation;
use ensogl::application::Application;
use ensogl::data::color;
use ensogl::display;
use ensogl::display::shape::StyleWatch;
use ensogl::display::shape::StyleWatchFrp;
use ensogl_component::text;
use ensogl_hardcoded_theme as theme;
use span_tree;



// =================
// === Constants ===
// =================

const DEBUG: bool = false;
const HIDE_DELAY_DURATION_MS: f32 = 150.0;
const SHOW_DELAY_DURATION_MS: f32 = 150.0;



// ================
// === SpanTree ===
// ================

pub use span_tree::Crumb;
pub use span_tree::Crumbs;

/// Specialized `SpanTree` for the output ports model.
pub type SpanTree = span_tree::SpanTree<port::Model>;

/// Reference to port inside of a `SpanTree`.
pub type PortRef<'a> = span_tree::node::Ref<'a, port::Model>;

/// Mutable reference to port inside of a `SpanTree`.
pub type PortRefMut<'a> = span_tree::node::RefMut<'a, port::Model>;



// ==================
// === Expression ===
// ==================

/// Specialized version of `node::Expression`, containing the port information.
#[derive(Default)]
#[allow(missing_docs)]
pub struct Expression {
    pub code:            Option<String>,
    pub span_tree:       SpanTree,
    /// This field contains the type of the whole input expression. This is needed due to a bug in
    /// engine: https://github.com/enso-org/enso/issues/1038.
    pub whole_expr_type: Option<Type>,
    pub whole_expr_id:   Option<ast::Id>,
}

impl Expression {
    #[allow(missing_docs)] // FIXME[everyone] All pub functions should have docs.
    pub fn code(&self) -> String {
        self.code.clone().unwrap_or_default()
    }
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
        write!(f, "Expression({})", self.code.clone().unwrap_or_default())
    }
}


// === Conversions ===

impl From<node::Expression> for Expression {
    #[profile(Debug)]
    fn from(expr: node::Expression) -> Self {
        let code = expr.pattern.clone();
        let whole_expr_type = expr.input_span_tree.root.tp().map(|t| t.to_owned().into());
        let whole_expr_id = expr.whole_expression_id;
        let mut span_tree = expr.output_span_tree.map(|_| port::Model::default());
        span_tree.root_ref_mut().dfs_with_layer_data((), |node, ()| {
            let span = node.span();
            let port = node.payload_mut();
            port.index = span.start.into();
            port.length = span.size();
        });
        Expression { code, span_tree, whole_expr_type, whole_expr_id }
    }
}



// =============
// === Model ===
// =============

// FIXME: Update to `define_endpoints_2`. Note that `Model` must not own the `api::Private`,
// because `api::Private` owns the network, which contains (strong) references to the model.
ensogl::define_endpoints! {
    Input {
        set_size                  (Vector2),
        set_hover                 (bool),
        set_expression            (node::Expression),
        set_expression_visibility (bool),
        set_type_label_visibility (bool),
        set_view_mode             (view::Mode),

        /// Set the expression USAGE type. This is not the definition type, which can be set with
        /// `set_expression` instead. In case the usage type is set to None, ports still may be
        /// colored if the definition type was present.
        set_expression_usage_type (Crumbs,Option<Type>),
    }

    Output {
        on_port_press               (Crumbs),
        on_port_hover               (Switch<Crumbs>),
        on_port_type_change         (Crumbs,Option<Type>),
        port_size_multiplier        (f32),
        body_hover                  (bool),
        type_label_visibility       (bool),
        expression_label_visibility (bool),
        tooltip                     (tooltip::Style),
        view_mode                   (view::Mode),
        size                        (Vector2),
    }
}

/// Internal model of the port area.
#[derive(Debug)]
pub struct Model {
    app:            Application,
    display_object: display::object::Instance,
    ports:          display::object::Instance,
    label:          text::Text,
    expression:     RefCell<Expression>,
    id_crumbs_map:  RefCell<HashMap<ast::Id, Crumbs>>,
    port_count:     Cell<usize>,
    styles:         StyleWatch,
    styles_frp:     StyleWatchFrp,
    frp:            FrpEndpoints,
}

impl Model {
    /// Constructor.
    #[profile(Debug)]
    pub fn new(app: &Application, frp: &Frp) -> Self {
        let display_object = display::object::Instance::new();
        let ports = display::object::Instance::new();
        let app = app.clone_ref();
        let label = app.new_view::<text::Text>();
        let id_crumbs_map = default();
        let expression = default();
        let port_count = default();
        let styles = StyleWatch::new(&app.display.default_scene.style_sheet);
        let styles_frp = StyleWatchFrp::new(&app.display.default_scene.style_sheet);
        let frp = frp.output.clone_ref();
        display_object.add_child(&label);
        display_object.add_child(&ports);
        Self {
            app,
            display_object,
            ports,
            label,
            expression,
            id_crumbs_map,
            port_count,
            styles,
            styles_frp,
            frp,
        }
        .init()
    }

    #[profile(Debug)]
    fn init(self) -> Self {
        // FIXME[WD]: Depth sorting of labels to in front of the mouse pointer. Temporary solution.
        // It needs to be more flexible once we have proper depth management.
        let scene = &self.app.display.default_scene;
        scene.layers.main.remove(&self.label);
        self.label.add_to_scene_layer(&scene.layers.label);

        let text_color = self.styles.get_color(theme::graph_editor::node::text);
        self.label.set_single_line_mode(true);
        self.label.disable_command("cursor_move_up");
        self.label.disable_command("cursor_move_down");
        self.label.set_property_default(text_color);
        self.label.set_property_default(text::Size(input::area::TEXT_SIZE));
        self.label.remove_all_cursors();

        self.label.set_y(input::area::TEXT_SIZE / 2.0);

        self
    }

    /// Return a list of Node's output ports.
    pub fn ports(&self) -> Vec<port::Model> {
        let port_count = self.port_count.get();
        let mut ports = Vec::with_capacity(port_count);
        self.traverse_borrowed_expression(|is_a_port, node, _| {
            if is_a_port {
                ports.push(node.payload.clone());
            }
        });
        ports
    }

    #[profile(Debug)]
    fn set_label_layer(&self, layer: &display::scene::Layer) {
        self.label.add_to_scene_layer(layer);
    }

    #[profile(Debug)]
    fn set_label(&self, content: impl Into<String>) {
        let str = if ARGS.node_labels.unwrap_or(true) { content.into() } else { default() };
        self.label.set_content(str);
    }

    /// Update expression type for the particular `ast::Id`.
    #[profile(Debug)]
    fn set_expression_usage_type(&self, crumbs: &Crumbs, tp: &Option<Type>) {
        if let Ok(port) = self.expression.borrow().span_tree.root_ref().get_descendant(crumbs) {
            if let Some(frp) = &port.frp {
                frp.set_usage_type(tp)
            }
        }
    }

    /// Traverse all span tree nodes that are considered ports. In case of empty span tree, include
    /// its root as the port as well.
    #[profile(Debug)]
    fn traverse_borrowed_expression_mut(
        &self,
        mut f: impl FnMut(bool, &mut PortRefMut, &mut PortLayerBuilder),
    ) {
        let port_count = self.port_count.get();
        self.traverse_borrowed_expression_raw_mut(|is_a_port, node, builder| {
            let is_a_port = is_a_port || port_count == 0;
            f(is_a_port, node, builder)
        })
    }

    /// Traverse all span tree nodes that are considered ports. In case of empty span tree, include
    /// its root as the port as well.
    #[profile(Debug)]
    fn traverse_borrowed_expression(
        &self,
        mut f: impl FnMut(bool, &PortRef, &mut PortLayerBuilder),
    ) {
        let port_count = self.port_count.get();
        self.traverse_borrowed_expression_raw(|is_a_port, node, builder| {
            let is_a_port = is_a_port || port_count == 0;
            f(is_a_port, node, builder)
        })
    }

    /// Traverse all span tree nodes that are considered ports.
    #[profile(Debug)]
    fn traverse_borrowed_expression_raw_mut(
        &self,
        mut f: impl FnMut(bool, &mut PortRefMut, &mut PortLayerBuilder),
    ) {
        let mut expression = self.expression.borrow_mut();
        expression.root_ref_mut().dfs_with_layer_data(
            PortLayerBuilder::default(),
            |node, builder| {
                let is_leaf = node.children.is_empty();
                let is_this = node.is_this();
                let is_argument = node.is_argument();
                let is_a_port = (is_this || is_argument) && is_leaf;
                f(is_a_port, node, builder);
                builder.nested()
            },
        );
    }

    /// Traverse all span tree nodes that are considered ports.
    #[profile(Debug)]
    fn traverse_borrowed_expression_raw(
        &self,
        mut f: impl FnMut(bool, &PortRef, &mut PortLayerBuilder),
    ) {
        let expression = self.expression.borrow();
        expression.root_ref().dfs_with_layer_data(PortLayerBuilder::default(), |node, builder| {
            let is_leaf = node.children.is_empty();
            let is_this = node.is_this();
            let is_argument = node.is_argument();
            let is_a_port = (is_this || is_argument) && is_leaf;
            f(is_a_port, node, builder);
            builder.nested()
        });
    }

    fn count_ports(&self) -> usize {
        let mut count = 0;
        self.traverse_borrowed_expression_raw_mut(|is_a_port, _, _| {
            if is_a_port {
                count += 1
            }
        });
        count
    }

    #[profile(Debug)]
    fn set_size(&self, size: Vector2) {
        self.ports.set_x(size.x / 2.0);
    }

    #[profile(Debug)]
    fn set_label_on_new_expression(&self, expression: &Expression) {
        self.set_label(expression.code());
    }

    #[profile(Debug)]
    fn build_port_shapes_on_new_expression(&self) {
        let mut port_index = 0;
        let mut id_crumbs_map = HashMap::new();
        let whole_expr_id = self.expression.borrow().whole_expr_id;
        let port_count = self.port_count.get();
        self.traverse_borrowed_expression_mut(|is_a_port,mut node,builder| {
            let ast_id = if port_count == 0 { whole_expr_id } else { node.ast_id };
            if let Some(id) = ast_id {
                id_crumbs_map.insert(id,node.crumbs.clone_ref());
            }

            if DEBUG {
                let indent  = " ".repeat(4*builder.depth);
                let skipped = if !is_a_port { "(skip)" } else { "" };
                DEBUG!("{indent}[{node.payload.index},{node.payload.length}] \
                {skipped} {node.kind.variant_name():?} (tp: {node.tp():?}) (id: {node.ast_id:?})");
            }

            if is_a_port {
                let port   = &mut node;
                let crumbs = port.crumbs.clone_ref();
                let (port_shape,port_frp) = port.payload_mut()
                    .init_shape(&self.app,&self.styles,&self.styles_frp,port_index
                    ,port_count);
                let port_network = &port_frp.network;

                frp::extend! { port_network
                    self.frp.source.on_port_hover <+ port_frp.on_hover.map
                        (f!([crumbs](t) Switch::new(crumbs.clone(),*t)));
                    self.frp.source.on_port_press <+ port_frp.on_press.constant(crumbs.clone());

                    port_frp.set_size_multiplier        <+ self.frp.port_size_multiplier;
                    self.frp.source.on_port_type_change <+ port_frp.tp.map(move |t|(crumbs.clone(),t.clone()));
                    port_frp.set_type_label_visibility  <+ self.frp.type_label_visibility;
                    self.frp.source.tooltip             <+ port_frp.tooltip;
                    port_frp.set_view_mode              <+ self.frp.view_mode;
                    port_frp.set_size                   <+ self.frp.size;
                }

                port_frp.set_type_label_visibility.emit(self.frp.type_label_visibility.value());
                port_frp.set_view_mode.emit(self.frp.view_mode.value());
                port_frp.set_size.emit(self.frp.size.value());
                self.ports.add_child(&port_shape);
                port_index += 1;
            }
        });
        *self.id_crumbs_map.borrow_mut() = id_crumbs_map;
    }

    #[profile(Debug)]
    fn init_definition_types(&self) {
        let port_count = self.port_count.get();
        let whole_expr_type = self.expression.borrow().whole_expr_type.clone();
        let mut signals_to_emit = Vec::<(frp::Any<Option<Type>>, Option<Type>)>::new();
        self.traverse_borrowed_expression(|_, node, _| {
            if let Some(port_frp) = &node.payload.frp {
                let node_tp: Option<Type> = node.tp().cloned().map(|t| t.into());
                let node_tp = if port_count != 0 {
                    node_tp
                } else {
                    node_tp.or_else(|| whole_expr_type.clone())
                };
                signals_to_emit.push((port_frp.set_definition_type.clone_ref(), node_tp));
            }
        });
        for (endpoint, tp) in signals_to_emit {
            endpoint.emit(tp);
        }
    }

    #[profile(Debug)]
    fn set_expression(&self, new_expression: impl Into<node::Expression>) {
        let new_expression = Expression::from(new_expression.into());
        if DEBUG {
            DEBUG!("\n\n=====================\nSET EXPR: {new_expression:?}")
        }

        self.set_label_on_new_expression(&new_expression);
        *self.expression.borrow_mut() = new_expression;
        self.port_count.set(self.count_ports());
        self.build_port_shapes_on_new_expression();
        self.init_definition_types();
    }
}



// ============
// === Area ===
// ============

/// Implements the segmented output port area. Provides shapes that can be attached to a `Node` to
/// add an interactive area with output ports.
///
/// The `Area` facilitate the falling behaviour:
///  * when one of the output ports is hovered, after a set time, all ports are show and the hovered
///    port is highlighted.
///  * when a different port is hovered, it is highlighted immediately.
///  * when none of the ports is hovered all of the `Area` disappear. Note: there is a very small
///    delay for disappearing to allow for smooth switching between ports.
///
/// ## Origin
/// Please note that the origin of the node is on its left side, centered vertically. To learn more
/// about this design decision, please read the docs for the [`node::Node`].
#[derive(Clone, CloneRef, Debug)]
#[allow(missing_docs)]
pub struct Area {
    pub frp:   Frp,
    pub model: Rc<Model>,
}

impl Deref for Area {
    type Target = Frp;
    fn deref(&self) -> &Self::Target {
        &self.frp
    }
}


impl Area {
    #[allow(missing_docs)] // FIXME[everyone] All pub functions should have docs.
    pub fn new(app: &Application) -> Self {
        let frp = Frp::new();
        let model = Rc::new(Model::new(app, &frp));
        let network = &frp.network;
        let label_color = color::Animation::new(network);

        let hysteretic_transition =
            HystereticAnimation::new(network, SHOW_DELAY_DURATION_MS, HIDE_DELAY_DURATION_MS);

        frp::extend! { network

            // === Ports Show / Hide ===

            on_hover_out <- frp.on_port_hover.map(|t| t.is_off()).on_true();
            on_hover_in  <- frp.on_port_hover.map(|t| t.is_on()).on_true();

            hysteretic_transition.to_start <+ on_hover_in;
            hysteretic_transition.to_end   <+ on_hover_out;

            frp.source.port_size_multiplier <+ hysteretic_transition.value;
            eval frp.set_size ((t) model.set_size(*t));
            frp.source.size <+ frp.set_size;

            expr_label_x <- model.label.width.map(|width| -width - input::area::TEXT_OFFSET);
            eval expr_label_x ((x) model.label.set_x(*x));

            frp.source.type_label_visibility <+ frp.set_type_label_visibility;


            // === Expression ===

            eval frp.set_expression            ((a)     model.set_expression(a));
            eval frp.set_expression_usage_type (((a,b)) model.set_expression_usage_type(a,b));


            // === Label Color ===

            port_hover                             <- frp.on_port_hover.map(|t| t.is_on());
            frp.source.body_hover                  <+ frp.set_hover || port_hover;
            expr_vis                               <- frp.body_hover || frp.set_expression_visibility;
            in_normal_mode                         <- frp.set_view_mode.map(|m| m.is_normal());
            expr_vis                               <- expr_vis && in_normal_mode;
            frp.source.expression_label_visibility <+ expr_vis;

            let label_vis_color = color::Lcha::from(model.styles.get_color(theme::graph_editor::node::text));
            let label_vis_alpha = label_vis_color.alpha;
            label_alpha_tgt          <- expr_vis.map(move |t| if *t {label_vis_alpha} else {0.0} );
            label_color.target_alpha <+ label_alpha_tgt;
            label_color_on_change    <- label_color.value.sample(&frp.set_expression);
            new_label_color          <- any(&label_color.value,&label_color_on_change);
            eval new_label_color ((color) model.label.set_property(.., color::Rgba::from(color)));


            // === View Mode ===

            frp.source.view_mode <+ frp.set_view_mode;
        }

        label_color.target_alpha(0.0);
        label_color.target_color(label_vis_color.opaque);

        Self { frp, model }
    }

    /// Set a scene layer for text rendering.
    pub fn set_label_layer(&self, layer: &display::scene::Layer) {
        self.model.set_label_layer(layer);
    }

    #[allow(missing_docs)] // FIXME[everyone] All pub functions should have docs.
    pub fn port_type(&self, crumbs: &Crumbs) -> Option<Type> {
        let expression = self.model.expression.borrow();
        expression
            .span_tree
            .root_ref()
            .get_descendant(crumbs)
            .ok()
            .and_then(|t| t.frp.as_ref().and_then(|frp| frp.tp.value()))
    }

    #[allow(missing_docs)] // FIXME[everyone] All pub functions should have docs.
    pub fn get_crumbs_by_id(&self, id: ast::Id) -> Option<Crumbs> {
        self.model.id_crumbs_map.borrow().get(&id).cloned()
    }

    #[allow(missing_docs)] // FIXME[everyone] All pub functions should have docs.
    pub fn whole_expr_id(&self) -> Option<ast::Id> {
        self.model.expression.borrow().whole_expr_id
    }
}



// ==========================
// === Expression Setting ===
// ==========================

#[derive(Clone, Debug, Default)]
struct PortLayerBuilder {
    /// The depth at which the current expression is, where root is at depth 0.
    depth: usize,
}

impl PortLayerBuilder {
    fn nested(&self) -> Self {
        let depth = self.depth + 1;
        Self { depth }
    }
}

impl display::Object for Area {
    fn display_object(&self) -> &display::object::Instance {
        &self.model.display_object
    }
}
