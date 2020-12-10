//! Implements the segmented output port area.
use crate::prelude::*;

use ensogl::display::traits::*;

use enso_frp as frp;
use enso_frp;
use ensogl::Animation;
use ensogl::Easing;
use ensogl::application::Application;
use ensogl::data::color;
use ensogl::display::scene::Scene;
use ensogl::display::shape::StyleWatch;
use ensogl::display;
use ensogl::gui::component;
use ensogl_text as text;
use ensogl_theme as theme;
use span_tree;

use crate::Type;
use crate::component::node;
use crate::component::node::output::port;
use crate::component::node::input;



// =================
// === Constants ===
// =================

const DEBUG                  : bool = false;
const HIDE_DELAY_DURATION_MS : f32 = 150.0;
const SHOW_DELAY_DURATION_MS : f32 = 150.0;



// =============
// === Utils ===
// =============

// TODO: Implement proper sorting and remove.
/// Hack function used to register the elements for the sorting purposes. To be removed.
pub(crate) fn depth_sort_hack(scene:&Scene) {
    let logger = Logger::new("output shape order hack");
    component::ShapeView::<port::MultiPortShape>::new(&logger,scene);
    component::ShapeView::<port::SinglePortShape>::new(&logger,scene);
}



// ================
// === SpanTree ===
// ================

pub use span_tree::Crumb;
pub use span_tree::Crumbs;

/// Specialized `SpanTree` for the input ports model.
pub type SpanTree = span_tree::SpanTree<port::Model>;

/// Mutable reference to port inside of a `SpanTree`.
pub type PortRefMut<'a> = span_tree::node::RefMut<'a,port::Model>;



// ==================
// === Expression ===
// ==================

/// Specialized version of `node::Expression`, containing the port information.
#[derive(Default)]
pub struct Expression {
    pub code      : Option<String>,
    pub span_tree : SpanTree,
    /// This field contains the type of the whole input expression. This is needed due to a bug in
    /// engine: https://github.com/enso-org/enso/issues/1038.
    pub whole_expr_type : Option<Type>,
    pub whole_expr_id   : Option<ast::Id>,
}

impl Expression {
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
    fn fmt(&self, f:&mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f,"Expression({})",self.code.clone().unwrap_or_default())
    }
}


// === Conversions ===

impl From<node::Expression> for Expression {
    fn from(expr:node::Expression) -> Self {
        let code            = expr.pattern.clone();
        let whole_expr_type = expr.input_span_tree.root.tp().map(|t|t.to_owned().into());
        let whole_expr_id   = expr.input_span_tree.root.ast_id;
        let mut span_tree   = expr.output_span_tree.map(|_| port::Model::default());
        span_tree.root_ref_mut().dfs((),|node,()| {
            let span    = node.span();
            let port    = node.payload_mut();
            port.index  = span.index.value;
            port.length = span.size.value;
        });
        Expression{code,span_tree,whole_expr_type,whole_expr_id}
    }
}



// =============
// === Model ===
// =============

ensogl::define_endpoints! {
    Input {
        set_size (Vector2),

        /// Set the expression USAGE type. This is not the definition type, which can be set with
        /// `set_expression` instead. In case the usage type is set to None, ports still may be
        /// colored if the definition type was present.
        set_expression_usage_type (ast::Id,Option<Type>),
    }

    Output {
        on_port_hover        (Switch<Crumbs>),
        on_port_press        (Crumbs),
        port_size_multiplier (f32),
    }
}

/// Internal model of the port area.
#[derive(Debug)]
pub struct Model {
    logger         : Logger,
    app            : Application,
    display_object : display::object::Instance,
    ports          : display::object::Instance,
    label          : text::Area,
    expression     : RefCell<Expression>,
    id_crumbs_map  : RefCell<HashMap<ast::Id,Crumbs>>,
    port_count     : Cell<usize>,
    styles         : StyleWatch,
}



impl Model {
    /// Constructor.
    pub fn new(logger:impl AnyLogger, app:&Application) -> Self {
        let logger         = Logger::sub(&logger,"output_ports");
        let display_object = display::object::Instance::new(&logger);
        let ports          = display::object::Instance::new(&Logger::sub(&logger,"ports"));
        let app            = app.clone_ref();
        let label          = app.new_view::<text::Area>();
        let id_crumbs_map  = default();
        let expression     = default();
        let port_count     = default();
        let styles         = StyleWatch::new(&app.display.scene().style_sheet);
        display_object.add_child(&label);
        display_object.add_child(&ports);
        Self {logger,display_object,ports,app,label,expression,id_crumbs_map,port_count,styles}
            .init()
    }

    fn init(self) -> Self {
        // FIXME[WD]: Depth sorting of labels to in front of the mouse pointer. Temporary solution.
        // It needs to be more flexible once we have proper depth management.
        let scene = self.app.display.scene();
        self.label.remove_from_view(&scene.views.main);
        self.label.add_to_view(&scene.views.label);

        let text_color = self.styles.get_color(theme::graph_editor::node::text);
        self.label.single_line(true);
        self.label.disable_command("cursor_move_up");
        self.label.disable_command("cursor_move_down");
        self.label.set_default_color(color::Rgba::from(text_color));
        self.label.set_default_text_size(text::Size(input::area::TEXT_SIZE));
        self.label.remove_all_cursors();

        self.label.mod_position(|t| t.y = input::area::TEXT_SIZE/2.0);

        self
    }

    fn scene(&self) -> &Scene {
        self.app.display.scene()
    }

    fn set_label(&self, content:impl Into<String>) {
        self.label.set_content(content.into());
        self.label.set_position_x(-self.label.width.value() - input::area::TEXT_OFFSET);
    }

    /// Update expression type for the particular `ast::Id`.
    fn set_expression_usage_type(&self, id:ast::Id, tp:&Option<Type>) {
        if let Some(crumbs) = self.id_crumbs_map.borrow().get(&id) {
            if let Ok(port) = self.expression.borrow().span_tree.root_ref().get_descendant(crumbs) {
                if let Some(frp) = &port.frp {
                    frp.set_usage_type(tp)
                }
            }
        }
    }

    fn traverse_expression
    (&self, mut f:impl FnMut(bool, &mut PortRefMut, &mut PortLayerBuilder)) {
        let port_count = self.port_count.get();
        self.traverse_expression_raw(|is_a_port,node,builder| {
            let is_a_port = is_a_port || port_count == 0;
            f(is_a_port,node,builder)
        })
    }

    fn traverse_expression_raw
    (&self, mut f:impl FnMut(bool, &mut PortRefMut, &mut PortLayerBuilder)) {
        let mut expression = self.expression.borrow_mut();
        expression.root_ref_mut().dfs(PortLayerBuilder::default(),|node,builder| {
            let is_leaf     = node.children.is_empty();
            let is_this     = node.is_this();
            let is_argument = node.is_argument();
            let is_a_port   = (is_this || is_argument) && is_leaf;
            f(is_a_port,node,builder);
            builder.nested()
        });
    }

    fn count_ports(&self) -> usize {
        let mut count = 0;
        self.traverse_expression_raw(|is_a_port,_,_| if is_a_port { count += 1 });
        count
    }

    fn set_size(&self, size:Vector2) {
        self.ports.set_position_x(size.x/2.0);
        self.traverse_expression(|is_a_port,node,_| {
            if is_a_port { node.payload_mut().set_size(size) }
        })
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
///  * when none of the ports is hovered all of the `Area` disappear. Note: there is a very
///    small delay for disappearing to allow for smooth switching between ports.
///
/// ## Origin
/// Please note that the origin of the node is on its left side, centered vertically. To learn more
/// about this design decision, please read the docs for the [`node::Node`].
#[derive(Clone,CloneRef,Debug)]
pub struct Area {
    pub frp : Frp,
    model   : Rc<Model>,
}

impl Deref for Area {
    type Target = Frp;
    fn deref(&self) -> &Self::Target {
        &self.frp
    }
}


impl Area {
    pub fn new(logger:impl AnyLogger, app:&Application) -> Self {
        let model   = Rc::new(Model::new(logger,app));
        let frp     = Frp::new();
        let network = &frp.network;

        let port_size  = Animation::<f32>::new(network);
        let show_delay = Easing::new(network);
        let hide_delay = Easing::new(network);
        show_delay.set_duration(SHOW_DELAY_DURATION_MS);
        hide_delay.set_duration(HIDE_DELAY_DURATION_MS);

        frp::extend! { network

            // === Ports Show / Hide ===

            during_transition <- any_mut();

            on_hover_out                 <- frp.on_port_hover.map(|t| t.is_off()).on_true();
            on_hover_out_during_show     <- on_hover_out.gate(&during_transition);
            on_hover_out_not_during_show <- on_hover_out.gate_not(&during_transition);

            on_hover_in                  <- frp.on_port_hover.map(|t| t.is_on()).on_true();
            on_hover_in_during_show      <- on_hover_in.gate(&during_transition);
            on_hover_in_not_during_show  <- on_hover_in.gate_not(&during_transition);

            show_delay.target <+ on_hover_in_not_during_show.constant(1.0);
            hide_delay.target <+ on_hover_out_not_during_show.constant(1.0);

            show_delay.stop_and_rewind <+ on_hover_out.constant(0.0);
            hide_delay.stop_and_rewind <+ on_hover_in.constant(0.0);

            on_hide_start <- hide_delay.on_end.map(|t| t.is_normal()).on_true();
            on_show_start <- show_delay.on_end.map(|t| t.is_normal()).on_true();

            on_show_end <- port_size.value.map(|t| (t - 1.0).abs() < std::f32::EPSILON).on_true();
            on_hide_end <- port_size.value.map(|t| t.abs() < std::f32::EPSILON).on_true();

            port_size.target <+ on_show_start.constant(1.0);
            port_size.target <+ on_hide_start.constant(0.0);
            port_size.target <+ on_hover_out_during_show.constant(0.0);
            port_size.target <+ on_hover_in_during_show.constant(1.0);

            during_transition <+ bool(&on_show_end,&on_show_start);
            during_transition <+ bool(&on_hide_end,&on_hide_start);

            frp.source.port_size_multiplier <+ port_size.value;
            eval frp.set_size ((t) model.set_size(*t));


            // === Expression Type ===

            eval frp.set_expression_usage_type (((id,tp)) model.set_expression_usage_type(*id,tp));
        }
        Self {frp,model}
    }

    pub fn port_type(&self, crumbs:&Crumbs) -> Option<Type> {
        let expression = self.model.expression.borrow();
        expression.span_tree.root_ref().get_descendant(crumbs).ok()
            .and_then(|t|t.frp.as_ref().and_then(|frp|frp.tp.value()))
    }
}



// ==========================
// === Expression Setting ===
// ==========================

#[derive(Clone,Debug,Default)]
struct PortLayerBuilder {
    /// The depth at which the current expression is, where root is at depth 0.
    depth : usize,
}

impl PortLayerBuilder {
    fn nested(&self) -> Self {
        let depth = self.depth + 1;
        Self {depth}
    }
}

impl Area {
    fn set_label_on_new_expression(&self, expression:&Expression) {
        self.model.set_label(expression.code());
    }

    pub(crate) fn set_expression(&self, new_expression:impl Into<node::Expression>) {
        let new_expression = Expression::from(new_expression.into());
        if DEBUG { println!("\n\n=====================\nSET EXPR: {:?}", new_expression) }

        self.set_label_on_new_expression(&new_expression);
        *self.model.expression.borrow_mut() = new_expression;
        self.model.port_count.set(self.model.count_ports());
        self.build_port_shapes_on_new_expression();
    }

    fn build_port_shapes_on_new_expression(&self) {
        let mut port_index  = 0;
        let whole_expr_type = self.model.expression.borrow().whole_expr_type.clone();
        let whole_expr_id   = self.model.expression.borrow().whole_expr_id;
        let port_count      = self.model.port_count.get();
        self.model.traverse_expression(|is_a_port,mut node,builder|{
            let ast_id = if port_count == 0 { whole_expr_id } else { node.ast_id };
            if let Some(id) = ast_id {
                self.model.id_crumbs_map.borrow_mut().insert(id,node.crumbs.clone_ref());
            }

            if DEBUG {
                let indent  = " ".repeat(4*builder.depth);
                let skipped = if !is_a_port { "(skip)" } else { "" };
                println!("{}[{},{}] {} {:?} (tp: {:?}) (id: {:?})",indent,node.payload.index,
                    node.payload.length,skipped,node.kind.variant_name(),node.tp(),node.ast_id);
            }

            if is_a_port {
                let port   = &mut node;
                let crumbs = port.crumbs.clone_ref();
                let logger = &self.model.logger;
                let scene  = self.model.scene();
                let (port_shape,port_frp) = port.payload_mut()
                    .init_shape(logger,scene,&self.model.styles,port_index,port_count);
                let port_network = &port_frp.network;

                frp::extend! { port_network
                    self.frp.output.source.on_port_hover <+ port_frp.on_hover.map
                        (f!([crumbs](t) Switch::new(crumbs.clone(),*t)));
                    self.frp.output.source.on_port_press <+ port_frp.on_press.constant(crumbs);
                    port_frp.set_size_multiplier <+ self.frp.port_size_multiplier;
                }

                let node_tp : Option<Type> = node.tp().cloned().map(|t|t.into());
                let node_tp = if port_count != 0 { node_tp } else {
                    node_tp.or_else(||whole_expr_type.clone())
                };
                port_frp.set_definition_type.emit(node_tp);

                self.model.ports.add_child(&port_shape);
                port_index += 1;
            }
        })
    }
}

impl display::Object for Area {
    fn display_object(&self) -> &display::object::Instance {
        &self.model.display_object
    }
}
