//! Definition of the Node component.

#![allow(missing_docs)]
// WARNING! UNDER HEAVY DEVELOPMENT. EXPECT DRASTIC CHANGES.

#[deny(missing_docs)]
pub mod action_bar;
pub mod port;

pub use port::Expression;

use crate::prelude::*;

use enso_frp as frp;
use enso_frp;
use ensogl::application::Application;
use ensogl::data::color::animation::ColorAnimation;
use ensogl::data::color;
use ensogl::display::shape::*;
use ensogl::display::traits::*;
use ensogl::display;
use ensogl::gui::component::Animation;
use ensogl::gui::component;
use ensogl_text::Text;
use ensogl_theme;

use crate::Type;
use crate::component::node::port::output::OutputPorts;
use crate::component::visualization;

use super::edge;



// =================
// === Constants ===
// =================

pub const ACTION_BAR_HEIGHT  : f32 = 15.0;
pub const CORNER_RADIUS      : f32 = 14.0;
pub const NODE_HEIGHT        : f32 = 28.0;
pub const NODE_SHAPE_PADDING : f32 = 40.0;
pub const NODE_SHAPE_RADIUS  : f32 = 14.0;
pub const SHADOW_SIZE        : f32 = 10.0;
pub const TEXT_OFF           : f32 = 10.0;


// ============
// === Node ===
// ============

/// Canvas node shape definition.
pub mod shape {
    use super::*;

    ensogl::define_shape_system! {
        (style:Style, selection:f32, bg_color:Vector4 ) {
            use ensogl_theme::vars::graph_editor::node as node_theme;

            let bg_color        = Var::<color::Rgba>::from(bg_color);
            let selection_color = style.get_color(ensogl_theme::vars::graph_editor::node::selection::color);
            let _selection_size = style.get_number_or(ensogl_theme::vars::graph_editor::node::selection::size,8.0);

            let border_size_f = 16.0;

            let width  : Var<Pixels> = "input_size.x".into();
            let height : Var<Pixels> = "input_size.y".into();
            let width  = width  - NODE_SHAPE_PADDING.px() * 2.0;
            let height = height - NODE_SHAPE_PADDING.px() * 2.0;
            let radius = NODE_SHAPE_RADIUS.px();
            let shape  = Rect((&width,&height)).corners_radius(radius);
            let shape  = shape.fill(bg_color);


            // === Shadow ===

            let shadow_size   = SHADOW_SIZE.px();
            let shadow_width  = &width  + &shadow_size * 2.0;
            let shadow_height = &height + &shadow_size * 2.0;
            let shadow_radius = &shadow_height / 2.0;
            let shadow        = Rect((shadow_width,shadow_height)).corners_radius(shadow_radius);
            let base_color    = style.get_color(node_theme::shadow::color);
            let fading_color  = style.get_color(node_theme::shadow::fading_color);
            let exponent      = style.get_number_or(node_theme::shadow::exponent,2.0);
            let shadow_color  = color::LinearGradient::new()
                .add(0.0,color::Rgba::from(fading_color).into_linear())
                .add(1.0,color::Rgba::from(base_color).into_linear());
            let shadow_color = color::SdfSampler::new(shadow_color)
                .max_distance(border_size_f)
                .slope(color::Slope::Exponent(exponent));
            let shadow        = shadow.fill(shadow_color);


            // === Selection ===

            let selection_offset = 5.px();
            let selection_size   = 9.px();
            let select_width     = &width  - 2.px() + &selection_offset * 2.0 * &selection;
            let select_height    = &height - 2.px() + &selection_offset * 2.0 * &selection;
            let select_radius    = &select_height / 2.0;
            let select           = Rect((&select_width,&select_height)).corners_radius(&select_radius);

            let select2_width  = &width  - 2.px() + &selection_size * 2.0 * &selection;
            let select2_height = &height - 2.px() + &selection_size * 2.0 * &selection;
            let select2_radius = &select2_height / 2.0;
            let select2        = Rect((&select2_width,&select2_height)).corners_radius(&select2_radius);

            let select = select2 - select;
            let select = select.fill(color::Rgba::from(selection_color));

            let out = select + shadow + shape;
            out.into()
        }
    }
}

pub mod drag_area {
    use super::*;

    ensogl::define_shape_system! {
        (style:Style) {
            let width  : Var<Pixels> = "input_size.x".into();
            let height : Var<Pixels> = "input_size.y".into();
            let width  = width  - NODE_SHAPE_PADDING.px() * 2.0;
            let height = height - NODE_SHAPE_PADDING.px() * 2.0;
            let radius = 14.px();
            let shape  = Rect((&width,&height)).corners_radius(radius);
            let shape  = shape.fill(color::Rgba::new(0.0,0.0,0.0,0.000_001));

            let out = shape;
            out.into()
        }
    }
}



// ===========
// === Frp ===
// ===========

ensogl::define_endpoints! {
    Input {
        select              (),
        deselect            (),
        set_expression      (Expression),
        set_expression_type ((ast::Id,Option<Type>)),
        set_visualization   (Option<visualization::Definition>),
        set_dimmed          (bool),
    }
    Output {
        /// Press event. Emitted when user clicks on non-active part of the node, like its
        /// background. In edit mode, the whole node area is considered non-active.
        background_press (),
        expression (Text),
        skip       (bool),
        freeze     (bool),
    }
}



// ============
// === Node ===
// ============


/// Internal data of `Node`
#[derive(Clone,CloneRef,Debug)]
#[allow(missing_docs)]
pub struct Node {
    pub model : Rc<NodeModel>,
    pub frp   : Frp,
}

impl AsRef<Node> for Node {
    fn as_ref(&self) -> &Self {
        self
    }
}


impl Deref for Node {
    type Target = Frp;
    fn deref(&self) -> &Self::Target {
        &self.frp
    }
}

/// Internal data of `Node`
#[derive(Clone,CloneRef,Debug)]
#[allow(missing_docs)]
pub struct NodeModel {
    pub app            : Application,
    pub display_object : display::object::Instance,
    pub logger         : Logger,
    pub main_area      : component::ShapeView<shape::Shape>,
    pub drag_area      : component::ShapeView<drag_area::Shape>,
    pub ports          : port::Manager,
    pub visualization  : visualization::Container,
    pub action_bar     : action_bar::ActionBar,
    pub output_ports   : OutputPorts,

    main_color         : ColorAnimation,
}


impl NodeModel {
    /// Constructor.
    pub fn new(app:&Application, registry:visualization::Registry) -> Self {
        let scene  = app.display.scene();
        let logger = Logger::new("node");
        edge::sort_hack_1(scene);

        OutputPorts::order_hack(&scene);
        let main_logger = Logger::sub(&logger,"main_area");
        let drag_logger = Logger::sub(&logger,"drag_area");
        let main_area   = component::ShapeView::<shape::Shape>::new(&main_logger,scene);
        let main_color  = ColorAnimation::new(app);
        let drag_area   = component::ShapeView::<drag_area::Shape>::new(&drag_logger,scene);
        edge::sort_hack_2(scene);

        port::sort_hack(scene); // FIXME hack for sorting

        let display_object  = display::object::Instance::new(&logger);
        display_object.add_child(&drag_area);
        display_object.add_child(&main_area);

        // FIXME: maybe we can expose shape system from shape?
        let shape_system = scene.shapes.shape_system(PhantomData::<shape::Shape>);
        shape_system.shape_system.set_pointer_events(false);

        let ports = port::Manager::new(&logger,app);
        let scene = scene.clone_ref();
        let visualization = visualization::Container::new(&logger,&app,registry);
        visualization.mod_position(|t| {
            t.x = 60.0;
            t.y = -120.0;
        });

        display_object.add_child(&visualization);

        ports.mod_position(|p| {
            p.x = TEXT_OFF;
            p.y = NODE_HEIGHT/2.0;
        });
        display_object.add_child(&ports);

        let action_bar = action_bar::ActionBar::new(&app);
        display_object.add_child(&action_bar);
        action_bar.frp.show_icons();

        // TODO: Determine number of output ports based on node semantics.
        let output_ports = OutputPorts::new(&scene);
        display_object.add_child(&output_ports);

        let app = app.clone_ref();
        Self {app,display_object,logger,main_area,drag_area,output_ports,ports
             ,visualization,action_bar,main_color} . init()
    }

    fn init(self) -> Self {
        self.set_expression(Expression::debug_from_str("empty"));
        self
    }

    pub fn width(&self) -> f32 {
        self.ports.width() + TEXT_OFF * 2.0
    }

    pub fn height(&self) -> f32 {
        NODE_HEIGHT
    }

    fn set_expression(&self, expr:impl Into<Expression>) {
        let expr = expr.into();
        self.output_ports.set_pattern_span_tree(&expr.output_span_tree);
        self.ports.set_expression(expr);
    }

    fn set_width(&self, width:f32) {
        let height = self.height();
        let width  = width + TEXT_OFF * 2.0;
        let size   = Vector2::new(width+NODE_SHAPE_PADDING*2.0, height+NODE_SHAPE_PADDING*2.0);
        self.main_area.shape.sprite.size.set(size);
        self.drag_area.shape.sprite.size.set(size);
        self.main_area.mod_position(|t| t.x = width/2.0);
        self.main_area.mod_position(|t| t.y = height/2.0);
        self.drag_area.mod_position(|t| t.x = width/2.0);
        self.drag_area.mod_position(|t| t.y = height/2.0);

        self.output_ports.frp.set_size.emit(size);
        self.output_ports.mod_position(|t| t.x = width/2.0);
        self.output_ports.mod_position(|t| t.y = height/2.0);

        self.action_bar.mod_position(|t| {
            t.x = width/2.0 + CORNER_RADIUS;
            t.y = height + ACTION_BAR_HEIGHT;
        });
        self.action_bar.frp.set_size(Vector2::new(width,ACTION_BAR_HEIGHT));
    }

    pub fn visualization(&self) -> &visualization::Container {
        &self.visualization
    }
}

impl Node {
    pub fn new(app:&Application, registry:visualization::Registry) -> Self {
        let frp       = Frp::new_network();
        let network   = &frp.network;
        let inputs    = &frp.input;
        let out       = &frp.output;
        let model     = Rc::new(NodeModel::new(app,registry));
        let selection = Animation::<f32>::new(network);

        let color_animation = ColorAnimation::new(&app);
        let style           = StyleWatch::new(&app.display.scene().style_sheet);

        let actions         = &model.action_bar.frp;
        frp::extend! { network
            eval  selection.value ((v) model.main_area.shape.selection.set(*v));
            eval_ inputs.select   (selection.set_target_value(1.0));
            eval_ inputs.deselect (selection.set_target_value(0.0));

            eval inputs.set_expression ((expr) model.set_expression(expr));
            // eval inputs.set_expression_type (((ast_id,maybe_type)) {
            //     model.ports.set_expression_type(*ast_id,maybe_type.clone());
            //     model.output_ports.set_pattern_type(*ast_id,maybe_type.clone())
            // });

            eval inputs.set_visualization ((content)
                model.visualization.frp.set_visualization.emit(content)
            );

            eval model.ports.frp.width ((w) model.set_width(*w));

            out.source.background_press <+ model.drag_area.events.mouse_down;

            eval_ model.drag_area.events.mouse_over (model.ports.hover());
            eval_ model.drag_area.events.mouse_out  (model.ports.unhover());

            out.source.expression <+ model.ports.frp.expression.map(|t|t.clone_ref());

            eval actions.action_visbility ((visible){
                model.visualization.frp.set_visibility.emit(visible);
            });

            out.source.skip   <+ actions.action_skip;
            out.source.freeze <+ actions.action_freeze;


            // === Color Handling ===

            background_color <- inputs.set_dimmed.map(f!([model,style](should_dim) {
                model.ports.frp.set_dimmed.emit(*should_dim);
                let background_color_path = ensogl_theme::vars::graph_editor::node::background::color;
                if *should_dim {
                   style.get_color_dim(background_color_path)
                 } else {
                   style.get_color(background_color_path)
                 }
            }));

            eval background_color ((color)  color_animation.set_target(color) );

            eval color_animation.value ([model](color) {
                let color:color::Rgba = color.into();
                model.main_area.shape.bg_color.set(color.into())
            });


            // === Action Bar ===

            eval_ model.main_area.events.mouse_over  ( actions.show_icons() );
            eval_ model.main_area.events.mouse_out   ( actions.hide_icons() );
            eval_ model.drag_area.events.mouse_over  ( actions.show_icons() );
            eval_ model.drag_area.events.mouse_out   ( actions.hide_icons() );

            is_hovered <- model.ports.frp.hover.map(|item| item.is_some() );
            eval is_hovered ((hovered) actions.icon_visibility(hovered) );
        }

        model.action_bar.frp.hide_icons.emit(());
        frp.set_dimmed.emit(false);

        Self {frp,model}
    }
}

impl display::Object for Node {
    fn display_object(&self) -> &display::object::Instance {
        &self.model.display_object
    }
}
