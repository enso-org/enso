//! Definition of the Port component.

#[warn(missing_docs)]
pub mod output;

use crate::prelude::*;

use enso_frp as frp;
use enso_frp;
use ensogl::application::Application;
use ensogl::data::color;
use ensogl::display::scene::Scene;
use ensogl::display::shape::*;
use ensogl::display::traits::*;
use ensogl::display;
use ensogl::gui::component;
use ensogl::gui::cursor;
use ensogl_theme as theme;
use span_tree::SpanTree;
use ensogl_text as text;
use text::Text;

use super::super::node;

use crate::Type;
use crate::component::type_coloring::TypeColorMap;



// ============
// === Port ===
// ============

/// Canvas node shape definition.
pub mod shape {
    use super::*;

    ensogl::define_shape_system! {
        (style:Style) {
            let width  : Var<Pixels> = "input_size.x".into();
            let height : Var<Pixels> = "input_size.y".into();
            let radius = 6.px();
            let shape  = Rect((&width,&height)).corners_radius(radius);
            let color  : Var<color::Rgba> = "srgba(1.0,1.0,1.0,0.00001)".into();
            let shape  = shape.fill(color);
            shape.into()
        }
    }
}

pub fn sort_hack(scene:&Scene) {
    let logger = Logger::new("hack");
    component::ShapeView::<shape::Shape>::new(&logger,scene);
}



// ==================
// === Expression ===
// ==================

#[derive(Clone,Default)]
pub struct Expression {
    pub code             : String,
    pub input_span_tree  : SpanTree,
    pub output_span_tree : SpanTree,
}

impl Expression {
    pub fn debug_from_str(s:&str) -> Self {
        let code             = s.into();
        let input_span_tree  = default();
        let output_span_tree = default();
        Self {code,input_span_tree,output_span_tree}
    }
}

fn get_id_for_crumbs(span_tree:&SpanTree, crumbs:&[span_tree::Crumb]) -> Option<ast::Id> {
    if span_tree.root_ref().crumbs == crumbs {
        return span_tree.root.expression_id
    };
    let span_tree_descendant = span_tree.root_ref().get_descendant(crumbs);
    let expression_id        = span_tree_descendant.map(|node|{node.expression_id});
    expression_id.ok().flatten()
}


impl Debug for Expression {
    fn fmt(&self, f:&mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f,"Expression({})",self.code)
    }
}

impl From<&Expression> for Expression {
    fn from(t:&Expression) -> Self {
        t.clone()
    }
}



// ===========
// === FRP ===
// ===========

ensogl::define_endpoints! {
    Input {
        edit_mode_ready (bool),
        edit_mode       (bool),
        hover           (),
        unhover         (),
    }

    Output {
        pointer_style (cursor::Style),
        press        (span_tree::Crumbs),
        hover        (Option<span_tree::Crumbs>),
        width        (f32),
        expression   (Text),
        editing      (bool),
    }
}



// =============
// === Model ===
// =============

/// Internal model of the port manager.
#[derive(Debug)]
pub struct Model {
    logger         : Logger,
    display_object : display::object::Instance,
    ports_group    : display::object::Instance,
    app            : Application,
    expression     : RefCell<Expression>,
    label          : text::Area,
    ports          : RefCell<Vec<component::ShapeView<shape::Shape>>>,
    width          : Cell<f32>,
    port_networks  : RefCell<Vec<frp::Network>>,
    type_color_map : TypeColorMap,
}

impl Model {
    pub fn new(logger:impl AnyLogger, app:&Application) -> Self {
        let logger         = Logger::sub(&logger,"port_manager");
        let display_object = display::object::Instance::new(&logger);
        let ports_group    = display::object::Instance::new(&Logger::sub(&logger,"ports"));
        let app            = app.clone_ref();
        let port_networks  = default();
        let type_color_map = default();
        let label          = app.new_view::<text::Area>();
        let ports          = default();

        label.single_line(true);
        label.disable_command("cursor_move_up");
        label.disable_command("cursor_move_down");

        // FIXME[WD]: Depth sorting of labels to in front of the mouse pointer. Temporary solution.
        // It needs to be more flexible once we have proper depth management.
        let scene = app.display.scene();
        label.remove_from_view(&scene.views.main);
        label.add_to_view(&scene.views.label);

        label.mod_position(|t| t.y += 6.0);
        display_object.add_child(&label);
        display_object.add_child(&ports_group);

        // FIXME : StyleWatch is unsuitable here, as it was designed as an internal tool for shape system (#795)
        let styles     = StyleWatch::new(&app.display.scene().style_sheet);
        let text_color = styles.get_color(theme::vars::graph_editor::node::text::color);
        label.set_default_color(color::Rgba::from(text_color));
        label.set_default_text_size(text::Size(12.0));
        label.remove_all_cursors();

        let expression = default();
        let width      = default();

        Self {logger,display_object,ports_group,label,ports,width,app,expression,port_networks
             ,type_color_map}
    }
}



// ===============
// === Manager ===
// ===============

#[derive(Clone,CloneRef,Debug)]
pub struct Manager {
    model   : Rc<Model>,
    pub frp : Frp,
}

impl Deref for Manager {
    type Target = Frp;
    fn deref(&self) -> &Self::Target {
        &self.frp
    }
}

impl Manager {
    pub fn new(logger:impl AnyLogger, app:&Application) -> Self {
        let model   = Rc::new(Model::new(logger,app));
        let frp     = Frp::new_network();
        let network = &frp.network;

        frp::extend! { network
            // === Cursor setup ===

            eval frp.input.edit_mode ([model](enabled) {
                model.label.set_focus(enabled);
                if *enabled { model.label.set_cursor_at_mouse_position(); }
                else        { model.label.remove_all_cursors(); }
            });


            // === Show / Hide Phantom Ports ===

            edit_mode <- all_with(&frp.input.edit_mode,&frp.input.edit_mode_ready,|a,b|*a||*b);
            eval edit_mode ([model](edit_mode) {
                if *edit_mode {
                    model.display_object.remove_child(&model.ports_group)
                } else {
                    model.display_object.add_child(&model.ports_group);
                }
            });

            frp.output.source.hover   <+ edit_mode.gate_not(&edit_mode).constant(None);
            frp.output.source.editing <+ edit_mode.sampler();


            // === Label Hover ===

            hovered <- bool(&frp.input.unhover,&frp.input.hover);
            // The below pattern is a very special one. When mouse is over phantom port and the edit
            // mode button is pressed, the `edit_mode` event is emitted. It then emits trough the
            // network and hides phantom ports. In the next frame, the hovering is changed, so this
            // value needs to be updated.
            edit_mode_hovered <- all_with(&edit_mode,&hovered,|a,_|*a).gate(&hovered);
            label_hovered     <- all_with(&hovered,&edit_mode_hovered,|a,b|*a&&*b);
            eval label_hovered ((t) model.label.set_hover(t));


            // === Properties ===

            frp.output.source.width      <+ model.label.width;
            frp.output.source.expression <+ model.label.content.map(|t| t.clone_ref());
        }

        Self {model,frp}
    }

    fn scene(&self) -> &Scene {
        self.model.app.display.scene()
    }

    pub(crate) fn set_expression(&self, expression:impl Into<Expression>) {
        let model      = &self.model;
        let expression = expression.into();

        model.label.set_cursor(&default());
        model.label.select_all();
        model.label.insert(&expression.code);
        model.label.remove_all_cursors();
        if self.frp.editing.value() {
            model.label.set_cursor_at_end();
        }

        let glyph_width = 7.224_609_4; // FIXME hardcoded literal
        let width       = expression.code.len() as f32 * glyph_width;
        model.width.set(width);

        let mut to_visit      = vec![expression.input_span_tree.root_ref()];
        let mut ports         = vec![];
        let mut port_networks = vec![];

        loop {
            match to_visit.pop() {
                None => break,
                Some(node) => {
                    let span          = node.span();
                    let contains_root = span.index.value == 0;
                    let skip          = node.kind.is_empty() || contains_root;
                    if !skip {
                        let logger   = Logger::sub(&model.logger,"port");
                        let port     = component::ShapeView::<shape::Shape>::new(&logger,self.scene());
                        let type_map = &model.type_color_map;

                        let unit        = 7.224_609_4;
                        let width       = unit * span.size.value as f32;
                        let width2      = width + 8.0;
                        let node_height = 28.0;
                        let height      = 18.0;
                        let size        = Vector2::new(width2,height);
                        port.shape.sprite.size.set(Vector2::new(width2,node_height));
                        let x = width/2.0 + unit * span.index.value as f32;
                        port.mod_position(|t| t.x = x);
                        model.ports_group.add_child(&port);

                        // FIXME : StyleWatch is unsuitable here, as it was designed as an internal tool for shape system (#795)
                        let styles             = StyleWatch::new(&model.app.display.scene().style_sheet);
                        let missing_type_color = styles.get_color(theme::vars::graph_editor::edge::_type::missing::color);

                        let crumbs = node.crumbs.clone();
                        let ast_id = get_id_for_crumbs(&expression.input_span_tree,&crumbs);
                        let color  = ast_id.and_then(|id|type_map.type_color(id,styles.clone_ref()));
                        let color  = color.unwrap_or(missing_type_color);

                        let highlight = cursor::Style::new_highlight(&port,size,Some(color));

                        frp::new_network! { port_network
                            let mouse_over = port.events.mouse_over.clone_ref();
                            let mouse_out  = port.events.mouse_out.clone_ref();


                            // === Mouse Style ===

                            pointer_style_over  <- mouse_over.map(move |_| highlight.clone());
                            pointer_style_out   <- mouse_out.map(|_| default());
                            pointer_style_hover <- any(pointer_style_over,pointer_style_out);
                            pointer_style       <- all
                                [ pointer_style_hover
                                , self.model.label.pointer_style
                                ].fold();
                            self.frp.output.source.pointer_style <+ pointer_style;


                            // === Port Hover ===

                            let crumbs_over  = crumbs.clone();
                            let hover_source = &self.frp.output.source.hover;
                            eval_ mouse_over (hover_source.emit(&Some(crumbs_over.clone())));
                            eval_ mouse_out  (hover_source.emit(&None));


                            // === Port Press ===

                            let crumbs_down  = crumbs.clone();
                            let press_source = &self.frp.output.source.press;
                            eval_ port.events.mouse_down (press_source.emit(&crumbs_down));
                        }
                        ports.push(port);
                        port_networks.push(port_network);
                    }

                    to_visit.extend(node.children_iter());
                }
            }
        }

        *model.expression.borrow_mut()    = expression;
        *model.ports.borrow_mut()         = ports;
        *model.port_networks.borrow_mut() = port_networks;
    }

    pub fn get_port_offset(&self, crumbs:&[span_tree::Crumb]) -> Option<Vector2<f32>> {
        let span_tree = &self.model.expression.borrow().input_span_tree;
        span_tree.root_ref().get_descendant(crumbs).map(|node|{
            let span  = node.span();
            let unit  = 7.224_609_4;
            let width = unit * span.size.value as f32;
            let x     = width/2.0 + unit * span.index.value as f32;
            Vector2::new(x + node::TEXT_OFF,node::NODE_HEIGHT/2.0) // FIXME
        }).ok()
    }

    pub fn get_port_color(&self, crumbs:&[span_tree::Crumb]) -> Option<color::Lcha> {
        let ast_id = get_id_for_crumbs(&self.model.expression.borrow().input_span_tree,&crumbs)?;
        // FIXME : StyleWatch is unsuitable here, as it was designed as an internal tool for shape system (#795)
        let styles = StyleWatch::new(&self.model.app.display.scene().style_sheet);
        self.model.type_color_map.type_color(ast_id, styles)
    }

    pub fn width(&self) -> f32 {
        self.model.width.get()
    }

    pub fn set_expression_type(&self, id:ast::Id, maybe_type:Option<Type>) {
        self.model.type_color_map.update_entry(id,maybe_type);
    }
}

impl display::Object for Manager {
    fn display_object(&self) -> &display::object::Instance {
        &self.model.display_object
    }
}
