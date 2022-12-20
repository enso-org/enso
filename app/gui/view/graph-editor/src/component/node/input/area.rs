//! Definition of the node input port component.

use crate::prelude::*;
use enso_text::index::*;
use enso_text::unit::*;
use ensogl::display::shape::*;
use ensogl::display::traits::*;

use crate::component::type_coloring;
use crate::node;
use crate::node::input::port;
use crate::node::profiling;
use crate::view;
use crate::Type;

use enso_frp as frp;
use enso_frp;
use ensogl::application::Application;
use ensogl::data::color;
use ensogl::display;
use ensogl::gui::cursor;
use ensogl::Animation;
use ensogl_component::text;
use ensogl_hardcoded_theme as theme;



// =================
// === Constants ===
// =================

/// An offset from the port area position to the text position.
pub const TEXT_OFFSET: f32 = 10.0;

/// Width of a single glyph
// TODO: avoid using hardcoded value. See https://www.pivotaltracker.com/story/show/183567623.
pub const GLYPH_WIDTH: f32 = 7.224_609_4;

/// Enable visual port debug mode and additional port creation logging.
pub const DEBUG: bool = false;

/// Visual port offset for debugging purposes. Applied hierarchically. Applied only when `DEBUG` is
/// set to `true`.
pub const DEBUG_PORT_OFFSET: f32 = 5.0;

/// Skip creating ports on all operations. For example, in expression `foo bar`, `foo` is considered
/// an operation.
const SKIP_OPERATIONS: bool = true;
const PORT_PADDING_X: f32 = 4.0;

/// Text size used for input area text.
pub const TEXT_SIZE: f32 = 12.0;



// ================
// === SpanTree ===
// ================

pub use span_tree::Crumb;
pub use span_tree::Crumbs;

/// Specialized `SpanTree` for the input ports model.
pub type SpanTree = span_tree::SpanTree<port::Model>;

/// Mutable reference to port inside of a `SpanTree`.
pub type PortRefMut<'a> = span_tree::node::RefMut<'a, port::Model>;



// ==================
// === Expression ===
// ==================

/// Specialized version of `node::Expression`, containing the port information.
#[derive(Clone, Default)]
#[allow(missing_docs)]
pub struct Expression {
    /// Visual code representation. It can contain names of missing arguments, and thus can differ
    /// from `code`.
    pub viz_code:  ImString,
    pub code:      ImString,
    pub span_tree: SpanTree,
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


// === Conversions ===

/// Helper struct used for `Expression` conversions.
#[derive(Debug, Default)]
struct ExprConversion {
    prev_tok_local_index:  Byte,
    /// Index of the last traverse parent node in the `SpanTree`.
    last_parent_tok_index: Byte,
}

impl ExprConversion {
    fn new(last_parent_tok_index: Byte) -> Self {
        let prev_tok_local_index = default();
        Self { prev_tok_local_index, last_parent_tok_index }
    }
}

impl From<node::Expression> for Expression {
    /// Traverses the `SpanTree` and constructs `viz_code` based on `code` and the `SpanTree`
    /// structure. It also computes `port::Model` values in the `viz_code` representation.
    #[profile(Debug)]
    fn from(t: node::Expression) -> Self {
        // The length difference between `code` and `viz_code` so far.
        let mut shift = 0.byte();
        let mut span_tree = t.input_span_tree.map(|_| port::Model::default());
        let mut viz_code = String::new();
        let code = t.code;
        span_tree.root_ref_mut().dfs_with_layer_data(ExprConversion::default(), |node, info| {
            let is_expected_arg = node.is_expected_argument();
            let span = node.span();
            // TODO: remove unwrap. (https://www.pivotaltracker.com/story/show/183567590)
            let mut size = Byte::try_from(span.size()).unwrap();
            let mut index = span.start;
            let offset_from_prev_tok = node.offset - info.prev_tok_local_index.to_diff();
            info.prev_tok_local_index = size + node.offset;
            viz_code += &" ".repeat(offset_from_prev_tok.as_usize());
            if node.children.is_empty() {
                viz_code += &code.as_str()[enso_text::Range::new(index, index + size)];
            }
            index += shift;
            if is_expected_arg {
                if let Some(name) = node.name() {
                    size = name.len().into();
                    index += 1.byte();
                    shift += 1.byte() + size;
                    viz_code += " ";
                    viz_code += name;
                }
            }
            let port = node.payload_mut();
            port.local_index = index - info.last_parent_tok_index;
            port.index = index.into();
            port.length = size.into();
            ExprConversion::new(index)
        });
        Self { viz_code: viz_code.into(), code, span_tree }
    }
}



// =============
// === Model ===
// =============

/// Internal model of the port area.
#[derive(Debug)]
pub struct Model {
    app:            Application,
    display_object: display::object::Instance,
    ports:          display::object::Instance,
    header:         display::object::Instance,
    label:          text::Text,
    expression:     RefCell<Expression>,
    id_crumbs_map:  RefCell<HashMap<ast::Id, Crumbs>>,
    styles:         StyleWatch,
    styles_frp:     StyleWatchFrp,
}

impl Model {
    /// Constructor.
    #[profile(Debug)]
    pub fn new(app: &Application) -> Self {
        let display_object = display::object::Instance::new();
        let ports = display::object::Instance::new();
        let header = display::object::Instance::new();
        let app = app.clone_ref();
        let label = app.new_view::<text::Text>();
        let id_crumbs_map = default();
        let expression = default();
        let styles = StyleWatch::new(&app.display.default_scene.style_sheet);
        let styles_frp = StyleWatchFrp::new(&app.display.default_scene.style_sheet);
        display_object.add_child(&label);
        display_object.add_child(&ports);
        ports.add_child(&header);
        Self {
            app,
            display_object,
            ports,
            header,
            label,
            expression,
            id_crumbs_map,
            styles,
            styles_frp,
        }
        .init()
    }

    #[profile(Debug)]
    fn init(self) -> Self {
        // TODO: Depth sorting of labels to in front of the mouse pointer. Temporary solution.
        //   It needs to be more flexible once we have proper depth management.
        //   See https://www.pivotaltracker.com/story/show/183567632.
        let scene = &self.app.display.default_scene;
        scene.layers.main.remove(&self.label);
        self.label.add_to_scene_layer(&scene.layers.label);

        let text_color = self.styles.get_color(theme::graph_editor::node::text);
        self.label.set_single_line_mode(true);
        self.label.disable_command("cursor_move_up");
        self.label.disable_command("cursor_move_down");
        self.label.set_property_default(text_color);
        self.label.set_property_default(text::Size(TEXT_SIZE));
        self.label.remove_all_cursors();

        let origin = Vector2(TEXT_OFFSET, 0.0);
        self.ports.set_xy(origin);
        self.label.set_xy(origin);
        self.label.modify_position(|t| t.y += TEXT_SIZE / 2.0);

        self
    }

    /// Return a list of Node's input ports.
    pub fn ports(&self) -> Vec<port::Model> {
        let expression = self.expression.borrow();
        let mut ports = Vec::new();
        expression.span_tree.root_ref().dfs(|n| ports.push(n.payload.clone()));
        ports
    }


    fn set_label_layer(&self, layer: &display::scene::Layer) {
        self.label.add_to_scene_layer(layer);
    }

    /// Run the provided function on the target port if exists.
    fn with_port_mut(&self, crumbs: &Crumbs, f: impl FnOnce(PortRefMut)) {
        let mut expression = self.expression.borrow_mut();
        if let Ok(node) = expression.span_tree.root_ref_mut().get_descendant(crumbs) {
            f(node)
        }
    }

    /// Traverse all `SpanTree` leaves of the given port and emit hover style to set their colors.
    fn set_port_hover(&self, target: &Switch<Crumbs>) {
        self.with_port_mut(&target.value, |t| t.set_hover(target.is_on()))
    }

    /// Update expression type for the particular `ast::Id`.
    #[profile(Debug)]
    fn set_expression_usage_type(&self, crumbs: &Crumbs, tp: &Option<Type>) {
        if let Ok(port) = self.expression.borrow().span_tree.root_ref().get_descendant(crumbs) {
            port.set_usage_type(tp)
        }
    }

    #[profile(Debug)]
    fn set_label_on_new_expression(&self, expression: &Expression) {
        self.label.set_content(expression.viz_code.clone());
    }

    #[profile(Debug)]
    fn build_port_shapes_on_new_expression(
        &self,
        expression: &mut Expression,
        area_frp: &FrpEndpoints,
    ) {
        let mut is_header = true;
        let mut id_crumbs_map = HashMap::new();
        let builder = PortLayerBuilder::empty(&self.ports);
        let code = &expression.viz_code;
        expression.span_tree.root_ref_mut().dfs_with_layer_data(builder, |mut node, builder| {
            let is_parensed = node.is_parensed();
            let skip_opr = if SKIP_OPERATIONS {
                node.is_operation() && !is_header
            } else {
                let crumb = ast::Crumb::Infix(ast::crumbs::InfixCrumb::Operator);
                node.ast_crumbs.last().map(|t| t == &crumb) == Some(true)
            };

            let not_a_port = node.is_positional_insertion_point()
                || node.is_chained()
                || (node.is_root() && !node.children.is_empty())
                || skip_opr
                || node.is_token()
                || builder.parent_parensed;

            if let Some(id) = node.ast_id {
                if DEBUG {
                    DEBUG!("New id mapping: {id} -> {node.crumbs:?}");
                }
                id_crumbs_map.insert(id, node.crumbs.clone_ref());
            }

            if DEBUG {
                let indent = " ".repeat(4 * builder.depth);
                let skipped = if not_a_port { "(skip)" } else { "" };
                DEBUG!(
                    "{indent}[{node.payload.index},{node.payload.length}] \
                {skipped} {node.kind.variant_name():?} (tp: {node.tp():?}) (id: {node.ast_id:?})"
                );
            }

            let range_before_start = node.payload.index - node.payload.local_index;
            let range_before_end = node.payload.index;
            let range_before = enso_text::Range::new(range_before_start, range_before_end);
            let local_char_offset = code[range_before].chars().count();

            let new_parent = if not_a_port {
                builder.parent.clone_ref()
            } else {
                let port = &mut node;

                let index = local_char_offset + builder.shift;
                let size = code[port.payload.range()].chars().count();
                let unit = GLYPH_WIDTH;
                let width = unit * size as f32;
                let width_padded = width + 2.0 * PORT_PADDING_X;
                let height = 18.0;
                let padded_size = Vector2(width_padded, height);
                let size = Vector2(width, height);
                let port_shape = port.payload_mut().init_shape(size, node::HEIGHT);

                port_shape.set_x(unit * index as f32);
                if DEBUG {
                    port_shape.set_y(DEBUG_PORT_OFFSET)
                }

                if is_header {
                    is_header = false;
                    self.header.add_child(&port_shape);
                } else {
                    builder.parent.add_child(&port_shape);
                }

                // TODO: StyleWatch is unsuitable here, as it was designed as an internal tool for
                //   shape system. (https://www.pivotaltracker.com/story/show/183567648)
                let style_sheet = &self.app.display.default_scene.style_sheet;
                let styles = StyleWatch::new(style_sheet);
                let styles_frp = &self.styles_frp;
                let any_type_sel_color = styles_frp.get_color(theme::code::types::any::selection);
                let crumbs = port.crumbs.clone_ref();
                let port_network = &port.network;

                frp::extend! { port_network

                    // === Aliases ===

                    let mouse_over_raw = port_shape.hover.events.mouse_over.clone_ref();
                    let mouse_out      = port_shape.hover.events.mouse_out.clone_ref();
                    let mouse_down_raw = port_shape.hover.events.mouse_down_primary.clone_ref();


                    // === Body Hover ===

                    // This is meant to be on top of FRP network. Read more about `Node` docs to
                    // learn more about the architecture and the importance of the hover
                    // functionality.

                    // Please note, that this is computed first in order to compute `ports_visible`
                    // when needed, and thus it has to be run before the following lines.
                    area_frp.source.body_hover <+ bool(&mouse_out,&mouse_over_raw);

                    // TODO[WD] for FRP3: Consider the following code. Here, we have to first
                    //     handle `bg_down` and then `mouse_down`. Otherwise, `mouse_down` may
                    //     trigger some events and can change `ports_visible` status, and thus make
                    //     the `bg_down` emitted unnecessarily. For example, after plugging in
                    //     connections to selected port, the `ports_visible` will be set to `false`,
                    //     and `bg_down` will be emitted, causing the node to be selected. This can
                    //     be solved by solving in the FRP engine all children first, and then their
                    //     children (then both `bg_down` and `mouse_down` will be resolved before
                    //     the `ports_visible` changes).
                    bg_down    <- mouse_down_raw.gate_not(&area_frp.ports_visible);
                    mouse_down <- mouse_down_raw.gate(&area_frp.ports_visible);
                    mouse_over <- mouse_over_raw.gate(&area_frp.ports_visible);
                    area_frp.source.on_background_press <+ bg_down;


                    // === Press ===

                    area_frp.source.on_port_press <+ mouse_down.map(f_!([crumbs] crumbs.clone_ref()));

                    // === Hover ===

                    hovered <- bool(&mouse_out,&mouse_over);
                    hover   <- hovered.map (f!([crumbs](t) Switch::new(crumbs.clone_ref(),*t)));
                    area_frp.source.on_port_hover <+ hover;


                    // === Pointer Style ===

                    let port_shape_hover = port_shape.hover.clone_ref();
                    pointer_style_out   <- mouse_out.map(|_| default());

                    init_color         <- source::<()>();
                    any_type_sel_color <- all_with(&any_type_sel_color,&init_color,
                        |c,_| color::Lcha::from(c));
                    tp                 <- all_with(&port.tp,&area_frp.set_ports_active,
                        |tp,(_,edge_tp)| tp.clone().or_else(||edge_tp.clone()));
                    tp_color           <- tp.map(
                        f!([styles](tp) tp.map_ref(|tp| type_coloring::compute(tp,&styles))));
                    tp_color           <- all_with(&tp_color,&any_type_sel_color,
                        |tp_color,any_type_sel_color| tp_color.unwrap_or(*any_type_sel_color));
                    in_profiling_mode  <- area_frp.view_mode.map(|m| matches!(m,view::Mode::Profiling));
                    pointer_color_over <- in_profiling_mode.switch(&tp_color,&any_type_sel_color);
                    pointer_style_over <- pointer_color_over.map(move |color|
                        cursor::Style::new_highlight(&port_shape_hover,padded_size,Some(color))
                    );
                    pointer_style_over <- pointer_style_over.sample(&mouse_over);

                    pointer_style_hover <- any(pointer_style_over,pointer_style_out);
                    pointer_styles      <- all[pointer_style_hover,self.label.pointer_style];
                    pointer_style       <- pointer_styles.fold();
                    area_frp.source.pointer_style <+ pointer_style;
                }
                init_color.emit(());
                area_frp.set_view_mode.emit(area_frp.view_mode.value());
                port_shape.display_object().clone_ref()
            };

            if let Some(parent_frp) = &builder.parent_frp {
                frp::extend! { port_network
                    node.frp.set_active           <+ parent_frp.set_active;
                    node.frp.set_hover            <+ parent_frp.set_hover;
                    node.frp.set_parent_connected <+ parent_frp.set_parent_connected;
                }
            }
            let new_parent_frp = Some(node.frp.output.clone_ref());
            let new_shift = if !not_a_port { 0 } else { builder.shift + local_char_offset };
            builder.nested(new_parent, new_parent_frp, is_parensed, new_shift)
        });
        *self.id_crumbs_map.borrow_mut() = id_crumbs_map;
    }

    /// Initializes FRP network for every port. Please note that the networks are connected
    /// hierarchically (children get events from parents), so it is easier to init all networks
    /// this way, rather than delegate it to every port.
    #[profile(Debug)]
    fn init_port_frp_on_new_expression(
        &self,
        expression: &mut Expression,
        area_frp: &FrpEndpoints,
    ) {
        let model = &self;

        let parent_tp: Option<frp::Stream<Option<Type>>> = None;
        expression.root_ref_mut().dfs_with_layer_data(parent_tp, |node, parent_tp| {
            let frp = &node.frp;
            let port_network = &frp.network;
            let is_token = node.is_token();
            let crumbs = node.crumbs.clone();


            // === Type Computation ===

            let parent_tp = parent_tp.clone().unwrap_or_else(|| {
                frp::extend! { port_network
                    empty_parent_tp <- source::<Option<Type>>();
                }
                empty_parent_tp.into()
            });
            frp::extend! { port_network
                final_tp <- all_with3(&parent_tp,&frp.set_definition_type,&frp.set_usage_type,
                    move |parent_tp,def_tp,usage_tp| {
                        usage_tp.clone().or_else(||
                            if is_token {parent_tp.clone()} else {def_tp.clone()}
                        )
                    }
                );
                frp.source.tp <+ final_tp;

                area_frp.source.on_port_type_change <+ frp.tp.map(move |t|(crumbs.clone(),t.clone()));
            }


            // === Code Coloring ===

            let styles = model.styles.clone_ref();
            let styles_frp = model.styles_frp.clone_ref();

            if node.children.is_empty() {
                let is_expected_arg = node.is_expected_argument();

                use theme::code::syntax;
                let selected_color = styles_frp.get_color(theme::code::types::selected);
                let std_base_color = styles_frp.get_color(syntax::base);
                let std_disabled_color = styles_frp.get_color(syntax::disabled);
                let std_expected_color = styles_frp.get_color(syntax::expected);
                let std_editing_color = styles_frp.get_color(syntax::base);
                let profiled_base_color = styles_frp.get_color(syntax::profiling::base);
                let profiled_disabled_color = styles_frp.get_color(syntax::profiling::disabled);
                let profiled_expected_color = styles_frp.get_color(syntax::profiling::expected);
                let profiled_editing_color = styles_frp.get_color(syntax::profiling::base);

                frp::extend! { port_network
                    in_profiling_mode <- area_frp.view_mode.map(|m| m.is_profiling());
                    finished          <- area_frp.set_profiling_status.map(|s| s.is_finished());
                    profiled          <- in_profiling_mode && finished;
                    selected          <- frp.set_hover || frp.set_parent_connected;

                    init_colors         <- source::<()>();
                    std_base_color      <- all(std_base_color,init_colors)._0();
                    profiled_base_color <- all(profiled_base_color,init_colors)._0();

                    profiling_color <- finished.switch(&std_base_color,&profiled_base_color);
                    normal_color    <- frp.tp.map(f!([styles](t)
                        color::Rgba::from(type_coloring::compute_for_code(t.as_ref(),&styles))));
                    base_color      <- in_profiling_mode.switch(&normal_color,&profiling_color);

                    disabled_color <- profiled.switch(&std_disabled_color,&profiled_disabled_color);
                    expected_color <- profiled.switch(&std_expected_color,&profiled_expected_color);
                    editing_color  <- profiled.switch(&std_editing_color,&profiled_editing_color);
                    // TODO: `label_color` should be animated, when when we can set text colors
                    //  more efficiently. (See https://www.pivotaltracker.com/story/show/183567665)
                    label_color <- all_with8(
                        &area_frp.editing,
                        &selected,
                        &area_frp.set_disabled,
                        &editing_color,
                        &selected_color,
                        &disabled_color,
                        &expected_color,
                        &base_color,
                        move |&editing,
                              &selected,
                              &disabled,
                              &editing_color,
                              &selected_color,
                              &disabled_color,
                              &expected_color,
                              &base_color| {
                            if editing {
                                color::Lcha::from(editing_color)
                            } else if selected {
                                color::Lcha::from(selected_color)
                            } else if disabled {
                                color::Lcha::from(disabled_color)
                            } else if is_expected_arg {
                                color::Lcha::from(expected_color)
                            } else {
                                color::Lcha::from(base_color)
                            }
                        },
                    );
                }

                let index = node.payload.index;
                let length = node.payload.length;
                let label = model.label.clone_ref();
                frp::extend! { port_network
                    eval label_color ([label](color) {
                        let range = enso_text::Range::new(index, index + length);
                        // TODO: remove unwrap. (https://www.pivotaltracker.com/story/show/183567590)
                        let range = enso_text::Range::<Byte>::try_from(range).unwrap();
                        label.set_property(range,color::Rgba::from(color));
                    });
                }

                init_colors.emit(());
                area_frp.set_view_mode(area_frp.view_mode.value());
            }


            // === Highlight Coloring ===

            if let Some(port_shape) = &node.payload.shape {
                let viz_color = color::Animation::new(port_network);
                let any_type_sel_color = styles_frp.get_color(theme::code::types::any::selection);

                frp::extend! { port_network
                    normal_viz_color <- all_with(&frp.tp,&frp.set_connected,
                        f!([styles](port_tp,(_,edge_tp)) {
                            let tp = port_tp.as_ref().or(edge_tp.as_ref());
                            select_color(&styles,tp)
                        }));
                    init_color          <- source::<()>();
                    profiling_viz_color <- all_with(&any_type_sel_color,&init_color,
                        |c,_| color::Lcha::from(c));
                    profiling           <- area_frp.view_mode.map(|m| m.is_profiling());
                    connected_viz_color <- profiling.switch(&normal_viz_color,&profiling_viz_color);
                    is_connected        <- frp.set_connected.map(|(is_connected,_)| *is_connected);
                    transparent         <- init_color.constant(color::Lcha::transparent());
                    viz_color_target    <- is_connected.switch(&transparent,&connected_viz_color);

                    // We need to make sure that the network contains correct values before we
                    // connect the `viz_color` animation. The reason is that the animation will
                    // start from the first value that it receives, and during initialization of the
                    // network, while some nodes are still set to their defaults, this first  value
                    // would be incorrect, causing the animation in some cases to start from black
                    // (the default color) and animating towards the color that we really want to
                    // set.
                    init_color.emit(());

                    viz_color.target    <+ viz_color_target;
                    eval viz_color.value ((t)
                        port_shape.viz.color.set(color::Rgba::from(t).into())
                    );
                }
            }
            Some(frp.tp.clone_ref().into())
        });

        area_frp.set_view_mode(area_frp.view_mode.value());
    }

    /// This function first assigns the new expression to the model and then emits the definition
    /// type signals to all port FRP networks.
    ///
    /// As a design note, it is important to first assign the expression to the model, as the FRP
    /// signals can cause other parts of the network to fire, which may query the expression types.
    /// For example, firing the `port::set_definition_type` will fire `on_port_type_change`, which
    /// may require some edges to re-color, which consequently will require to checking the current
    /// expression types.
    #[profile(Debug)]
    fn init_new_expression(&self, expression: Expression) {
        *self.expression.borrow_mut() = expression;
        let expression = self.expression.borrow();
        expression.root_ref().dfs_with_layer_data((), |node, _| {
            node.frp.set_definition_type(node.tp().cloned().map(|t| t.into()));
        });
    }

    /// Set a displayed expression, updating the input ports. `is_editing` indicates whether the
    /// expression is being edited by the user.
    #[profile(Debug)]
    fn set_expression(
        &self,
        new_expression: impl Into<node::Expression>,
        is_editing: bool,
        area_frp: &FrpEndpoints,
    ) -> Expression {
        let mut new_expression = Expression::from(new_expression.into());
        if DEBUG {
            DEBUG!("\n\n=====================\nSET EXPR: " new_expression.code)
        }
        self.set_label_on_new_expression(&new_expression);
        self.build_port_shapes_on_new_expression(&mut new_expression, area_frp);
        self.init_port_frp_on_new_expression(&mut new_expression, area_frp);
        self.init_new_expression(new_expression.clone());
        if is_editing {
            self.label.set_cursor_at_text_end();
        }
        new_expression
    }
}

fn select_color(styles: &StyleWatch, tp: Option<&Type>) -> color::Lcha {
    let opt_color = tp.as_ref().map(|tp| type_coloring::compute(tp, styles));
    opt_color.unwrap_or_else(|| styles.get_color(theme::code::types::any::selection).into())
}



// ===========
// === FRP ===
// ===========

ensogl::define_endpoints! {
    Input {
        /// Set the node expression.
        set_expression (node::Expression),

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

        /// Set the connection status of the port indicated by the breadcrumbs. The optional type
        /// is the type of the edge that was connected or disconnected if the edge was typed.
        set_connected (Crumbs,Option<Type>,bool),

        /// Set the expression USAGE type. This is not the definition type, which can be set with
        /// `set_expression` instead. In case the usage type is set to None, ports still may be
        /// colored if the definition type was present.
        set_expression_usage_type (Crumbs,Option<Type>),

        /// Enable / disable port hovering. The optional type indicates the type of the active edge
        /// if any. It is used to highlight ports if they are missing type information or if their
        /// types are polymorphic.
        set_ports_active (bool,Option<Type>),

        set_view_mode        (view::Mode),
        set_profiling_status (profiling::Status),
    }

    Output {
        pointer_style       (cursor::Style),
        width               (f32),
        expression          (ImString),
        editing             (bool),
        ports_visible       (bool),
        body_hover          (bool),
        on_port_press       (Crumbs),
        on_port_hover       (Switch<Crumbs>),
        on_port_type_change (Crumbs,Option<Type>),
        on_background_press (),
        view_mode           (view::Mode),
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
#[derive(Clone, CloneRef, Debug)]
pub struct Area {
    #[allow(missing_docs)]
    pub frp:          Frp,
    pub(crate) model: Rc<Model>,
}

impl Deref for Area {
    type Target = Frp;
    fn deref(&self) -> &Self::Target {
        &self.frp
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

            // === Body Hover ===
            // This is meant to be on top of FRP network. Read more about `Node` docs to
            // learn more about the architecture and the importance of the hover
            // functionality.

            frp.output.source.body_hover <+ frp.set_hover;


            // === Cursor setup ===

            eval frp.input.set_editing ([model](edit_mode) {
                model.label.deprecated_set_focus(edit_mode);
                if *edit_mode {
                    // Reset the code to hide non-connected port names.
                    model.label.set_content(model.expression.borrow().code.clone());
                    model.label.set_cursor_at_mouse_position();
                } else {
                    model.label.remove_all_cursors();
                }
            });


            // === Show / Hide Phantom Ports ===

            edit_ready_mode <- all_with3
                ( &frp.input.set_editing
                , &frp.input.set_edit_ready_mode
                , &frp.input.set_ports_active
                , |editing, edit_ready_mode, (set_ports_active, _)|
                     (*editing || *edit_ready_mode) && !set_ports_active
                );

            port_vis <- all_with(&frp.input.set_ports_active,&edit_ready_mode,|(a,_),b|*a&&(!b));
            frp.output.source.ports_visible <+ port_vis;
            frp.output.source.editing       <+ frp.set_editing;


            // === Label Hover ===

            label_hovered <- edit_ready_mode && frp.output.body_hover;
            eval label_hovered ((t) model.label.set_hover(t));


            // === Port Hover ===

            eval frp.on_port_hover ((t) model.set_port_hover(t));

            eval frp.set_connected ([model]((crumbs,edge_tp,is_connected)) {
                model.with_port_mut(crumbs,|n|n.set_connected(is_connected,edge_tp));
                model.with_port_mut(crumbs,|n|n.set_parent_connected(is_connected));
            });


            // === Properties ===

            width <- model.label.width.map(|t| t + 2.0 * TEXT_OFFSET);
            frp.output.source.width      <+ width;


            // === Expression ===

            let frp_endpoints = &frp.output;
            expression <- frp.input.set_expression.map2(
                &frp.input.set_editing, f!([frp_endpoints, model](expr, is_editing)
                    model.set_expression(expr, *is_editing, &frp_endpoints)
                )
            );
            frp.output.source.expression <+ expression.map(|e| e.code.clone_ref());
            expression_changed_by_user <- model.label.content.gate(&frp.input.set_editing);
            frp.output.source.expression <+ expression_changed_by_user.ref_into();


            // === Expression Type ===

            eval frp.set_expression_usage_type (((a,b)) model.set_expression_usage_type(a,b));


            // === View Mode ===

            frp.output.source.view_mode <+ frp.set_view_mode;

            in_profiling_mode <- frp.view_mode.map(|m| m.is_profiling());
            finished          <- frp.set_profiling_status.map(|s| s.is_finished());
            profiled          <- in_profiling_mode && finished;

            use theme::code::syntax;
            let std_selection_color      = model.styles_frp.get_color(syntax::selection);
            let profiled_selection_color = model.styles_frp.get_color(syntax::profiling::selection);
            let std_base_color           = model.styles_frp.get_color(syntax::base);
            let profiled_base_color      = model.styles_frp.get_color(syntax::profiling::base);

            selection_color_rgba <- profiled.switch(&std_selection_color,&profiled_selection_color);

            selection_color.target          <+ selection_color_rgba.map(|c| color::Lcha::from(c));
            model.label.set_selection_color <+ selection_color.value.map(|c| color::Lch::from(c));

            init_colors         <- source::<()>();
            std_base_color      <- all(std_base_color,init_colors)._0();
            profiled_base_color <- all(profiled_base_color,init_colors)._0();
            base_color          <- profiled.switch(&std_base_color,&profiled_base_color);
            eval base_color ((color) model.label.set_property_default(color));
            init_colors.emit(());
        }

        Self { frp, model }
    }

    /// An offset from node position to a specific port.
    pub fn port_offset(&self, crumbs: &[Crumb]) -> Option<Vector2<f32>> {
        let expr = self.model.expression.borrow();
        expr.root_ref().get_descendant(crumbs).ok().map(|node| {
            let unit = GLYPH_WIDTH;
            let range_before = enso_text::Range::new(ByteDiff(0), node.payload.index);
            let char_offset = expr.viz_code[range_before].chars().count();
            let char_count = expr.viz_code[node.payload.range()].chars().count();
            let width = unit * (char_count as f32);
            let x = width / 2.0 + unit * (char_offset as f32);
            Vector2::new(TEXT_OFFSET + x, 0.0)
        })
    }

    /// A type of the specified port.
    pub fn port_type(&self, crumbs: &Crumbs) -> Option<Type> {
        let expression = self.model.expression.borrow();
        expression.span_tree.root_ref().get_descendant(crumbs).ok().and_then(|t| t.tp.value())
    }

    /// A crumb by AST id.
    pub fn get_crumbs_by_id(&self, id: ast::Id) -> Option<Crumbs> {
        self.model.id_crumbs_map.borrow().get(&id).cloned()
    }

    /// Set a scene layer for text rendering.
    pub fn set_label_layer(&self, layer: &display::scene::Layer) {
        self.model.set_label_layer(layer);
    }
}



// ==========================
// === Expression Setting ===
// ==========================

/// Helper struct used to keep information about the current expression layer when building visual
/// port representation. A "layer" is a visual layer in terms of span tree. For example, given
/// expression `img.blur (foo (bar baz))`, we've got several layers, like the whole expression,
/// `img.blur`, `foo (bar baz)`, or `(bar baz)`. The layer builder keeps information passed from the
/// parent layer when building the nested one.
#[derive(Clone, Debug)]
struct PortLayerBuilder {
    parent_frp:      Option<port::FrpEndpoints>,
    /// Parent port display object.
    parent:          display::object::Instance,
    /// Information whether the parent port was a parensed expression.
    parent_parensed: bool,
    /// The number of chars the expression should be shifted. For example, consider
    /// `(foo bar)`, where expression `foo bar` does not get its own port, and thus a 1 char
    /// shift should be applied when considering its children.
    shift:           usize,
    /// The depth at which the current expression is, where root is at depth 0.
    depth:           usize,
}

impl PortLayerBuilder {
    /// Constructor.
    #[profile(Debug)]
    fn new(
        parent: impl display::Object,
        parent_frp: Option<port::FrpEndpoints>,
        parent_parensed: bool,
        shift: usize,
        depth: usize,
    ) -> Self {
        let parent = parent.display_object().clone_ref();
        Self { parent_frp, parent, parent_parensed, shift, depth }
    }

    fn empty(parent: impl display::Object) -> Self {
        Self::new(parent, default(), default(), default(), default())
    }

    /// Create a nested builder with increased depth and updated `parent_frp`.
    #[profile(Debug)]
    fn nested(
        &self,
        parent: display::object::Instance,
        new_parent_frp: Option<port::FrpEndpoints>,
        parent_parensed: bool,
        shift: usize,
    ) -> Self {
        let depth = self.depth + 1;
        let parent_frp = new_parent_frp.or_else(|| self.parent_frp.clone());
        Self::new(parent, parent_frp, parent_parensed, shift, depth)
    }
}

impl display::Object for Area {
    fn display_object(&self) -> &display::object::Instance {
        &self.model.display_object
    }
}
