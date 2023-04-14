//! Definition of all hardcoded node widget variants and common widget FRP API.

use crate::prelude::*;

use crate::component::node::input::widget::Config;
use crate::component::node::input::widget::ConfigContext;
use crate::component::node::input::widget::DynWidget;
use crate::component::node::input::widget::SpanWidget;
use crate::component::node::input::widget::WidgetsFrp;
use crate::component::node::ConnectionStatus;
use enso_frp as frp;
use ensogl::application::Application;
use ensogl::control::io::mouse;
use ensogl::data::color;
use ensogl::display;
use ensogl::display::scene::layer::LayerSymbolPartition;



// =================
// === Constants ===
// =================

/// The horizontal padding of ports. It affects how the port hover should extend the target text
/// boundary on both sides.
pub const PORT_PADDING_X: f32 = 4.0;

/// The vertical hover padding of ports at low depth. It affects how the port hover should extend
/// the target text boundary on both sides.
pub const PRIMARY_PORT_HOVER_PADDING_Y: f32 = 4.0;

/// The maximum depth of the widget port that is still considered primary. This is used to determine
/// the hover area of the port.
pub const PRIMARY_PORT_MAX_DEPTH: usize = 0;



// ============
// === Port ===
// ============

/// Visible shape of connected ports.
pub mod shape {
    use super::*;
    ensogl::shape! {
        pointer_events = false;
        (style:Style, color:Vector4) {
            let size = Var::canvas_size();
            let shape_color = Var::<color::Rgba>::from(color);
            let visual_shape = Rect(&size).corners_radius(size.y() / 2.0).fill(shape_color);
            visual_shape.into()
        }
    }
}

/// Hover area of port shape, reacts to mouse when an edge is dragged.
pub mod hover_shape {
    use super::*;
    ensogl::shape! {
        above = [shape];
        (style:Style) {
            let size = Var::canvas_size();
            let transparent = Var::<color::Rgba>::from("srgba(1.0,1.0,1.0,0.00001)");
            let hover_shape = Rect(&size).fill(transparent);
            hover_shape.into()
        }
    }
}

#[derive(Clone, CloneRef)]
struct PortHoverLayers {
    hover_layer:      display::scene::Layer,
    hover_partitions: Rc<RefCell<Vec<LayerSymbolPartition<hover_shape::Shape>>>>,
}

impl display::scene::Extension for PortHoverLayers {
    fn init(scene: &display::Scene) -> Self {
        let hover_layer = scene.layers.main.clone_ref();
        Self { hover_layer, hover_partitions: default() }
    }
}

impl PortHoverLayers {
    fn add_to_partition(&self, object: &display::object::Instance, depth: usize) {
        let mut hover_partitions = self.hover_partitions.borrow_mut();
        if hover_partitions.len() <= depth {
            hover_partitions.resize_with(depth + 1, || {
                self.hover_layer.create_symbol_partition::<hover_shape::Shape>("input port hover")
            })
        }
        hover_partitions[depth].add(object);
    }
}


#[derive(Debug)]
pub(super) struct Port {
    #[allow(dead_code)]
    on_cleanup:      frp::DropSource,
    #[allow(dead_code)]
    network:         frp::Network,
    crumbs:          Rc<RefCell<span_tree::Crumbs>>,
    port_root:       display::object::Instance,
    widget_root:     display::object::Instance,
    widget:          DynWidget,
    port_shape:      shape::View,
    hover_shape:     hover_shape::View,
    last_node_depth: usize,
    is_primary:      bool,
}

impl Port {
    pub fn new(widget: DynWidget, app: &Application, frp: &WidgetsFrp) -> Self {
        let port_root = display::object::Instance::new();
        let widget_root = widget.root_object().clone_ref();
        let port_shape = shape::View::new();
        let hover_shape = hover_shape::View::new();

        port_root.add_child(&widget_root);
        widget_root.set_margin_left(0.0);
        port_shape
            .allow_grow()
            .set_margin_left(-PORT_PADDING_X)
            .set_margin_right(-PORT_PADDING_X)
            .set_alignment_left_center();
        hover_shape
            .allow_grow()
            .set_margin_left(-PORT_PADDING_X)
            .set_margin_right(-PORT_PADDING_X)
            .set_alignment_left_center();

        let layers = app.display.default_scene.extension::<PortHoverLayers>();
        layers.add_to_partition(hover_shape.display_object(), 0);

        let mouse_enter = hover_shape.on_event::<mouse::Enter>();
        let mouse_leave = hover_shape.on_event::<mouse::Leave>();
        let mouse_down = hover_shape.on_event::<mouse::Down>();

        let crumbs = Rc::new(RefCell::new(span_tree::Crumbs::default()));

        if frp.ports_visible.value() {
            port_root.add_child(&hover_shape);
        }

        frp::new_network! { network
            on_cleanup <- on_drop();
            hovering <- bool(&mouse_leave, &mouse_enter);
            cleanup_hovering <- on_cleanup.constant(false);
            hovering <- any(&hovering, &cleanup_hovering);
            hovering <- hovering.on_change();

            frp.on_port_hover <+ hovering.map(
                f!([crumbs](t) Switch::new(crumbs.borrow().clone(),*t))
            );

            frp.on_port_press <+ mouse_down.map(f!((_) crumbs.borrow().clone()));
            eval frp.ports_visible([port_root, hover_shape] (active) {
                if *active {
                    port_root.add_child(&hover_shape);
                } else {
                    port_root.remove_child(&hover_shape);
                }
            });

            // Port shape is only connected to the display hierarchy when the port is connected.
            // Thus the `on_updated` event is automatically disabled when the port is not connected.
            let shape_display_object = port_shape.display_object();
            frp.connected_port_updated <+ shape_display_object.on_transformed;
        };

        Self {
            on_cleanup,
            port_shape,
            hover_shape,
            widget,
            widget_root,
            port_root,
            network,
            crumbs,
            is_primary: false,
            last_node_depth: 0,
        }
    }

    pub fn configure(&mut self, config: &Config, ctx: ConfigContext) {
        self.crumbs.replace(ctx.span_tree_node.crumbs.clone());
        self.set_port_layout(&ctx);
        self.widget.configure(config, ctx);
        self.update_root();
    }

    pub fn set_connected(&self, status: ConnectionStatus) {
        match status {
            ConnectionStatus::Connected { color } => {
                self.port_root.add_child(&self.port_shape);
                self.port_shape.color.set(color::Rgba::from(color).into())
            }
            ConnectionStatus::Disconnected => {
                self.port_root.remove_child(&self.port_shape);
            }
        };
    }

    fn update_root(&mut self) {
        let new_root = self.widget.root_object();
        if new_root != &self.widget_root {
            self.port_root.remove_child(&self.widget_root);
            self.port_root.add_child(new_root);
            self.widget_root = new_root.clone_ref();
        }
    }

    fn set_port_layout(&mut self, ctx: &ConfigContext) {
        let node_depth = ctx.span_tree_node.crumbs.len();
        if self.last_node_depth != node_depth {
            self.last_node_depth = node_depth;
            let layers = ctx.app().display.default_scene.extension::<PortHoverLayers>();
            layers.add_to_partition(self.hover_shape.display_object(), node_depth);
        }

        let is_primary = ctx.depth <= PRIMARY_PORT_MAX_DEPTH;
        if self.is_primary != is_primary {
            self.is_primary = is_primary;
            if is_primary {
                self.hover_shape.set_size_y(crate::node::HEIGHT);
            } else {
                self.hover_shape.set_size_y_to_hug();
            }
        }
    }

    pub fn into_widget(self) -> DynWidget {
        self.widget
    }
}

impl display::Object for Port {
    fn display_object(&self) -> &display::object::Instance {
        self.port_root.display_object()
    }
}
