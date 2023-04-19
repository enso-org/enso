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

/// The minimum size of the port visual area.
pub const BASE_PORT_HEIGHT: f32 = 18.0;

/// The vertical hover padding of ports at low depth. It affects how the port hover should extend
/// the target text boundary on both sides.
pub const PRIMARY_PORT_HOVER_PADDING_Y: f32 = 4.0;

/// The maximum depth of the widget port that is still considered primary. This is used to determine
/// the hover area of the port.
pub const PRIMARY_PORT_MAX_DEPTH: usize = 0;



// =============
// === Shape ===
// =============

/// Shapes the port is build from. It consist of the `hover_shape`, which represents a hover area of
/// a height dependent on logical widget depth, and the `shape`, which is a nice, visual highlight
/// representation with padding extending out of the widget bounding box. Both shapes are children
/// of `Port`'s `port_root` display object:
///
/// ```text
///     hover_shape (appears when hovering over the node while dragging an edge)
///      ◄──────►
/// ╭───┬────────┬──┄
/// │   │╭──────╮│▼ shape
/// │   │╰──────╯│▲
/// ╰───┴────────┴──┄
/// ```
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
            let hover_shape = Rect(size).fill(transparent);
            hover_shape.into()
        }
    }
}

/// An scene extension that holds the partitions of hover shapes for all ports. This is used to
/// visually sort the ports based on port depth in the widget tree.
#[derive(Clone, CloneRef)]
struct HoverLayers {
    hover_layer:      display::scene::Layer,
    hover_partitions: Rc<RefCell<Vec<LayerSymbolPartition<hover_shape::Shape>>>>,
}

impl display::scene::Extension for HoverLayers {
    fn init(scene: &display::Scene) -> Self {
        let hover_layer = scene.layers.main.clone_ref();
        Self { hover_layer, hover_partitions: default() }
    }
}

impl HoverLayers {
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


// ============
// === Port ===
// ============

/// A port on the node side. It can be connected to other ports.
#[derive(Debug)]
pub struct Port {
    #[allow(dead_code)]
    on_cleanup:      frp::DropSource,
    crumbs:          Rc<RefCell<span_tree::Crumbs>>,
    port_root:       display::object::Instance,
    widget_root:     display::object::Instance,
    widget:          DynWidget,
    port_shape:      shape::View,
    hover_shape:     hover_shape::View,
    current_depth:   usize,
    current_primary: bool,
}

impl Port {
    /// Create a new port for given widget. The widget will be placed as a child of the port's
    /// `port_root` display object, and its layout size will be used to determine the port's size.
    pub fn new(widget: DynWidget, app: &Application, frp: &WidgetsFrp) -> Self {
        let port_root = display::object::Instance::new();
        let widget_root = widget.root_object().clone_ref();
        let port_shape = shape::View::new();
        let hover_shape = hover_shape::View::new();

        port_root.add_child(&widget_root);
        widget_root.set_margin_left(0.0);
        port_shape
            .set_size_y(BASE_PORT_HEIGHT)
            .allow_grow()
            .set_margin_left(-PORT_PADDING_X)
            .set_margin_right(-PORT_PADDING_X)
            .set_alignment_left_center();
        hover_shape
            .set_size_y(BASE_PORT_HEIGHT)
            .allow_grow()
            .set_margin_left(-PORT_PADDING_X)
            .set_margin_right(-PORT_PADDING_X)
            .set_alignment_left_center();

        let layers = app.display.default_scene.extension::<HoverLayers>();
        layers.add_to_partition(hover_shape.display_object(), 0);

        let mouse_enter = hover_shape.on_event::<mouse::Enter>();
        let mouse_leave = hover_shape.on_event::<mouse::Leave>();
        let mouse_down = hover_shape.on_event::<mouse::Down>();

        let crumbs = Rc::new(RefCell::new(span_tree::Crumbs::default()));

        if frp.set_ports_visible.value() {
            port_root.add_child(&hover_shape);
        }

        let network = &port_root.network;
        frp::extend! { network
            on_cleanup <- on_drop();
            hovering <- bool(&mouse_leave, &mouse_enter);
            cleanup_hovering <- on_cleanup.constant(false);
            hovering <- any(&hovering, &cleanup_hovering);
            hovering <- hovering.on_change();

            frp.on_port_hover <+ hovering.map(
                f!([crumbs](t) Switch::new(crumbs.borrow().clone(),*t))
            );

            frp.on_port_press <+ mouse_down.map(f!((_) crumbs.borrow().clone()));
            eval frp.set_ports_visible([port_root, hover_shape] (active) {
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
            crumbs,
            current_primary: false,
            current_depth: 0,
        }
    }

    /// Configure the port and its attached widget.
    pub fn configure(&mut self, config: &Config, ctx: ConfigContext) {
        self.crumbs.replace(ctx.span_tree_node.crumbs.clone());
        self.set_connected(ctx.state.connection);
        self.set_port_layout(&ctx);
        self.widget.configure(config, ctx);
        self.update_root();
    }

    /// Update connection status of this port. Changing the connection status will add or remove the
    /// port's visible shape from the display hierarchy.
    fn set_connected(&self, status: ConnectionStatus) {
        match status {
            ConnectionStatus::Connected(data) => {
                self.port_root.add_child(&self.port_shape);
                self.port_shape.color.set(color::Rgba::from(data.color).into())
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
        if self.current_depth != node_depth {
            self.current_depth = node_depth;
            let layers = ctx.app().display.default_scene.extension::<HoverLayers>();
            layers.add_to_partition(self.hover_shape.display_object(), node_depth);
        }

        #[allow(clippy::absurd_extreme_comparisons)]
        let is_primary = ctx.state.depth <= PRIMARY_PORT_MAX_DEPTH;
        if self.current_primary != is_primary {
            self.current_primary = is_primary;
            let margin = if is_primary { -PRIMARY_PORT_HOVER_PADDING_Y } else { 0.0 };
            self.hover_shape.set_margin_top(margin);
            self.hover_shape.set_margin_bottom(margin);
        }
    }

    /// Extract the widget out of the port, dropping the port specific display objects. The widget
    /// can be reinserted into the display hierarchy of widget tree.
    pub(super) fn into_widget(self) -> DynWidget {
        self.widget
    }

    /// Get the port's hover shape. Used for testing to simulate mouse events.
    pub fn hover_shape(&self) -> &hover_shape::View {
        &self.hover_shape
    }
}

impl display::Object for Port {
    fn display_object(&self) -> &display::object::Instance {
        self.port_root.display_object()
    }
}
