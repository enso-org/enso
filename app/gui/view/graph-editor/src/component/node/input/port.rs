//! Definition of all hardcoded node widget variants and common widget FRP API.

use crate::prelude::*;

use crate::component::node::input::widget::ConfigContext;
use crate::component::node::input::widget::DynConfig;
use crate::component::node::input::widget::DynWidget;
use crate::component::node::input::widget::EdgeData;

use enso_frp as frp;
use ensogl::control::io::mouse;
use ensogl::display;
use ensogl::display::scene::layer::LayerSymbolPartition;
use ensogl::display::shape;
use ensogl::display::shape::compound::rectangle;
use ensogl::display::shape::Rectangle;
use ensogl::display::world::with_context;
use span_tree::PortId;



// =================
// === Constants ===
// =================

/// The default horizontal padding of ports. It affects how the port shape should extend the target
/// text boundary on both sides. Can be overriden per widget by using
/// [`super::ConfigContext::set_port_hover_padding`].
pub const PORT_PADDING_X: f32 = 4.0;

/// The default horizontal padding of port hover areas. It affects how the port hover should extend
/// the target text boundary on both sides.
const HOVER_PADDING_X: f32 = 2.0;

/// The minimum size of the port visual area.
pub const BASE_PORT_HEIGHT: f32 = 24.0;

/// The minimum size of the port hover area.
const BASE_PORT_HOVER_HEIGHT: f32 = 16.0;

/// The vertical hover padding of ports at low depth. It affects how the port hover should extend
/// the target text boundary on both sides.
const PRIMARY_PORT_HOVER_PADDING_Y: f32 = (crate::node::HEIGHT - BASE_PORT_HOVER_HEIGHT) / 2.0;



// ============
// === Port ===
// ============

/// Node of a widget tree that can be a source of an edge. Displays a visual representation of the
/// connection below the widget, and handles mouse hover and click events when an edge is dragged.
#[derive(Debug, display::Object)]
pub struct Port {
    /// Drop source must be kept at the top of the struct, so it will be dropped first.
    _on_cleanup: frp::DropSource,
    port_id:     frp::Source<PortId>,
    #[display_object]
    port_root:   display::object::Instance,
    widget_root: display::object::Instance,
    widget:      DynWidget,
    port_shape:  Rectangle,
    hover_shape: Rectangle,
}

impl Port {
    /// Create a new port for given widget. The widget will be placed as a child of the port's root
    /// display object, and its layout size will be used to determine the port's size.
    pub fn new(widget: DynWidget, ctx: &ConfigContext) -> Self {
        let port_root = display::object::Instance::new_named("Port");
        let widget_root = widget.display_object().clone_ref();
        let port_shape = Rectangle();
        let hover_shape = Rectangle();
        port_shape.set_pointer_events(false).set_corner_radius_max();
        hover_shape.set_pointer_events(true).set_color(shape::INVISIBLE_HOVER_COLOR);
        ctx.layers.hover.add(&hover_shape);

        port_root.add_child(&widget_root);
        widget_root.set_margin_left(0.0);
        port_shape
            .set_size_y(BASE_PORT_HEIGHT)
            .allow_grow()
            .set_margin_xy((-PORT_PADDING_X, 0.0))
            .set_alignment_left_center();
        port_root.add_child(&port_shape);
        hover_shape.set_size_y(BASE_PORT_HOVER_HEIGHT).allow_grow().set_alignment_left_center();

        let mouse_enter = hover_shape.on_event::<mouse::Enter>();
        let mouse_leave = hover_shape.on_event::<mouse::Leave>();
        let mouse_down = hover_shape.on_event::<mouse::Down>();

        let frp = ctx.frp();
        if frp.set_ports_visible.value() {
            port_root.add_child(&hover_shape);
        }

        let port_root_weak = port_root.downgrade();
        let network = &port_root.network;

        frp::extend! { network
            on_cleanup <- on_drop();
            port_id <- source();
            hovering <- bool(&mouse_leave, &mouse_enter);
            cleanup_hovering <- on_cleanup.constant(false);
            hovering <- any(&hovering, &cleanup_hovering);
            hovering <- hovering.on_change();

            frp.on_port_hover <+ hovering.map2(&port_id, |t, id| Switch::new(*id, *t));
            frp.on_port_press <+ port_id.sample(&mouse_down);
            eval frp.set_ports_visible([port_root_weak, hover_shape] (active) {
                if let Some(port_root) = port_root_weak.upgrade() {
                    if *active {
                        port_root.add_child(&hover_shape);
                    } else {
                        port_root.remove_child(&hover_shape);
                    }
                }
            });

            // Port shape is only connected to the display hierarchy when the port is connected.
            // Thus the `on_transformed` event is automatically disabled when the port is not
            // connected.
            let shape_display_object = port_shape.display_object();
            frp.connected_port_updated <+ shape_display_object.on_transformed;
        };

        Self {
            _on_cleanup: on_cleanup,
            port_shape,
            hover_shape,
            widget,
            widget_root,
            port_root,
            port_id,
        }
    }

    /// Configure the port and its attached widget. If the widget has changed its root object after
    /// reconfiguration, the port display object hierarchy will be updated to use it.
    ///
    /// See [`crate::component::node::input::widget`] module for more information about widget
    /// lifecycle.
    pub fn configure(
        &mut self,
        config: &DynConfig,
        ctx: ConfigContext,
        pad_x_override: Option<f32>,
        port_hover_layer: &LayerSymbolPartition<rectangle::Shape>,
    ) {
        port_hover_layer.add(&self.hover_shape);
        match ctx.span_node.port_id {
            Some(id) => self.port_id.emit(id),
            None => error!("Port widget created on node with no port ID assigned."),
        };

        self.set_connected(ctx.info.connection);
        self.set_port_layout(&ctx, pad_x_override.unwrap_or(HOVER_PADDING_X));
        self.widget.configure(config, ctx);
        self.update_root();
    }

    /// Update connection status of this port. Changing the connection status will add or remove the
    /// port's visible shape from the display hierarchy.
    fn set_connected(&self, status: Option<EdgeData>) {
        match status {
            None => {
                // Put the port shape of a disconnected node into the detached layer, instead of
                // removing it from the display hierarchy. This is required for the port shape
                // position and size to be calculated for disconnected ports, as that information
                // is used when hovering over the port with a detached edge.
                with_context(|ctx| ctx.layers.DETACHED.add(&self.port_shape));
            }
            Some(data) => {
                self.port_shape.set_color(data.color.into());
                with_context(|ctx| ctx.layers.DETACHED.remove(&self.port_shape));
            }
        };
    }

    fn update_root(&mut self) {
        let new_root = self.widget.display_object();
        if new_root != &self.widget_root {
            self.port_root.remove_child(&self.widget_root);
            self.port_root.add_child(new_root);
            self.widget_root = new_root.clone_ref();
            self.widget_root.set_margin_left(0.0);
        }
    }

    fn set_port_layout(&mut self, ctx: &ConfigContext, margin_x: f32) {
        let is_primary = ctx.info.nesting_level.is_primary();
        let margin_y = if is_primary { PRIMARY_PORT_HOVER_PADDING_Y } else { 0.0 };

        let current_margin = self.hover_shape.margin();
        let margin_needs_update = current_margin.x().start.as_pixels() != Some(-margin_x)
            || current_margin.y().start.as_pixels() != Some(-margin_y);

        if margin_needs_update {
            self.hover_shape.set_size_y(BASE_PORT_HOVER_HEIGHT + 2.0 * margin_y);
            self.hover_shape.set_margin_xy((-margin_x, -margin_y));
        }
    }

    /// Extract the widget out of the port, dropping the port specific display objects. The widget
    /// can be reinserted into the display hierarchy of widget tree.
    pub(super) fn into_widget(self) -> DynWidget {
        self.widget
    }

    /// Get a reference to a widget currently wrapped by the port. The widget may change during
    /// the next tree rebuild.
    pub(super) fn widget(&self) -> &DynWidget {
        &self.widget
    }

    /// Get a mutable reference to a widget currently wrapped by the port. The widget may change
    /// during the next tree rebuild.
    pub(super) fn widget_mut(&mut self) -> &mut DynWidget {
        &mut self.widget
    }

    /// Get the port's hover shape. Used for testing to simulate mouse events.
    pub fn hover_shape(&self) -> &Rectangle {
        &self.hover_shape
    }

    /// Get the port's visual shape.
    pub fn visual_shape(&self) -> &Rectangle {
        &self.port_shape
    }
}
