//! Example scene showing a graph editor edge.
//! - Click-and-drag the source and target node-like objects to see the computed edge shape.
//! - Hover over the edge to see the focused portion.

#![recursion_limit = "256"]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]

use ensogl::display::shape::*;
use ensogl::display::traits::*;
use ensogl::prelude::*;

use ensogl::application::Application;
use ensogl::control::io::mouse;
use ensogl::data::color;
use ensogl::display;
use ensogl::display::navigation::navigator::Navigator;
use ensogl::display::Scene;
use ensogl::frp;
use ensogl::system::web;
use ide_view_graph_editor::component::edge::Edge;
use ide_view_graph_editor::component::node;



// =================
// === Constants ===
// =================

/// Location of the center of the source node-like object, in screen coordinates.
const SOURCE_CENTER: Vector2 = Vector2(0.0, 0.0);
/// Width of the example source node-like object, in pixels.
const ARBITRARY_NODE_WIDTH: f32 = 100.0;
/// The initial value, in pixels, of the offset between the center of the source and center of the
/// target.
const INITIAL_TARGET_OFFSET: Vector2 = Vector2(150.0, -200.0);



// ===================
// === Entry Point ===
// ===================

/// The example entry point.
#[entry_point(graph_editor_edges)]
pub fn main() {
    let app = Application::new("root");

    let world = &app.display;
    let scene = &world.default_scene;
    let navigator = Navigator::new(scene, &scene.layers.node_searcher.camera());
    let greenish = color::Lcha(0.5, 0.5, 0.5, 1.0);
    let lowest_layer = &scene.layers.viz;

    let source = Rectangle::new();
    let source_center = SOURCE_CENTER;
    let source_size = Vector2(ARBITRARY_NODE_WIDTH, node::HEIGHT);
    source.set_size(source_size);
    source.set_color(color::Rgba(0.75, 0.75, 0.75, 1.0));
    source.set_border_color(color::Rgba::transparent());
    source.set_corner_radius_max();
    source.set_xy(source_center - source_size / 2.0);
    lowest_layer.add(&source);
    world.add_child(&source);
    let source_moved = make_draggable(scene, &source);

    let target = Rectangle::new();
    let target_center = source_center + INITIAL_TARGET_OFFSET;
    let target_size = Vector2(node::HEIGHT, node::HEIGHT);
    target.set_size(target_size);
    target.set_color(color::Rgba(0.75, 0.75, 0.75, 1.0));
    target.set_border_color(color::Rgba::transparent());
    target.set_corner_radius_max();
    target.set_xy(target_center - target_size / 2.0);
    lowest_layer.add(&target);
    world.add_child(&target);
    let target_moved = make_draggable(scene, &target);

    let edge = Edge::new(&app);
    edge.set_disabled(false);
    edge.set_color(greenish);
    edge.source_size(source_size);
    edge.target_attached(true);
    edge.source_attached(true);
    world.add_child(&edge);

    let network = edge.network();
    let target_ = target.display_object().clone_ref();
    let source_ = source.display_object().clone_ref();
    frp::extend! { network
        init <- source_();
        _eval <- all_with(&target_moved, &init, f!([edge, target_] (_, _) {
            edge.target_position(target_.xy() + target_size / 2.0)
        }));
        _eval <- all_with(&source_moved, &init, f!([edge, source_] (_, _) {
            edge.set_xy(source_.xy() + source_size / 2.0)
        }));
    }
    init.emit(());

    web::document
        .get_element_by_id("loader")
        .map(|t| t.parent_node().map(|p| p.remove_child(&t).unwrap()));

    mem::forget(app);
    mem::forget(source);
    mem::forget(target);
    mem::forget(edge);
    mem::forget(navigator);
}

fn make_draggable(scene: &Scene, object: impl display::Object) -> frp::Stream<()> {
    let object = object.display_object();
    let mouse_down = object.on_event::<mouse::Down>();
    let mouse_move = object.on_event::<mouse::Move>();
    let mouse_up = scene.on_event::<mouse::Up>();
    let network = &object.network;
    frp::extend! { network
        on_drag <- source::<()>();
        drag_offset <- any(...);
        drag_offset <+ mouse_down.map(f!([object] (e) Some(object.xy() - e.client_centered())));
        drag_offset <+ mouse_up.constant(None);
        _eval <- mouse_move.map2(&drag_offset, f!([object, on_drag] (e, offset) {
            if let Some(offset) = offset {
                object.set_xy(e.client_centered() + offset);
                on_drag.emit(());
            }
        }));
    }
    on_drag.into()
}
