//! Example scene showing simple usage of a shape system.

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]

use ensogl_core::display::shape::*;
use ensogl_core::display::world::*;
use ensogl_core::prelude::*;

use ensogl_core::data::color;
use ensogl_core::display::navigation::navigator::Navigator;
use ensogl_core::display::object::ObjectOps;
use ensogl_core::display::scene;
use ensogl_core::display::style::theme;
use ensogl_core::system::web;



// ==============
// === Shapes ===
// ==============

mod shape {
    // This scene uses the `shape_old` definition because it demonstrates dynamic theme changes
    // which is currently not working with shader-precompilation.
    use super::*;
    ensogl_core::shape_old! {
        (style:Style) {
            let base_color = style.get_color("base_color");
            let circle1    = Circle(50.px());
            let circle_bg  = circle1.translate_x(-(50.0.px()));
            let circle_sub = circle1.translate_y(-(50.0.px()));
            let rect       = Rect((100.0.px(),100.0.px()));
            let shape      = circle_bg + rect - circle_sub;
            let shape      = shape.fill(base_color);
            shape.into()
        }
    }
}

mod mask {
    use super::*;
    ensogl_core::shape_old! {
        (style:Style) {
            let shape = Circle(60.px());
            let shape = shape.fill(color::Rgb::new(1.0,0.0,0.0));
            shape.into()
        }
    }
}



// ===================
// === Entry Point ===
// ===================

/// The example entry point.
#[entry_point]
#[allow(dead_code)]
pub fn main() {
    let world = World::new().displayed_in("root");
    let scene = &world.default_scene;
    let camera = scene.camera().clone_ref();
    let navigator = Navigator::new(scene, &camera);
    let theme_manager = theme::Manager::from(&scene.style_sheet);

    let theme1 = theme::Theme::new();
    theme1.set("base_color", color::Rgba::new(0.0, 0.0, 1.0, 1.0));
    theme1.set("animation.duration", 0.5);
    theme1.set("graph.node.shadow.color", 5.0);
    theme1.set("graph.node.shadow.size", 5.0);
    theme1.set("mouse.pointer.color", color::Rgba::new(0.3, 0.3, 0.3, 1.0));

    let theme2 = theme::Theme::new();
    theme2.set("base_color", color::Rgba::new(0.0, 1.0, 0.0, 1.0));
    theme2.set("animation.duration", 0.7);
    theme2.set("graph.node.shadow.color", 5.0);
    theme2.set("graph.node.shadow.size", 5.0);
    theme2.set("mouse.pointer.color", color::Rgba::new(0.3, 0.3, 0.3, 1.0));

    theme_manager.register("theme1", theme1);
    theme_manager.register("theme2", theme2);

    theme_manager.set_enabled(["theme1".to_string()]);

    let style_watch = ensogl_core::display::shape::StyleWatch::new(&scene.style_sheet);
    // style_watch.set_on_style_change(|| debug!("Style changed!"));
    style_watch.get("base_color");

    let view1 = shape::View::new();
    view1.set_size((300.0, 300.0));
    view1.set_position(Vector3::new(50.0, 50.0, 0.0));

    let mask_layer = scene.layers.main.create_mask_sublayer("MaskLayer");

    let mask = mask::View::new();
    mask.set_size((300.0, 300.0));
    mask.set_position(Vector3::new(-50.0, 0.0, 0.0));

    let view2 = shape::View::new();
    view2.set_size((300.0, 300.0));
    view2.set_position(Vector3::new(50.0, 0.0, 0.0));

    world.add_child(&view1);
    world.add_child(&mask);

    world.keep_alive_forever();
    let scene = world.default_scene.clone_ref();

    let mut frame = 0;
    world
        .on
        .before_frame
        .add(move |_time| {
            mask.set_x(((frame as f32) / 30.0).sin() * 100.0);
            let _keep_alive = &navigator;
            let _keep_alive = &style_watch;
            let _keep_alive = &theme_manager;
            let _keep_alive = &view1;

            // FIXME[WD]: scissor box should not be computed from the left screen border. It should
            //            be affected by the camera position.
            let screen_size: Vector2 = scene.camera().screen().into();
            let screen_size = screen_size * web::window.device_pixel_ratio() as f32;
            let scissor_box = scene::layer::ScissorBox::new_with_position_and_size(
                Vector2(0, 0),
                Vector2((screen_size.x / 2.0) as i32, screen_size.y as i32),
            );
            scene.layers.main.set_scissor_box(Some(&scissor_box));

            if frame == 50 {
                // These comments are left for easy debugging in the future.
                // debug!("---------------");
                // debug!("{scene.layers.node_searcher:#?}");
                // debug!("{scene.layers.main:#?}");
                // debug!("{scene.layers.mask:#?}");
                // debug!("{scene.layers.node_searcher_mask:#?}");
                // debug!("{scene.layers.viz:#?}");
            }
            if frame == 100 {
                debug!("Adding previously hidden element.");
                scene.add_child(&view2);
                // These comments are left for easy debugging in the future.
                // debug!("---------------");
                // debug!("{scene.layers.node_searcher:#?}");
                // debug!("{scene.layers.main:#?}");
                // debug!("{scene.layers.mask:#?}");
                // debug!("{scene.layers.node_searcher_mask:#?}");
                // debug!("{scene.layers.viz:#?}");
            }
            if frame == 150 {
                debug!("Enabling masking.");
                // These comments are left for easy debugging in the future.
                // debug!("---------------");
                // debug!("{scene.layers.node_searcher:#?}");
                // debug!("{scene.layers.main:#?}");
                // debug!("{scene.layers.mask:#?}");
                // debug!("{scene.layers.node_searcher_mask:#?}");
                // debug!("{scene.layers.viz:#?}");
                scene.layers.main.set_mask(&mask_layer);
                mask_layer.add(&mask);
            }
            if frame == 200 {
                debug!("Changing the theme.");
                theme_manager.set_enabled(["theme2".to_string()]);
            }
            frame += 1;
        })
        .forget();
}
