//! This module provides the [`ScrollArea`] component.

#![feature(option_result_contains)]
#![feature(trait_alias)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]
#![recursion_limit = "512"]

use ensogl_core::prelude::*;

use enso_frp as frp;
use ensogl_core::application::Application;
use ensogl_core::control::callback;
use ensogl_core::control::io::mouse;
use ensogl_core::display;
use ensogl_core::display::object::ObjectOps;
use ensogl_scrollbar as scrollbar;
use ensogl_scrollbar::Scrollbar;



// ===========
// === Frp ===
// ===========

ensogl_core::define_endpoints! {
    Input {
        /// Set the width and height in px.
        resize             (Vector2),
        /// Set the content width in px. Affects how far one can scroll horizontally.
        set_content_width  (f32),
        /// Set the content height in px. Affects how far one can scroll vertically.
        set_content_height (f32),
        /// Scrolls smoothly to the given x coordinate.
        scroll_to_x        (f32),
        /// Scrolls smoothly to the given y coordinate.
        scroll_to_y        (f32),
        /// Jumps instantly to the given x coordinate, without animation.
        jump_to_x          (f32),
        /// Jumps instantly to the given y coordinate, without animation.
        jump_to_y          (f32),
    }
    Output {
        /// The content's x coordinate at the left edge of the area.
        scroll_position_x (f32),
        /// The content's y coordinate at the top edge of the area.
        scroll_position_y (f32),
    }
}



// ===================
// === Scroll Area ===
// ===================

/// This struct provides a scroll area component. It displays two scrollbars, for horizontal and
/// vertical scrolling. Content can be added to the `content` attribute. The content size has to be
/// set through `set_content_height` and `set_content_width`. The component is anchored at the top
/// left corner. All scroll coordinates describe the point of the `content` object at that corner.
/// The scrollbars are only active when the content is actually larger than the viewport on the
/// respective axis.
#[derive(Debug, Clone, CloneRef)]
pub struct ScrollArea {
    /// All objects that should be inside the scroll area and affected by the scrolling, have to be
    /// added as children to `content`.
    pub content:           display::object::Instance,
    display_object:        display::object::Instance,
    h_scrollbar:           Scrollbar,
    v_scrollbar:           Scrollbar,
    scroll_handler_handle: callback::Handle,
    frp:                   Frp,
}

impl Deref for ScrollArea {
    type Target = Frp;

    fn deref(&self) -> &Self::Target {
        &self.frp
    }
}

impl display::Object for ScrollArea {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}

impl ScrollArea {
    /// Create a new scroll area for use in the given application.
    pub fn new(app: &Application) -> ScrollArea {
        let scene = app.display.scene();
        let logger = Logger::new("ScrollArea");
        let display_object = display::object::Instance::new(&logger);

        let content = display::object::Instance::new(&logger);
        display_object.add_child(&content);

        let h_scrollbar = Scrollbar::new(app);
        display_object.add_child(&h_scrollbar);

        let v_scrollbar = Scrollbar::new(app);
        display_object.add_child(&v_scrollbar);
        v_scrollbar.set_rotation_z(-90.0_f32.to_radians());

        let frp = Frp::new();
        let network = &frp.network;

        frp::extend! { network

            // === Size and Position ===

            h_scrollbar.set_max        <+ frp.set_content_width;
            v_scrollbar.set_max        <+ frp.set_content_height;
            h_scrollbar.set_thumb_size <+ frp.resize.map(|size| size.x);
            v_scrollbar.set_thumb_size <+ frp.resize.map(|size| size.y);
            h_scrollbar.set_length     <+ frp.resize.map(|size| size.x);
            v_scrollbar.set_length     <+ frp.resize.map(|size| size.y);

            eval frp.resize([h_scrollbar,v_scrollbar](size) {
                h_scrollbar.set_position_y(-size.y+scrollbar::WIDTH/2.0);
                v_scrollbar.set_position_x(size.x-scrollbar::WIDTH/2.0);
                h_scrollbar.set_position_x(size.x/2.0);
                v_scrollbar.set_position_y(-size.y/2.0);
            });


            // === Scrolling ===

            h_scrollbar.scroll_to <+ frp.scroll_to_x;
            v_scrollbar.scroll_to <+ frp.scroll_to_y;
            h_scrollbar.jump_to   <+ frp.jump_to_x;
            v_scrollbar.jump_to   <+ frp.jump_to_y;

            frp.source.scroll_position_x <+ h_scrollbar.thumb_position.map(|x| -x);
            frp.source.scroll_position_y <+ v_scrollbar.thumb_position;

            eval frp.scroll_position_x((&pos) content.set_position_x(pos));
            eval frp.scroll_position_y((&pos) content.set_position_y(pos));
        }


        // === Mouse Wheel ===

        let mouse = &scene.mouse;
        frp::extend! { network
            hovering <- all_with(&mouse.frp.position,&frp.resize,
                f!([scene,display_object](&pos,&size) {
                    let local_pos = scene.screen_to_object_space(&display_object,pos);
                    (0.0..=size.x).contains(&local_pos.x) && (-size.y..=0.0).contains(&local_pos.y)
                }));
            hovering <- hovering.sampler();
        }

        let mouse_manager = &mouse.mouse_manager;
        let scroll_handler = f!([v_scrollbar,h_scrollbar](event:&mouse::OnWheel)
            if hovering.value() {
                h_scrollbar.scroll_by(event.delta_x() as f32);
                v_scrollbar.scroll_by(event.delta_y() as f32);
            }
        );
        let scroll_handler_handle = mouse_manager.on_wheel.add(scroll_handler);


        ScrollArea { content, display_object, h_scrollbar, v_scrollbar, scroll_handler_handle, frp }
    }
}
