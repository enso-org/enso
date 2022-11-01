//! This module provides the [`ScrollArea`] component.

#![recursion_limit = "512"]
// === Features ===
#![feature(option_result_contains)]
#![feature(trait_alias)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]

use ensogl_core::prelude::*;

use enso_frp as frp;
use ensogl_core::application::Application;
use ensogl_core::control::io::mouse;
use ensogl_core::display;
use ensogl_core::display::camera::Camera2d;
use ensogl_core::display::object::ObjectOps;
use ensogl_core::display::scene::layer;
use ensogl_core::display::shape;
use ensogl_scrollbar as scrollbar;
use ensogl_scrollbar::Scrollbar;



// ===========
// === Frp ===
// ===========

ensogl_core::define_endpoints! {
    Input {
        /// Set the width and height in px.
        resize             (Vector2),
        /// Set the corners radius (in pixels) of all corners of the visible area.
        set_corner_radius (f32),
        /// Set the corners radius (in pixels) of the top right corners of the visible area.
        set_corner_radius_top_right  (f32),
        /// Set the corners radius (in pixels) of the top left corners of the visible area.
        set_corner_radius_top_left  (f32),
        /// Set the corners radius (in pixels) of the bottom right corners of the visible area.
        set_corner_radius_bottom_right  (f32),
        /// Set the corners radius (in pixels) of the bottom left corners of the visible area.
        set_corner_radius_bottom_left  (f32),
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
        /// The target of the content's x coordinate animation.
        scroll_position_target_x (f32),
        /// The target of the content's y coordinate animation.
        scroll_position_target_y (f32),
        /// The visible content's height in px.
        scroll_area_height (f32),
        /// Position of the scroll viewport.
        viewport(Viewport)
    }
}

/// Coordinates of the visible viewport of the scroll area.
#[derive(Clone, Debug, Copy, Default)]
#[allow(missing_docs)]
pub struct Viewport {
    pub top:    f32,
    pub bottom: f32,
    pub left:   f32,
    pub right:  f32,
}

impl Viewport {
    /// Return a viewport with given center position and size.
    pub fn from_center_point_and_size(position: Vector2, size: Vector2) -> Self {
        let half_size = size / 2.0;
        let top = position.y + half_size.y;
        let bottom = position.y - half_size.y;
        let left = position.x - half_size.x;
        let right = position.x + half_size.x;
        Self { top, bottom, left, right }
    }

    /// Return a viewport with unchanged size, moved such that it contains the top-left corner of
    /// the other viewport and as much of its area as fits in the size. If there is more than one
    /// such viewport possible, return the one closest to the original viewport.
    pub fn moved_to_contain(self, other: Self) -> Self {
        let size = self.size();
        let top = if self.top < other.top {
            other.top
        } else if self.bottom > other.bottom {
            other.bottom + size.y
        } else {
            self.top
        };
        let bottom = top - size.y;
        let left = if self.left > other.left {
            other.left
        } else if self.right < other.right {
            other.right - size.x
        } else {
            self.left
        };
        let right = left + size.x;
        Self { top, bottom, left, right }
    }


    /// Clamp the given coordinates to this viewport.
    pub fn clamp(&self, pos: Vector2) -> Vector2 {
        let x = pos.x.clamp(self.left, self.right);
        let y = pos.y.clamp(self.bottom, self.top);
        Vector2::new(x, y)
    }

    /// Return whether some object with the given position and size intersects the viewport and thus
    /// should be visible.
    pub fn intersects(&self, pos: Vector2, size: Vector2) -> bool {
        let top = pos.y;
        let bottom = pos.y - size.y;
        let left = pos.x;
        let right = pos.x + size.x;
        !(top < self.bottom || bottom > self.top || left > self.right || right < self.left)
    }

    /// Return Viewport's size.
    pub fn size(&self) -> Vector2 {
        Vector2(self.right - self.left, self.top - self.bottom)
    }

    /// Return the central point of the viewport.
    pub fn center_point(&self) -> Vector2 {
        Vector2(self.left + self.right, self.top + self.bottom) / 2.0
    }
}



// =============
// === Model ===
// =============

/// A mask for clipping the scroll area content.
mod mask {
    use super::*;
    ensogl_core::shape! {
        (style:Style, corner_radius_top_right: f32, corner_radius_top_left: f32,
            corner_radius_bottom_right: f32, corner_radius_bottom_left: f32) {
            let width: Var<Pixels> = "input_size.x".into();
            let height: Var<Pixels> = "input_size.y".into();
            let color = color::Rgba::white();
            let rect = shape::Rect((width, height)).corners_radiuses(
                corner_radius_top_left,
                corner_radius_top_right,
                corner_radius_bottom_left,
                corner_radius_bottom_right,
            );
            rect.fill(color).into()
        }
    }
}

/// Internal representaton of the scroll area.
#[derive(Debug, Clone, CloneRef)]
struct Model {
    content:        display::object::Instance,
    scrollbars:     display::object::Instance,
    display_object: display::object::InstanceWithLayer<layer::Masked>,
    content_layer:  layer::Layer,
    ui_layer:       layer::Layer,
    mask:           mask::View,
    h_scrollbar:    Scrollbar,
    v_scrollbar:    Scrollbar,
}

impl Model {
    fn resize(&self, size: Vector2) {
        self.h_scrollbar.set_position_y(-size.y + scrollbar::WIDTH / 2.0);
        let scrollbar_y = size.x - scrollbar::WIDTH / 2.0 + scrollbar::PADDING / 2.0 + 1.0;
        self.v_scrollbar.set_position_x(scrollbar_y);
        self.h_scrollbar.set_position_x(size.x / 2.0);
        self.v_scrollbar.set_position_y(-size.y / 2.0);
        self.mask.size.set(size);
        self.mask.set_position_x(size.x / 2.0);
        self.mask.set_position_y(-size.y / 2.0);
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
/// respective axis. The component does not have a background.
#[derive(Debug, Clone, CloneRef)]
pub struct ScrollArea {
    model: Model,
    frp:   Frp,
}

impl Deref for ScrollArea {
    type Target = Frp;

    fn deref(&self) -> &Self::Target {
        &self.frp
    }
}

impl display::Object for ScrollArea {
    fn display_object(&self) -> &display::object::Instance {
        &self.model.display_object
    }
}

impl ScrollArea {
    /// Create a new scroll area for use in the given application.
    #[profile(Detail)]
    pub fn new(app: &Application) -> ScrollArea {
        let scene = &app.display.default_scene;
        let display_object = display::object::Instance::new();
        let masked_layer = layer::Masked::new();
        let display_object = display::object::InstanceWithLayer::new(display_object, masked_layer);

        let content_layer = display_object.layer.masked_layer.create_sublayer("content_layer");
        let ui_layer = display_object.layer.masked_layer.create_sublayer("ui_layer");

        let content = display::object::Instance::new();
        display_object.add_child(&content);
        content_layer.add(&content);

        let scrollbars = display::object::Instance::new();
        display_object.add_child(&scrollbars);
        ui_layer.add(&scrollbars);

        let mask = mask::View::new();
        display_object.add_child(&mask);
        display_object.layer.mask_layer.add(&mask);

        let h_scrollbar = Scrollbar::new(app);
        scrollbars.add_child(&h_scrollbar);

        let v_scrollbar = Scrollbar::new(app);
        scrollbars.add_child(&v_scrollbar);
        v_scrollbar.set_rotation_z(-90.0_f32.to_radians());

        let model = Model {
            display_object,
            content,
            v_scrollbar,
            h_scrollbar,
            mask,
            scrollbars,
            content_layer,
            ui_layer,
        };

        let frp = Frp::new();
        let network = &frp.network;

        frp::extend! { network

            // === Size and Position ===

            model.h_scrollbar.set_max        <+ frp.set_content_width;
            model.v_scrollbar.set_max        <+ frp.set_content_height;
            model.h_scrollbar.set_thumb_size <+ frp.resize.map(|size| size.x);
            model.v_scrollbar.set_thumb_size <+ frp.resize.map(|size| size.y);
            model.h_scrollbar.set_length     <+ frp.resize.map(|size| size.x);
            model.v_scrollbar.set_length     <+ frp.resize.map(|size| size.y);
            frp.source.scroll_area_height    <+ frp.resize.map(|size| size.y);

            eval frp.resize((size) model.resize(*size));

            // === Shape ===

            frp.set_corner_radius_top_right <+ frp.set_corner_radius;
            frp.set_corner_radius_bottom_right <+ frp.set_corner_radius;
            frp.set_corner_radius_top_left <+ frp.set_corner_radius;
            frp.set_corner_radius_bottom_left <+ frp.set_corner_radius;
            eval frp.set_corner_radius_top_right((radius)
                model.mask.corner_radius_top_right.set(*radius);
            );
            eval frp.set_corner_radius_bottom_right((radius)
                model.mask.corner_radius_bottom_right.set(*radius);
            );
            eval frp.set_corner_radius_top_left((radius)
                model.mask.corner_radius_top_left.set(*radius);
            );
            eval frp.set_corner_radius_bottom_left((radius)
                model.mask.corner_radius_bottom_left.set(*radius);
            );


            // === Scrolling ===

            model.h_scrollbar.scroll_to <+ frp.scroll_to_x;
            model.v_scrollbar.scroll_to <+ frp.scroll_to_y;
            model.h_scrollbar.jump_to   <+ frp.jump_to_x;
            model.v_scrollbar.jump_to   <+ frp.jump_to_y;

            frp.source.scroll_position_x <+ model.h_scrollbar.thumb_position.map(|x| -x);
            frp.source.scroll_position_y <+ model.v_scrollbar.thumb_position;
            frp.source.scroll_position_target_x <+ model.h_scrollbar.thumb_position_target.map(|x| -x);
            frp.source.scroll_position_target_y <+ model.v_scrollbar.thumb_position_target;

            eval frp.scroll_position_x((&pos) model.content.set_position_x(pos));
            eval frp.scroll_position_y((&pos) model.content.set_position_y(pos));

            scroll_position <- all(&frp.scroll_position_x, &frp.scroll_position_y);
            scroll_position <- scroll_position.map(|(x,y)| Vector2::new(*x,*y));

            let scroll_dimension = frp.resize.clone_ref();

            viewport <- all(&scroll_position,&scroll_dimension);
            viewport <- viewport.map(|(position,dimension)|{
                Viewport{
                    top: -position.y,
                    left: -position.x,
                    right: - position.x + dimension.x,
                    bottom: -position.y - dimension.y,
                }
            });
            frp.source.viewport <+ viewport;

        }


        // === Mouse Wheel ===

        let mouse = &scene.mouse;
        frp::extend! { network
            hovering <- all_with(&mouse.frp.position,&frp.resize,
                f!([scene,model](&pos,&size) {
                    let local_pos = scene.screen_to_object_space(&*model.display_object,pos);
                    (0.0..=size.x).contains(&local_pos.x) && (-size.y..=0.0).contains(&local_pos.y)
                }));
            hovering <- hovering.sampler();
        }

        let mouse_manager = &mouse.mouse_manager;
        let scroll_handler = f!([model](event:&mouse::OnWheel)
            if hovering.value() {
                model.h_scrollbar.scroll_by(event.delta_x() as f32);
                model.v_scrollbar.scroll_by(event.delta_y() as f32);
            }
        );
        let scroll_handler_handle = mouse_manager.on_wheel.add(scroll_handler);
        network.store(&scroll_handler_handle);


        ScrollArea { model, frp }
    }

    /// All objects that should be inside the scroll area and affected by the scrolling, have to be
    /// added as children to `content`.
    pub fn content(&self) -> &display::object::Instance {
        &self.model.content
    }

    /// A scene layer containing the content of the ScrollArea.
    pub fn content_layer(&self) -> &layer::Layer {
        &self.model.content_layer
    }

    /// Set a scene layer for scrollbars.
    pub fn set_scrollbars_layer(&self, layer: &layer::Layer) {
        layer.add(&self.model.scrollbars);
    }

    /// A scene layer used as a mask for the content.
    pub fn mask_layer(&self) -> &layer::Layer {
        &self.model.display_object.layer.mask_layer
    }

    /// Set camera in the every layer handled by this Scroll Area.
    pub fn set_camera(&self, camera: impl Into<Camera2d>) {
        let camera = camera.into();
        self.model.display_object.layer.masked_layer.set_camera(camera.clone_ref());
        self.model.display_object.layer.mask_layer.set_camera(camera.clone_ref());
        self.model.ui_layer.set_camera(camera.clone_ref());
        self.model.content_layer.set_camera(camera.clone_ref());
    }


    /// Return whether some object with the given position and size is visible in the scoll area.
    pub fn is_visible(&self, pos: Vector2, size: Vector2) -> bool {
        use enso_frp::stream::ValueProvider;

        let viewport = self.frp.source.viewport.value();
        viewport.intersects(pos, size)
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_viewport_intersection() {
        let viewport = Viewport { top: -100.0, bottom: -200.5, left: 0.0, right: 400.1 };
        let should_not_intersect =
            [(Vector2::new(200.66666, -250.0), Vector2::new(200.83333, 121.0))];

        for (pos, size) in should_not_intersect.into_iter() {
            assert!(!viewport.intersects(pos, size))
        }

        let should_intersect = [(Vector2::new(0.0, 0.0), Vector2::new(200.00, 200.0))];

        for (pos, size) in should_intersect.into_iter() {
            assert!(viewport.intersects(pos, size))
        }
    }
}
