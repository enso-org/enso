//! Base model for the number and range selector components. Contains all functionality needed by
//! both selectors and can be configured to suit the needs of both.

use crate::prelude::*;
use crate::shape::*;
use ensogl_core::display::shape::*;

use crate::decimal_aligned::FloatLabel;
use crate::Bounds;

use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display;
use ensogl_text as text;



// =================
// === Constants ===
// =================

const LABEL_OFFSET: f32 = 13.0;



// =============
// === Model ===
// =============

/// A Selector Component Model.
#[allow(missing_docs)]
#[derive(Clone, CloneRef, Debug)]
pub struct Model {
    /// Background shape that the other UI elements are placed on.
    pub background:         background::View,
    /// Visual element that indicates where in the available range the selected number or track is
    /// located. Looks like a colored in bit of the background.
    pub track:              track::View,
    /// Invisible UI element that enables mouse interaction with the left end of the track. Will
    /// always be placed on the left edge of the track.
    pub track_handle_left:  io_rect::View,
    /// Invisible UI element that enables mouse interaction with the right end of the track. Will
    /// always be placed on the right edge of the track.
    pub track_handle_right: io_rect::View,
    /// Icon that can be used out of range values. The left overflow is placed on the left side
    /// of the shape and looks like an arrow/triangle pointing left.
    pub left_overflow:      left_overflow::View,
    /// Icon that can be used out of range values. The left overflow is placed on the right side
    /// of the shape and looks like an arrow/triangle pointing right.
    pub right_overflow:     right_overflow::View,
    /// A label that is centered  on the background and can be set to show a floating point value
    /// that is centered on the decimal label.
    pub label:              FloatLabel,
    /// A label that is aligned to the left edge of the background
    pub label_left:         text::Text,
    /// A label that is aligned to the right edge of the background
    pub label_right:        text::Text,
    /// A label that is left aligned on the background. Meant to contain a caption describing the
    /// value that is selected. For example "Alpha", "Red", or "Size".
    pub caption_left:       text::Text,
    /// A label that is centered on the background. Meant to contain a caption describing the
    /// range that is selected. For example "Allowed Size", or "Valid Price".
    pub caption_center:     text::Text,
    /// Shape root that all other elements are parented to. Should be used to place the shapes as
    /// a group.
    pub root:               display::object::Instance,

    background_color: Rc<RefCell<color::Rgba>>,
    track_color: Rc<RefCell<color::Rgba>>,
    background_left_corner_roundness: Rc<Cell<bool>>,
    background_right_corner_roundness: Rc<Cell<bool>>,
    padding: Rc<Cell<f32>>,

    pub app: Application,
}

#[allow(missing_docs)]
impl Model {
    pub fn new(app: &Application) -> Self {
        let root = display::object::Instance::new();
        let label = FloatLabel::new(app);
        let label_left = app.new_view::<text::Text>();
        let label_right = app.new_view::<text::Text>();
        let caption_center = app.new_view::<text::Text>();
        let caption_left = app.new_view::<text::Text>();
        let background = background::View::new();
        let track = track::View::new();
        let track_handle_left = io_rect::View::new();
        let track_handle_right = io_rect::View::new();
        let left_overflow = left_overflow::View::new();
        let right_overflow = right_overflow::View::new();
        let background_color = default();
        let track_color = default();
        let background_left_corner_roundness = default();
        let background_right_corner_roundness = default();
        let padding = default();

        let app = app.clone_ref();
        let scene = &app.display.default_scene;

        root.add_child(&label);
        root.add_child(&label_left);
        root.add_child(&label_right);
        root.add_child(&caption_left);
        root.add_child(&caption_center);
        root.add_child(&background);
        root.add_child(&track);
        root.add_child(&right_overflow);

        scene.layers.main.remove(&label_left);
        label_left.add_to_scene_layer(&scene.layers.label);
        scene.layers.main.remove(&label_right);
        label_right.add_to_scene_layer(&scene.layers.label);
        scene.layers.main.remove(&caption_left);
        caption_left.add_to_scene_layer(&scene.layers.label);
        scene.layers.main.remove(&caption_center);
        caption_center.add_to_scene_layer(&scene.layers.label);

        Self {
            background,
            track,
            track_handle_left,
            track_handle_right,
            left_overflow,
            right_overflow,
            label,
            label_left,
            label_right,
            caption_left,
            caption_center,
            root,
            background_color,
            track_color,
            background_left_corner_roundness,
            background_right_corner_roundness,
            padding,
            app,
        }
    }

    /// Set the size of the overall shape, taking into account the extra padding required to
    /// render the shadow.
    pub fn set_size(&self, size: Vector2, shadow_padding: Vector2) {
        let size_with_shadow = size + shadow_padding;
        self.background.size.set(size_with_shadow);
        self.left_overflow.size.set(size_with_shadow);
        self.right_overflow.size.set(size_with_shadow);
        let padding = Vector2(self.padding.get() * 2.0, self.padding.get() * 2.0);
        self.track.size.set(size_with_shadow - padding);

        let left_padding = LABEL_OFFSET;
        let overflow_icon_size = size.y;
        let label_offset = size.x / 2.0 - overflow_icon_size + left_padding;

        self.label_left.set_x(-label_offset);
        self.label_right.set_x(label_offset - self.label_right.width.value());

        let overflow_icon_offset = size.x / 2.0 - overflow_icon_size / 2.0;
        self.left_overflow.set_x(-overflow_icon_offset);
        self.right_overflow.set_x(overflow_icon_offset);

        let track_handle_size = Vector2::new(size.y / 2.0, size.y);
        self.track_handle_left.size.set(track_handle_size);
        self.track_handle_right.size.set(track_handle_size);
    }

    /// Update the position of the captions based on the size of the shape and the text size. Takes
    ///arguments in the way they are provided in the FRP network (as reference to a tuple) to make
    ///usage more ergonomic on the call-site.
    pub fn update_caption_position(&self, (size, text_size): &(Vector2, f32)) {
        let left_padding = LABEL_OFFSET;
        let overflow_icon_size = size.y / 2.0;
        let caption_offset = size.x / 2.0 - overflow_icon_size - left_padding;
        self.caption_left.set_x(-caption_offset);
        self.caption_left.set_y(text_size / 2.0);
        self.caption_center.set_y(text_size / 2.0);
    }

    /// Set whether to allow interactions with the edges of the track shape. If this is set to
    /// `false`, both `track_handle_left` and `track_handle_right` are removed from the component.
    pub fn use_track_handles(&self, value: bool) {
        if value {
            self.track.add_child(&self.track_handle_left);
            self.track.add_child(&self.track_handle_right);
        } else {
            self.track.remove_child(&self.track_handle_left);
            self.track.remove_child(&self.track_handle_right);
        }
    }

    /// Set the track to cover the area from zero up to the given value. The value indicates the
    /// width of the background that should be covered in the range 0..1.
    pub fn set_background_value(&self, value: f32) {
        self.track.left.set(0.0);
        self.track.right.set(value);
    }

    pub fn set_background_color(&self, color: color::Rgba) {
        self.background_color.as_ref().replace(color);
        self.background.color.set(color.into());
    }

    /// Set the track to cover the area indicated by the `value` Bounds that are passed. The value
    /// indicates location aon the background in the range 0..1 (were 0 is the left edge and 1 is
    /// the right edge). To do the proper layout of the track handles this method also needs to be
    /// passed the size of the shape.
    pub fn set_background_range(&self, value: Bounds, size: Vector2) {
        self.track.left.set(value.start);
        self.track.right.set(value.end);

        self.track_handle_left.set_x(value.start * size.x - size.x / 2.0);
        self.track_handle_right.set_x(value.end * size.x - size.x / 2.0);
    }

    /// Set the label in the center of the background to show the given numeric value.
    pub fn set_center_label_content(&self, value: f32) {
        self.label.frp.set_content.emit(value)
    }

    /// Set the label at the left edge of the background to show the given numeric value.
    pub fn set_left_label_content(&self, value: f32) {
        self.label_left.frp.set_content.emit(format!("{:.2}", value))
    }

    /// Set the label at the right edge of the background to show the given numeric value.
    pub fn set_right_label_content(&self, value: f32) {
        self.label_right.frp.set_content.emit(format!("{:.2}", value))
    }

    pub fn set_caption_left(&self, caption: Option<String>) {
        let caption = caption.unwrap_or_default();
        self.caption_left.frp.set_content.emit(caption);
    }

    pub fn set_caption_center(&self, caption: Option<String>) {
        let caption = caption.unwrap_or_default();
        self.caption_center.frp.set_content.emit(caption);
    }

    pub fn show_left_overflow(&self, value: bool) {
        if value {
            self.root.add_child(&self.left_overflow);
        } else {
            self.root.remove_child(&self.left_overflow);
        }
    }

    pub fn show_right_overflow(&self, value: bool) {
        if value {
            self.root.add_child(&self.right_overflow);
        } else {
            self.root.remove_child(&self.right_overflow);
        }
    }

    pub fn left_corner_round(&self, value: bool) {
        let corner_roundness = if value { 1.0 } else { 0.0 };
        self.background.corner_left.set(corner_roundness);
        self.track.corner_left.set(corner_roundness);
        self.background_left_corner_roundness.set(value);
    }

    pub fn right_corner_round(&self, value: bool) {
        let corner_roundness = if value { 1.0 } else { 0.0 };
        self.background.corner_right.set(corner_roundness);
        self.track.corner_right.set(corner_roundness);
        self.background_right_corner_roundness.set(value);
    }

    pub fn set_track_color(&self, color: color::Rgba) {
        self.track.track_color.set(color.into());
    }

    pub fn set_track_corner_round(&self, value: bool) {
        let corner_roundness = if value { 1.0 } else { 0.0 };
        self.track.corner_inner.set(corner_roundness)
    }

    pub fn set_padding(&self, padding: f32) {
        self.padding.set(padding);
    }

    pub fn show_shadow(&self, value: bool) {
        if value {
            self.background.show_shadow.set(1.0);
        } else {
            self.background.show_shadow.set(0.0);
        }
    }

    pub fn show_background(&self, value: bool) {
        if value {
            self.show_shadow(true);
            self.background.color.set(self.background_color.as_ref().clone().into_inner().into());
            let left_corner_roundness =
                if self.background_left_corner_roundness.get() { 1.0 } else { 0.0 };
            let right_corner_roundness =
                if self.background_right_corner_roundness.get() { 1.0 } else { 0.0 };
            self.track.corner_right.set(right_corner_roundness);
            self.track.corner_left.set(left_corner_roundness);
        } else {
            self.show_shadow(false);
            self.background.color.set(INVISIBLE_HOVER_COLOR.into());
            self.track.corner_right.set(0.0);
            self.track.corner_left.set(0.0);
        }
    }
}

impl display::Object for Model {
    fn display_object(&self) -> &display::object::Instance {
        &self.root
    }
}
