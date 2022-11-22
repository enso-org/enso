//! Model for the slider component.

use crate::SliderOrientation;

use ensogl_core::display::shape::*;
use ensogl_core::prelude::*;

use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display;
use ensogl_hardcoded_theme as theme;
use ensogl_text as text;
use ensogl_text::formatting::ResolvedProperty;
use ensogl_tooltip::Tooltip;



// =================
// === Constants ===
// =================

/// Size of the margin around the component's shapes for proper anti-aliasing.
const COMPONENT_MARGIN: f32 = 4.0;
/// Default component width on initialization.
const COMPONENT_WIDTH_DEFAULT: f32 = 200.0;
/// Default component height on initialization.
const COMPONENT_HEIGHT_DEFAULT: f32 = 50.0;



// =====================================================
// === Slider background and track shape definitions ===
// =====================================================

struct Background {
    pub width:  Var<Pixels>,
    pub height: Var<Pixels>,
    pub shape:  AnyShape,
}

impl Background {
    /// Create new rounded rectangle to serve as the component's background.
    fn new() -> Self {
        let width: Var<Pixels> = "input_size.x".into();
        let height: Var<Pixels> = "input_size.y".into();
        let width = width - COMPONENT_MARGIN.px() * 2.0;
        let height = height - COMPONENT_MARGIN.px() * 2.0;
        let shape = Rect((&width, &height)).corners_radius(&height / 2.0);
        let shape = shape.into();
        Background { width, height, shape }
    }
}

/// Background shape.
mod background {
    use super::*;

    ensogl_core::shape! {
        (style:Style, color:Vector4) {
            let shape = Background::new().shape;
            let shape = shape.fill(color);
            shape.into()
        }
    }
}

/// Track shape that fills the slider proportional to the slider value.
mod track {
    use super::*;

    ensogl_core::shape! {
        above = [background];
        pointer_events = false;
        (style:Style, slider_fraction_horizontal:f32, slider_fraction_vertical:f32, color:Vector4) {
            let Background{width,height,shape: background} = Background::new();
            let track = Rect((
                &width * &slider_fraction_horizontal,
                &height * &slider_fraction_vertical
            ));
            let track = track.translate_x(&width * (&slider_fraction_horizontal - 1.0) * 0.5);
            let track = track.translate_y(&height * (&slider_fraction_vertical - 1.0) * 0.5);
            let track = track.intersection(background).fill(color);
            track.into()
        }
    }
}

/// Triangle shape used as an overflow indicator on either side of the range.
mod overflow {
    use super::*;

    ensogl_core::shape! {
        above = [background, track];
        pointer_events = false;
        (style:Style, color:Vector4) {
            let width: Var<Pixels> = "input_size.x".into();
            let height: Var<Pixels> = "input_size.y".into();
            let width = width - COMPONENT_MARGIN.px() * 2.0;
            let height = height - COMPONENT_MARGIN.px() * 2.0;

            let color = style.get_color(theme::component::slider::overflow::color);
            let triangle = Triangle(width, height);
            let triangle = triangle.fill(color);

            triangle.into()
        }
    }
}



// ===============================
// === Slider model definition ===
// ===============================

/// The slider model contains the visual elements of the slider component.
#[derive(Debug)]
pub struct Model {
    /// Background element
    pub background:       background::View,
    /// Slider track element that fills the slider proportional to the slider value.
    pub track:            track::View,
    /// Indicator for overflow when the value is below the lower limit.
    pub overflow_lower:   overflow::View,
    /// Indicator for overflow when the value is above the upper limit.
    pub overflow_upper:   overflow::View,
    /// Slider label that is shown next to the slider.
    pub label:            text::Text,
    /// Textual representation of the slider value, only part left of the decimal point.
    pub value_text_left:  text::Text,
    /// Decimal point that is used to display non-integer slider values.
    pub value_text_dot:   text::Text,
    /// Textual representation of the slider value, only part right of the decimal point.
    pub value_text_right: text::Text,
    /// Textual representation of the slider value used when editing the value as text input.
    pub value_text_edit:  text::Text,
    /// Tooltip component showing either a tooltip message or slider precision changes.
    pub tooltip:          Tooltip,
    /// Root of the display object.
    pub root:             display::object::Instance,
}

impl Model {
    /// Create a new slider model.
    pub fn new(app: &Application) -> Self {
        let root = display::object::Instance::new();
        let label = app.new_view::<text::Text>();
        let value_text_left = app.new_view::<text::Text>();
        let value_text_dot = app.new_view::<text::Text>();
        let value_text_right = app.new_view::<text::Text>();
        let value_text_edit = app.new_view::<text::Text>();
        let tooltip = Tooltip::new(app);
        let background = background::View::new();
        let track = track::View::new();
        let overflow_lower = overflow::View::new();
        let overflow_upper = overflow::View::new();
        let scene = &app.display.default_scene;
        let style = StyleWatch::new(&app.display.default_scene.style_sheet);

        root.add_child(&background);
        root.add_child(&track);
        root.add_child(&label);
        root.add_child(&value_text_left);
        root.add_child(&value_text_dot);
        root.add_child(&value_text_right);
        app.display.default_scene.add_child(&tooltip);

        value_text_left.add_to_scene_layer(&scene.layers.label);
        value_text_dot.add_to_scene_layer(&scene.layers.label);
        value_text_right.add_to_scene_layer(&scene.layers.label);
        value_text_edit.add_to_scene_layer(&scene.layers.label);
        label.add_to_scene_layer(&scene.layers.label);

        let model = Self {
            background,
            track,
            overflow_lower,
            overflow_upper,
            label,
            value_text_left,
            value_text_dot,
            value_text_right,
            value_text_edit,
            tooltip,
            root,
        };
        model.init(style)
    }

    /// Initialise slider model.
    pub fn init(self, style: StyleWatch) -> Self {
        let background_color = style.get_color(theme::component::slider::background::color);
        let track_color = style.get_color(theme::component::slider::track::color);
        self.background.color.set(background_color.into());
        self.track.color.set(track_color.into());
        self.set_size(Vector2(COMPONENT_WIDTH_DEFAULT, COMPONENT_HEIGHT_DEFAULT));
        self.value_text_dot.set_content(".");
        self
    }

    /// Set the component size.
    pub fn set_size(&self, size: Vector2<f32>) {
        let margin = Vector2(COMPONENT_MARGIN * 2.0, COMPONENT_MARGIN * 2.0);
        self.background.size.set(size + margin);
        self.track.size.set(size + margin);
    }

    /// Set the fraction of the slider filled by the track.
    pub fn set_track_fraction(&self, (fraction, orientation): &(f32, SliderOrientation)) {
        match orientation {
            SliderOrientation::Horizontal => {
                self.track.slider_fraction_horizontal.set(*fraction);
                self.track.slider_fraction_vertical.set(1.0);
                self.overflow_lower.set_rotation_z(std::f32::consts::FRAC_PI_2);
                self.overflow_upper.set_rotation_z(-std::f32::consts::FRAC_PI_2);
            }
            SliderOrientation::Vertical => {
                self.track.slider_fraction_horizontal.set(1.0);
                self.track.slider_fraction_vertical.set(*fraction);
                self.overflow_lower.set_rotation_z(std::f32::consts::PI);
                self.overflow_upper.set_rotation_z(0.0);
            }
        }
    }

    /// Set the size of the overflow markers.
    pub fn set_overflow_marker_size(&self, size: f32) {
        let margin = Vector2(COMPONENT_MARGIN * 2.0, COMPONENT_MARGIN * 2.0);
        let size = Vector2(size, size) + margin;
        self.overflow_lower.size.set(size);
        self.overflow_upper.size.set(size);
    }

    /// Set the color of the slider track.
    pub fn set_track_color(&self, color: &color::Lcha) {
        self.track.color.set(color::Rgba::from(color).into());
    }

    /// Set the color of the slider background.
    pub fn set_background_color(&self, color: &color::Lcha) {
        self.background.color.set(color::Rgba::from(color).into());
    }

    /// Set whether the lower overfow marker is visible.
    pub fn set_overflow_lower_visible(&self, visible: bool) {
        if visible {
            self.root.add_child(&self.overflow_lower);
        } else {
            self.root.remove_child(&self.overflow_lower);
        }
    }

    /// Set whether the upper overfow marker is visible.
    pub fn set_overflow_upper_visible(&self, visible: bool) {
        if visible {
            self.root.add_child(&self.overflow_upper);
        } else {
            self.root.remove_child(&self.overflow_upper);
        }
    }

    /// Set whether the slider label is hidden.
    pub fn set_label_hidden(&self, hidden: bool) {
        if hidden {
            self.root.remove_child(&self.label);
        } else {
            self.root.add_child(&self.label);
        }
    }

    /// Set whether the value is being edited. This hides the value display and shows a text editor
    /// field to enter a new value.
    pub fn set_edit_mode(&self, editing: bool) {
        if editing {
            self.root.remove_child(&self.value_text_left);
            self.root.remove_child(&self.value_text_dot);
            self.root.remove_child(&self.value_text_right);
            self.root.add_child(&self.value_text_edit);
            self.value_text_edit.deprecated_focus();
            self.value_text_edit.add_cursor_at_front();
            self.value_text_edit.cursor_select_to_text_end();
        } else {
            self.root.add_child(&self.value_text_left);
            self.root.add_child(&self.value_text_dot);
            self.root.add_child(&self.value_text_right);
            self.root.remove_child(&self.value_text_edit);
            self.value_text_edit.deprecated_defocus();
            self.value_text_edit.remove_all_cursors();
        }
    }

    /// Set whether the value display decimal point and the text right of it are visible.
    pub fn set_value_text_right_visible(&self, enabled: bool) {
        if enabled {
            self.root.add_child(&self.value_text_dot);
            self.root.add_child(&self.value_text_right);
        } else {
            self.root.remove_child(&self.value_text_dot);
            self.root.remove_child(&self.value_text_right);
        }
    }

    /// Set default properties to the group of text elements displaying the slider value.
    pub fn set_value_text_property(&self, property: impl Into<ResolvedProperty> + Copy) {
        self.value_text_left.set_property_default(property.into());
        self.value_text_dot.set_property_default(property.into());
        self.value_text_right.set_property_default(property.into());
    }
}

impl display::Object for Model {
    fn display_object(&self) -> &display::object::Instance {
        &self.root
    }
}
