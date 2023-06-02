//! Provides a button to switch back from the project view to the dashboard.

use crate::prelude::*;

use ensogl::application::tooltip;
use ensogl::application::Application;
use ensogl::display;
use ensogl::display::style;
use ensogl_component::toggle_button;
use ensogl_component::toggle_button::ToggleButton;



// =================
// === Constants ===
// =================

/// The width and height of the button.
pub const SIZE: f32 = 16.0;



// ============
// === Icon ===
// ============

/// Defines an icon for returning to the dashboard. It looks like a hamburger button.
mod icon {
    use super::*;

    use ensogl::data::color;
    use ensogl_component::toggle_button::ColorableShape;

    ensogl::shape! {
        (style: Style, color_rgba: Vector4<f32>) {
            let fill_color = Var::<color::Rgba>::from(color_rgba);
            let width = Var::<Pixels>::from("input_size.x");
            let height = Var::<Pixels>::from("input_size.y");
            let unit = &width / SIZE;
            let mid_bar = Rect((&unit * 12.0, &unit * 3.0)).corners_radius(&unit);
            let top_bar = mid_bar.translate_y(&unit * -5.0);
            let bottom_bar = mid_bar.translate_y(&unit * 5.0);
            let all_bars = top_bar + mid_bar + bottom_bar;
            let shape = all_bars.fill(fill_color);
            let hover_area = Rect((&width, &height)).fill(INVISIBLE_HOVER_COLOR);
            (shape + hover_area).into()
        }
    }

    impl ColorableShape for Shape {
        fn set_color(&self, color: color::Rgba) {
            self.color_rgba.set(Vector4::new(color.red, color.green, color.blue, color.alpha));
        }
    }
}



// ============
// === View ===
// ============

/// Provides a button to switch back from the project view to the dashboard.
#[derive(Debug, Clone, CloneRef, Deref)]
pub struct View {
    button: ToggleButton<icon::Shape>,
}

impl View {
    /// Constructor.
    pub fn new(app: &Application) -> Self {
        let scene = &app.display.default_scene;

        let tooltip_style = tooltip::Style::set_label("Dashboard".to_owned())
            .with_placement(tooltip::Placement::Right);
        let button = ToggleButton::<icon::Shape>::new(app, tooltip_style);
        scene.layers.panel.add(&button);

        button.set_color_scheme(Self::color_scheme(&scene.style_sheet));
        button.set_size(Vector2(SIZE, SIZE));

        Self { button }
    }

    fn color_scheme(style_sheet: &style::Sheet) -> toggle_button::ColorScheme {
        let default_color_scheme = toggle_button::default_color_scheme(style_sheet);
        toggle_button::ColorScheme {
            // Make it look like a normal button (as opposed to a toggle button) by not having a
            // toggled state visually.
            toggled: default_color_scheme.non_toggled,
            toggled_hovered: default_color_scheme.hovered,
            ..default_color_scheme
        }
    }
}

impl display::Object for View {
    fn display_object(&self) -> &display::object::Instance {
        self.button.display_object()
    }
}
