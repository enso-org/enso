//! Provides a button to switch back from the project view to the dashboard.

use crate::prelude::*;

use ensogl::application::tooltip;
use ensogl::application::Application;
use ensogl::display;
use ensogl::display::shape::StyleWatch;
use ensogl::display::Scene;
use ensogl_component::toggle_button;
use ensogl_component::toggle_button::ToggleButton;
use ensogl_hardcoded_theme::application::dashboard_button as theme;



// ============
// === Icon ===
// ============

/// Defines an icon for returning to the dashboard. It looks like a hamburger button.
mod icon {
    use super::*;

    use ensogl::data::color;

    use ensogl_component::toggle_button::ColorableShape;

    ensogl::shape! {
        alignment = center;
        (style: Style, color_rgba: Vector4<f32>) {
            let fill_color = Var::<color::Rgba>::from(color_rgba);
            let width = Var::<Pixels>::from("input_size.x");
            let height = Var::<Pixels>::from("input_size.y");
            let unit = &width / 16.0;
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
// === Frp  ===
// ============

ensogl::define_endpoints! {
    Input {
    }
    Output {
        pressed (),
        size (Vector2<f32>),
    }
}



// ============
// === View ===
// ============

/// Provides a button to switch back from the project view to the dashboard.
#[derive(Debug, Clone, CloneRef)]
pub struct View {
    button:  ToggleButton<icon::Shape>,
    #[allow(missing_docs)]
    pub frp: Frp,
}

impl View {
    /// Constructor.
    pub fn new(app: &Application) -> Self {
        let scene = &app.display.default_scene;
        let frp = Frp::new();
        let network = &frp.network;

        let tooltip_style = tooltip::Style::set_label("Dashboard".to_owned())
            .with_placement(tooltip::Placement::Right);
        let button = ToggleButton::<icon::Shape>::new(app, tooltip_style);
        scene.layers.panel.add(&button);

        frp::extend! { network
            frp.source.pressed <+ button.is_pressed.on_true();
        }

        button.set_visibility(true);
        button.set_color_scheme(&Self::color_scheme(scene));
        let size = Vector2(16.0, 16.0);
        button.set_size(size);
        frp.source.size.emit(size);

        Self { button, frp }
    }

    fn color_scheme(scene: &Scene) -> toggle_button::ColorScheme {
        let styles = StyleWatch::new(&scene.style_sheet);
        toggle_button::ColorScheme {
            non_toggled: Some(styles.get_color(theme::non_toggled).into()),
            toggled: Some(styles.get_color(theme::toggled).into()),
            hovered: Some(styles.get_color(theme::hovered).into()),
            ..default()
        }
    }
}

impl display::Object for View {
    fn display_object(&self) -> &display::object::Instance {
        self.button.display_object()
    }
}
