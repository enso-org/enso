//! Provides a button that can be used to toggle the editor's profiling mode.

use crate::prelude::*;
use ensogl::display::shape::*;

use crate::view;

use enso_frp as frp;
use ensogl::application::Application;
use ensogl::data::color;
use ensogl::display;
use ensogl_component::toggle_button;
use ensogl_component::toggle_button::ToggleButton;



// ============
// === Icon ===
// ============

/// Defines an icon as described here:
/// https://github.com/enso-org/ide/pull/1546#issuecomment-838169795
///
/// It consists of a *circle outline* with an *aperture* in the lower right quadrant. The edges
/// of the aperture have rounded *caps*. In the center is an *inner circle* with a *needle*
/// pointing to the lower right.
mod icon {
    use super::*;
    use ensogl_component::toggle_button::ColorableShape;

    ensogl::shape! {
        (style: Style, color_rgba: Vector4<f32>) {
            let fill_color = Var::<color::Rgba>::from(color_rgba);
            let width      = Var::<Pixels>::from("input_size.x");
            let height     = Var::<Pixels>::from("input_size.y");


            // === Measurements ===

            let unit                   = &width * 0.3;
            let outer_circle_radius    = &unit * 1.0;
            let outer_circle_thickness = &unit * 0.33;
            let inner_circle_radius    = &unit * 0.2;
            let needle_angle           = (135.0_f32).to_radians().radians();
            let needle_radius_inner    = &unit * 0.14;
            let needle_radius_outer    = &unit * 0.09;
            let needle_length          = &outer_circle_radius-&needle_radius_outer;
            let aperture_cap_1_x       = &outer_circle_radius-&outer_circle_thickness*0.5;
            let aperture_cap_2_y       = -(&outer_circle_radius-&outer_circle_thickness*0.5);


            // === Circle Outline ===

            let circle         = Circle(&outer_circle_radius);
            let gap            = Circle(&outer_circle_radius-&outer_circle_thickness);
            let circle_outline = circle - gap;


            // === Aperture ===

            // To produce the aperture, we cut a triangular gap from the outline and use small
            // circular caps to round off the edges.

            // We make the gap a little bit larger than the circle to be sure that we really cover
            // everything that we want to cut, even if there are rounding errors or other
            // imprecisions.
            let aperture_gap_size  = &outer_circle_radius * 1.1;
            let aperture_gap_angle = needle_angle+180.0_f32.to_radians().radians();

            let aperture_gap = Triangle(&aperture_gap_size*2.0,aperture_gap_size.clone());
            let aperture_gap = aperture_gap.rotate(aperture_gap_angle);
            let aperture_gap = aperture_gap.translate_x(&aperture_gap_size*2.0.sqrt()*0.25);
            let aperture_gap = aperture_gap.translate_y(-&aperture_gap_size*2.0.sqrt()*0.25);

            let aperture_cap_1 = Circle(&outer_circle_thickness*0.5);
            let aperture_cap_1 = aperture_cap_1.translate_x(aperture_cap_1_x);
            let aperture_cap_2 = Circle(&outer_circle_thickness*0.5);
            let aperture_cap_2 = aperture_cap_2.translate_y(aperture_cap_2_y);

            let circle_outline = circle_outline - aperture_gap + aperture_cap_1 + aperture_cap_2;


            // === Needle ===

            let needle       = UnevenCapsule(needle_radius_outer,needle_radius_inner,needle_length);
            let needle       = needle.rotate(needle_angle);
            let inner_circle = Circle(&inner_circle_radius);


            // === Composition ===

            let shape      = (circle_outline + needle + inner_circle).fill(fill_color);
            let hover_area = Rect((&width,&height)).fill(INVISIBLE_HOVER_COLOR);
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
        set_view_mode (view::Mode),
    }
    Output {
        view_mode (view::Mode),
    }
}



// =======================
// === ProfilingButton ===
// =======================

/// A toggle button that can be used to toggle the graph editor's view mode. It positions itself in
/// the upper right corner of the scene.
#[derive(Debug, Clone, CloneRef)]
pub struct Button {
    frp:    Frp,
    button: ToggleButton<icon::Shape>,
    styles: StyleWatchFrp,
}

impl Deref for Button {
    type Target = Frp;

    fn deref(&self) -> &Self::Target {
        &self.frp
    }
}

impl Button {
    /// Constructs a new button for toggling the editor's view mode.
    pub fn new(app: &Application) -> Button {
        let scene = &app.display.default_scene;
        let styles = StyleWatchFrp::new(&scene.style_sheet);
        let frp = Frp::new();
        let network = &frp.network;

        let button = ToggleButton::<icon::Shape>::new();
        scene.layers.panel.add(&button);
        button.set_visibility(true);
        button.set_size(Vector2(32.0, 32.0));

        frp::extend! { network

            // === State ===

            frp.source.view_mode <+ button.state.map(|&toggled| {
                if toggled { view::Mode::Profiling } else { view::Mode::Normal }
            });
            button.set_state <+ frp.set_view_mode.map(|&mode| mode.is_profiling());


            // === Position ===

            eval scene.frp.camera_changed([button,scene](_) {
                let screen = scene.camera().screen();
                button.set_position_x(screen.width/2.0 - 16.0);
                button.set_position_y(screen.height/2.0 - 16.0);
            });


            // === Color ===

            use ensogl_hardcoded_theme::graph_editor::profiling_button as button_theme;
            let non_toggled_color      = styles.get_color(button_theme::non_toggled);
            let toggled_color          = styles.get_color(button_theme::toggled);
            let hovered_color          = styles.get_color(button_theme::hovered);
            let toggled_hovered_color  = styles.get_color(button_theme::toggled_hovered);
            init_color_scheme         <- source::<()>();
            button.set_color_scheme   <+ all_with5(&non_toggled_color,&toggled_color,&hovered_color
                ,&toggled_hovered_color,&init_color_scheme
                ,|&non_toggled,&toggled,&hovered,&toggled_hovered,_|
                    toggle_button::ColorScheme {
                        non_toggled     : Some(non_toggled.into()),
                        toggled         : Some(toggled.into()),
                        hovered         : Some(hovered.into()),
                        toggled_hovered : Some(toggled_hovered.into()),
                        ..default()
                    }
            );
        }

        init_color_scheme.emit(());
        Button { frp, button, styles }
    }
}

impl display::Object for Button {
    fn display_object(&self) -> &display::object::Instance {
        self.button.display_object()
    }
}
