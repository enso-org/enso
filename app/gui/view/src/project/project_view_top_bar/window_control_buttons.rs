//! The component with buttons in the top left corner. See [[View]].

use ensogl::display::shape::*;
use ensogl::prelude::*;

use enso_frp as frp;
use ensogl::application;
use ensogl::application::Application;
use ensogl::display;
use ensogl::display::object::ObjectOps;
use ensogl::shape;
use ensogl_hardcoded_theme::application::window_control_buttons as theme;


// ==============
// === Export ===
// ==============

pub mod close;
pub mod fullscreen;



// ==============
// === Shapes ===
// ==============

mod shape {
    use super::*;
    shape! {
        alignment = center;
        (style: Style) {
            Plane().fill(INVISIBLE_HOVER_COLOR).into()
        }
    }
}



// =============
// === Model ===
// =============

/// An internal model of Status Bar component
#[derive(Clone, CloneRef, Debug)]
pub struct Model {
    display_object: display::object::Instance,
    shape:          shape::View,
    close:          close::View,
    fullscreen:     fullscreen::View,
}

impl Model {
    /// Constructor.
    pub fn new(app: &Application) -> Self {
        let display_object = display::object::Instance::new_named("WindowControlButtons");

        ensogl::shapes_order_dependencies! {
            app.display.default_scene => {
                shape -> close::shape;
                shape -> fullscreen::shape;
            }
        };
        let close = close::View::new(app);
        display_object.add_child(&close);

        let fullscreen = fullscreen::View::new(app);
        display_object.add_child(&fullscreen);

        let shape = shape::View::new();
        display_object.add_child(&shape);

        app.display.default_scene.layers.panel.add(&display_object);

        Self { display_object, shape, close, fullscreen }
    }

    /// Updates positions of the buttons and sizes of the mouse area.
    pub fn set_layout(&self, spacing: f32) {
        let close_size = self.close.size.value();
        let fullscreen_size = self.fullscreen.size.value();
        let fullscreen_offset = Vector2(close_size.x + spacing, 0.0);
        self.fullscreen.set_xy(fullscreen_offset);
        let width = fullscreen_offset.x + fullscreen_size.x;
        let height = max(close_size.y, fullscreen_size.y);
        let size = Vector2(width, height);
        self.shape.set_size(size);
        self.display_object.set_size(size);
    }
}



// ===========
// === FRP ===
// ===========

ensogl::define_endpoints! {
    Input {
        enabled (bool),
    }
    Output {
        close(),
        fullscreen(),
    }
}



// ============
// === View ===
// ============

/// The Top Buttons Panel component.
///
/// The panel contains two buttons: one for closing IDE and one for toggling the fullscreen mode.
/// The panel is meant to be displayed only when IDE runs in a cloud environment.
#[derive(Clone, CloneRef, Debug)]
pub struct View {
    #[allow(missing_docs)]
    pub frp: Frp,
    model:   Model,
    style:   StyleWatchFrp,
}

impl View {
    /// Constructor.
    pub fn new(app: &Application) -> Self {
        let frp = Frp::new();
        let model = Model::new(app);
        let network = &frp.network;

        let style = StyleWatchFrp::new(&app.display.default_scene.style_sheet);
        let radius = style.get_number(theme::radius);
        let spacing = style.get_number(theme::spacing);

        frp::extend! { network
            // Layout
            button_size <- radius.map(|&r| Vector2(2.0 * r, 2.0 * r));
            model.close.set_size <+ button_size;
            model.fullscreen.set_size <+ button_size;
            button_resized <- any_(&model.close.size, &model.fullscreen.size);
            _eval <- all_with(&button_resized, &spacing,
                f!((_, spacing) model.set_layout(*spacing))
            );

            // Handle the panel-wide hover
            mouse_near_buttons <- bool(
                &model.shape.events_deprecated.mouse_out,
                &model.shape.events_deprecated.mouse_over
            );
            mouse_on_any_buttton          <- model.close.is_hovered.or(&model.fullscreen.is_hovered);
            mouse_nearby                  <- mouse_near_buttons.or(&mouse_on_any_buttton);
            model.close.mouse_nearby      <+ mouse_nearby;
            model.fullscreen.mouse_nearby <+ mouse_nearby;

            // === Handle buttons' clicked events ===
            frp.source.close      <+ model.close.clicked;
            frp.source.fullscreen <+ model.fullscreen.clicked;
        }

        model.set_layout(spacing.value());

        Self { frp, model, style }
    }
}

impl display::Object for View {
    fn display_object(&self) -> &display::object::Instance {
        &self.model.display_object
    }
}

impl Deref for View {
    type Target = Frp;
    fn deref(&self) -> &Self::Target {
        &self.frp
    }
}

impl FrpNetworkProvider for View {
    fn network(&self) -> &frp::Network {
        &self.frp.network
    }
}

impl application::View for View {
    fn label() -> &'static str {
        "TopButtons"
    }

    fn new(app: &Application) -> Self {
        View::new(app)
    }
}
