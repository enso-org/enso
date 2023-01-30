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
        (style: Style) {
            Plane().fill(INVISIBLE_HOVER_COLOR).into()
        }
    }
}



// ==============
// === Layout ===
// ==============

/// Information on how to layout shapes in the top buttons panel, as defined in the theme.
#[allow(missing_docs)]
#[derive(Clone, Copy, Debug)]
pub struct LayoutParams<T> {
    pub spacing:        T,
    pub padding_left:   T,
    pub padding_top:    T,
    pub padding_right:  T,
    pub padding_bottom: T,
}

impl Default for LayoutParams<f32> {
    fn default() -> Self {
        Self {
            spacing:        8.0,
            padding_left:   13.0,
            padding_top:    13.0,
            padding_right:  13.0,
            padding_bottom: 13.0,
        }
    }
}

impl<T> LayoutParams<T> {
    /// Applies a given function over all stored values and return layout with resulting values.
    pub fn map<U>(&self, f: impl Fn(&T) -> U) -> LayoutParams<U> {
        LayoutParams {
            spacing:        f(&self.spacing),
            padding_left:   f(&self.padding_left),
            padding_top:    f(&self.padding_top),
            padding_right:  f(&self.padding_right),
            padding_bottom: f(&self.padding_bottom),
        }
    }
}

impl LayoutParams<frp::Sampler<f32>> {
    /// Get layout from theme. Each layout parameter will be an frp sampler.
    pub fn from_theme(style: &StyleWatchFrp) -> Self {
        let default = LayoutParams::default();
        let spacing = style.get_number_or(theme::spacing, default.spacing);
        let padding_left = style.get_number_or(theme::padding::left, default.padding_left);
        let padding_top = style.get_number_or(theme::padding::top, default.padding_top);
        let padding_right = style.get_number_or(theme::padding::right, default.padding_right);
        let padding_bottom = style.get_number_or(theme::padding::bottom, default.padding_bottom);
        Self { spacing, padding_left, padding_top, padding_right, padding_bottom }
    }

    /// Take values from the parameters' samplers.
    pub fn value(&self) -> LayoutParams<f32> {
        self.map(|sampler| sampler.value())
    }

    /// Join all member frp streams into a single stream with aggregated values.
    pub fn flatten(&self, network: &frp::Network) -> frp::Stream<LayoutParams<f32>> {
        /// Helper method that puts back LayoutParams from its fields.
        /// Be careful, as the arguments must be in the same order as they are in `all_with5`
        /// invocation below.
        // We intentionally take references to f32 for seamless usage in FRP.
        #[allow(clippy::trivially_copy_pass_by_ref)]
        fn to_layout(
            spacing: &f32,
            padding_left: &f32,
            padding_top: &f32,
            padding_right: &f32,
            padding_bottom: &f32,
        ) -> LayoutParams<f32> {
            let ret =
                LayoutParams { spacing, padding_left, padding_top, padding_right, padding_bottom };
            ret.map(|v| **v)
        }

        network.all_with5(
            "TopButtonsLayoutStyle",
            &self.spacing,
            &self.padding_left,
            &self.padding_top,
            &self.padding_right,
            &self.padding_bottom,
            to_layout,
        )
    }
}



// =============
// === Model ===
// =============

/// An internal model of Status Bar component
#[derive(Clone, CloneRef, Debug)]
pub struct Model {
    app:            Application,
    display_object: display::object::Instance,
    shape:          shape::View,
    close:          close::View,
    fullscreen:     fullscreen::View,
}

impl Model {
    /// Constructor.
    pub fn new(app: &Application) -> Self {
        let app = app.clone_ref();
        let display_object = display::object::Instance::new();

        ensogl::shapes_order_dependencies! {
            app.display.default_scene => {
                shape -> close::shape;
                shape -> fullscreen::shape;
            }
        };
        let close = close::View::new(&app);
        display_object.add_child(&close);

        let fullscreen = fullscreen::View::new(&app);
        display_object.add_child(&fullscreen);

        let shape = shape::View::new();
        display_object.add_child(&shape);

        Self { app, display_object, shape, close, fullscreen }
    }

    /// Updates positions of the buttons and sizes of the mouse area.
    /// Returns the new size of the panel (being also the size of mouse area).
    pub fn set_layout(&self, layout: LayoutParams<f32>) -> Vector2 {
        let LayoutParams { spacing, padding_left, padding_top, padding_right, padding_bottom } =
            layout;
        let close_size = self.close.size.value();
        let fullscreen_size = self.fullscreen.size.value();
        let padding_offset = Vector2(padding_left, -padding_top);
        let origin_offset = |size: Vector2| Vector2(size.x / 2.0, -size.y / 2.0);

        self.close.set_xy(padding_offset + origin_offset(close_size));
        let fullscreen_x = padding_left + close_size.x + spacing;
        self.fullscreen
            .set_xy(Vector2(fullscreen_x, -padding_top) + origin_offset(fullscreen_size));

        let width = fullscreen_x + fullscreen_size.x + padding_right;
        let height = padding_top + max(close_size.y, fullscreen_size.y) + padding_bottom;

        let size = Vector2(width, height);
        self.shape.set_xy(Vector2(size.x, -size.y) / 2.0);
        self.shape.set_size(size);
        size
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
        size(Vector2<f32>),
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
    frp:   Frp,
    model: Model,
    style: StyleWatchFrp,
}

impl View {
    /// Constructor.
    pub fn new(app: &Application) -> Self {
        let frp = Frp::new();
        let model = Model::new(app);
        let network = &frp.network;

        let style = StyleWatchFrp::new(&app.display.default_scene.style_sheet);
        let style_frp = LayoutParams::from_theme(&style);
        let layout_style = style_frp.flatten(network);
        let radius = style.get_number(theme::radius);

        frp::extend! { network
            // Layout
            button_size               <- radius.map(|&r| Vector2(2.0 * r, 2.0 * r));
            model.close.set_size      <+ button_size;
            model.fullscreen.set_size <+ button_size;
            button_resized            <- any_(&model.close.size,&model.fullscreen.size);
            layout_on_button_change   <- sample(&layout_style,&button_resized);
            need_relayout             <- any(&layout_style,&layout_on_button_change);
            frp.source.size           <+ need_relayout.map(f!((layout) model.set_layout(*layout)));

            // Handle the panel-wide hover
            mouse_near_buttons            <- bool(&model.shape.events.mouse_out,&model.shape.events.mouse_over);
            mouse_on_any_buttton          <- model.close.is_hovered.or(&model.fullscreen.is_hovered);
            mouse_nearby                  <- mouse_near_buttons.or(&mouse_on_any_buttton);
            model.close.mouse_nearby      <+ mouse_nearby;
            model.fullscreen.mouse_nearby <+ mouse_nearby;

            // === Handle buttons' clicked events ===
            frp.source.close      <+ model.close.clicked;
            frp.source.fullscreen <+ model.fullscreen.clicked;
        }

        let initial_style = style_frp.value();
        let initial_size = model.set_layout(initial_style);
        frp.source.size.emit(initial_size);

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
    fn app(&self) -> &Application {
        &self.model.app
    }
}
