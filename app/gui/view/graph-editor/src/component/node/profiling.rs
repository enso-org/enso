//! Provides [`Status`] to represent a node's execution status, [`Status::display_color`] to express
//! that status as a color and [`ProfilingLabel`] to display a node's execution status.

use crate::prelude::*;
use ensogl::display::shape::*;

use crate::view;

use enso_frp as frp;
use ensogl::application::Application;
use ensogl::data::color;
use ensogl::display;
use ensogl::gui::text;



// ==============
// === Status ===
// ==============

/// Describes whether the source code in a node is currently running or already finished. If it is
/// finished then the status contains the number of milliseconds that it took to run the code.
#[derive(Debug, Copy, Clone)]
pub enum Status {
    /// The node's code is still running.
    Running,
    /// The node finished execution.
    Finished {
        /// How many milliseconds the node took to execute.
        duration: f32,
    },
}

impl Status {
    /// Returns `true` if the node is still running.
    pub fn is_running(self) -> bool {
        matches!(self, Status::Running)
    }

    /// Returns `true` if the node finished execution.
    pub fn is_finished(self) -> bool {
        matches!(self, Status::Finished { .. })
    }
}

impl Default for Status {
    fn default() -> Self {
        Status::Running
    }
}

impl Display for Status {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Status::Running => {
                write!(f, "")
            }
            Status::Finished { duration } => {
                let milliseconds = duration;
                let seconds = milliseconds / 1000.0;
                let minutes = seconds / 60.0;
                let hours = minutes / 60.0;
                if hours >= 1.0 {
                    write!(f, "{:.1} h", hours)
                } else if minutes >= 1.0 {
                    write!(f, "{:.1} m", minutes)
                } else if seconds >= 1.0 {
                    write!(f, "{:.1} s", seconds)
                } else {
                    write!(f, "{:.0} ms", milliseconds)
                }
            }
        }
    }
}



// =============
// === Color ===
// =============

/// A theme that determines how we express the running time of a node (compared to other nodes on
/// the stage) in a color. The color's lightness and chroma in LCh color space are directly taken
/// from the theme. The chroma will be `min_time_hue` for the node with the shortest running time,
/// `max_time_hue` for the node with the longest running time and linearly interpolated in-between
/// depending on the relative running, time for all other nodes.
#[derive(Debug, Copy, Clone, Default)]
pub struct Theme {
    /// The lightness for all running times.
    pub lightness:    f32,
    /// The chroma for all running times.
    pub chroma:       f32,
    /// The hue for the minimum running time.
    pub min_time_hue: f32,
    /// The hue for the maximum running time.
    pub max_time_hue: f32,
}

impl Theme {
    /// Returns a new `Sampler` exposing the profiling theme, as it is defined in `styles` at path
    /// `ensogl_hardcoded_theme::graph_editor::node::profiling`. The sampler is registered under
    /// `network`.
    pub fn from_styles(styles: &StyleWatchFrp, network: &frp::Network) -> frp::Sampler<Theme> {
        use ensogl_hardcoded_theme::graph_editor::node::profiling as theme_path;
        let lightness = styles.get_number_or(theme_path::lightness, 0.5);
        let chroma = styles.get_number_or(theme_path::chroma, 1.0);
        let min_time_hue = styles.get_number_or(theme_path::min_time_hue, 0.4);
        let max_time_hue = styles.get_number_or(theme_path::max_time_hue, 0.1);

        frp::extend! { network
            init_theme    <- source::<()>();
            theme         <- all_with5(&lightness,&chroma,&min_time_hue,&max_time_hue,&init_theme
                ,|&lightness,&chroma,&min_time_hue,&max_time_hue,_|
                    Theme {lightness,chroma,min_time_hue,max_time_hue});
            theme_sampler <- theme.sampler();
        }

        init_theme.emit(());
        theme_sampler
    }
}

impl Status {
    /// Expresses the profiling status as a color, depending on the minimum and maximum running
    /// time of any node on the stage and a [`Theme`] that allows to tweak how the colors are
    /// chosen. A node that is still running will be treated like finished node with the current
    /// maximum execution time.
    pub fn display_color(
        self,
        min_global_duration: f32,
        max_global_duration: f32,
        theme: Theme,
    ) -> color::Lch {
        let duration = match self {
            Status::Running => max_global_duration,
            Status::Finished { duration } => duration,
        };
        let duration_delta = max_global_duration - min_global_duration;
        let hue_delta = theme.max_time_hue - theme.min_time_hue;
        let relative_duration = if duration_delta != 0.0 && !duration_delta.is_nan() {
            (duration - min_global_duration) / duration_delta
        } else {
            0.0
        };
        let relative_hue = relative_duration;
        let hue = theme.min_time_hue + relative_hue * hue_delta;
        color::Lch::new(theme.lightness, theme.chroma, hue)
    }
}



// ============
// === Frp  ===
// ============

ensogl::define_endpoints! {
    Input {
        set_status              (Status),
        set_min_global_duration (f32),
        set_max_global_duration (f32),
        set_view_mode           (view::Mode),
    }
}



// ==========================
// === Running Time Label ===
// ==========================

/// A `display::Object` providing a label for nodes that displays the node's running time in
/// profiling mode after the node finished execution. The node's execution status has to be provided
/// through `set_status`, the view mode through `set_view_mode`, the minimum and maximum running
/// time of any node on the stage through `set_min_global_duration` and `set_max_global_duration`.
/// The color of the label will reflect the status and be determined by [`Status::display_color`].
/// The necessary theme will be taken from the application's style sheet. The origin of the label,
/// as a `display::Object` should be placed on the node's center.
#[derive(Clone, CloneRef, Debug)]
pub struct ProfilingLabel {
    root:   display::object::Instance,
    label:  text::Text,
    frp:    Frp,
    styles: StyleWatchFrp,
}

impl Deref for ProfilingLabel {
    type Target = Frp;

    fn deref(&self) -> &Self::Target {
        &self.frp
    }
}

impl ProfilingLabel {
    /// Constructs a `ProfilingLabel` for the given application.
    pub fn new(app: &Application) -> Self {
        let scene = &app.display.default_scene;
        let root = display::object::Instance::new();

        let label = text::Text::new(app);
        root.add_child(&label);
        label.set_position_y(crate::component::node::input::area::TEXT_SIZE / 2.0);
        label.remove_from_scene_layer(&scene.layers.main);
        label.add_to_scene_layer(&scene.layers.label);

        let frp = Frp::new();
        let network = &frp.network;
        let color = color::Animation::new(network);

        frp::extend! { network

            // === Visibility ===

            visibility <- all_with(&frp.set_view_mode,&frp.set_status,|mode,status| {
                matches!((mode,status),(view::Mode::Profiling,Status::Finished {..}))
            });

            color.target_alpha <+ visibility.map(|&is_visible| {
                if is_visible { 1.0 } else { 0.0 }
            });


            // === Color ===

            let styles = StyleWatchFrp::new(&scene.style_sheet);
            let theme  = Theme::from_styles(&styles,network);
            color.target_color <+ all_with4
                (&frp.set_status,&frp.set_min_global_duration,&frp.set_max_global_duration,&theme,
                    |&status,&min,&max,&theme| status.display_color(min,max,theme)
                );
            label.set_property_default <+ color.value.ref_into_some();


            // === Position ===

            let x_offset = crate::component::node::input::area::TEXT_OFFSET;
            eval label.width((&width) label.set_position_x(-width-x_offset));


            // === Content ===

            label.set_content <+ frp.set_status.map(|status| status.to_im_string());
        }

        ProfilingLabel { root, label, frp, styles }
    }

    /// Set a scene layer for text rendering.
    pub fn set_label_layer(&self, layer: &display::scene::Layer) {
        self.label.add_to_scene_layer(layer);
    }
}

impl display::Object for ProfilingLabel {
    fn display_object(&self) -> &display::object::Instance {
        &self.root
    }
}
