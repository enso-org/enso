//! The structures related to styling Component Browser Entries.

use crate::prelude::*;

use enso_frp as frp;
use ensogl_core::data::color;
use ensogl_core::display::shape::StyleWatchFrp;
use ensogl_core::display::style::data::DataMatch;
use ensogl_derive_theme::FromTheme;
use ensogl_hardcoded_theme::application::component_browser::component_list_panel as panel_theme;
use ensogl_hardcoded_theme::application::component_browser::component_list_panel::grid as grid_theme;
use entry_theme::highlight::selection as selection_theme;
use grid_theme::entry as entry_theme;



// =============
// === Color ===
// =============

/// Color of the different parts of the entry.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Color {
    /// The final color is "main" color of the component group with the specified alpha value.
    #[allow(missing_docs)]
    ComponentGroup { alpha_multiplier: f32 },
    /// The final color is defined as an arbitrary color in the stylesheet.
    Arbitrary(color::Lcha),
}

impl Default for Color {
    fn default() -> Self {
        Self::Arbitrary(color::Lcha::black())
    }
}

impl Color {
    /// Get the final color by either taking the value from [`Self::Arbitrary`] or by applying the
    /// specified transparency from [`Self::MainColorWithAlpha`] to [`main`] color.
    fn resolve(&self, main: &color::Lcha) -> color::Lcha {
        match self {
            Self::ComponentGroup { alpha_multiplier } => main.multiply_alpha(*alpha_multiplier),
            Self::Arbitrary(color) => *color,
        }
    }

    /// A custom accessor for retrieving the color from the stylesheet using the [`FromTheme`]
    /// macro. In the stylesheet, the color can be defined as either a `color::Rgba` or a `float`
    /// value for the mixing coefficient. This accessor produces the corresponding variants of
    /// [`Color`] or returns a default value ([`Color::MainColorWithAlpha(0.0)`]) if there is no
    /// such property in the stylesheet.
    fn accessor<P: Into<ensogl_core::display::style::Path>>(
        network: &frp::Network,
        style: &StyleWatchFrp,
        path: P,
    ) -> frp::Sampler<Self> {
        let path = path.into();
        let value = style.get(path.clone());
        frp::extend! { network
            init <- source_();
            color <- value.all_with(&init, move |data, _| {
                data.color().map(|color| {
                    let color = color::Lcha::from(color);
                    Color::Arbitrary(color)
                }).unwrap_or_else(|| {
                    let alpha_multiplier = match data.number() {
                        Some(number) => number,
                        None => {
                            error!("Neither color nor alpha defined for {path}.");
                            0.0
                        }
                    };
                    Color::ComponentGroup { alpha_multiplier }
                })
            });
            sampler <- color.sampler();
        }
        init.emit(());
        sampler
    }
}



// =============
// === Style ===
// =============

// === Colors ===

/// The colors of various parts of the Component Entry view. The actual color can be computed by
/// modifying the transparency of the "main" color of the component group - see [`Color`] for more
/// information.
#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, Default, PartialEq, FromTheme)]
pub struct Colors {
    #[theme_path = "entry_theme::text::color"]
    #[accessor = "Color::accessor"]
    pub text:                 Color,
    #[theme_path = "entry_theme::background::intensity"]
    pub background_intensity: f32,
    #[theme_path = "entry_theme::highlight::hover::color"]
    #[accessor = "Color::accessor"]
    pub hover_highlight:      Color,
    #[theme_path = "entry_theme::icon::color"]
    #[accessor = "Color::accessor"]
    pub icon:                 Color,
}

/// The colors of various parts of selected the Component Entry view. A subset of
/// [`StyleColors`], but `FromTheme` derive takes different style's paths, plus unrelated
/// entries are omitted.
#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, Default, PartialEq, FromTheme)]
pub struct SelectionColors {
    #[theme_path = "selection_theme::text::color"]
    #[accessor = "Color::accessor"]
    pub text:                 Color,
    #[theme_path = "selection_theme::background::intensity"]
    pub background_intensity: f32,
    #[theme_path = "selection_theme::icon::color"]
    #[accessor = "Color::accessor"]
    pub icon:                 Color,
}

impl From<SelectionColors> for Colors {
    fn from(selection: SelectionColors) -> Self {
        let SelectionColors { text, background_intensity, icon } = selection;
        let hover_highlight = default();
        Self { text, background_intensity, icon, hover_highlight }
    }
}


// === Style ===

/// The style of entries in Component List Panel Grid view, passed as a part of
/// [`Entry::Params`](ensogl_grid_view::entry::Entry).
#[allow(missing_docs)]
#[derive(Clone, Debug, Default, PartialEq, FromTheme)]
pub struct Style {
    /// The distance between left/right edge of the entry and its content.
    #[theme_path = "entry_theme::padding"]
    pub padding:                  f32,
    #[theme_path = "entry_theme::icon::size"]
    pub icon_size:                f32,
    #[theme_path = "entry_theme::text::size"]
    pub text_size:                f32,
    #[theme_path = "entry_theme::text::y_offset"]
    pub text_y_offset:            f32,
    #[theme_path = "entry_theme::text::y_offset_header"]
    pub text_y_offset_header:     f32,
    #[theme_path = "entry_theme::text::x_offset_header"]
    pub text_x_offset_header:     f32,
    /// The distance between right edge of the icon and left edge of the caption.
    #[theme_path = "entry_theme::icon::text_padding"]
    pub icon_text_padding:        f32,
    #[theme_path = "entry_theme::text::font"]
    pub font:                     ImString,
    #[theme_path = "entry_theme::highlight::corners_radius"]
    pub selection_corners_radius: f32,
    #[theme_path = "entry_theme::text::highlight_bold"]
    pub highlight_bold:           f32,
    #[theme_path = "entry_theme::shadow::size"]
    pub header_shadow_size:       f32,
}



// ======================
// === ResolvedColors ===
// ======================

/// Colors used in the Component Group Entries.
///
/// This structure can be created from a single "main color" input. Each of these colors can be
/// computed by modifying the transparency of the "main color". See [`Color`] for more information.
#[allow(missing_docs)]
#[derive(Clone, CloneRef, Debug)]
pub struct ResolvedColors {
    pub background:      frp::Sampler<color::Lcha>,
    pub hover_highlight: frp::Sampler<color::Lcha>,
    pub text:            frp::Sampler<color::Lcha>,
    pub icon:            frp::Sampler<color::Lcha>,
}

impl ResolvedColors {
    /// Create color outputs from given main color and style.
    ///
    /// Each of these colors can be computed by modifying the transparency of the "main color". See
    /// [`Color`] for more information. All color changes are animated.
    pub fn from_main_color(
        network: &frp::Network,
        style_watch: &StyleWatchFrp,
        main_color: &frp::Stream<color::Lcha>,
        colors: &frp::Stream<Colors>,
    ) -> Self {
        let panel_background = style_watch.get_color(panel_theme::background_color);
        let colors = colors.clone_ref();

        frp::extend! { network
            init <- source_();

            // We do not support the semi-transparent background of entries. This was needed in
            // times when the group headers could be displayed over entries (and the headers shared
            // the same background color; therefore, the semi-transparent header's background
            // would reveal the underlying entries). It's kept for case we would return to
            // displaying headers some day.
            panel_bg <- all_with(&panel_background, &init, |col, ()| color::Lcha::from(col));
            panel_bg_and_main <- all(&panel_bg, main_color);
            bg_intensity <- colors.map(|c| c.background_intensity);
            background <- all_with(&panel_bg_and_main, &bg_intensity,
                |(bg, main), intensity| color::mix(*bg, *main, *intensity)
            ).sampler();
            hover_highlight <- all_with(main_color, &colors,
                |main, colors| colors.hover_highlight.resolve(main)
            ).sampler();
            text <- all_with(main_color, &colors,
                |main, colors| colors.text.resolve(main)
            ).sampler();
            icon <- all_with(main_color, &colors,
                |main, colors| colors.icon.resolve(main)
            ).sampler();
        }
        init.emit(());
        Self { icon, text, background, hover_highlight }
    }
}
