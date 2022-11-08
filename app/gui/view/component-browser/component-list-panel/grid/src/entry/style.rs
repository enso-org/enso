//! The structures related to styling Component Browser Entries.

use crate::prelude::*;

use enso_frp as frp;
use ensogl_core::data::color;
use ensogl_core::display::shape::StyleWatchFrp;
use ensogl_core::display::style::data::DataMatch;
use ensogl_derive_theme::FromTheme;
use ensogl_hardcoded_theme::application::component_browser::component_list_panel::grid as grid_theme;
use entry_theme::highlight::selection as selection_theme;
use grid_theme::entry as entry_theme;



/// Color of the different parts of the entry.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Color {
    /// The final color is a result of mixing the application background color with the "main"
    /// color of the component group using a given coefficient.
    MainColorIntensity(f32),
    /// The final color is defined as an arbitrary color in the stylesheet.
    Arbitrary(color::Lcha),
}

impl Default for Color {
    fn default() -> Self {
        Self::MainColorIntensity(0.0)
    }
}

impl Color {
    /// Get the final color by either taking the value from [`Self::Arbitrary`] or mixing the two
    /// provided colors.
    fn mix(&self, color: &color::Lcha) -> color::Lcha {
        match self {
            Self::MainColorIntensity(intensity) =>
                color::Lcha::new(color.lightness, color.chroma, color.hue, *intensity),
            Self::Arbitrary(color) => *color,
        }
    }

    /// A custom accessor for retrieving the color from the stylesheet using the [`FromTheme`]
    /// macro. In the stylesheet, the color can be defined as either a `color::Rgba` or a `float`
    /// value for the mixing coefficient. This accessor produces the corresponding variants of
    /// [`Color`] or returns a default value ([`Color::MainColorIntensity(0.0)`]) if there is no
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
                    let intensity = match data.number() {
                        Some(number) => number,
                        None => {
                            error!("Neither color nor intensity defined ({path}).");
                            0.0
                        }
                    };
                    Color::MainColorIntensity(intensity)
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

// === Style Colors ===

/// The colors of various parts of the Component Entry view. The actual color can be computed by
/// mixing the "main" group's color with the application background - see [`Color`] for more
/// information.
#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, Default, PartialEq, FromTheme)]
pub struct StyleColors {
    #[theme_path = "entry_theme::text::color"]
    #[accessor = "Color::accessor"]
    pub text:            Color,
    #[theme_path = "entry_theme::background::color"]
    #[accessor = "Color::accessor"]
    pub background:      Color,
    #[theme_path = "entry_theme::highlight::hover::color"]
    #[accessor = "Color::accessor"]
    pub hover_highlight: Color,
    /// The more contrasting parts of the [icon](crate::icon::Any).
    #[theme_path = "entry_theme::icon::color"]
    #[accessor = "Color::accessor"]
    pub icon:            Color,
    #[theme_path = "entry_theme::dimmed"]
    pub dimmed:          color::Rgba,
}

/// The colors of various parts of selected the Component Entry view. A subset of
/// [`StyleColors`], but `FromTheme` derive takes different style's paths, plus unrelated
/// entries are omitted.
#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, Default, PartialEq, FromTheme)]
pub struct SelectionStyleColors {
    #[theme_path = "selection_theme::text::color"]
    #[accessor = "Color::accessor"]
    pub text:       Color,
    #[theme_path = "selection_theme::background::color"]
    #[accessor = "Color::accessor"]
    pub background: Color,
    /// The more contrasting parts of the [icon](crate::icon::Any).
    #[theme_path = "selection_theme::icon::color"]
    #[accessor = "Color::accessor"]
    pub icon:       Color,
}

impl From<SelectionStyleColors> for StyleColors {
    fn from(selection: SelectionStyleColors) -> Self {
        let SelectionStyleColors { text, background, icon } = selection;
        let dimmed = default();
        let hover_highlight = background;
        Self { text, background, icon, dimmed, hover_highlight }
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



// ==============
// === Colors ===
// ==============

/// Colors used in the Component Group Entries.
///
/// This structure can be created from single "main color" input. Each of these colors will be
/// computed by mixing "main color" with application background.
///
/// `icon_strong` and `icon_weak` parameters represent the more/less contrasting parts of the
/// [icon](crate::icon::Any), they do not represent highlighted state of the icon.
#[allow(missing_docs)]
#[derive(Clone, CloneRef, Debug)]
pub struct Colors {
    pub background:      frp::Sampler<color::Lcha>,
    pub hover_highlight: frp::Sampler<color::Lcha>,
    pub text:            frp::Sampler<color::Lcha>,
    pub icon:            frp::Sampler<color::Lcha>,
    pub skip_animations: frp::Any,
}

impl Colors {
    /// Create color outputs from given main color and style.
    ///
    /// Each of these colors will be computed by mixing "main color" with application background
    /// according to the proper field in [`Style::color_intensities`]. The dimming of group is
    /// animated.
    pub fn from_main_color(
        network: &frp::Network,
        style_watch: &StyleWatchFrp,
        main_color: &frp::Stream<color::Lcha>,
        style_colors: &frp::Stream<StyleColors>,
        is_dimmed: &frp::Stream<bool>,
    ) -> Self {
        let app_bg = style_watch.get_color(ensogl_hardcoded_theme::application::background);
        let style_colors = style_colors.clone_ref();
        let color_anim = color::Animation::new(network);

        frp::extend! { network
            init <- source_();

            app_bg <- all_with(&app_bg, &init, |col, ()| color::Lcha::from(col));
            app_bg_and_main <- all(&app_bg, &color_anim.value);
            dimmed <- style_colors.map(|s| color::Lcha::from(s.dimmed));
            let is_dimmed = is_dimmed.clone_ref();
            color_anim.target <+ switch(&is_dimmed, main_color, &dimmed);

            background <- all_with(&app_bg_and_main, &style_colors,
                |(bg, main), colors| match colors.background {
                    Color::Arbitrary(color) => color,
                    Color::MainColorIntensity(intensity) => color::mix(*bg, *main, intensity),
                }
            ).sampler();
            hover_highlight <- all_with(&color_anim.value, &style_colors,
                |main, colors| colors.hover_highlight.mix(main)
            ).sampler();
            text <- all_with(&color_anim.value, &style_colors,
                |main, colors| colors.text.mix(main)
            ).sampler();
            icon <- all_with(&color_anim.value, &style_colors,
                |main, colors| colors.icon.mix(main)
            ).sampler();

            skip_animations <- any(...);
            color_anim.skip <+ skip_animations;
        }
        init.emit(());
        Self { icon, text, background, hover_highlight, skip_animations }
    }
}
