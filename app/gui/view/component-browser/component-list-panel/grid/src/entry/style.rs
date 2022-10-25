//! The structures related to styling Component Browser Entries.

use crate::prelude::*;

use enso_frp as frp;
use ensogl_core::data::color;
use ensogl_core::display::shape::StyleWatchFrp;
use ensogl_core::Animation;
use ensogl_derive_theme::FromTheme;
use ensogl_hardcoded_theme::application::component_browser::component_list_panel::grid as grid_theme;
use entry_theme::highlight::selection as selection_theme;
use grid_theme::entry as entry_theme;



// =============
// === Style ===
// =============

// === Color Intensities ===

/// The intensities of various parts of Component Entry view. The actual color is computed by mixing
/// the main groups color with the application background - see [`Colors`] for more information.
#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, Default, PartialEq, FromTheme)]
pub struct ColorIntensities {
    #[theme_path = "entry_theme::text::color_intensity"]
    pub text:            f32,
    #[theme_path = "entry_theme::background::color_intensity"]
    pub background:      f32,
    #[theme_path = "entry_theme::highlight::hover::color_intensity"]
    pub hover_highlight: f32,
    #[theme_path = "entry_theme::dimmed::color_intensity"]
    pub dimmed:          f32,
    /// The more contrasting parts of the [icon](crate::icon::Any).
    #[theme_path = "entry_theme::icon::strong_color_intensity"]
    pub icon_strong:     f32,
    /// The less contrasting parts of the [icon](crate::icon::Any).
    #[theme_path = "entry_theme::icon::weak_color_intensity"]
    pub icon_weak:       f32,
}

/// The intensities of various parts of selected Component Entry view. A subset of
/// [`ColorIntensities`], but `FromTheme` derive takes different style's paths, plus unrelated
/// entries are omitted.
#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, Default, PartialEq, FromTheme)]
pub struct SelectionColorIntensities {
    #[theme_path = "selection_theme::text::color_intensity"]
    pub text:        f32,
    #[theme_path = "selection_theme::background::color_intensity"]
    pub background:  f32,
    /// The more contrasting parts of the [icon](crate::icon::Any).
    #[theme_path = "selection_theme::icon_strong::color_intensity"]
    pub icon_strong: f32,
    /// The less contrasting parts of the [icon](crate::icon::Any).
    #[theme_path = "selection_theme::icon_weak::color_intensity"]
    pub icon_weak:   f32,
}

impl From<SelectionColorIntensities> for ColorIntensities {
    fn from(selection: SelectionColorIntensities) -> Self {
        let SelectionColorIntensities { text, background, icon_strong, icon_weak } = selection;
        let dimmed = 1.0;
        let hover_highlight = background;
        Self { text, background, icon_weak, icon_strong, dimmed, hover_highlight }
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
    pub icon_strong:     frp::Sampler<color::Lcha>,
    pub icon_weak:       frp::Sampler<color::Lcha>,
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
        color: &frp::Stream<color::Lcha>,
        color_intensities: &frp::Stream<ColorIntensities>,
        is_dimmed: &frp::Stream<bool>,
    ) -> Self {
        fn mix((c1, c2): &(color::Lcha, color::Lcha), coefficient: &f32) -> color::Lcha {
            color::mix(*c1, *c2, *coefficient)
        }
        let app_bg = style_watch.get_color(ensogl_hardcoded_theme::application::background);
        let color_intensities = color_intensities.clone_ref();

        let intensity = Animation::new(network);
        frp::extend! { network
            init <- source_();

            text_intensity <- color_intensities.map(|c| c.text);
            bg_intensity <- color_intensities.map(|c| c.background);
            hover_hg_intensity <- color_intensities.map(|c| c.hover_highlight);
            dimmed_intensity <- color_intensities.map(|c| c.dimmed);
            icon_strong_intensity <- color_intensities.map(|c| c.icon_strong);
            icon_weak_intensity <- color_intensities.map(|c| c.icon_weak);

            one <- init.constant(1.0);
            let is_dimmed = is_dimmed.clone_ref();
            intensity.target <+ is_dimmed.switch(&one, &dimmed_intensity);
            app_bg <- all_with(&app_bg, &init, |col, ()| color::Lcha::from(col));
            app_bg_and_input <- all(&app_bg, color);
            main <- app_bg_and_input.all_with(&intensity.value, mix);
            app_bg_and_main <- all(&app_bg, &main);
            background <- app_bg_and_main.all_with(&bg_intensity, mix).sampler();
            hover_highlight <- app_bg_and_main.all_with(&hover_hg_intensity, mix).sampler();
            text <- app_bg_and_main.all_with(&text_intensity, mix).sampler();
            icon_weak <- app_bg_and_main.all_with(&icon_weak_intensity, mix).sampler();
            icon_strong <- app_bg_and_main.all_with(&icon_strong_intensity, mix).sampler();

            skip_animations <- any(...);
            intensity.skip <+ skip_animations;
        }
        init.emit(());
        Self { icon_weak, icon_strong, text, background, hover_highlight, skip_animations }
    }
}
