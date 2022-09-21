//! The structures related to styling Component Browser Entries.

use crate::prelude::*;

use enso_frp as frp;
use ensogl_core::data::color;
use ensogl_core::Animation;
use ensogl_text as text;



// =============
// === Style ===
// =============

// === Color Intensities ===

/// The intensities of various parts of Component Entry view. The actual color is computed by mixing
/// the main groups color with the application background - see [`Colors`] for more information.
#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct ColorIntensities {
    pub text:            f32,
    pub background:      f32,
    pub hover_highlight: f32,
    pub dimmed:          f32,
    /// The more contrasting parts of the [icon](crate::icon::Any).
    pub icon_strong:     f32,
    /// The less contrasting parts of the [icon](crate::icon::Any).
    pub icon_weak:       f32,
}


// === Style ===

/// The style of entries in Component List Panel Grid view, passed as a part of
/// [`Entry::Params`](ensogl_grid_view::entry::Entry).
#[allow(missing_docs)]
#[derive(Clone, Debug, Default, PartialEq)]
pub struct Style {
    pub color_intensities:        ColorIntensities,
    /// The width of groups. The entries in main grid will have this width regardless of column
    /// widths set in GridView, thus making a vertical groups separators.
    pub group_width:              f32,
    /// The horizontal gap size in pixels between groups in main grip.
    pub gap_between_groups:       f32,
    /// The distance between left/right edge of the entry and its content.
    pub padding:                  f32,
    pub icon_size:                f32,
    pub text_size:                text::Size,
    /// The distance between right edge of the icon and left edge of the caption.
    pub icon_text_padding:        f32,
    pub font:                     ImString,
    pub selection_corners_radius: f32,
    pub highlight_bold:           f32,
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
    pub background:      frp::Sampler<color::Rgba>,
    pub hover_highlight: frp::Sampler<color::Rgba>,
    pub text:            frp::Sampler<color::Rgba>,
    pub icon_strong:     frp::Sampler<color::Rgba>,
    pub icon_weak:       frp::Sampler<color::Rgba>,
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
        color: &frp::Stream<color::Rgba>,
        style: &frp::Stream<Style>,
        is_dimmed: &frp::Stream<bool>,
    ) -> Self {
        fn mix((c1, c2): &(color::Rgba, color::Rgba), coefficient: &f32) -> color::Rgba {
            color::mix(*c1, *c2, *coefficient)
        }
        let app_bg = style_watch.get_color(ensogl_hardcoded_theme::application::background);
        let style = style.clone_ref();

        let intensity = Animation::new(network);
        frp::extend! { network
            init <- source_();

            text_intensity <- style.map(|p| p.color_intensities.text);
            bg_intensity <- style.map(|p| p.color_intensities.background);
            hover_hg_intensity <- style.map(|p| p.color_intensities.hover_highlight);
            dimmed_intensity <- style.map(|p| p.color_intensities.dimmed);
            icon_strong_intensity <- style.map(|p| p.color_intensities.icon_strong);
            icon_weak_intensity <- style.map(|p| p.color_intensities.icon_weak);

            one <- init.constant(1.0);
            let is_dimmed = is_dimmed.clone_ref();
            intensity.target <+ is_dimmed.switch(&one, &dimmed_intensity);
            app_bg <- all(&app_bg, &init)._0();
            app_bg_and_input <- all(&app_bg, color);
            main <- app_bg_and_input.all_with(&intensity.value, mix);
            app_bg_and_main <- all(&app_bg, &main);
            background <- app_bg_and_main.all_with(&bg_intensity, mix).sampler();
            hover_highlight <- app_bg_and_main.all_with(&hover_hg_intensity, mix).sampler();
            text <- app_bg_and_main.all_with(&text_intensity, mix).sampler();
            icon_weak <- app_bg_and_main.all_with(&icon_weak_intensity, mix).sampler();
            icon_strong <- app_bg_and_main.all_with(&icon_strong_intensity, mix).sampler();
        }
        init.emit(());
        Self { icon_weak, icon_strong, text, background, hover_highlight }
    }
}
