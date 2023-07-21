//! The [section navigator bar](Navigator). This is a narrow bar on the left of the Searcher List
//! Panel that contains two sets of navigation buttons.
//!
//! See the [Component Browser Design Document](https://github.com/enso-org/design/blob/e6cffec2dd6d16688164f04a4ef0d9dff998c3e7/epics/component-browser/design.md).

use ensogl_core::prelude::*;

use enso_frp as frp;
use ensogl_core::data::color;
use ensogl_core::display::shape::StyleWatchFrp;
use ensogl_derive_theme::FromTheme;
use ensogl_hardcoded_theme::application::component_browser::component_list_panel as list_panel_theme;
use list_panel_theme::navigator as theme;



// =============
// === Style ===
// =============

#[allow(missing_docs)]
#[derive(Copy, Clone, Debug, Default, FromTheme)]
#[base_path = "theme"]
pub struct Style {
    pub width:                    f32,
    pub button_size:              f32,
    pub top_padding:              f32,
    pub bottom_padding:           f32,
    pub hover_color:              color::Rgba,
    #[theme_path = "theme::highlight::color"]
    pub highlight_color:          color::Rgba,
    #[theme_path = "theme::highlight::size"]
    pub highlight_size:           f32,
    #[theme_path = "theme::highlight::corners_radius"]
    pub highlight_corners_radius: f32,
}
