//! The [section navigator bar](Navigator). This is a narrow bar on the left of the Searcher List
//! Panel that contains two sets of navigation buttons.
//!
//! See the [Component Browser Design Document](https://github.com/enso-org/design/blob/e6cffec2dd6d16688164f04a4ef0d9dff998c3e7/epics/component-browser/design.md).

use ensogl_core::display::shape::*;
use ensogl_core::prelude::*;
use ensogl_tooltip::tooltip::Placement;

use crate::grid::entry::icon;
use crate::AllStyles;

use enso_frp as frp;
use ensogl_core::animation::animation::delayed::DelayedAnimation;
use ensogl_core::application;
use ensogl_core::application::tooltip;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display;
use ensogl_core::display::shape::compound::rectangle::Rectangle;
use ensogl_derive_theme::FromTheme;
use ensogl_grid_view as grid;
use ensogl_hardcoded_theme::application::component_browser::component_list_panel as list_panel_theme;
use ensogl_toggle_button::ColorableShape;
use ensogl_toggle_button::ToggleButton;
use ensogl_tooltip::Tooltip;
use grid::Col;
use grid::Row;
use ide_view_component_list_panel_grid::SectionId;
use list_panel_theme::navigator as theme;



mod entry;



// =================
// === Constants ===
// =================

const MARKETPLACE_TOOLTIP: &str = "Marketplace will be available soon.";
const LOCAL_SCOPE_TOOLTIP: &str = "Search local scope";
const SHOW_SHORTCUTS_TOOLTIP: &str = "Show shortcuts";
const UNSTABLE_TOOLTIP: &str = "Search unstable/advanced components";
const DOC_PANEL_TOOLTIP: &str = "Show/hide documentation";
const MARKETPLACE_TOOLTIP_HIDE_DELAY_MS: f32 = 3000.0;
const MARKETPLACE_TOOLTIP_PLACEMENT: tooltip::Placement = tooltip::Placement::Bottom;
const TOP_BUTTONS: [icon::Id; 2] = [icon::Id::Libraries, icon::Id::Marketplace];
const TOP_BUTTONS_COUNT: usize = TOP_BUTTONS.len();
/// This is the minmum number of bottom buttons available, when no namespace sections are present.
const MIN_BOTTOM_BUTTONS_COUNT: usize = 2;


// =============
// === Style ===
// =============

#[derive(Copy, Clone, Debug, Default, FromTheme)]
#[base_path = "theme"]
pub struct Style {
    pub width:             f32,
    pub height:            f32,
    pub corner_radius:     f32,
    pub border_width:      f32,
    pub border_color:      color::Rgba,
    pub padding:           f32,
    pub gap:               f32,
    pub right_side_margin: f32,
}



// ===========
// === FRP ===
// ===========

ensogl_core::define_endpoints_2! {
    Input {
        set_local_scope_mode(bool),
        set_show_shortcuts(bool),
        set_search_unstable(bool),
        set_side_panel(bool),
    }
    Output {
        local_scope_mode(bool),
        show_shortcuts(bool),
        search_unstable(bool),
        side_panel(bool),
    }
}



// =================
// === Navigator ===
// =================

/// A section navigator bar. Contains two sets of buttons placed on the left of the Searcher List
/// Panel.
///
/// The first set on top of the bar contains "Libraries" and "Marketplace" buttons. The second
/// set on the bottom contains section navigation buttons used to quickly scroll to a specific
/// section.
#[derive(Debug, Clone, CloneRef)]
struct Navigator {
    background:  Rectangle,
    local_scope: ToggleButton<ensogl_toggle_button::any_cached::Shape>,
    shortcuts:   ToggleButton<ensogl_toggle_button::any_cached::Shape>,
    unstable:    ToggleButton<ensogl_toggle_button::any_cached::Shape>,
    marketplace: ToggleButton<ensogl_toggle_button::any_cached::Shape>,
    doc_panel:   ToggleButton<ensogl_toggle_button::any_cached::Shape>,
}

impl Navigator {
    pub fn new(app: &Application) -> Self {
        let background = Rectangle::new();
        background
            .use_auto_layout()
            .set_color(color::Rgba::transparent())
            .set_children_alignment_left_center()
            .justify_content_center_y();
        let local_scope_tooltip =
            tooltip::Style::set_label(LOCAL_SCOPE_TOOLTIP).with_placement(Placement::Top);
        let local_scope =
            ToggleButton::new_from_cached::<icon::local_scope::Shape>(app, local_scope_tooltip);
        let shortcuts_tooltip =
            tooltip::Style::set_label(SHOW_SHORTCUTS_TOOLTIP).with_placement(Placement::Top);
        let shortcuts =
            ToggleButton::new_from_cached::<icon::command_key::Shape>(app, shortcuts_tooltip);
        let unstable_tooltip =
            tooltip::Style::set_label(UNSTABLE_TOOLTIP).with_placement(Placement::Top);
        let unstable =
            ToggleButton::new_from_cached::<icon::unstable::Shape>(app, unstable_tooltip);
        let marketplace_tooltip =
            tooltip::Style::set_label(MARKETPLACE_TOOLTIP).with_placement(Placement::Top);
        let marketplace =
            ToggleButton::new_from_cached::<icon::marketplace::Shape>(app, marketplace_tooltip);
        let doc_panel_tooltip =
            tooltip::Style::set_label(DOC_PANEL_TOOLTIP).with_placement(Placement::Top);
        let doc_panel =
            ToggleButton::new_from_cached::<icon::right_side_panel::Shape>(app, doc_panel_tooltip);
        let size = Vector2(icon::SIZE, icon::SIZE);
        local_scope.set_size(size);
        shortcuts.set_size(size);
        unstable.set_size(size);
        marketplace.set_size(size);
        doc_panel.set_size(size);
        background.add_child(&local_scope);
        background.add_child(&shortcuts);
        background.add_child(&unstable);
        background.add_child(&marketplace);
        background.add_child(&doc_panel);

        Self { background, local_scope, shortcuts, unstable, marketplace, doc_panel }
    }

    fn update_style(&self, style: &Style) {
        self.background.set_size(Vector2(style.width, style.height));
        self.background.set_corner_radius(style.corner_radius);
        self.background.set_border_and_inset(style.border_width);
        self.background.set_border_color(style.border_color);
        self.background.set_gap((style.gap, 0.0)).set_padding_all(style.padding);
        self.doc_panel.set_margin_left(style.right_side_margin);
    }
}

#[derive(Debug, Clone, CloneRef, Deref)]
pub struct View {
    model: Navigator,
    #[deref]
    frp:   Frp,
}

impl View {
    pub fn new(app: &Application) -> Self {
        let model = Navigator::new(app);
        let frp = Frp::new();

        let network = frp.network();

        let styles = StyleWatchFrp::new(&scene().style_sheet);
        let style = Style::from_theme(network, &styles);
        frp::extend! { network
            eval style.update((style) model.update_style(style));
        }
        style.init.emit(());

        Self { model, frp }
    }
}

impl display::Object for View {
    fn display_object(&self) -> &display::object::Instance {
        self.model.background.display_object()
    }
}
