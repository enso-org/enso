//! The [button panel](Model). This is a narrow bar on the top of the Component List Panel
//! that contains various buttons for controlling the Component Browser.

use ensogl_core::display::shape::*;
use ensogl_core::prelude::*;

use crate::grid::entry::icon;

use enso_frp as frp;
use ensogl_core::application::tooltip;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display;
use ensogl_core::display::shape::compound::rectangle::Rectangle;
use ensogl_derive_theme::FromTheme;
use ensogl_hardcoded_theme::application::component_browser::component_list_panel as list_panel_theme;
use ensogl_toggle_button::ToggleButton;
use ensogl_tooltip::tooltip::Placement;
use list_panel_theme::button_panel as theme;



// =================
// === Constants ===
// =================

const MARKETPLACE_TOOLTIP: &str = "Marketplace will be available soon.";
const LOCAL_SCOPE_TOOLTIP: &str = "Search local scope";
const SHOW_SHORTCUTS_TOOLTIP: &str = "Show shortcuts";
const UNSTABLE_TOOLTIP: &str = "Search unstable/advanced components";
const SIDE_PANEL_TOOLTIP: &str = "Show/hide documentation";


// =============
// === Style ===
// =============

#[derive(Copy, Clone, Debug, Default, FromTheme)]
#[base_path = "theme"]
pub struct Style {
    pub width:                 f32,
    pub height:                f32,
    pub corner_radius:         f32,
    pub border_width:          f32,
    pub inner_border_distance: f32,
    pub border_color:          color::Rgba,
    pub background_color:      color::Rgba,
    pub padding:               f32,
    pub gap:                   f32,
    pub right_side_margin:     f32,
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



// =============
// === Model ===
// =============

#[derive(Debug, Clone, CloneRef)]
struct Model {
    background:  Rectangle,
    local_scope: ToggleButton<ensogl_toggle_button::any_cached::Shape>,
    shortcuts:   ToggleButton<ensogl_toggle_button::any_cached::Shape>,
    unstable:    ToggleButton<ensogl_toggle_button::any_cached::Shape>,
    marketplace: ToggleButton<ensogl_toggle_button::any_cached::Shape>,
    side_panel:  ToggleButton<ensogl_toggle_button::any_cached::Shape>,
}

impl Model {
    pub fn new(app: &Application) -> Self {
        let background = Rectangle::new();
        background
            .use_auto_layout()
            .set_color(color::Rgba::transparent())
            .set_children_alignment_left_center()
            .justify_content_center_y();
        let tooltip = |label: &str| tooltip::Style::set_label(label).with_placement(Placement::Top);
        let local_scope_tooltip = tooltip(LOCAL_SCOPE_TOOLTIP);
        let local_scope = new_button::<icon::local_scope::Shape>(app, local_scope_tooltip);
        let shortcuts_tooltip = tooltip(SHOW_SHORTCUTS_TOOLTIP);
        let shortcuts = new_button::<icon::command_key::Shape>(app, shortcuts_tooltip);
        let unstable_tooltip = tooltip(UNSTABLE_TOOLTIP);
        let unstable = new_button::<icon::unstable::Shape>(app, unstable_tooltip);
        let marketplace_tooltip = tooltip(MARKETPLACE_TOOLTIP);
        let marketplace = new_button::<icon::marketplace::Shape>(app, marketplace_tooltip);
        let side_panel_tooltip = tooltip(SIDE_PANEL_TOOLTIP);
        let side_panel = new_button::<icon::right_side_panel::Shape>(app, side_panel_tooltip);
        let size = Vector2(icon::SIZE, icon::SIZE);
        local_scope.set_size(size);
        shortcuts.set_size(size);
        unstable.set_size(size);
        marketplace.set_size(size);
        side_panel.set_size(size);
        background.add_child(&local_scope);
        background.add_child(&shortcuts);
        background.add_child(&unstable);
        background.add_child(&marketplace);
        background.add_child(&side_panel);

        Self { background, local_scope, shortcuts, unstable, marketplace, side_panel }
    }

    fn update_style(&self, style: &Style) {
        self.background.set_size(Vector2(style.width, style.height));
        self.background.set_corner_radius(style.corner_radius);
        self.background.set_inner_border(style.border_width, style.inner_border_distance);
        self.background.set_border_color(style.border_color);
        self.background.set_color(style.background_color);
        self.background.set_gap((style.gap, 0.0)).set_padding_all(style.padding);
        self.side_panel.set_margin_left(style.right_side_margin);
    }
}



// ============
// === View ===
// ============

/// A narrow bar on the top of the Component List Panel that contains various buttons for
/// controlling the Component Browser.
#[derive(Debug, Clone, CloneRef, Deref)]
pub struct View {
    model: Model,
    #[deref]
    frp:   Frp,
}

impl View {
    pub fn new(app: &Application) -> Self {
        let model = Model::new(app);
        let frp = Frp::new();

        let network = frp.network();
        let out = &frp.private.output;

        let styles = StyleWatchFrp::new(&scene().style_sheet);
        let style = Style::from_theme(network, &styles);
        frp::extend! { network
            init <- source_();
            eval style.update((style) model.update_style(style));


            // === Initial state ===

            model.side_panel.set_state <+ init.constant(true);
            // Buttons below are not implemented.
            model.local_scope.set_read_only <+ init.constant(true);
            model.shortcuts.set_read_only <+ init.constant(true);
            model.unstable.set_read_only <+ init.constant(true);
            model.marketplace.set_read_only <+ init.constant(true);


            // === Buttons ===

            model.local_scope.set_state <+ frp.set_local_scope_mode;
            model.shortcuts.set_state <+ frp.set_show_shortcuts;
            model.unstable.set_state <+ frp.set_search_unstable;
            model.side_panel.set_state <+ frp.set_side_panel;
            out.local_scope_mode <+ model.local_scope.state;
            out.search_unstable <+ model.unstable.state;
            out.show_shortcuts <+ model.shortcuts.state;
            out.side_panel <+ model.side_panel.state;
        }
        init.emit(());
        style.init.emit(());

        Self { model, frp }
    }
}

impl display::Object for View {
    fn display_object(&self) -> &display::object::Instance {
        self.model.background.display_object()
    }
}



// ===============
// === Helpers ===
// ===============

/// Create new ToggleButton with cached icon. A call site becomes a bit shorter.
fn new_button<T: CachedShape>(
    app: &Application,
    tooltip: tooltip::Style,
) -> ToggleButton<ensogl_toggle_button::any_cached::Shape> {
    ToggleButton::new_from_cached::<T>(app, tooltip)
}
