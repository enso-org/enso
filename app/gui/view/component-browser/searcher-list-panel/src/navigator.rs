use crate::component_group::icon;
use crate::Style;
use enso_frp as frp;
use ensogl_core::application::frp::API;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::define_endpoints_2;
use ensogl_core::display;
use ensogl_core::display::scene::Layer;
use ensogl_core::display::shape::StyleWatchFrp;
use ensogl_core::display::shape::*;
use ensogl_core::display::style;
use ensogl_core::display::style::Path;
use ensogl_core::prelude::*;
use ensogl_gui_component::component;
use ensogl_hardcoded_theme::application::component_browser::searcher as searcher_theme;
use ensogl_list_view as list_view;
use ensogl_list_view::entry::AnyModelProvider;
use ensogl_scroll_area::ScrollArea;
use ensogl_shadow as shadow;
use ensogl_text as text;
use ide_view_component_group as component_group;
use ide_view_component_group::Layers as GroupLayers;
use searcher_theme::list_panel as list_panel_theme;



// =============
// === Style ===
// =============

#[derive(Clone, Debug, Default)]
pub struct NavigatorStyle {
    pub size: Vector2,
}



// ==============
// === Shadow ===
// ==============

pub mod navigator_shadow {
    use super::*;

    ensogl_core::define_shape_system! {
        above = [crate::background];
        (style:Style) {
            let theme_path: style::Path = list_panel_theme::HERE.into();
            let content_height = style.get_number(theme_path.sub("content_height"));
            let menu_height = style.get_number(theme_path.sub("menu_height"));
            let navigator_width = style.get_number(theme_path.sub("section_navigator_width"));
            let height = content_height + menu_height;
            let width = navigator_width;
            let base_shape = Rect((width.px(), height.px() * 2.0)).translate_x(width.px());
            let shadow = shadow::from_shape(base_shape.into(), style);
            shadow.into()
        }
    }
}


// ============
// === Icon ===
// ============


// === IconParams ===

#[derive(Clone, CloneRef, Debug)]
pub struct Params {
    pub strong_color: frp::Sampler<color::Rgba>,
    pub weak_color:   frp::Sampler<color::Rgba>,
}

impl Default for Params {
    fn default() -> Self {
        let network = frp::Network::new("searcher_list_panel::navigator::Params::default");
        frp::extend! { network
            default_color <- source::<color::Rgba>().sampler();
        }
        Self { strong_color: default_color.clone_ref(), weak_color: default_color.clone_ref() }
    }
}

#[derive(Debug, Clone, CloneRef)]
struct Icon {
    display_object: display::object::Instance,
    logger:         Logger,
    icon:           Rc<RefCell<Option<icon::Any>>>,
    icon_id:        Rc<Cell<Option<icon::Id>>>,
    params:         Params,
}

impl display::Object for Icon {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}

impl list_view::Entry for Icon {
    type Model = icon::Id;
    type Params = Params;

    fn new(app: &Application, _style_prefix: &Path, params: &Self::Params) -> Self {
        let logger = app.logger.sub("NavigatorIcon");
        let display_object = display::object::Instance::new(&logger);
        let icon: Rc<RefCell<Option<icon::Any>>> = default();
        let icon_id = default();
        let network = frp::Network::new("searcher_list_panel::navigator::Icon");
        frp::extend! { network
            eval params.strong_color((&c) icon.borrow().as_ref().map(|icon| icon.strong_color.set(c
                .into()
            )));
        }

        Self { display_object, logger, icon, icon_id, params: params.clone_ref() }
    }

    fn update(&self, model: &Self::Model) {
        if !self.icon_id.get().contains(model) {
            let size = Vector2(icon::SIZE, icon::SIZE);
            let icon = model.create_shape(&self.logger, size);
            icon.strong_color.set(self.params.strong_color.value().into());
            icon.weak_color.set(self.params.weak_color.value().into());
            self.display_object.add_child(&icon);
            *self.icon.borrow_mut() = Some(icon);
            self.icon_id.set(Some(*model));
        }
    }

    fn set_max_width(&self, _max_width_px: f32) {}

    fn set_label_layer(&self, _label_layer: &Layer) {}
}



// =================
// === Navigator ===
// =================


// === Section enum ===

#[derive(Debug, Copy, Clone)]
pub enum Section {
    SubModules,
    LocalScope,
    Favourites,
}

impl Default for Section {
    fn default() -> Self {
        Section::Favourites
    }
}


// === Navigator ===

/// TODO: docs
#[derive(Debug, Clone, CloneRef)]
pub struct Navigator {
    display_object:     display::object::Instance,
    network:            frp::Network,
    bottom_buttons:     list_view::ListView<Icon>,
    top_buttons:        list_view::ListView<Icon>,
    pub chosen_section: frp::Source<Option<Section>>,
}

const TOP_BUTTONS: [icon::Id; 2] = [icon::Id::Libraries, icon::Id::Marketplace];
const BOTTOM_BUTTONS: [icon::Id; 3] = [icon::Id::SubModules, icon::Id::LocalScope, icon::Id::Star];

impl Navigator {
    pub fn new(app: &Application) -> Self {
        let display_object = display::object::Instance::new(&app.logger);
        let top_buttons = app.new_view::<list_view::ListView<Icon>>();
        let bottom_buttons = app.new_view::<list_view::ListView<Icon>>();
        top_buttons.set_background_color(HOVER_COLOR);
        bottom_buttons.set_background_color(HOVER_COLOR);
        top_buttons.show_background_shadow(false);
        bottom_buttons.show_background_shadow(false);
        top_buttons.resize(Vector2(39.0, list_view::entry::HEIGHT * TOP_BUTTONS.len() as f32));
        bottom_buttons
            .resize(Vector2(39.0, list_view::entry::HEIGHT * BOTTOM_BUTTONS.len() as f32));
        display_object.add_child(&top_buttons);
        display_object.add_child(&bottom_buttons);
        top_buttons.hide_selection();
        top_buttons.set_style_prefix(list_panel_theme::navigator::HERE.str);
        bottom_buttons.set_style_prefix(list_panel_theme::navigator::HERE.str);

        top_buttons.set_entries(AnyModelProvider::new(TOP_BUTTONS.to_vec()));
        bottom_buttons.set_entries(AnyModelProvider::new(BOTTOM_BUTTONS.to_vec()));

        let network = frp::Network::new("ComponentBrowser.Navigator");
        frp::extend! { network
            chosen_section <- source();
            eval bottom_buttons.chosen_entry([chosen_section](id) match id {
                Some(0) => chosen_section.emit(Some(Section::SubModules)),
                Some(1) => chosen_section.emit(Some(Section::LocalScope)),
                Some(2) => chosen_section.emit(Some(Section::Favourites)),
                _ => {}
            });
        }
        bottom_buttons.select_entry(Some(2));

        Self { display_object, top_buttons, bottom_buttons, network, chosen_section }
    }

    pub(crate) fn set_bottom_buttons_entry_params(&self, params: Params) {
        self.bottom_buttons.set_entry_params_and_recreate_entries(params.clone());
    }

    pub(crate) fn update_layout(&self, style: Style) {
        let top = style.navigator.size.y / 2.0;
        let bottom = -style.navigator.size.y / 2.0;
        let top_buttons_height = TOP_BUTTONS.len() as f32 * list_view::entry::HEIGHT;
        let bottom_buttons_height = BOTTOM_BUTTONS.len() as f32 * list_view::entry::HEIGHT;
        let top_padding = -3.0;
        let bottom_padding = 7.0;
        let x_pos = -style.content.size.x / 2.0 - style.navigator.size.x / 2.0;
        self.top_buttons
            .set_position_xy(Vector2(x_pos, top - top_buttons_height / 2.0 - top_padding));
        self.bottom_buttons
            .set_position_xy(Vector2(x_pos, bottom + bottom_buttons_height / 2.0 + bottom_padding));
    }
}

impl display::Object for Navigator {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}
