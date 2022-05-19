//! This module defines an entry in a component group view.
//!
//! The entry data is represented by the [`Model`] and visualized by the [`View`].

use crate::prelude::*;

use crate::icon;
use crate::Colors;

use enso_frp as frp;
use ensogl::application::Application;
use ensogl::data::color;
use ensogl::display;
use ensogl::display::scene::Layer;
use ensogl::display::style;
use ensogl::display::Scene;
use ensogl_hardcoded_theme::application::component_browser::component_group::entry_list as theme;
use ensogl_list_view as list_view;
use ensogl_list_view::entry::GlyphHighlightedLabel;
use ensogl_list_view::entry::GlyphHighlightedLabelModel;



// ===============
// === Aliases ===
// ===============

/// ID of a component group entry in a ListView.
pub type Id = list_view::entry::Id;



// =================
// === Constants ===
// =================

/// Path to the theme of the component group entries in the application style sheet.
pub const STYLE_PATH: &str = theme::HERE.str;



// =============
// === Model ===
// =============

/// Data underlying an entry in a component group view.
#[allow(missing_docs)]
#[derive(Clone, Debug, Default)]
pub struct Model {
    pub icon:             icon::Id,
    pub highlighted_text: GlyphHighlightedLabelModel,
}

impl From<String> for Model {
    fn from(label: String) -> Self {
        Model {
            icon:             icon::Id::Star,
            highlighted_text: GlyphHighlightedLabelModel { label, highlighted: default() },
        }
    }
}

impl From<&str> for Model {
    fn from(label: &str) -> Self {
        label.to_owned().into()
    }
}



// ==============
// === Params ===
// ==============

/// A type parametrizing the visual aspects of how an entry will be rendered in an instance of
/// component group view.
#[allow(missing_docs)]
#[derive(Clone, CloneRef, Debug)]
pub struct Params {
    pub colors: Colors,
}

impl Default for Params {
    fn default() -> Self {
        let network = frp::Network::new("component_browser::entry::Params::default");
        frp::extend! { network
            icon_strong <- source::<color::Rgba>().sampler();
            icon_weak <- source::<color::Rgba>().sampler();
            header_text <- source::<color::Rgba>().sampler();
            entry_text <- source::<color::Rgba>().sampler();
            background <- source::<color::Rgba>().sampler();
        }

        let colors = Colors { icon_strong, icon_weak, header_text, entry_text, background };
        Self { colors }
    }
}



// ============
// === View ===
// ============


// === CurrentIcon ===

/// The structure keeping a currently displayed incon in Component Group Entry [`View`]. Remembering
/// id allows us to skip icon generation when not changed.
#[derive(Debug, Default)]
struct CurrentIcon {
    shape: Option<icon::Any>,
    id:    Option<icon::Id>,
}


// === View ===

/// A visual representation of a [`Model`].
#[derive(Clone, CloneRef, Debug)]
pub struct View {
    logger:            Logger,
    display_object:    display::object::Instance,
    icon:              Rc<RefCell<CurrentIcon>>,
    max_width_px:      frp::Source<f32>,
    icon_strong_color: frp::Sampler<color::Rgba>,
    icon_weak_color:   frp::Sampler<color::Rgba>,
    label:             GlyphHighlightedLabel,
}

impl list_view::Entry for View {
    type Model = Model;
    type Params = Params;

    fn new(app: &Application, style_prefix: &style::Path, Params { colors }: &Params) -> Self {
        let logger = Logger::new("component-group::Entry");
        let display_object = display::object::Instance::new(&logger);
        let icon: Rc<RefCell<CurrentIcon>> = default();
        let label = GlyphHighlightedLabel::new(app, style_prefix, &());
        display_object.add_child(&label);

        let network = &label.inner.network;
        let style = &label.inner.style_watch;
        let icon_text_gap = style.get_number(theme::icon_text_gap);
        frp::extend! { network
            init <- source_();
            max_width_px <- source::<f32>();
            icon_text_gap <- all(&icon_text_gap, &init)._0();
            label_x_position <- icon_text_gap.map(|gap| icon::SIZE + gap);
            label_max_width <-
                all_with(&max_width_px, &icon_text_gap, |width,gap| width - icon::SIZE- gap);
            eval label_x_position ((x) label.set_position_x(*x));
            eval label_max_width ((width) label.set_max_width(*width));
            label.inner.label.set_default_color <+ all(&colors.entry_text, &init)._0();
            eval colors.icon_strong ([icon](color)
                if let Some(shape) = &icon.borrow().shape {
                    shape.strong_color.set(color.into());
                }
            );
            eval colors.icon_weak ([icon](color)
                if let Some(shape) = &icon.borrow().shape {
                    shape.weak_color.set(color.into());
                }
            );
            icon_strong_color <- colors.icon_strong.sampler();
            icon_weak_color <- colors.icon_weak.sampler();
        }
        init.emit(());
        Self {
            logger,
            display_object,
            icon,
            max_width_px,
            icon_strong_color,
            icon_weak_color,
            label,
        }
    }

    fn update(&self, model: &Self::Model) {
        self.label.update(&model.highlighted_text);
        let mut icon = self.icon.borrow_mut();
        if !icon.id.contains(&model.icon) {
            icon.id = Some(model.icon);
            let shape = model.icon.create_shape(&self.logger, Vector2(icon::SIZE, icon::SIZE));
            shape.strong_color.set(self.icon_strong_color.value().into());
            shape.weak_color.set(self.icon_weak_color.value().into());
            shape.set_position_x(icon::SIZE / 2.0);
            self.display_object.add_child(&shape);
            icon.shape = Some(shape);
        }
    }

    fn set_max_width(&self, max_width_px: f32) {
        self.max_width_px.emit(max_width_px);
    }

    fn set_label_layer(&self, label_layer: &Layer) {
        self.label.set_label_layer(label_layer)
    }
}

impl display::Object for View {
    fn display_object(&self) -> &display::object::Instance<Scene> {
        &self.display_object
    }
}
