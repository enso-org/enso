//! This module defines an entry in a component group view.
//!
//! The entry data is represented by the [`Model`] and visualized by the [`View`].

use crate::prelude::*;

use crate::icon;
use crate::icon::AnyIcon;
use crate::icon::ICON_SIZE;

use enso_frp as frp;
use ensogl::application::Application;
use ensogl::data::color;
use ensogl::display;
use ensogl::display::scene::Layer;
use ensogl::display::style;
use ensogl::display::Scene;
use ensogl_core::application::Application;
use ensogl_core::data::color::Rgba;
use ensogl_core::display;
use ensogl_core::display::scene::Layer;
use ensogl_core::display::style;
use ensogl_core::display::Scene;
use ensogl_hardcoded_theme::application::component_browser::component_group::entries as theme;
use ensogl_hardcoded_theme::application::component_browser::component_group::entry as theme;
use ensogl_list_view as list_view;
use ensogl_list_view::entry::GlyphHighlightedLabel;
use ensogl_list_view::entry::GlyphHighlightedLabelModel;
use ensogl_list_view::entry::Label;



// ===============
// === Aliases ===
// ===============

/// ID of a component group entry in a ListView.
pub type Id = list_view::entry::Id;

<<<<<<< HEAD


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
#[derive(Clone, Debug, Default, From)]
=======
#[derive(Clone, CloneRef, Debug)]
pub struct Params {
    pub icon_strong_color: frp::Stream<color::Rgba>,
    pub icon_weak_color:   frp::Stream<color::Rgba>,
    pub text_color:        frp::Stream<color::Rgba>,
    pub background_color:  frp::Stream<color::Rgba>,
}

impl Default for Params {
    fn default() -> Self {
        let network = frp::Network::new("component_browser::entry::Params::default");
        frp::extend! { network
            icon_strong_color <- source::<color::Rgba>();
            icon_weak_color <- source::<color::Rgba>();
            text_color <- source::<color::Rgba>();
            background_color <- source::<color::Rgba>();
        }

        Self {
            icon_strong_color: icon_strong_color.into(),
            icon_weak_color:   icon_weak_color.into(),
            text_color:        text_color.into(),
            background_color:  background_color.into(),
        }
    }
}

#[derive(Clone, Debug, Default)]
>>>>>>> e427a17f9 (Selection highlight)
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
    pub color: frp::Sampler<Rgba>,
}



// ============
// === View ===
// ============


// === CurrentIcon ===

#[derive(Default)]
struct CurrentIcon {
    shape: Option<AnyIcon>,
    id:    Option<icon::Id>,
}

impl Debug for CurrentIcon {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.id.fmt(f)
    }
}


// === View ===

/// A visual representation of a [`Model`].
#[derive(Clone, CloneRef, Debug)]
pub struct View {
    logger:            Logger,
    display_object:    display::object::Instance,
    icon:              Rc<RefCell<CurrentIcon>>,
    max_width_px:      frp::Source<f32>, // Refactoring Idea: make Entry API more frp-like.
    icon_strong_color: frp::Sampler<color::Rgba>,
    icon_weak_color:   frp::Sampler<color::Rgba>,
    label:             GlyphHighlightedLabel,
}

impl list_view::Entry for View {
    type Model = Model;
    type Params = Params;

    fn new(app: &Application, style_prefix: &style::Path, params: &Params) -> Self {
        let logger = Logger::new("component-group::Entry");
        let display_object = display::object::Instance::new(&logger);
        let style_prefix = style_prefix.sub("entry");
        let icon: Rc<RefCell<CurrentIcon>> = default();
        let label = GlyphHighlightedLabel::new(app, &style_prefix, &());
        display_object.add_child(&label);

        let network = &label.inner.network;
        let style = &label.inner.style_watch;

        let icon_text_gap = style.get_number(theme::icon_text_gap);

        frp::extend! { network
            init <- source_();
            max_width_px <- source::<f32>();
            icon_text_gap <- all(&icon_text_gap, &init)._0();
            label_x_position <- icon_text_gap.map(|gap| ICON_SIZE + gap);
            label_max_width <- all_with(&max_width_px, &icon_text_gap, |width,gap| width - ICON_SIZE - gap);
            eval label_x_position ((x) label.set_position_x(*x));
            eval label_max_width ((width) label.set_max_width(*width));
            label.inner.label.set_color_all <+ params.text_color;
            label.inner.label.set_default_color <+ params.text_color;
            eval params.icon_strong_color ([icon](color)
                if let Some(shape) = &icon.borrow().shape {
                    shape.strong_color.set(color.into());
                }
            );
            eval params.icon_weak_color ([icon](color)
                if let Some(shape) = &icon.borrow().shape {
                    shape.weak_color.set(color.into());
                }
            );
            icon_strong_color <- params.icon_strong_color.sampler();
            icon_weak_color <- params.icon_weak_color.sampler();
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
            let shape = model.icon.create_shape(&self.logger, Vector2(ICON_SIZE, ICON_SIZE));
            shape.strong_color.set(self.icon_strong_color.value().into());
            shape.weak_color.set(self.icon_weak_color.value().into());
            shape.set_position_x(ICON_SIZE / 2.0);
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
