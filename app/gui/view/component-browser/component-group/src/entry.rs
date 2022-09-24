//! This module defines an entry in a component group view.
//!
//! The entry data is represented by the [`Model`] and visualized by the [`View`].

use crate::prelude::*;

use crate::icon;
use crate::Colors;
use crate::SelectedColors;

use enso_frp as frp;
use ensogl::application::Application;
use ensogl::data::color;
use ensogl::display;
use ensogl::display::scene::layer::Layer;
use ensogl::display::scene::layer::WeakLayer;
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
    pub is_enterable:     bool,
}

impl From<String> for Model {
    fn from(label: String) -> Self {
        Model {
            icon:             icon::Id::Star,
            highlighted_text: GlyphHighlightedLabelModel { label, highlighted: default() },
            is_enterable:     false,
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
    pub colors:          Colors,
    pub selection_layer: Rc<Option<WeakLayer>>,
}

impl Default for Params {
    fn default() -> Self {
        let network = frp::Network::new("component_browser::entry::Params::default");
        frp::extend! { network
            default_color <- source::<color::Rgba>().sampler();
        }
        let selected = SelectedColors {
            background:  default_color.clone_ref(),
            header_text: default_color.clone_ref(),
            entry_text:  default_color.clone_ref(),
            icon_strong: default_color.clone_ref(),
            icon_weak:   default_color.clone_ref(),
        };
        let colors = Colors {
            icon_strong: default_color.clone_ref(),
            icon_weak: default_color.clone_ref(),
            header_text: default_color.clone_ref(),
            entry_text: default_color.clone_ref(),
            background: default_color.clone_ref(),
            selected,
        };
        Self { colors, selection_layer: default() }
    }
}



// ============
// === View ===
// ============


// === CurrentIcon ===

/// The structure keeping a currently displayed icon in Component Group Entry [`View`]. Remembering
/// id allows us to skip icon generation when not changed.
#[derive(Debug, Default)]
struct CurrentIcon {
    shape: Option<icon::Any>,
    id:    Option<icon::Id>,
}

impl CurrentIcon {
    fn set_strong_color(&self, color: color::Rgba) {
        if let Some(shape) = &self.shape {
            shape.strong_color.set(color.into());
        }
    }

    fn set_weak_color(&self, color: color::Rgba) {
        if let Some(shape) = &self.shape {
            shape.weak_color.set(color.into());
        }
    }
}



// === View ===

/// A visual representation of a [`Model`].
#[derive(Clone, CloneRef, Debug)]
pub struct View {
    network:         frp::Network,
    logger:          Logger,
    display_object:  display::object::Instance,
    icon:            Rc<RefCell<CurrentIcon>>,
    selected_icon:   Rc<RefCell<CurrentIcon>>,
    max_width_px:    frp::Source<f32>,
    label:           GlyphHighlightedLabel,
    selected_label:  GlyphHighlightedLabel,
    selection_layer: Rc<Option<WeakLayer>>,
    colors:          Colors,
}

impl View {
    /// Update an icon shape (create it if necessary), update its color, and add it to the
    /// [`layer`] if supplied.
    fn update_icon(
        &self,
        model: &Model,
        icon: &RefCell<CurrentIcon>,
        layer: Option<Layer>,
        strong_color: color::Rgba,
        weak_color: color::Rgba,
    ) {
        let mut icon = icon.borrow_mut();
        if !icon.id.contains(&model.icon) {
            icon.id = Some(model.icon);
            let shape = model.icon.create_shape(&self.logger, Vector2(icon::SIZE, icon::SIZE));
            shape.strong_color.set(strong_color.into());
            shape.weak_color.set(weak_color.into());
            shape.set_position_x(icon::SIZE / 2.0);
            self.display_object.add_child(&shape);
            if let Some(layer) = layer {
                layer.add_exclusive(&shape);
            }
            icon.shape = Some(shape);
        }
    }
}

impl list_view::Entry for View {
    type Model = Model;
    type Params = Params;

    fn new(
        app: &Application,
        style_prefix: &style::Path,
        Params { colors, selection_layer }: &Params,
    ) -> Self {
        let logger = Logger::new("component_group::Entry");
        let display_object = display::object::Instance::new(&logger);
        let icon: Rc<RefCell<CurrentIcon>> = default();
        let selected_icon: Rc<RefCell<CurrentIcon>> = default();
        let label = GlyphHighlightedLabel::new(app, style_prefix, &());
        let selected_label = GlyphHighlightedLabel::new(app, style_prefix, &());
        display_object.add_child(&label);

        if let Some(selection_layer) = &**selection_layer {
            if let Some(layer) = selection_layer.upgrade() {
                selected_label.set_label_layer(&layer);
                display_object.add_child(&selected_label);
            } else {
                error!(logger, "Selection layer is dropped.");
            }
        }

        let network = frp::Network::new("component_group::Entry");
        let style = &label.inner.style_watch;
        let icon_text_gap = style.get_number(theme::icon_text_gap);
        frp::extend! { network
            init <- source_();
            max_width_px <- source::<f32>();
            icon_text_gap <- all(&icon_text_gap, &init)._0();
            label_x_position <- icon_text_gap.map(|gap| icon::SIZE + gap);
            label_max_width <-
                all_with(&max_width_px, &icon_text_gap, |width,gap| width - icon::SIZE- gap);
            eval label_x_position ([label, selected_label](x) {
                label.set_position_x(*x);
                selected_label.set_position_x(*x);
            });
            eval label_max_width ([label, selected_label](width) {
                label.set_max_width(*width);
                selected_label.set_max_width(*width);
            });
            label.inner.label.set_default_color <+ all(&colors.entry_text, &init)._0();
            selected_label.inner.label.set_default_color <+ all(&colors.selected.entry_text,&init)._0();
            eval colors.icon_strong ((&c) icon.borrow().set_strong_color(c));
            eval colors.selected.icon_strong((&c) selected_icon.borrow().set_strong_color(c));
            eval colors.icon_weak ((&c) icon.borrow().set_weak_color(c));
            eval colors.selected.icon_weak((&c) selected_icon.borrow().set_weak_color(c));
        }
        init.emit(());
        let selection_layer = selection_layer.clone_ref();
        let colors = colors.clone_ref();
        Self {
            logger,
            network,
            display_object,
            icon,
            selected_icon,
            max_width_px,
            colors,
            label,
            selected_label,
            selection_layer,
        }
    }

    fn update(&self, model: &Self::Model) {
        self.label.update(&model.highlighted_text);
        self.selected_label.update(&model.highlighted_text);
        self.update_icon(
            model,
            &self.icon,
            None,
            self.colors.icon_strong.value(),
            self.colors.icon_weak.value(),
        );
        if let Some(weak_layer) = &*self.selection_layer {
            if let Some(layer) = weak_layer.upgrade() {
                self.update_icon(
                    model,
                    &self.selected_icon,
                    Some(layer),
                    self.colors.selected.icon_strong.value(),
                    self.colors.selected.icon_weak.value(),
                );
            } else {
                error!(self.logger, "Cannot add icon shape to a dropped scene layer.");
            }
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
