use crate::prelude::*;

use crate::icon;

use crate::set::GroupId;
use crate::set::SectionId;
use enso_frp as frp;
use ensogl_core::application::command::FrpNetworkProvider;
use ensogl_core::application::frp::API;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display;
use ensogl_core::display::scene::Layer;
use ensogl_core::display::Scene;
use ensogl_core::Animation;
use ensogl_grid_view as grid_view;
use ensogl_text as text;

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub enum Kind {
    #[default]
    Entry,
    Header,
    LocalScopeEntry,
}

#[derive(Clone, Debug, Default)]
pub struct Model {
    kind:     Kind,
    color:    color::Rgba,
    caption:  ImString,
    icon:     Option<icon::Id>,
    group_id: GroupId,
}

#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct ColorIntensities {
    background: f32,
    dimmed:     f32,
    icon_weak:  f32,
}

#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct Style {
    pub color_intensities:  ColorIntensities,
    pub group_width:        f32,
    pub gap_between_groups: f32,
    pub padding:            f32,
    pub icon_size:          f32,
    pub text_size:          f32,
    pub icon_text_padding:  f32,
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub enum DimmedGroups {
    #[default]
    None,
    AllExcept(GroupId),
    AllExceptSection(SectionId),
}

impl DimmedGroups {
    fn is_group_dimmed(self, id: GroupId) -> bool {
        match self {
            Self::None => false,
            Self::AllExcept(undimmed_group) => id != undimmed_group,
            Self::AllExceptSection(undimmed_section) => id.section != undimmed_section,
        }
    }
}

#[derive(Clone, Copy, Debug, Default)]
pub struct Params {
    pub style:         Style,
    pub dimmed_groups: DimmedGroups,
}


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

    fn set_position(&self, position: Vector2) {
        if let Some(shape) = &self.shape {
            shape.set_position_xy(position)
        }
    }
}



/// Colors used in the Component Group View.
///
/// This structure, used in both can be created from single "main color" input. Each of
/// these colors will be computed by mixing "main color" with application background - for details,
/// see [`Colors::from_main_color`].
///
/// `icon_strong` and `icon_weak` parameters represent the more/less contrasting parts of the
/// [icon](crate::icon::Any), they do not represent highlighted state of the icon.
#[allow(missing_docs)]
#[derive(Clone, CloneRef, Debug)]
pub struct Colors {
    pub background:  frp::Sampler<color::Rgba>,
    pub text:        frp::Sampler<color::Rgba>,
    pub icon_strong: frp::Sampler<color::Rgba>,
    pub icon_weak:   frp::Sampler<color::Rgba>,
}

impl Colors {
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

            bg_intensity <- style.map(|p| p.color_intensities.background);
            dimmed_intensity <- style.map(|p| p.color_intensities.dimmed);
            icon_weak_intensity <- style.map(|p| p.color_intensities.icon_weak);

            one <- init.constant(1.0);
            let is_dimmed = is_dimmed.clone_ref();
            intensity.target <+ is_dimmed.switch(&one, &dimmed_intensity);
            app_bg <- all(&app_bg, &init)._0();
            app_bg_and_input <- all(&app_bg, color);
            main <- app_bg_and_input.all_with(&intensity.value, mix);
            app_bg_and_main <- all(&app_bg, &main);
            background <- app_bg_and_main.all_with(&bg_intensity, mix).sampler();
            text <- main.sampler();
            icon_weak <- app_bg_and_main.all_with(&icon_weak_intensity, mix).sampler();
            icon_strong <- main.sampler();
        }
        init.emit(());
        Self { icon_weak, icon_strong, text, background }
    }
}

#[derive(Clone, CloneRef, Debug)]
pub struct Data {
    display_object: display::object::Instance,
    label:          text::Area,
    background:     grid_view::entry::shape::View,
    icon:           Rc<RefCell<CurrentIcon>>,
    style:          StyleWatchFrp,
}

impl Data {
    fn new(app: &Application, text_layer: Option<&Layer>) -> Self {
        let display_object = display::object::Instance::new(Logger::new("ComponentGroupEntry"));
        let label = app.new_view::<text::Area>();
        let background = grid_view::entry::shape::View::new(Logger::new("ComponentGroupEntry"));
        let icon = default();
        let style = StyleWatchFrp::new(&app.display.default_scene.style_sheet);
        display_object.add_child(&background);
        display_object.add_child(&label);
        if let Some(layer) = text_layer {
            label.add_to_scene_layer(layer);
        }
        Self { display_object, label, background, icon, style }
    }

    fn update_layout(&self, kind: Kind, style: Style, entry_size: Vector2) {
        let bg_width = if kind == Kind::LocalScopeEntry { entry_size.x } else { style.group_width };
        self.background.size.set(Vector2(bg_width, entry_size.y));
        let left = -entry_size.y / 2.0 + style.padding;
        self.icon.borrow().set_position(Vector2(left + style.icon_size / 2.0, 0.0));
        let text_x = Self::text_x_position(kind, style);
        self.label.set_position_x(text_x);
    }

    fn max_text_width(kind: Kind, style: Style) -> f32 {
        let right = style.group_width / 2.0 - style.padding;
        let text_x = Self::text_x_position(kind, style);
        right - text_x
    }

    fn text_x_position(kind: Kind, style: Style) -> f32 {
        let left = -style.group_width / 2.0 + style.padding;
        if kind == Kind::Header {
            left
        } else {
            left + style.icon_size + style.icon_text_padding
        }
    }

    /// Update an icon shape (create it if necessary), update its color, and add it to the
    /// [`layer`] if supplied.
    fn update_icon(
        &self,
        new_icon: Option<icon::Id>,
        strong_color: color::Rgba,
        weak_color: color::Rgba,
    ) {
        let mut icon = self.icon.borrow_mut();
        if icon.id != new_icon {
            icon.id = new_icon;
            if let Some(icon_id) = new_icon {
                let shape = icon_id.create_shape(
                    Logger::new("ComponentBrowserEntry"),
                    Vector2(icon::SIZE, icon::SIZE),
                );
                shape.strong_color.set(strong_color.into());
                shape.weak_color.set(weak_color.into());
                self.display_object.add_child(&shape);
                icon.shape = Some(shape);
            } else {
                icon.shape = None;
            }
        }
    }
}

#[derive(Clone, CloneRef, Debug)]
pub struct View {
    frp:  grid_view::entry::EntryFrp<Self>,
    data: Data,
}

impl grid_view::Entry for View {
    type Model = Model;
    type Params = Params;

    fn new(app: &Application, text_layer: Option<&Layer>) -> Self {
        let frp = grid_view::entry::EntryFrp::<Self>::new();
        let data = Data::new(app, text_layer);
        let network = frp.network();
        let input = &frp.private().input;

        frp::extend! { network
            kind <- input.set_model.map(|m| m.kind).on_change();
            style <- input.set_params.map(|p| p.style).on_change();
            kind_and_style <- all(kind, style);


            // === Colors ===

            color <- input.set_model.map(|m| m.color);
            color_intensities <- style.map(|s| s.color_intensities);
            is_dimmed <- all_with(&input.set_model, &input.set_params, |m,p| p.dimmed_groups.is_group_dimmed(m.group_id));
            let colors = Colors::from_main_color(network, &data.style, &color, &style, &is_dimmed);
            eval colors.background ((c) data.background.color.set(c.into()));
            data.label.set_default_color <+ colors.text;
            eval colors.icon_strong ((c) data.icon.borrow().set_strong_color(*c));
            eval colors.icon_weak ((c) data.icon.borrow().set_weak_color(*c));


            // === Icon and Text ===

            max_text_width <- kind_and_style.map(|&(kind, style)| Data::max_text_width(kind, style));
            caption <- input.set_model.map(|m| m.caption.to_string());
            icon <- input.set_model.map(|m| m.icon);
            data.label.set_content_truncated <+ all(caption, max_text_width);
            _eval <- icon.map3(&colors.icon_strong, &colors.icon_weak, f!((&icon, &strong, &weak) data.update_icon(icon, strong, weak)));


            // === Layout ===

            layout_data <- all(kind_and_style, input.set_size);
            eval layout_data ((&((kind, style), entry_sz)) data.update_layout(kind, style, entry_sz));
        }
        Self { frp, data }
    }

    fn frp(&self) -> &grid_view::entry::EntryFrp<Self> {
        &self.frp
    }
}

impl display::Object for View {
    fn display_object(&self) -> &display::object::Instance<Scene> {
        &self.data.display_object
    }
}
