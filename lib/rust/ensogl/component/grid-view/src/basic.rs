use crate::prelude::*;
use ensogl_core::application::command::FrpNetworkProvider;
use ensogl_core::application::frp::API;
use ensogl_core::application::Application;

use crate::EntryFrp;
use ensogl_core::data::color;
use ensogl_core::display;
use ensogl_core::display::scene::layer::WeakLayer;
use ensogl_core::display::scene::Layer;
use ensogl_core::display::shape::*;
use ensogl_text as text;


// ==================
// === Background ===
// ==================

pub mod background {
    use super::*;

    pub const PADDING_PX: f32 = 2.0;

    ensogl_core::define_shape_system! {
        (style:Style, color: Vector4) {
            let shape_width  : Var<Pixels> = "input_size.x".into();
            let shape_height : Var<Pixels> = "input_size.y".into();
            let width = shape_width - 2.0.px() * PADDING_PX;
            let height = shape_height - 2.0.px() * PADDING_PX;
            Rect((width, height)).fill(color).into()
        }
    }
}



// ===================
// === EntryParams ===
// ===================

#[derive(Clone, Debug)]
pub struct EntryParams {
    pub bg_color:    color::Rgba,
    pub bg_margin:   f32,
    pub font:        ImString,
    pub text_offset: f32,
    pub text_size:   text::Size,
    pub text_color:  color::Rgba,
    pub text_layer:  Option<WeakLayer>,
}

impl EntryParams {
    pub fn with_text_layer(mut self, layer: &Layer) -> Self {
        self.text_layer = Some(layer.downgrade());
        self
    }
}

impl Default for EntryParams {
    fn default() -> Self {
        Self {
            bg_color:    color::Rgba::transparent(),
            bg_margin:   0.0,
            font:        text::typeface::font::DEFAULT_FONT.into(),
            text_offset: 7.0,
            text_size:   text::Size(14.0),
            text_color:  default(),
            text_layer:  default(),
        }
    }
}



// =============
// === Entry ===
// =============

#[derive(Clone, Debug)]
pub struct EntryModel {
    display_object:     display::object::Instance,
    pub label:          text::Area,
    pub background:     background::View,
    current_text_layer: RefCell<Option<WeakLayer>>,
}

impl EntryModel {
    fn new(app: &Application) -> Self {
        let logger = Logger::new("list_view::entry::Label");
        let display_object = display::object::Instance::new(&logger);
        let label = app.new_view::<ensogl_text::Area>();
        let background = background::View::new(&logger);
        let current_text_layer = default();
        display_object.add_child(&label);
        display_object.add_child(&background);
        Self { display_object, label, background, current_text_layer }
    }

    fn update_layout(
        &self,
        entry_size: Vector2,
        bg_margin: f32,
        text_size: text::Size,
        text_offset: f32,
    ) {
        use background::PADDING_PX;
        let bg_size = entry_size - Vector2(bg_margin, bg_margin) * 2.0;
        let bg_size_with_padding = bg_size + Vector2(PADDING_PX, PADDING_PX) * 2.0;
        self.background.size.set(bg_size_with_padding);
        self.label.set_position_xy(Vector2(text_offset - entry_size.x / 2.0, text_size.raw / 2.0));
    }

    fn set_label_layer(&self, new_layer: &Option<WeakLayer>) {
        let mut current_layer = self.current_text_layer.borrow_mut();
        let current_layer = &mut *current_layer;
        let current_upg = current_layer.as_ref().and_then(|l| l.upgrade());
        let new_upg = new_layer.as_ref().and_then(|l| l.upgrade());
        match (current_upg, new_upg) {
            (Some(current_upg), Some(new_upg)) =>
                if current_upg.id() != new_upg.id() {
                    self.label.remove_from_scene_layer(&current_upg);
                    self.label.add_to_scene_layer(&new_upg);
                    *current_layer = Some(new_upg.downgrade());
                },
            (Some(current_upg), None) => {
                self.label.remove_from_scene_layer(&current_upg);
                *current_layer = None;
            }
            (None, Some(new_upg)) => {
                self.label.add_to_scene_layer(&new_upg);
                *current_layer = Some(new_upg.downgrade());
            }
            (None, None) => {}
        }
    }
}


#[derive(Clone, CloneRef, Debug)]
pub struct Entry {
    frp:   EntryFrp<Self>,
    model: Rc<EntryModel>,
}

impl Entry {}

impl crate::Entry for Entry {
    type Model = ImString;
    type Params = EntryParams;

    fn new(app: &Application) -> Self {
        let model = Rc::new(EntryModel::new(app));
        let frp = EntryFrp::<Self>::new();
        let input = &frp.private().input;
        let network = frp.network();


        enso_frp::extend! { network
            bg_color <- input.set_params.map(|params| params.bg_color).on_change();
            bg_margin <- input.set_params.map(|params| params.bg_margin).on_change();
            font <- input.set_params.map(|params| params.font.clone_ref()).on_change();
            text_offset <- input.set_params.map(|params| params.text_offset).on_change();
            text_color <- input.set_params.map(|params| params.text_color).on_change();
            text_size <- input.set_params.map(|params| params.text_size).on_change();
            text_layer <- input.set_params.map(|params| params.text_layer.clone());

            layout <- all(input.set_size, bg_margin, text_size, text_offset);
            eval layout ((&(es, m, ts, to)) model.update_layout(es, m, ts, to));

            eval bg_color ((color) model.background.color.set(color.into()));
            model.label.set_default_color <+ text_color.on_change();
            model.label.set_font <+ font.on_change().map(ToString::to_string);
            model.label.set_default_text_size <+ text_size.on_change();
            eval text_layer ((layer) model.set_label_layer(layer));

            content <- input.set_model.map(|s| s.to_string());
            max_width_px <- input.set_size.map(|size| size.x);
            model.label.set_content_truncated <+ all(&content, &max_width_px);
        }
        Self { frp, model }
    }

    fn frp(&self) -> &EntryFrp<Self> {
        &self.frp
    }
}

impl display::Object for Entry {
    fn display_object(&self) -> &display::object::Instance {
        &self.model.display_object
    }
}

// =====================
// === BasicGridView ===
// =====================

pub type BasicGridView = crate::GridView<Entry>;
