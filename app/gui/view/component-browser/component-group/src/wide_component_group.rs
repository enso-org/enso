use ensogl_core::prelude::*;

use enso_frp as frp;
use ensogl_core::application::Application;
use ensogl_core::data::color::Rgba;
use ensogl_core::display;
use ensogl_core::display::shape::*;
use ensogl_gui_component::component;
use ensogl_hardcoded_theme::application::component_browser::component_group as theme;
use ensogl_list_view as list_view;
use ensogl_text as text;
use list_view::entry::AnyModelProvider;



// ==========================
// === Shapes Definitions ===
// ==========================


// === Background ===

/// The background of the Component Group View.
pub mod background {
    use super::*;

    ensogl_core::define_shape_system! {
        below = [list_view::background];
        (style:Style, color:Vector4) {
            let sprite_width: Var<Pixels> = "input_size.x".into();
            let sprite_height: Var<Pixels> = "input_size.y".into();
            let color = Var::<Rgba>::from(color);
            let shape = Rect((&sprite_width, &sprite_height)).fill(color);
            shape.into()
        }
    }
}

// =================
// === ModelProvider ===
// =================

type Entry = list_view::entry::Label;
#[derive(Debug, Clone, CloneRef, Default)]
pub struct ModelProvider {
    inner: AnyModelProvider<Entry>,
    index: Immutable<usize>,
}

impl ModelProvider {
    pub fn new(inner: &AnyModelProvider<Entry>, index: usize) -> AnyModelProvider<Entry> {
        AnyModelProvider::new(Self { inner: inner.clone_ref(), index: Immutable(index) })
    }
}

impl list_view::entry::ModelProvider<Entry> for ModelProvider {
    fn entry_count(&self) -> usize {
        let count = self.inner.entry_count();
        let count = count / 3 + if count % 3 > 0 && *self.index < count % 3 { 1 } else { 0 };
        count
    }

    fn get(&self, id: list_view::entry::Id) -> Option<String> {
        let idx = (self.entry_count() - 1 - id) * 3 + *self.index;
        self.inner.get(idx)
    }
}


// ===========
// === FRP ===
// ===========

ensogl_core::define_endpoints_2! {
    Input {
        set_header_text(String),
        set_entries(list_view::entry::AnyModelProvider<list_view::entry::Label>),
        set_background_color(Rgba),
        set_size(Vector2),
    }
    Output {}
}

impl component::Frp<Model> for Frp {
    fn init(api: &Self::Private, _app: &Application, model: &Model, style: &StyleWatchFrp) {
        let network = &api.network;
        let input = &api.input;
        frp::extend! { network
            eval input.set_size((size) model.resize_background(*size));
            eval input.set_background_color((c)
                model.background.color.set(c.into()));
            chosen_entry <- any_mut();
            eval chosen_entry (((entry, group)) model.on_entry_chosen(*entry, *group));
        }
        for (idx, group) in model.groups.iter().enumerate() {
            frp::extend! { network
                let group = group.clone_ref();
                entries <- input.set_entries.map(move |p| ModelProvider::new(p, idx));
                group.set_entries <+ entries;
                _eval <- all_with(&input.set_size, &entries, f!([group](size, count) Model::resize(&group,count.entry_count(),*size)));
                group.set_background_color(Rgba(1.0, 1.0, 1.0, 0.0));
                group.show_background_shadow(false);
                group.set_background_corners_radius(0.0);
                
                chosen_entry <+ group.frp.output.selected_entry.filter_map(|e| *e).map(move |e| (*e, idx));
            }
        }
    }
}



// =============
// === Model ===
// =============

/// The Model of the [`View`] component.
#[derive(Clone, CloneRef, Debug)]
pub struct Model {
    display_object: display::object::Instance,
    background: background::View,
    groups:         Rc<[list_view::ListView<list_view::entry::Label>; 3]>,
}

impl display::Object for Model {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}

impl component::Model for Model {
    fn label() -> &'static str {
        "WideComponentGroupView"
    }

    fn new(app: &Application, logger: &Logger) -> Self {
        let display_object = display::object::Instance::new(&logger);
        let background = background::View::new(&logger);
        display_object.add_child(&background);
        let groups = Rc::new([
            app.new_view::<list_view::ListView<list_view::entry::Label>>(),
            app.new_view::<list_view::ListView<list_view::entry::Label>>(),
            app.new_view::<list_view::ListView<list_view::entry::Label>>(),
        ]);
        let mut x = -150.0;
        for group in groups.iter() {
            group.set_position_x(x);
            x += 150.0;
            display_object.add_child(group);
        }

        Model { display_object, background, groups }
    }
}

impl Model {
    fn on_entry_chosen(&self, _entry: usize, group_id: usize) {
        for (_, group) in self.groups.iter().enumerate().filter(|(i, _)| *i != group_id) {
            group.deselect_entries();
        }
    }

    fn resize_background(&self, size: Vector2) {
        self.background.size.set(size);
    }

    fn resize(group: &list_view::ListView<list_view::entry::Label>, entries_count: usize, size: Vector2) {
        let size = Vector2(size.x / 3.0, size.y);
        group.resize(size);
        group.set_position_y(- size.y + entries_count as f32 * list_view::entry::HEIGHT);
    }
}



// ============
// === View ===
// ============

/// The implementation of the visual component described in the module's documentation.
pub type View = component::ComponentView<Model, Frp>;
