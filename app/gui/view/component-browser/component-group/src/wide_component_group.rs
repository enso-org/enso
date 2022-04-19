use ensogl_core::prelude::*;

use enso_frp as frp;
use ensogl_core::application::Application;
use ensogl_core::data::color::Rgba;
use ensogl_core::display;
use ensogl_core::display::shape::*;
use ensogl_gui_component::component;
use ensogl_list_view as list_view;
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
        let count = Model::entry_count_in_column(*self.index, count);
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
        set_width(f32),
    }
    Output {}
}

impl component::Frp<Model> for Frp {
    fn init(api: &Self::Private, _app: &Application, model: &Model, style: &StyleWatchFrp) {
        let network = &api.network;
        let input = &api.input;
        frp::extend! { network
            eval input.set_width ((width) model.set_width(*width));
            eval input.set_entries((e) model.set_height(e.entry_count()));
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
    background:     background::View,
    size:           Rc<Cell<Vector2>>,
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
        background.size.set(Vector2(0.0, 200.0));
        display_object.add_child(&background);
        let groups = Rc::new([
            app.new_view::<list_view::ListView<list_view::entry::Label>>(),
            app.new_view::<list_view::ListView<list_view::entry::Label>>(),
            app.new_view::<list_view::ListView<list_view::entry::Label>>(),
        ]);
        let size = Vector2(450.0, 200.0);
        let mut x = -150.0;
        for group in groups.iter() {
            group.set_position_x(x);
            group.resize(Vector2(150.0, 200.0));
            x += 150.0;
            display_object.add_child(group);
        }

        Model { display_object, size: Rc::new(Cell::new(size)), background, groups }
    }
}

impl Model {
    fn on_entry_chosen(&self, _entry: usize, group_id: usize) {
        for (_, group) in self.groups.iter().enumerate().filter(|(i, _)| *i != group_id) {
            group.deselect_entries();
        }
    }

    fn set_width(&self, width: f32) {
        let size = self.size.get();
        self.background.size.set(Vector2(width, size.y));
        for (i, group) in self.groups.iter().enumerate() {
            group.resize(Vector2(width / 3.0, size.y));
            group.set_position_x(-width / 3.0 + width / 3.0 * i as f32);
        }
        self.size.set(Vector2(width, size.y));
    }

    fn set_height(&self, entries_count: usize) {
        let max_entries_in_list = Model::entry_count_in_column(0, entries_count);
        let background_height = max_entries_in_list as f32 * list_view::entry::HEIGHT;
        let size = self.size.get();
        self.background.size.set(Vector2(size.x, background_height));
        self.background.set_position_y(-background_height / 2.0);
        for (i, group) in self.groups.iter().enumerate() {
            let count_in_group = Self::entry_count_in_column(i, entries_count);
            let group_height = count_in_group as f32 * list_view::entry::HEIGHT;
            group.resize(Vector2(size.x / 3.0, group_height));
            let half_group_height = group_height / 2.0;
            let background_bottom = -background_height;
            let pos = background_bottom + half_group_height;
            group.set_position_y(pos);
        }
        self.size.set(Vector2(size.x, background_height));
    }

    fn entry_count_in_column(index: usize, total_entry_count: usize) -> usize {
        total_entry_count / 3
            + if total_entry_count % 3 > 0 && index < total_entry_count % 3 { 1 } else { 0 }
    }
}



// ============
// === View ===
// ============

/// The implementation of the visual component described in the module's documentation.
pub type View = component::ComponentView<Model, Frp>;
