use ensogl_core::prelude::*;

use enso_frp as frp;
use ensogl_core::application::Application;
use ensogl_core::data::color::Rgba;
use ensogl_core::display;
use ensogl_core::display::shape::*;
use ensogl_gui_component::component;
use ensogl_label::Label;
use ensogl_list_view as list_view;
use list_view::entry::AnyModelProvider;


// =================
// === Constants ===
// =================

const NO_ITEMS_LABEL_TEXT: &str = "No local variables";

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
            let background_width = input.set_width.clone_ref();
            column_width <- background_width.map(|w| *w / 3.0);

            entry_count <- input.set_entries.map(|p| p.entry_count());
            background_height <- entry_count.map(|c| Model::background_height(*c));

            _eval <- all_with(&background_width, &background_height, f!([model] (width, height) {
                model.background.size.set(Vector2(*width, *height));
            }));

            no_entries_provided <- entry_count.map(|c| *c == 0);
            show_no_items_label <- no_entries_provided.on_true();
            hide_no_items_label <- no_entries_provided.on_false();
            eval_ show_no_items_label(model.show_no_items_label());
            eval_ hide_no_items_label(model.hide_no_items_label());

            eval input.set_background_color((c)
                model.background.color.set(c.into()));
            chosen_entry <- any_mut();
            eval chosen_entry (((entry, group)) model.on_entry_chosen(*entry, *group));
        }
        for (idx, group) in model.groups.iter().enumerate() {
            frp::extend! { network
                let group = group.clone_ref();
                entries <- input.set_entries.map(move |p| ModelProvider::new(p, idx));
                entry_count_in_column <- entries.map(|p| p.entry_count());
                group.set_entries <+ entries;

                column_height <- entry_count_in_column.map(|count| *count as f32 * list_view::entry::HEIGHT);

                _eval <- all_with3(&column_width, &column_height, &background_height, f!([group](&width, &height, bg_height) {
                    group.resize(Vector2(width, height));
                    group.set_position_x(- width + width * idx as f32);
                    let half_height = height / 2.0;
                    let background_bottom = -*bg_height / 2.0;
                    group.set_position_y(background_bottom + half_height);
                }));

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
    groups:         Rc<[list_view::ListView<list_view::entry::Label>; 3]>,
    no_items_label: Label,
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
        background.size.set(Vector2(0.0, 200.0));
        let groups = Rc::new([
            app.new_view::<list_view::ListView<list_view::entry::Label>>(),
            app.new_view::<list_view::ListView<list_view::entry::Label>>(),
            app.new_view::<list_view::ListView<list_view::entry::Label>>(),
        ]);
        for group in groups.iter() {
            display_object.add_child(group);
        }
        let no_items_label = Label::new(app);
        no_items_label.set_content(NO_ITEMS_LABEL_TEXT);

        Model { no_items_label, display_object, background, groups }
    }
}

impl Model {
    fn show_no_items_label(&self) {
        self.display_object.add_child(&self.no_items_label);
    }

    fn hide_no_items_label(&self) {
        self.display_object.remove_child(&self.no_items_label);
    }

    fn on_entry_chosen(&self, _entry: usize, group_id: usize) {
        for (_, group) in self.groups.iter().enumerate().filter(|(i, _)| *i != group_id) {
            group.deselect_entries();
        }
    }
    
    fn background_height(entry_count: usize) -> f32 {
        let min_background_height = list_view::entry::HEIGHT;
        let entry_count_in_largest_column = Self::entry_count_in_column(0, entry_count);
        let background_height = entry_count_in_largest_column as f32 * list_view::entry::HEIGHT;
        background_height.max(min_background_height)
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
