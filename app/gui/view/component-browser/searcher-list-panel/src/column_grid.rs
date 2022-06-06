use ensogl_core::display::shape::*;
use ensogl_core::prelude::*;

use crate::ColumnContent;
use enso_frp as frp;
use ensogl_core::application::frp::API;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::define_endpoints_2;
use ensogl_core::display;
use ensogl_core::display::object::ObjectOps;
use ensogl_gui_component::component;
use ide_view_component_group as component_group;
use ide_view_component_group::Layers;
use ordered_float::OrderedFloat;


const COLUMN_GAP: f32 = 2.0;

const BASE_COLORS: &[color::Rgba] = &[
    color::Rgba::new(0.527, 0.554, 0.18, 1.0),
    color::Rgba::new(43.0 / 255.0, 117.0 / 255.0, 239.0 / 255.0, 1.0),
    color::Rgba::new(62.0 / 255.0, 139.0 / 255.0, 41.0 / 255.0, 1.0),
    color::Rgba::new(192.0 / 255.0, 71.0 / 255.0, 171.0 / 255.0, 1.0),
];

const fn get_base_color_for_index(ix: usize) -> color::Rgba {
    BASE_COLORS[ix % BASE_COLORS.len()]
}

// =============
// === Model ===
// =============

#[derive(Clone, Debug, CloneRef)]
pub struct Model {
    app:            Application,
    display_object: display::object::Instance,
    content:        Rc<RefCell<Vec<component_group::View>>>,
    size:           Rc<Cell<Vector2>>,
}


impl Model {
    fn new(app: &Application) -> Self {
        let logger = Logger::new("ColumnGrid");
        let app = app.clone_ref();
        let display_object = display::object::Instance::new(&logger);

        Self {
            app,
            display_object,
            content: Rc::new(RefCell::new(vec![])),
            size: Rc::new(Cell::new(Vector2::default())),
        }
    }

    fn set_content(&self, content: &[ColumnContent]) -> Vector2 {
        let overall_width = crate::WIDTH_INNER - 2.0 * crate::PADDING_INNER;
        let column_width = (overall_width - 2.0 * COLUMN_GAP) / 3.0;
        let content = content
            .iter()
            .map(|data| {
                let view = self.app.new_view::<component_group::View>();
                view.set_width(column_width);
                view.set_entries(data);
                self.display_object.add_child(&view);
                view
            })
            .collect_vec();

        // Layout
        let mut columns = vec![vec![], vec![], vec![]];
        let mut heights = [-COLUMN_GAP; 3]; // We need to subtract one COLUMN_GAP as we only need (n-1) gaps. TODO better explanation.

        for (ix, entry) in content.iter().enumerate() {
            let column_index = ix % 3;
            columns[column_index].push(entry);
            heights[column_index] += entry.size.value().y + COLUMN_GAP;
        }


        let mut entry_ix = 0;
        for (ix, column) in columns.iter().enumerate() {
            let pos_x = (column_width + COLUMN_GAP) * (ix as f32 + 0.5);
            let mut pos_y = 0.0;
            for entry in column {
                let entry_height = entry.size.value().y;
                entry.set_position_y(pos_y - entry_height / 2.0);
                entry.set_position_x(pos_x);
                entry.set_header("Header");

                entry.set_color(get_base_color_for_index(entry_ix));
                entry_ix += 1;

                pos_y -= entry_height;
                pos_y -= COLUMN_GAP;
            }
        }

        *self.content.borrow_mut() = content;
        let height: f32 = heights.into_iter().map(OrderedFloat).max().unwrap().into(); // TODO Ordered float
        let width = self.size.get().x;
        self.size.set(Vector2::new(width, height));
        self.size.get()
    }

    pub fn set_layers(&self, layers: &Layers, scroll_layer: &display::scene::Layer) {
        self.content.borrow().iter().for_each(|entry| entry.model().set_layers(layers));
        scroll_layer.add_exclusive(&self.display_object);
    }
}

impl display::Object for Model {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}

impl component::Model for Model {
    fn label() -> &'static str {
        "ColumnGrid"
    }

    fn new(app: &Application, _logger: &DefaultWarningLogger) -> Self {
        Self::new(app)
    }
}



// ===========
// === FRP ===
// ===========


define_endpoints_2! {
    Input{
        set_content(Vec<ColumnContent>), // TODO define proper API. This is for testing and illustrative purposes only.
        set_width(f32)
    }
    Output{
        size(Vector2)
    }
}


impl component::Frp<Model> for Frp {
    fn init(
        frp_api: &<Self as API>::Private,
        _app: &Application,
        model: &Model,
        _style: &StyleWatchFrp,
    ) {
        let network = &frp_api.network;

        frp::extend! { network
            size_update <- frp_api.input.set_content.map(f!((content) model.set_content(content)));
            frp_api.output.size <+ size_update;
        }
    }
}



pub type ColumnGrid = component::ComponentView<Model, Frp>;
