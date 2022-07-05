//! Wrapper around multiple [`component_group::View`] that provides a layout where the
//! [`component_group::View`] are stacked in three columns. Designed for use in the sections of a
//! Component Browser Panel.

use ensogl_core::display::shape::*;
use ensogl_core::prelude::*;

use crate::Layers;
use enso_frp as frp;
use ensogl_core::application::frp::API;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::define_endpoints_2;
use ensogl_core::display;
use ensogl_core::display::style;
use ensogl_gui_component::component;
use ensogl_list_view as list_view;
use ensogl_scroll_area as scroll_area;
use ide_view_component_group as component_group;
use ordered_float::OrderedFloat;



// =============
// === Model ===
// =============

#[derive(Clone, Debug)]
struct Entry {
    index:   usize,
    content: component_group::View,
    visible: bool,
}


/// Contains a [`AnyModelProvider`] with a label. Can be used to populate a
/// [`component_group::View`].
#[derive(Clone, Debug, Default)]
pub struct LabeledAnyModelProvider {
    /// Label of the data provided to be used as a header of the list.
    pub label:   String,
    /// Content to be used to populate a list.
    pub content: list_view::entry::AnyModelProvider<component_group::Entry>,
}



// =============
// === Model ===
// =============

/// The Model of the [`ColumnGrid`] component.
#[derive(Clone, Debug, CloneRef)]
pub struct Model {
    app:            Application,
    display_object: display::object::Instance,
    content:        Rc<RefCell<Vec<Entry>>>,
    size:           Rc<Cell<Vector2>>,
    layers:         Rc<RefCell<Option<Layers>>>,
}

impl Model {
    fn new(app: &Application) -> Self {
        let logger = Logger::new("ColumnGrid");
        let app = app.clone_ref();
        let display_object = display::object::Instance::new(&logger);
        Self { app, display_object, content: default(), size: default(), layers: default() }
    }

    fn update_content_layout(&self, content: &[LabeledAnyModelProvider], style: &Style) -> Vector2 {
        const NUMBER_OF_COLUMNS: usize = 3;
        let overall_width = style.content_width - 2.0 * style.content_padding;
        let column_width = (overall_width - 2.0 * style.column_gap) / NUMBER_OF_COLUMNS as f32;
        let content = content
            .iter()
            .enumerate()
            .filter_map(|(index, LabeledAnyModelProvider { content, label })| {
                if content.entry_count() > 0 {
                    let view = self.app.new_view::<component_group::View>();
                    if let Some(layers) = self.layers.borrow().as_ref() {
                        view.model().set_layers(&layers.groups);
                    } else {
                        tracing::log::warn!("Created ColumnGrid entry without layers.");
                    }
                    view.set_width(column_width);
                    view.set_entries(content);
                    view.set_header(label.as_str());
                    self.display_object.add_child(&view);
                    Some(Entry { index, content: view, visible: false })
                } else {
                    None
                }
            })
            .collect_vec();

        let mut columns = vec![vec![]; NUMBER_OF_COLUMNS];
        // We need to subtract one `column_gap` as we only need (n-1) gaps, but through iteration
        // below we add one gap per item. So we initialise the heights with `-column_gap`.
        let mut heights = [-style.column_gap; NUMBER_OF_COLUMNS];

        for entry in content.iter() {
            let column_index = entry.index % NUMBER_OF_COLUMNS;
            columns[column_index].push(&entry.content);
            heights[column_index] += entry.content.size.value().y + style.column_gap;
        }
        let height: f32 = heights.into_iter().map(OrderedFloat).max().unwrap_or_default().into();

        let mut entry_ix = 0;
        for (column_index, column) in columns.iter().enumerate() {
            // The +0.5 required as a way to center the columns in the x direction by shifting by an
            // additional half-width.
            let pos_x = (column_width + style.column_gap) * (column_index as f32 + 0.5);
            let mut pos_y = -height;
            for entry in column {
                let entry_height = entry.size.value().y;
                entry.set_position_y(pos_y + entry_height / 2.0);
                entry.set_position_x(pos_x);
                entry.set_color(style.get_entry_color_for_index(entry_ix));

                entry_ix += 1;
                pos_y += entry_height;
                pos_y += style.column_gap;
            }
        }

        *self.content.borrow_mut() = content;
        let height: f32 = heights.into_iter().map(OrderedFloat).max().unwrap_or_default().into();
        let width = self.size.get().x;
        self.size.set(Vector2::new(width, height));
        self.size.get()
    }

    /// Assign a set of layers to render the component group in. Must be called after constructing
    /// the [`View`].
    pub(crate) fn set_layers(&self, layers: &Layers) {
        self.content
            .borrow()
            .iter()
            .for_each(|entry| entry.content.model().set_layers(&layers.groups));
        layers.scroll_layer.add_exclusive(&self.display_object);
        self.layers.set(layers.clone_ref());
    }

    fn set_scroll_viewport(&self, viewport: scroll_area::Viewport) {
        self.content.borrow_mut().iter_mut().for_each(|entry| {
            let view = &entry.content;
            let root_pos = self.display_object.position().xy();
            let center_offset = Vector2::new(-view.size.value().x, view.size.value().y) / 2.0;
            let element_pos = root_pos + view.position().xy() + center_offset;
            let is_visible = viewport.intersects(element_pos, view.size.value());
            entry.visible = is_visible;

            if is_visible {
                let element_top = element_pos.y;
                let clamped = viewport.clamp(Vector2::new(0.0, element_top)).y;
                let rel_pos = element_top - clamped;
                view.set_header_pos(rel_pos);

                self.add_child(view);
            } else {
                view.unset_parent();
            }
        })
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

#[derive(Clone, Debug, Default)]
struct Style {
    column_gap:      f32,
    entry_colors:    [color::Rgba; 6],
    content_width:   f32,
    content_padding: f32,
}

impl Style {
    /// Choose a color from the `entry_colors` based on the index of the entry within the
    /// [`ColumnGrid`].
    fn get_entry_color_for_index(&self, ix: usize) -> color::Rgba {
        self.entry_colors[ix % self.entry_colors.len()]
    }
}


define_endpoints_2! {
    Input{
        set_content(Vec<LabeledAnyModelProvider>),
        set_scroll_viewport(scroll_area::Viewport),
    }
    Output{
        size(Vector2)
    }
}


fn get_layout(
    network: &enso_frp::Network,
    style: &StyleWatchFrp,
) -> (enso_frp::Stream<Style>, enso_frp::stream::WeakNode<enso_frp::SourceData>) {
    let searcher_theme_path: style::Path =
        ensogl_hardcoded_theme::application::component_browser::searcher::HERE.into();
    let theme_path: style::Path =
        searcher_theme_path.sub("list_panel").sub("section").sub("column_grid");
    let column_gap = style.get_number(theme_path.sub("column_gap"));
    let entry_color_0 = style.get_color(theme_path.sub("entry_color_0"));
    let entry_color_1 = style.get_color(theme_path.sub("entry_color_1"));
    let entry_color_2 = style.get_color(theme_path.sub("entry_color_2"));
    let entry_color_3 = style.get_color(theme_path.sub("entry_color_3"));
    let entry_color_4 = style.get_color(theme_path.sub("entry_color_4"));
    let entry_color_5 = style.get_color(theme_path.sub("entry_color_5"));

    let theme_path: style::Path = searcher_theme_path.sub("list_panel");
    let content_padding = style.get_number(theme_path.sub("content_padding"));
    let content_width = style.get_number(theme_path.sub("content_width"));

    frp::extend! { network
        init <- source_();

        entry_colors <- all6(
            &entry_color_0,
            &entry_color_1,
            &entry_color_2,
            &entry_color_3,
            &entry_color_4,
            &entry_color_5,
        );
        entry_colors <- all(&init,&entry_colors);
        entry_colors <- entry_colors.map(|(_,(c1,c2,c3,c4,c5,c6))| [*c1,*c2,*c3,*c4,*c5,*c6]);

        layout_update <- all5(&init, &column_gap, &entry_colors, &content_padding, &content_width);
        layout_update <- layout_update.map(|(_, column_gap,entry_colors,content_padding,content_width)|{
            Style{
                column_gap:*column_gap,
                entry_colors:*entry_colors,
                content_padding:*content_padding,
                content_width:*content_width
            }
        });

    }
    (layout_update, init)
}

impl component::Frp<Model> for Frp {
    fn init(
        frp_api: &<Self as API>::Private,
        _app: &Application,
        model: &Model,
        style: &StyleWatchFrp,
    ) {
        let network = &frp_api.network;
        let (layout_update, init) = get_layout(network, style);

        frp::extend! { network
            content_update <- all(&frp_api.input.set_content,&layout_update);
            size_update <- content_update.map(f!(((content,layout))
                model.update_content_layout(content,layout))
            );
            frp_api.output.size <+ size_update;

            eval frp_api.input.set_scroll_viewport((viewport) model.set_scroll_viewport(*viewport));
        }
        init.emit(());
    }
}

/// Wrapper around multiple [`component_group::View`] that provides a layout where the
/// `[component_group::View`] are stacked in three columns. Designed for use in the sections of a
/// Component Browser Panel.
pub type ColumnGrid = component::ComponentView<Model, Frp>;
