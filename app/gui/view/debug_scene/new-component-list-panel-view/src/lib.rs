//! Example scene showing a grid view with Component Group Entries.

#![recursion_limit = "512"]
// === Features ===
#![allow(incomplete_features)]
#![feature(negative_impls)]
#![feature(associated_type_defaults)]
#![feature(cell_update)]
#![feature(const_type_id)]
#![feature(drain_filter)]
#![feature(entry_insert)]
#![feature(fn_traits)]
#![feature(marker_trait_attr)]
#![feature(specialization)]
#![feature(trait_alias)]
#![feature(type_alias_impl_trait)]
#![feature(unboxed_closures)]
#![feature(trace_macros)]
#![feature(const_trait_impl)]
#![feature(slice_as_chunks)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![allow(clippy::option_map_unit_fn)]
#![allow(clippy::precedence)]
#![allow(dead_code)]
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]

use ensogl_core::prelude::*;
use wasm_bindgen::prelude::*;

use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display::object::ObjectOps;
use ensogl_core::display::scene;
use ensogl_core::display::shape::StyleWatch;
use ensogl_core::frp;
use ensogl_grid_view as grid_view;
use ensogl_grid_view::Col;
use ensogl_grid_view::Row;
use ensogl_hardcoded_theme as theme;
use ensogl_hardcoded_theme::application::component_browser::searcher::list_panel::section::column_grid;
use ensogl_text as text;
use ide_view_component_group::icon;
use ide_view_component_group::new_entry;
use ide_view_component_group::new_entry::Kind;
use ide_view_component_group::set::GroupId;
use ide_view_component_group::set::SectionId;
use ide_view_component_list_panel::layout;
use ide_view_component_list_panel::layout::GroupElement;
use ide_view_component_list_panel::layout::Layout;
use ide_view_component_list_panel::layout::HEADER_HEIGHT_IN_ROWS;
use ide_view_component_list_panel::layouting;



// ====================
// === Mock Entries ===
// ====================

const PREPARED_ITEMS: &[(&str, icon::Id)] = &[
    // ("long sample entry with text overflowing the width", icon::Id::Star),
    ("convert", icon::Id::Convert),
    ("table input", icon::Id::DataInput),
    ("text input", icon::Id::TextInput),
    ("number input", icon::Id::NumberInput),
    ("table output", icon::Id::TableEdit),
    ("dataframe clean", icon::Id::DataframeClean),
    ("data input", icon::Id::DataInput),
];

const fn make_group(index: usize, size: usize) -> layout::Group {
    let group_id = GroupId { section: SectionId::Favorites, index };
    layout::Group { id: group_id, height: size, original_height: size }
}

const GROUPS: &[layout::Group] = &[
    make_group(1, 3),
    make_group(2, 2),
    make_group(3, 1),
    make_group(4, 3),
    make_group(5, 2),
    make_group(6, 6),
    make_group(7, 6),
    make_group(8, 6),
    make_group(9, 5),
    make_group(10, 4),
    make_group(11, 8),
    make_group(12, 45),
    make_group(13, 60),
    make_group(14, 51),
];

struct EntryModelProvider {
    layout:            Layout,
    colors:            Vec<color::Rgba>,
    local_scope_color: color::Rgba,
}

impl EntryModelProvider {
    fn get(&self, row: Row, column: Col) -> Option<(Row, Col, new_entry::Model)> {
        self.layout.element_at_location(row, column).map(|element| {
            let kind = match element.element {
                GroupElement::Header => new_entry::Kind::Header,
                GroupElement::Entry(_) if element.group.section == SectionId::LocalScope =>
                    new_entry::Kind::LocalScopeEntry,
                GroupElement::Entry(_) => new_entry::Kind::Entry,
            };
            let color = if element.group.section == SectionId::LocalScope {
                self.local_scope_color
            } else {
                self.colors[element.group.index % self.colors.len()]
            };
            let (caption, icon) = match element.element {
                GroupElement::Header => (format!("Group {}", element.group.index).into(), None),
                GroupElement::Entry(index) => {
                    let (caption, icon) = PREPARED_ITEMS[index % PREPARED_ITEMS.len()];
                    (caption.into(), Some(icon))
                }
            };
            let highlighted = if row == 4 {
                vec![text::Range::new(text::Byte(2), text::Byte(4))]
            } else {
                vec![]
            };
            let model = new_entry::Model {
                kind,
                color,
                caption,
                icon,
                group_id: element.group,
                highlighted: Rc::new(highlighted),
            };
            (row, column, model)
        })
    }

    fn get_section(&self, row: Row, column: Col) -> Option<(Range<Row>, Col, new_entry::Model)> {
        let element = self.layout.element_at_location(row, column)?;
        let group = self.layout.group_at_location(row, column)?;
        let group_end = group.header_row + group.group.height + HEADER_HEIGHT_IN_ROWS;
        let model = new_entry::Model {
            kind:        Kind::Header,
            color:       self.colors[element.group.index % self.colors.len()],
            caption:     format!("Group {}", element.group.index).into(),
            highlighted: default(),
            icon:        None,
            group_id:    element.group,
        };
        Some((group.header_row..group_end, column, model))
    }
}



// ===================
// === Entry Point ===
// ===================

/// The example entry point.
#[entry_point]
#[allow(dead_code)]
pub fn main() {
    ensogl_text_msdf::run_once_initialized(|| {
        let app = Application::new("root");
        theme::builtin::light::register(&app);
        theme::builtin::light::enable(&app);

        let world = &app.display;
        let scene = &world.default_scene;
        let main_layer = &app.display.default_scene.layers.node_searcher;
        let grid_layer = main_layer.create_sublayer();
        let selection_layer = main_layer.create_sublayer();
        let style = StyleWatch::new(&scene.style_sheet);
        let group_color_paths = vec![
            column_grid::entry_color_0,
            column_grid::entry_color_1,
            column_grid::entry_color_2,
            column_grid::entry_color_3,
            column_grid::entry_color_4,
            column_grid::entry_color_5,
        ];
        let group_colors =
            group_color_paths.into_iter().map(|path| style.get_color(path)).collect();
        let entry_size = Vector2(133.0, 30.0);
        let params = new_entry::Params {
            style: new_entry::Style {
                color_intensities: new_entry::style::ColorIntensities {
                    text: 1.0,
                    background: style.get_number(theme::application::component_browser::component_group::background_color_intensity),
                    dimmed: style.get_number(theme::application::component_browser::component_group::dimmed_color_intensity),
                    icon_strong: 1.0,
                    icon_weak: style.get_number(theme::application::component_browser::component_group::entry_list::icon::weak_color_intensity),
                    hover_highlight: style.get_number(theme::application::component_browser::component_group::entry_list::icon::weak_color_intensity) * 0.8,
                },
                group_width: entry_size.x,
                gap_between_groups: 3.0,
                padding: 17.0,
                icon_size: 16.0,
                text_size: text::Size(12.0),
                icon_text_padding: 8.0,
                font: "dejavusans".into(),
                selection_corners_radius: 12.0,
                highlight_bold: style.get_number(theme::application::component_browser::component_group::entry_list::text::highlight_bold),
                header_shadow_size: 27.0,
            },
            dimmed_groups: default(),
        };
        let selection_params = new_entry::Params {
            style: new_entry::Style {
                color_intensities: new_entry::style::ColorIntensities {
                    text:  style.get_number(theme::application::component_browser::component_group::background_color_intensity),
                    background: style.get_number(theme::application::component_browser::component_group::selection_color_intensity),
                    dimmed: style.get_number(theme::application::component_browser::component_group::dimmed_color_intensity),
                    icon_strong: style.get_number(theme::application::component_browser::component_group::background_color_intensity),
                    icon_weak: style.get_number(theme::application::component_browser::component_group::background_color_intensity),
                    hover_highlight: default(),
                },
                ..params.style.clone()
            },
            dimmed_groups: default(),
        };


        let layout = layouting::Layouter::new(GROUPS.iter().copied()).create_layout(6);
        let provider = Rc::new(EntryModelProvider { layout, colors: group_colors, local_scope_color: style.get_color(theme::application::component_browser::searcher::list_panel::favourites_section_base_color) });
        let grid = grid_view::scrollable::SelectableGridViewWithHeaders::<
            new_entry::View,
            new_entry::View,
        >::new(&app);
        grid_layer.add_exclusive(&grid);
        grid.set_position_xy(Vector2(-200.0, 200.0));
        let network = frp::Network::new("new_component_list_panel_view");
        let header_frp = grid.header_frp();
        let adjust_pixels = f!([grid](&shape: &scene::Shape) {
            let device_size = shape.device_pixels();
            let origin_left_top_pos = Vector2(device_size.width, device_size.height)/ 2.0;
            let adjusted_left_top_pos = Vector2(origin_left_top_pos.x.floor(), origin_left_top_pos.y.floor());
            let offset = adjusted_left_top_pos - origin_left_top_pos;
            grid.set_position_xy(offset);
        });
        frp::extend! { network
            grid.model_for_entry <+ grid.model_for_entry_needed.filter_map(f!((&(r, c)) provider.get(r, c)));
            header_frp.section_info <+ header_frp.section_info_needed.filter_map(f!((&(r, c)) provider.get_section(r,c)));
            _adjust <- scene.frp.shape.map(adjust_pixels);
        }
        grid.scroll_frp().resize(Vector2(405.0, 406.0));
        grid.set_entries_size(entry_size);
        grid.set_entries_params(&params);
        grid.selection_highlight_frp().setup_masked_layer(Some(selection_layer.downgrade()));
        grid.selection_highlight_frp().set_entries_params(selection_params);
        grid.reset_entries(provider.layout.row_count(), provider.layout.column_count());
        grid.set_column_width((1, entry_size.x + params.style.gap_between_groups * 2.0));

        scene.add_child(&grid);
        mem::forget(app);
        mem::forget(grid);
        mem::forget(network);
        mem::forget(grid_layer);
        mem::forget(selection_layer);
    })
}
