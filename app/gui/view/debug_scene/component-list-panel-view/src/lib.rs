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
use ensogl_core::display;
use ensogl_core::display::navigation::navigator::Navigator;
use ensogl_core::display::object::ObjectOps;
use ensogl_core::frp;
use ensogl_hardcoded_theme as theme;
use ensogl_text as text;
use ide_view_component_list_panel::grid;
use ide_view_component_list_panel::grid::entry::icon;



// ====================
// === Mock Entries ===
// ====================

const PREPARED_ITEMS: &[(&str, icon::Id)] = &[
    // ("long sample entry with text overflowing the width", icon::Id::Star),
    ("convert", icon::Id::Convert),
    ("data input", icon::Id::DataInput),
    ("data output", icon::Id::DataOutput),
    ("table input", icon::Id::TableEdit),
    ("number input", icon::Id::NumberInput),
    ("text input", icon::Id::TextInput),
    ("add column", icon::Id::AddColumn),
    ("select column", icon::Id::SelectColumn),
    ("clean", icon::Id::DataframeClean),
    ("add row", icon::Id::AddRow),
    ("map row", icon::Id::DataframeMapRow),
    ("map column", icon::Id::DataframeMapColumn),
];

const fn make_group(section: grid::SectionId, index: usize, size: usize) -> grid::content::Group {
    let group_id = grid::GroupId { section, index };
    grid::content::Group {
        id:              group_id,
        height:          size,
        original_height: size,
        color:           None,
    }
}

/// Use this groups setup to compare to Figma design.
#[allow(dead_code)]
const GROUPS_AS_IN_DESIGN: &[grid::content::Group] = &[
    make_group(grid::SectionId::Popular, 0, 7),
    make_group(grid::SectionId::Popular, 1, 7),
    make_group(grid::SectionId::Popular, 2, 5),
    make_group(grid::SectionId::Namespace(0), 3, 10),
    make_group(grid::SectionId::Namespace(0), 4, 3),
    make_group(grid::SectionId::Namespace(0), 5, 10),
    make_group(grid::SectionId::Namespace(0), 6, 10),
];

const GROUPS: &[grid::content::Group] = &[
    make_group(grid::SectionId::Popular, 1, 3),
    make_group(grid::SectionId::Popular, 2, 2),
    make_group(grid::SectionId::Popular, 3, 1),
    make_group(grid::SectionId::Popular, 4, 3),
    make_group(grid::SectionId::Popular, 5, 2),
    make_group(grid::SectionId::Popular, 6, 6),
    make_group(grid::SectionId::Popular, 7, 6),
    make_group(grid::SectionId::Popular, 8, 6),
    make_group(grid::SectionId::Popular, 9, 5),
    make_group(grid::SectionId::Popular, 10, 4),
    make_group(grid::SectionId::Popular, 11, 8),
    make_group(grid::SectionId::Popular, 12, 10),
    make_group(grid::SectionId::Popular, 13, 12),
    make_group(grid::SectionId::Popular, 14, 3),
    make_group(grid::SectionId::Namespace(0), 15, 23),
    make_group(grid::SectionId::Namespace(0), 16, 12),
    make_group(grid::SectionId::Namespace(0), 17, 21),
    make_group(grid::SectionId::Namespace(0), 18, 33),
    make_group(grid::SectionId::Namespace(0), 19, 5),
    make_group(grid::SectionId::Namespace(0), 20, 14),
    make_group(grid::SectionId::Namespace(0), 21, 44),
    make_group(grid::SectionId::Namespace(0), 22, 12),
    make_group(grid::SectionId::Namespace(0), 23, 14),
    make_group(grid::SectionId::Namespace(0), 24, 7),
    make_group(grid::SectionId::Namespace(0), 25, 8),
    make_group(grid::SectionId::Namespace(0), 26, 13),
    make_group(grid::SectionId::Namespace(0), 27, 32),
    make_group(grid::SectionId::Namespace(0), 28, 34),
];

const LOCAL_SCOPE_GROUP_SIZE: usize = 1024;
const NAMESPACE_SECTION_COUNT: usize = 1;

fn content_info() -> grid::content::Info {
    grid::content::Info {
        groups:                  GROUPS_AS_IN_DESIGN.into(),
        local_scope_entry_count: LOCAL_SCOPE_GROUP_SIZE,
        namespace_section_count: NAMESPACE_SECTION_COUNT,
    }
}

const GROUP_NAMES: &[&str] = &[
    "Input / Output",
    "Preparation",
    "Join",
    "Text",
    "Date and Time",
    "Transform",
    "Machine Learning",
];

fn get_header_model(group: grid::GroupId) -> Option<(grid::GroupId, grid::HeaderModel)> {
    let caption = GROUP_NAMES.get(group.index % GROUP_NAMES.len()).copied().unwrap_or("");
    let model = grid::HeaderModel { caption: caption.into(), can_be_entered: false };
    Some((group, model))
}

fn get_entry_model(entry: grid::GroupEntryId) -> Option<(grid::GroupEntryId, grid::EntryModel)> {
    let (caption, icon) = PREPARED_ITEMS[entry.entry % PREPARED_ITEMS.len()];
    let highlighted = if entry.entry == 4 {
        vec![text::Range::new(text::Byte(2), text::Byte(4))]
    } else {
        vec![]
    };
    let model = grid::EntryModel {
        caption: caption.into(),
        highlighted: Rc::new(highlighted),
        icon,
        can_be_entered: false,
    };
    Some((entry, model))
}

fn snap_to_pixel_offset(size: Vector2, scene_shape: &display::scene::Shape) -> Vector2 {
    let device_size = scene_shape.device_pixels();
    let origin_left_top_pos = Vector2(device_size.width, device_size.height) / 2.0;
    let origin_snapped = Vector2(origin_left_top_pos.x.floor(), origin_left_top_pos.y.floor());
    let origin_offset = origin_snapped - origin_left_top_pos;
    let panel_left_top_pos = (size * scene_shape.pixel_ratio) / 2.0;
    let panel_snapped = Vector2(panel_left_top_pos.x.floor(), panel_left_top_pos.y.floor());
    let panel_offset = panel_snapped - panel_left_top_pos;
    origin_offset - panel_offset
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
        let navigator = Navigator::new(scene, &scene.layers.node_searcher.camera());
        let panel = app.new_view::<ide_view_component_list_panel::View>();
        scene.layers.node_searcher.add(&panel);
        panel.show();
        let network = frp::Network::new("new_component_list_panel_view");
        //TODO[ao] should be done by panel itself.
        let grid = &panel.model().grid;
        frp::extend! { network
            init <- source_();
            grid.model_for_header <+ grid.model_for_header_needed.filter_map(|&id| get_header_model(id));
            grid.model_for_entry <+ grid.model_for_entry_needed.filter_map(|&id| get_entry_model(id));
            size <- all_with(&init, &panel.size, |(), panel_size| *panel_size);
            snap <- all_with(&size, &scene.frp.shape, |sz, sh| snap_to_pixel_offset(*sz, sh));
            eval snap((snap) panel.set_xy(*snap));


            // === Disable navigator on hover ===

            navigator.frp.set_enabled <+ panel.is_hovered.not();
        }
        init.emit(());

        grid.reset(content_info());
        scene.add_child(&panel);
        panel.show();
        mem::forget(app);
        mem::forget(panel);
        mem::forget(network);
        mem::forget(navigator);
    })
}
