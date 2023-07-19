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

use ensogl_core::application::Application;
use ensogl_core::display;
use ensogl_core::display::navigation::navigator::Navigator;
use ensogl_core::display::object::ObjectOps;
use ensogl_core::frp;
use ensogl_text as text;
use ide_view_component_list_panel::grid;
use ide_view_component_list_panel::grid::entry::icon;



// ====================
// === Mock Entries ===
// ====================

const PREPARED_ITEMS: &[(&str, icon::Id, grid::GroupId)] = &[
    // ("long sample entry with text overflowing the width", icon::Id::Star),
    ("Data.read", icon::Id::DataInput, 0),
    ("Data.fetch", icon::Id::DataframeClean, 0),
    ("Query.google", icon::Id::DataScience, 0),
    ("Query.chat_gpt", icon::Id::DataScience, 0),
    ("number", icon::Id::NumberInput, 1),
    ("text", icon::Id::TextInput, 1),
    ("Table.new", icon::Id::TableEdit, 1),
    ("Array.new", icon::Id::TableEdit, 1),
    ("Date.current", icon::Id::DateAndTime, 2),
    ("Date.current_time", icon::Id::DateAndTime, 2),
    ("Table.count_rows", icon::Id::TableEdit, 2),
];

const fn make_group(group_id: grid::GroupId) -> grid::content::Group {
    grid::content::Group { id: group_id, color: None }
}

#[allow(dead_code)]
const GROUP_COUNT: usize = 3;


fn content_info() -> grid::content::Info {
    grid::content::Info {
        entry_count: 100,
        groups:      (0..GROUP_COUNT).map(make_group).collect(),
        is_filtered: false,
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

fn get_entry_model(entry: grid::EntryId) -> Option<(grid::EntryId, grid::EntryModel)> {
    let (caption, icon, group) = PREPARED_ITEMS[entry % PREPARED_ITEMS.len()];
    let opt_group = (entry < PREPARED_ITEMS.len()).as_some(group);
    let highlighted =
        if entry == 4 { vec![text::Range::new(text::Byte(2), text::Byte(4))] } else { vec![] };
    let model = grid::EntryModel {
        caption: caption.into(),
        highlighted: Rc::new(highlighted),
        icon,
        can_be_entered: false,
        group: opt_group,
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
