use crate::prelude::*;

use crate::{component_group, layout};
use crate::Layout;
use ensogl_grid_view as grid_view;

pub struct GroupInfo {
    groups: Vec<layout::Group>,
    local_scope_size
}

ensogl_core::define_endpoints_2! {
    Input {
        reset()
    }
    Output {

    }
}


// ============
// === Grid ===
// ============

pub type Grid = grid_view::scrollable::SelectableGridViewWithHeaders<
    component_group::new_entry::View,
    component_group::new_entry::View,
>;



pub struct Style {
    pub width:  f32,
    pub height: f32,
}


struct Model {
    grid:   Grid,
    layout: Layout,
}
