//! This module contains the logic for the execution environment selector.

use super::*;

use crate::Frp;


// =============================
// === Execution Environment ===
// =============================

/// Initialise the FRP logic for the execution environment selector.
pub fn init_frp(frp: &Frp, model: &GraphEditorModelWithNetwork) {
    let out = &frp.private.output;
    let network = frp.network();
    let inputs = &frp.private.input;
    let selector = &model.execution_environment_selector;

    frp::extend! { network


        // === Execution Environment Changes ===

        selector.set_available_execution_environments <+ frp.set_available_execution_environments;
        out.execution_environment <+ selector.selected_execution_environment;
        out.execution_environment_play_button_pressed <+ selector.play_press;


        // === Layout ===

        init <- source::<()>();
        size_update <- all(init,selector.size,inputs.space_for_window_buttons);
        eval size_update ([model]((_,size,gap_size)) {
            let y_offset = MACOS_TRAFFIC_LIGHTS_VERTICAL_CENTER;
            let traffic_light_width = traffic_lights_gap_width();

            let execution_environment_selector_x = gap_size.x + traffic_light_width;
            model.execution_environment_selector.set_x(execution_environment_selector_x);
            let breadcrumb_gap_width =
                execution_environment_selector_x + size.x + TOP_BAR_ITEM_MARGIN;
            model.breadcrumbs.gap_width(breadcrumb_gap_width);

            model.execution_environment_selector.set_y(y_offset + size.y / 2.0);
            model.breadcrumbs.set_y(y_offset + component::breadcrumbs::HEIGHT / 2.0);
        });
    }
    init.emit(());
}
