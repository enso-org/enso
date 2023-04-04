//! This module contains the logic for the execution mode selector.

use super::*;

use crate::Frp;

use ide_view_execution_mode_selector::ExecutionMode;



fn get_next_execution_mode(
    current: &ExecutionMode,
    available: &[ExecutionMode],
) -> Option<ExecutionMode> {
    let index = available.iter().position(|mode| mode == current)?;
    let next_index = (index + 1) % available.len();
    Some(available[next_index].clone())
}

/// Initialise the FRP logic for the execution mode selector.
pub fn init_frp(frp: &Frp, model: &GraphEditorModelWithNetwork) {
    let out = &frp.private.output;
    let network = frp.network();
    let inputs = &model.frp;
    let execution_mode_selector = &model.execution_mode_selector;

    frp::extend! { network
        // === Execution Mode Changes ===

        execution_mode_selector.set_available_execution_modes <+ frp.set_available_execution_modes;
        execution_mode_selector.set_execution_mode <+ frp.set_execution_mode;
        execution_mode_state <- all(out.execution_mode,frp.set_available_execution_modes);

        execution_mode_toggled <- execution_mode_state.sample(&frp.toggle_execution_mode);
        toggled_execution_mode <- execution_mode_toggled.map(|(mode,available)| get_next_execution_mode(mode,available)).unwrap();

        external_execution_mode_update <- any(frp.set_execution_mode,toggled_execution_mode);
        execution_mode_selector.set_execution_mode <+ external_execution_mode_update;

        execution_mode_update <- any(execution_mode_selector.selected_execution_mode,external_execution_mode_update);
        out.execution_mode <+ execution_mode_update;
        out.execution_mode_play_button_pressed <+ execution_mode_selector.play_press;


        // === Layout ===
        init <- source::<()>();
        size_update <- all(init,execution_mode_selector.size,inputs.space_for_window_buttons);
        eval size_update ([model]((_,size,gap_size)) {
            let y_offset = MACOS_TRAFFIC_LIGHTS_VERTICAL_CENTER;
            let traffic_light_width = traffic_lights_gap_width();

            let execution_mode_selector_x = gap_size.x + traffic_light_width;
            model.execution_mode_selector.set_x(execution_mode_selector_x);
            let breadcrumb_gap_width = execution_mode_selector_x + size.x + TOP_BAR_ITEM_MARGIN;
            model.breadcrumbs.gap_width(breadcrumb_gap_width);

            model.execution_mode_selector.set_y(y_offset + size.y / 2.0);
            model.breadcrumbs.set_y(y_offset + component::breadcrumbs::HEIGHT / 2.0);
        });
    }
    init.emit(());
}
