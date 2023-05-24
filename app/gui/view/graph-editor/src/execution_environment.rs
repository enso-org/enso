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

        switch_to_live <-
            frp.switch_to_live_execution_environment.constant(ExecutionEnvironment::Live);
        switch_to_design <-
            frp.switch_to_design_execution_environment.constant(ExecutionEnvironment::Design);
        external_update <- any(switch_to_live,switch_to_design);
        selector.set_execution_environment <+ external_update;

        out.execution_environment <+ selector.selected_execution_environment.on_change();
        out.execution_environment_play_button_pressed <+ selector.play_press;
        frp.set_read_only <+ selector.play_press.constant(true);

        // === Play Button ===
        selector.reset_play_button_state <+ frp.execution_finished;

        // === Layout ===

        init <- source::<()>();
        size_update <- all(init, selector.size, inputs.graph_editor_top_bar_offset_x);
        eval size_update ([model] ((_, size, graph_editor_top_bar_offset_x)) {
            let y_offset = MACOS_TRAFFIC_LIGHTS_VERTICAL_CENTER;
            let traffic_light_width = traffic_lights_gap_width();

            let execution_environment_selector_x =
                graph_editor_top_bar_offset_x + traffic_light_width;
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
