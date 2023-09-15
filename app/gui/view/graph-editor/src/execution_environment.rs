//! This module contains the logic for the execution environment selector.

use super::*;

use crate::Frp;



// =============================
// === Execution Environment ===
// =============================

/// Initialise the FRP logic for the execution environment selector.
pub fn init_frp(
    frp: &Frp,
    selector: &ide_view_execution_environment_selector::ExecutionEnvironmentSelector,
) {
    let out = &frp.private.output;
    let network = frp.network();

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

        selector.reset_play_button_state <+ frp.execution_complete;
    }
}
