//! Edited node growth/shrink animation implementation.
//!
//! When the user starts editing of the node - it smoothly growth in size to match the 1.0x zoom
//! factor size. After editing is finished, the node smothly shrinks to its original size. This is
//! implemented by using a separate `edited_node` camera that is moved in synchronization with
//! `node_searcher` camera.

use ensogl::prelude::*;

use crate::application::command::FrpNetworkProvider;
use crate::GraphEditorModelWithNetwork;
use crate::NodeId;

use enso_frp as frp;
use ensogl::animation::easing::EndStatus::Normal;
use ensogl::display::Scene;
use ensogl::Animation;
use ensogl::Easing;



/// Describes the "speed" of growth/shrink animation.
///
/// To determine the duration of the blending animation, we divide the length of the camera path by
/// this value. This is primarily used to move the edited node back to the `main` layer once editing
/// is done. If the camera is already at its destination – the duration would be close to zero, so
/// we would immediately change the layer of the node. If the camera needs to travel a lot - we
/// increase the animation duration proportionally so that the layer would be changed later.
///
/// The exact value is selected empirically. The maximum camera travel distance is about 9000.0
/// units, so our coefficient determines the maximum animation duration as 600 ms.
const ANIMATION_LENGTH_COEFFIENT: f32 = 15.0;

/// Initialize edited node growth/shrink animator. It would handle scene layer change for the edited
/// node as well.
pub fn initialize_edited_node_animator(
    model: &GraphEditorModelWithNetwork,
    frp: &crate::Frp,
    scene: &Scene,
) {
    let network = &frp.network();
    let out = &frp.output;
    let searcher_cam = scene.layers.node_searcher.camera();
    let edited_node_cam = scene.layers.edited_node.camera();
    let main_cam = scene.layers.main.camera();

    let growth_animation = Animation::new(network);
    let animation_blending = Easing::new(network);

    frp::extend! { network
        let searcher_cam_frp = searcher_cam.frp();
        let main_cam_frp = main_cam.frp();


        // === Starting node editing ===

        previous_edited_node <- out.node_editing_started.previous();
        _eval <- out.node_editing_started.map2(&previous_edited_node, f!([model] (current, previous) {
            model.move_node_to_main_layer(*previous);
            model.move_node_to_edited_node_layer(*current);
        }));


        // === Edited node camera position animation ===

        is_growing <- bool(&out.node_editing_finished, &out.node_editing_started);
        edited_node_cam_target <- switch(&is_growing, &main_cam_frp.position, &searcher_cam_frp.position);
        growth_animation.target <+ edited_node_cam_target;

        camera_path_length <- all_with
            (&growth_animation.value, &growth_animation.target, |v, t| (v - t).magnitude());
        on_node_editing_start_or_finish <- any(&out.node_editing_started, &out.node_editing_finished);
        start_animation_blending <- camera_path_length.sample(&on_node_editing_start_or_finish);
        eval start_animation_blending ((length) {
            animation_blending.set_duration(*length / ANIMATION_LENGTH_COEFFIENT);
            animation_blending.stop_and_rewind(0.0);
            animation_blending.target(1.0);
        });

        // We want to:
        // 1. Smoothly animate edited node camera from `main_cam` position to `searcher_cam` position when we
        // start/finish node editing so that the edited node "grows" or "shrinks" to reach the correct
        // visible size. This is `growth_animation`.
        // 2. Keep `searcher_cam` and `edited_node_cam` at the same position everywhere else so that the
        // searcher and the edited node are at the same visible position at all times. This is
        // `edited_node_cam_target`.
        //
        // Enabling/disabling "follow me" mode for the edited node camera is hard
        // to implement and leads to serious visualization lags. To avoid that, we blend these two
        // components together using `animation_blending` as a weight coefficient. This allows a very smooth
        // transition between "follow me" mode and node growth/shrink animation.
        edited_node_cam_position <- all_with3
            (&edited_node_cam_target, &growth_animation.value, &animation_blending.value, |target,animation,weight| {
            let weight = Vector3::from_element(*weight);
            let inv_weight = Vector3::from_element(1.0) - weight;
            target.component_mul(&weight) + animation.component_mul(&inv_weight)
        });
        eval edited_node_cam_position([edited_node_cam] (pos) edited_node_cam.set_position(*pos));


        // === Finishing shrinking animation ===

        on_animation_end <- animation_blending.on_end.filter(|end_status| *end_status == Normal);
        shrinking_finished <- on_animation_end.gate_not(&is_growing);
        node_that_finished_editing <- out.node_editing_started.sample(&shrinking_finished);
        eval node_that_finished_editing ([model] (id) {
            model.move_node_to_main_layer(*id);
        });
    }
}


// === Helpers ===

impl GraphEditorModelWithNetwork {
    /// Move node to the `edited_node` scene layer, so that it is rendered by the separate camera.
    #[profile(Debug)]
    fn move_node_to_edited_node_layer(&self, node_id: NodeId) {
        if let Some(node) = self.nodes.get_cloned(&node_id) {
            node.model().move_to_edited_node_layer();
        }
    }

    /// Move node to the `main` scene layer, so that it is rendered by the main camera.
    #[profile(Debug)]
    fn move_node_to_main_layer(&self, node_id: NodeId) {
        if let Some(node) = self.nodes.get_cloned(&node_id) {
            node.model().move_to_main_layer();
        }
    }
}
