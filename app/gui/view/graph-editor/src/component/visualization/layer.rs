//! Functionality that allows one to manage occlusion/layers of visualizations in the scene.

use ensogl::display::DomSymbol;
use ensogl::display::Scene;



// =============
// === Layer ===
// =============

/// Indicates where the visualization should be displayed.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Layer {
    /// Display the visualization as part of the scene.
    Default,
    /// Display the visualization over the scene.
    Front,
    /// Display the visualization in fullscreen mode.
    Fullscreen,
}

impl Layer {
    /// Apply the layer setting to the given `DomSymbol`.
    pub fn apply_for_html_component(self, scene: &Scene, dom: &DomSymbol) {
        match self {
            Layer::Default => scene.dom.layers.back.manage(dom),
            Layer::Front => scene.dom.layers.front.manage(dom),
            Layer::Fullscreen => scene.dom.layers.fullscreen_vis.manage(dom),
        }
    }
}

impl Default for Layer {
    fn default() -> Self {
        Layer::Default
    }
}
