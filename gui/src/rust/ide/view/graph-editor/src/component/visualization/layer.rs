//! Functionality that allows one to manage occlusion/layers of visualisations in the scene.
use ensogl::display::DomSymbol;
use ensogl::display::Scene;



// =============
// === Layer ===
// =============

/// Indicates where the visualisation should be displayed.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Layer {
    /// Display the visualisation as part of the scene.
    Default,
    /// Display the visualisation over the scene.
    Front,
}

impl Layer {
    /// Apply the layer setting to the given `DomSymbol`.
    pub fn apply_for_html_component(self, scene: &Scene, dom: &DomSymbol) {
        match self {
            Layer::Default => scene.dom.layers.back.manage(dom),
            Layer::Front => scene.dom.layers.front.manage(dom),
        }
    }
}

impl Default for Layer {
    fn default() -> Self {
        Layer::Default
    }
}
