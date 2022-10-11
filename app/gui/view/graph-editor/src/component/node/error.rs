//! Contains a struct definition for error information on nodes.

use crate::prelude::*;
use ensogl::system::web::traits::*;

use crate::builtin::visualization::native::error as error_visualization;
use crate::component::visualization;

use ensogl::display;
use ensogl::display::shape::StyleWatch;
use ensogl::display::DomSymbol;
use ensogl::display::Scene;
use ensogl::system::web;
use ensogl_component::shadow;
use serde::Deserialize;
use serde::Serialize;



// =============
// === Error ===
// =============

/// An error kind.
#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub enum Kind {
    Panic,
    Dataflow,
}

/// Additional error information (beside the error value itself) for some erroneous node.
#[derive(Clone, CloneRef, Debug, Eq, PartialEq)]
#[allow(missing_docs)]
pub struct Error {
    pub kind:       Immutable<Kind>,
    /// An error message overriding the error visualization data. Should be set in cases when the
    /// visualization won't work (e.g. in case of panics).
    pub message:    Rc<Option<String>>,
    /// Flag indicating that the error is propagated from another node visible on the scene.
    pub propagated: Immutable<bool>,
}

impl Error {
    /// Return data which should be sent to the Error Visualization to display this error.
    /// Returns [`None`] if the data should arrive from the Engine.
    pub fn visualization_data(&self) -> Option<error_visualization::Input> {
        Some(error_visualization::Input {
            kind:    Some(*self.kind),
            message: self.message.as_ref().as_ref()?.clone(),
        })
    }
}



// =====================================
// === Error Visualization Container ===
// =====================================

// === Constants ===

const SIZE: (f32, f32) = super::super::visualization::container::DEFAULT_SIZE;
const Z_INDEX: usize = 1;
const BORDER_RADIUS: f32 = 14.0;


// === Container ===

/// The container containing just the error visualization and background.
#[derive(Clone, CloneRef, Debug)]
pub struct Container {
    logger:         Logger,
    visualization:  error_visualization::Error,
    scene:          Scene,
    // TODO : We added a HTML background to the `View`, because "shape" background was
    //     overlapping the DOM created by error visualization. This should be further
    //     investigated while fixing rust visualization displaying. (#796)
    background_dom: DomSymbol,
    display_object: display::object::Instance,
}

impl Deref for Container {
    type Target = error_visualization::Error;

    fn deref(&self) -> &Self::Target {
        &self.visualization
    }
}

impl Container {
    /// Constructor of error container.
    pub fn new(scene: &Scene) -> Self {
        let scene = scene.clone_ref();
        let logger = Logger::new("error::Container");
        let display_object = display::object::Instance::new();
        let background_dom = Self::create_background_dom(&scene);
        let visualization = error_visualization::Error::new(&scene);

        display_object.add_child(&background_dom);
        display_object.add_child(&visualization);

        Self { logger, visualization, scene, background_dom, display_object }
    }

    fn create_background_dom(scene: &Scene) -> DomSymbol {
        // FIXME : StyleWatch is unsuitable here, as it was designed as an internal tool for shape
        //     system (#795)
        let styles = StyleWatch::new(&scene.style_sheet);
        let bg_color =
            styles.get_color(ensogl_hardcoded_theme::graph_editor::visualization::background);
        let bg_red = bg_color.red * 255.0;
        let bg_green = bg_color.green * 255.0;
        let bg_blue = bg_color.blue * 255.0;
        let bg_hex = format!("rgba({},{},{},{})", bg_red, bg_green, bg_blue, bg_color.alpha);

        let div = web::document.create_div_or_panic();
        let background_dom = DomSymbol::new(&div);
        let (width, height) = SIZE;
        let width = format!("{}.px", width);
        let height = format!("{}.px", height);
        let z_index = Z_INDEX.to_string();
        let border_radius = format!("{}.px", BORDER_RADIUS);
        background_dom.dom().set_style_or_warn("width", width);
        background_dom.dom().set_style_or_warn("height", height);
        background_dom.dom().set_style_or_warn("z-index", z_index);
        background_dom.dom().set_style_or_warn("overflow-y", "auto");
        background_dom.dom().set_style_or_warn("overflow-x", "auto");
        background_dom.dom().set_style_or_warn("background", bg_hex);
        background_dom.dom().set_style_or_warn("border-radius", border_radius);
        shadow::add_to_dom_element(&background_dom, &styles);
        background_dom
    }

    /// Move the container with visualization to `layer`.
    pub fn set_layer(&self, layer: visualization::Layer) {
        self.visualization.frp.set_layer.emit(layer);
        layer.apply_for_html_component(&self.scene, &self.background_dom);
    }
}

impl display::Object for Container {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}
