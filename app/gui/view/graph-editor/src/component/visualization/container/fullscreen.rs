//! A module containing the fullscreen view of visualization.

use crate::prelude::*;
use ensogl::display::shape::*;
use ensogl::display::traits::*;
use ensogl::system::web::traits::*;

use ensogl::display;
use ensogl::display::scene::Scene;
use ensogl::display::DomSymbol;
use ensogl::system::web;
use ensogl_hardcoded_theme as theme;



// ==============
// === Shapes ===
// ==============

/// Container background shape definition.
///
/// Provides a backdrop and outline for visualisations. Can indicate the selection status of the
/// container.
/// TODO : We do not use backgrounds because otherwise they would overlap JS
///        visualizations. Instead we added a HTML background to the `View`.
///        This should be further investigated while fixing rust visualization displaying. (#526)
pub mod background {
    use super::*;

    ensogl::shape! {
        (style:Style,selected:f32,radius:f32,roundness:f32) {
            let width  : Var<Pixels> = "input_size.x".into();
            let height : Var<Pixels> = "input_size.y".into();
            let radius        = 1.px() * &radius;
            let color_path    = theme::graph_editor::visualization::background;
            let color_bg      = style.get_color(color_path);
            let corner_radius = &radius * &roundness;
            let background    = Rect((&width,&height)).corners_radius(corner_radius);
            let background    = background.fill(color_bg);
            background.into()
        }
    }
}



// ======================
// === FullscreenView ===
// ======================

/// View of the visualization container meant to be used in fullscreen mode.
#[derive(Clone, CloneRef, Debug)]
#[allow(missing_docs)]
pub struct Panel {
    display_object:     display::object::Instance,
    pub background_dom: DomSymbol,
    // TODO: See TODO above.
    // background     : background::View,
}

impl Panel {
    /// Constructor.
    pub fn new(scene: &Scene) -> Self {
        let display_object = display::object::Instance::new();

        // FIXME : StyleWatch is unsuitable here, as it was designed as an internal tool for shape
        // system (#795)
        let styles = StyleWatch::new(&scene.style_sheet);
        let bg_color =
            styles.get_color(ensogl_hardcoded_theme::graph_editor::visualization::background);
        let red = bg_color.red * 255.0;
        let green = bg_color.green * 255.0;
        let blue = bg_color.blue * 255.0;
        let bg_hex = format!("rgba({},{},{},{})", red, green, blue, bg_color.alpha);

        let div = web::document.create_div_or_panic();
        let background_dom = DomSymbol::new(&div);
        // TODO : We added a HTML background to the `View`, because "shape" background was
        // overlapping        the JS visualization. This should be further investigated
        // while fixing rust        visualization displaying. (#796)
        background_dom.dom().set_style_or_warn("width", "0");
        background_dom.dom().set_style_or_warn("height", "0");
        background_dom.dom().set_style_or_warn("z-index", "1");
        background_dom.dom().set_style_or_warn("overflow-y", "auto");
        background_dom.dom().set_style_or_warn("overflow-x", "auto");
        background_dom.dom().set_style_or_warn("background", bg_hex);
        background_dom.dom().set_style_or_warn("border-radius", "0");
        display_object.add_child(&background_dom);
        scene.dom.layers.fullscreen_vis.manage(&background_dom);

        Self { display_object, background_dom }
    }
}

impl display::Object for Panel {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}
