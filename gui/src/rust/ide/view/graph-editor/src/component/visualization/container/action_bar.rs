//! Definition of the `ActionBar` component for the `visualization::Container`.

use crate::prelude::*;

use crate::component::node;
use crate::component::visualization::container::visualization_chooser;
use crate::component::visualization;

use enso_frp as frp;
use enso_frp;
use ensogl::application::Application;
use ensogl::data::color;
use ensogl::display::shape::*;
use ensogl::display::traits::*;
use ensogl::display;
use ensogl::gui::component;



// =================
// === Constants ===
// =================

const HOVER_COLOR : color::Rgba = color::Rgba::new(1.0,0.0,0.0,0.000_001);
/// Gap between action bar and selection menu
const MENU_GAP    : f32 = 5.0;



// ===============
// === Shapes  ===
// ===============

/// Invisible rectangular area that can be hovered.
mod hover_area {
    use super::*;

    ensogl::define_shape_system! {
        () {
            let width  : Var<Pixels> = "input_size.x".into();
            let height : Var<Pixels> = "input_size.y".into();
            let background           = Rect((&width,&height));
            let background           = background.fill(HOVER_COLOR);
            background.into()
        }
    }
}

/// Background of the action bar.
/// Note: needs to be an extra shape for sorting purposes.
mod background {
    use super::*;

    ensogl::define_shape_system! {
        (style:Style) {
            let width  : Var<Pixels> = "input_size.x".into();
            let height : Var<Pixels> = "input_size.y".into();
            let radius               = node::NODE_SHAPE_RADIUS.px() ;
            let background_rounded   = Rect((&width,&height)).corners_radius(&radius);
            let background_sharp     = Rect((&width,&height/2.0)).translate_y(-&height/4.0);
            let background           = background_rounded + background_sharp;
            let color_path           = ensogl_theme::vars::graph_editor::
                                           visualization::action_bar::background::color;
            let fill_color           = style.get_color(color_path);
            let background           = background.fill(color::Rgba::from(fill_color));
            background.into()
        }
    }
}



// ===========
// === Frp ===
// ===========

ensogl_text::define_endpoints! {
    Input {
        set_size                       (Vector2),
        show_icons                     (),
        hide_icons                     (),
        set_visualization_alternatives (Vec<visualization::Path>),
        set_selected_visualization     (Option<visualization::Path>),
    }
    Output {
        visualisation_selection  (Option<visualization::Path>),
        mouse_over               (),
        mouse_out                (),
    }
}



// ========================
// === Action Bar Model ===
// ========================

#[derive(Clone,CloneRef,Debug)]
struct Model {
    hover_area            : component::ShapeView<hover_area::Shape>,
    visualization_chooser : visualization_chooser::VisualizationChooser,
    background            : component::ShapeView<background::Shape>,
    display_object        : display::object::Instance,
    size                  : Rc<Cell<Vector2>>,
}

impl Model {
    fn new(app:&Application) -> Self {
        let scene                 = app.display.scene();
        let logger                = Logger::new("ActionBarModel");
        let background            = component::ShapeView::new(&logger,scene);
        let hover_area            = component::ShapeView::new(&logger,scene);
        let visualization_chooser = visualization_chooser::VisualizationChooser::new(&app);

        let display_object        = display::object::Instance::new(&logger);
        let size                  = default();

        Model{hover_area,visualization_chooser,display_object,size,background}.init()
    }

    fn init(self) -> Self {
        self.add_child(&self.hover_area);
        self.add_child(&self.visualization_chooser);

        // Remove default parent, then hide icons.
        self.show();
        self.hide();
        self
    }

    fn set_size(&self, size:Vector2) {
        self.size.set(size);
        self.hover_area.shape.size.set(size);
        self.background.shape.size.set(size);

        let height        = size.y;
        let width         = size.x;
        let right_padding = height / 2.0;
        self.visualization_chooser.frp.set_icon_size(Vector2::new(height,height));
        self.visualization_chooser.frp.set_icon_padding(Vector2::new(height/3.0,height/3.0));
        self.visualization_chooser.set_position_x((width/2.0) - right_padding);
        self.visualization_chooser.frp.set_menu_offset_y(MENU_GAP);
    }

    fn show(&self) {
        self.add_child(&self.visualization_chooser);
        self.add_child(&self.background);
    }

    fn hide(&self) {
        self.visualization_chooser.unset_parent();
        self.background.unset_parent();
        self.visualization_chooser.frp.hide_selection_menu.emit(());
    }
}

impl display::Object for Model {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}



// ==================
// === Action Bar ===
// ==================

/// UI for executing actions on a node. Consists of label indicating the active visualization
/// and a drop-down menu for selecting a new visualisation.
///
/// Layout
/// ------
/// ```text
///     / ---------------------------- \
///    |              <vis chooser> V   |
///    |--------------------------------|
///
/// ```
#[derive(Clone,CloneRef,Debug)]
pub struct ActionBar {
         model : Rc<Model>,
    pub frp    : Frp
}

impl ActionBar {

    /// Constructor.
    pub fn new(app:&Application) -> Self {
        let model = Rc::new(Model::new(app));
        let frp   = Frp::new_network();
        ActionBar {model,frp}.init_frp()
    }

    fn init_frp(self) -> Self {
        let network = &self.frp.network;
        let frp     = &self.frp;
        let model   = &self.model;

        let hover_area            = &model.hover_area.events;
        let visualization_chooser = &model.visualization_chooser.frp;

        frp::extend! { network


            // === Input Processing ===

            eval  frp.set_size ((size) model.set_size(*size));
            eval_ frp.hide_icons ( model.hide() );
            eval_ frp.show_icons ( model.show() );

            eval frp.input.set_visualization_alternatives ((entries){
                visualization_chooser.input.set_entries.emit(entries);
            });

            eval frp.input.set_selected_visualization ((vis){
                visualization_chooser.input.set_selected.emit(vis);
            });

            // === Mouse Interactions ===
            any_component_over <- any(&hover_area.mouse_over,&visualization_chooser.mouse_over);
            any_component_out  <- any(&hover_area.mouse_out,&visualization_chooser.mouse_out);

            is_over_true  <- any_component_over.constant(true);
            is_over_false <- any_component_out.constant(false);
            any_hovered   <- any(is_over_true,is_over_false);

            eval_ any_component_over (model.show());

            mouse_out_no_menu <- any_component_out.gate_not(&visualization_chooser.menu_visible);
            remote_click      <- visualization_chooser.menu_closed.gate_not(&any_hovered);
            hide              <- any(mouse_out_no_menu,remote_click);
            eval_ hide (model.hide());

            frp.source.visualisation_selection <+ visualization_chooser.chosen_entry;
        }
        self
    }
}

impl display::Object for ActionBar {
    fn display_object(&self) -> &display::object::Instance {
        &self.model.display_object()
    }
}
