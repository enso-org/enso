//! Definition of the `ActionBar` component for the `visualization::Container`.

mod icon;

use crate::prelude::*;

use enso_frp as frp;
use ensogl::application::Application;
use ensogl::display::shape::*;
use ensogl::display;
use ensogl::gui::component;
use ensogl_gui_components::toggle_button::ToggleButton;
use ensogl_gui_components::toggle_button::ColorableShape;
use ensogl_theme as theme;



// ==================
// === Constants  ===
// ==================

const BUTTON_PADDING : f32 = 0.5;
const BUTTON_OFFSET  : f32 = 0.5;



// ===============
// === Shapes  ===
// ===============

/// Invisible rectangular area that can be hovered.
mod hover_rect {
    use super::*;

    ensogl::define_shape_system! {
        (corner_radius:f32) {
            let width  : Var<Pixels> = "input_size.x".into();
            let height : Var<Pixels> = "input_size.y".into();
            let rect                 = Rect((&width,&height));
            let rect_rounded         = rect.corners_radius(corner_radius);
            let rect_filled          = rect_rounded.fill(constants::HOVER_COLOR);
            rect_filled.into()
        }
    }
}



// ===========
// === Frp ===
// ===========

ensogl::define_endpoints! {
    Input {
        set_size       (Vector2),
        set_visibility (bool),
        // show_icons     (),
        // hide_icons     (),
    }

    Output {
        mouse_over       (),
        mouse_out        (),
        action_visbility (bool),
        action_freeze    (bool),
        action_skip      (bool),
    }
}



// ========================
// === Action Bar Icons ===
// ========================

#[derive(Clone,CloneRef,Debug)]
struct Icons {
    display_object : display::object::Instance,
    freeze         : ToggleButton<icon::freeze::Shape>,
    visibility     : ToggleButton<icon::visibility::Shape>,
    skip           : ToggleButton<icon::skip::Shape>,
}

impl Icons {
    fn new(logger:impl AnyLogger, app:&Application) -> Self {
        let logger         = Logger::sub(logger,"Icons");
        let display_object = display::object::Instance::new(&logger);
        let freeze         = ToggleButton::new(&app);
        let visibility     = ToggleButton::new(&app);
        let skip           = ToggleButton::new(&app);
        display_object.add_child(&freeze);
        display_object.add_child(&visibility);
        display_object.add_child(&skip);
        Self {display_object,freeze,visibility,skip}
    }

    fn set_visibility(&self, visible:bool) {
        self.freeze.frp.set_visibility(visible);
        self.skip.frp.set_visibility(visible);
        self.visibility.frp.set_visibility(visible);
    }
}

impl display::Object for Icons {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}



// ========================
// === Action Bar Model ===
// ========================

#[derive(Clone,CloneRef,Debug)]
struct Model {
    display_object : display::object::Instance,
    hover_area     : component::ShapeView<hover_rect::Shape>,
    icons          : Icons,
    size           : Rc<Cell<Vector2>>,
    shapes         : compound::events::MouseEvents,
}

impl Model {
    fn new(logger:impl AnyLogger, app:&Application) -> Self {
        let scene          = app.display.scene();
        let logger         = Logger::sub(logger,"ActionBar");
        let display_object = display::object::Instance::new(&logger);
        let hover_area     = component::ShapeView::new(&logger,scene);
        let icons          = Icons::new(&logger,app);
        let shapes         = compound::events::MouseEvents::default();
        let size           = default();

        shapes.add_sub_shape(&hover_area);
        shapes.add_sub_shape(&icons.freeze.view());
        shapes.add_sub_shape(&icons.visibility.view());
        shapes.add_sub_shape(&icons.skip.view());

        Self{hover_area,display_object,size,icons,shapes}.init()
    }

    fn init(self) -> Self {
        self.add_child(&self.hover_area);
        self.add_child(&self.icons);
        self
    }

    fn place_button_in_slot<T:ColorableShape>(&self, button:&ToggleButton<T>, index:usize) {
        let icon_size = self.icon_size();
        let index     = index as f32;
        let padding   = BUTTON_PADDING;
        let offset    = BUTTON_OFFSET;
        button.mod_position(|p| p.x = ((1.0 + padding) * index + offset) * icon_size.x);
        button.frp.set_size(icon_size);
    }

    fn icon_size(&self) -> Vector2 {
        Vector2::new(self.size.get().y, self.size.get().y)
    }

    fn layout_hover_area_to_cover_buttons(&self, button_count:usize) {
        let button_count   = button_count as f32;
        let size           = self.size.get();
        let padding        = BUTTON_PADDING;
        let offset         = BUTTON_OFFSET;
        let hover_padding  = 1.0;
        let button_width   = self.icon_size().x;
        let hover_width    = button_width * (button_count + hover_padding + offset + padding);
        let hover_height   = button_width * 2.0;
        let hover_ara_size = Vector2::new(hover_width,hover_height);
        self.hover_area.shape.size.set(hover_ara_size);
        let center_offset  = -size.x / 2.0 + hover_ara_size.x / 2.0;
        let padding_offset = - 0.5 * hover_padding * button_width;
        self.hover_area.set_position_x(center_offset + padding_offset);
    }

    fn set_size(&self, size:Vector2) {
        self.size.set(size);
        self.icons.set_position_x(-size.x/2.0);

        self.place_button_in_slot(&self.icons.visibility , 0);
        self.place_button_in_slot(&self.icons.skip       , 1);
        self.place_button_in_slot(&self.icons.freeze     , 2);

        self.layout_hover_area_to_cover_buttons(3);

        // The appears smaller than the other ones, so this is an aesthetic adjustment.
        self.icons.visibility.set_scale_xy(Vector2::new(1.2,1.2));
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

/// UI for executing actions on a node.
///
/// Layout
/// ------
/// ```text
///    / ----------------------------- \
///    | <icon1> <icon2> <icon3>       |
///    \ ----------------------------- /
///
/// ```
#[derive(Clone,CloneRef,Debug)]
#[allow(missing_docs)]
pub struct ActionBar {
    pub frp : Frp,
    model   : Rc<Model>,
}

impl Deref for ActionBar {
    type Target = Frp;
    fn deref(&self) -> &Self::Target {
        &self.frp
    }
}

impl ActionBar {
    /// Constructor.
    pub fn new(logger:impl AnyLogger, app:&Application) -> Self {
        let model = Rc::new(Model::new(logger,app));
        let frp   = Frp::new();
        ActionBar{model,frp}.init_frp()
    }

    fn init_frp(self) -> Self {
        let network = &self.frp.network;
        let frp     = &self.frp;
        let model   = &self.model;

        frp::extend! { network

            // === Input Processing ===

            eval frp.set_size ((size) model.set_size(*size));
            eval frp.set_visibility ((t) model.icons.set_visibility(*t));


            // === Mouse Interactions ===

            visibility <- bool(&model.shapes.mouse_out,&model.shapes.mouse_over);
            eval visibility ((t) model.icons.set_visibility(*t));


            // === Icon Actions ===

            frp.source.action_skip      <+ model.icons.skip.state;
            frp.source.action_freeze    <+ model.icons.freeze.state;
            frp.source.action_visbility <+ model.icons.visibility.state;
        }

        let icon_color_source = ColorSource::from(theme::graph_editor::node::actions::icon);
        model.icons.freeze.frp.set_base_color(&icon_color_source);
        model.icons.skip.frp.set_base_color(&icon_color_source);
        model.icons.visibility.frp.set_base_color(&icon_color_source);
        self
    }
}

impl display::Object for ActionBar {
    fn display_object(&self) -> &display::object::Instance {
        &self.model.display_object()
    }
}
