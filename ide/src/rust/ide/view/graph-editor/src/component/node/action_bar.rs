//! Definition of the `ActionBar` component for the `visualization::Container`.

mod icon;

use crate::prelude::*;

use enso_frp as frp;
use ensogl::application::Application;
use ensogl::display::style;
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
        set_size        (Vector2),
        show_icons      (),
        hide_icons      (),
        icon_visibility (bool)
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
// === Action Bar Model ===
// ========================

#[derive(Clone,CloneRef,Debug)]
struct Model {
    hover_area            : component::ShapeView<hover_rect::Shape>,
    icons                 : display::object::Instance,
    icon_freeze           : ToggleButton<icon::freeze::Shape>,
    icon_visibility       : ToggleButton<icon::visibility::Shape>,
    icon_skip             : ToggleButton<icon::skip::Shape>,
    display_object        : display::object::Instance,
    size                  : Rc<Cell<Vector2>>,
    all_shapes            : compound::events::MouseEvents,
}

impl Model {
    fn new(app:&Application) -> Self {
        let scene                 = app.display.scene();
        let logger                = Logger::new("ActionBarModel");
        let hover_area            = component::ShapeView::new(&logger,scene);
        let icon_freeze           = ToggleButton::new(&app);
        let icon_visibility       = ToggleButton::new(&app);
        let icon_skip             = ToggleButton::new(&app);
        let display_object        = display::object::Instance::new(&logger);
        let icons                 = display::object::Instance::new(&logger);
        let size                  = default();
        let all_shapes            = compound::events::MouseEvents::default();

        all_shapes.add_sub_shape(&hover_area);
        all_shapes.add_sub_shape(&icon_freeze.view());
        all_shapes.add_sub_shape(&icon_visibility.view());
        all_shapes.add_sub_shape(&icon_skip.view());


        Self{hover_area,icons,display_object,size,icon_freeze,icon_visibility,
             icon_skip,all_shapes}.init()
    }

    fn init(self) -> Self {
        self.add_child(&self.hover_area);
        self.add_child(&self.icons);
        self.icons.add_child(&self.icon_freeze);
        self.icons.add_child(&self.icon_skip);
        self.icons.add_child(&self.icon_visibility);

        // Default state is hidden.
        self.hide();
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

        self.place_button_in_slot(&self.icon_visibility, 0);
        self.place_button_in_slot(&self.icon_skip, 1);
        self.place_button_in_slot(&self.icon_freeze, 2);

        self.layout_hover_area_to_cover_buttons(3);

        // The appears smaller than the other ones, so this is an aesthetic adjustment.
        self.icon_visibility.set_scale_xy(Vector2::new(1.2,1.2));
    }

    fn set_icon_visibility(&self, visible:bool) {
        self.icon_freeze.frp.set_visibility(visible);
        self.icon_skip.frp.set_visibility(visible);
        self.icon_visibility.frp.set_visibility(visible);
    }

    fn show(&self) {
        self.set_icon_visibility(true);
    }

    fn hide(&self) {
        self.set_icon_visibility(false);
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
pub struct ActionBar {
         model : Rc<Model>,
    /// Public FRp api.
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

        let compound_shape = &model.all_shapes.frp;

        frp::extend! { network


            // === Input Processing ===

            eval  frp.set_size ((size)   model.set_size(*size));
            eval_ frp.hide_icons ( model.hide() );
            eval_ frp.show_icons ( model.show() );
            eval  frp.icon_visibility ([model](visible) {
                if *visible { model.show() } else { model.hide() }
            });


            // === Mouse Interactions ===

            eval_ compound_shape.mouse_over (model.show());
            eval_ compound_shape.mouse_out (model.hide());


            // === Icon Actions ===

            frp.source.action_skip      <+ model.icon_skip.frp.toggle_state;
            frp.source.action_freeze    <+ model.icon_freeze.frp.toggle_state;
            frp.source.action_visbility <+ model.icon_visibility.frp.toggle_state;
        }

        let icon_path:style::Path = theme::vars::graph_editor::node::actions::icon::color.into();
        let icon_color_source     = ColorSource::from(icon_path);
        model.icon_freeze.frp.set_base_color(icon_color_source.clone());
        model.icon_skip.frp.set_base_color(icon_color_source.clone());
        model.icon_visibility.frp.set_base_color(icon_color_source);

        frp.hide_icons.emit(());

        self
    }
}

impl display::Object for ActionBar {
    fn display_object(&self) -> &display::object::Instance {
        &self.model.display_object()
    }
}
