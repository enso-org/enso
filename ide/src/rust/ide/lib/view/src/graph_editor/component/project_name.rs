//! This module provides a view for project's name which can be used to edit it.

use crate::prelude::*;

use enso_frp as frp;
use ensogl::data::color;
use ensogl::display;
use ensogl::display::Attribute;
use ensogl::display::Buffer;
use ensogl::display::object::ObjectOps;
use ensogl::display::scene::Scene;
use ensogl::display::shape::text::text_field::FocusManager;
use ensogl::display::shape::text::text_field::TextField;
use ensogl::display::shape::text::text_field::TextFieldProperties;
use ensogl::display::shape::*;
use ensogl::display::Sprite;
use ensogl::gui::component::Animation;
use ensogl::gui::component;
use logger::enabled::Logger;
use logger::AnyLogger;
use nalgebra::Vector2;
use ensogl::animation::linear_interpolation;



// =================
// === Constants ===
// =================

const TEXT_SIZE              : f32         = 12.0;
const TEXT_COLOR             : color::Rgba = color::Rgba::new(1.0, 1.0, 1.0, 0.7);
const TRANSPARENT_TEXT_COLOR : color::Rgba = color::Rgba::new(1.0, 1.0, 1.0, 0.4);

/// Project name used as a placeholder in ProjectName view when it's initialized.
pub const UNKNOWN_PROJECT_NAME:&str = "Unknown";



// ==================
// === Background ===
// ==================

mod background {
    use super::*;

    ensogl::define_shape_system! {
        (style:Style, selection:f32) {
            let bg_color = color::Rgba::new(0.0,0.0,0.0,0.000_001);
            Plane().fill(bg_color).into()
        }
    }
}



// =================
// === FrpInputs ===
// =================

#[derive(Debug,Clone,CloneRef)]
#[allow(missing_docs)]
pub struct FrpInputs {
    /// Set the project name.
    pub name : frp::Source<String>,
    /// Reset the project name to the one before editing.
    pub cancel_editing : frp::Source
}

impl FrpInputs {
    /// Create new FrpInputs.
    pub fn new(network:&frp::Network) -> Self {
        frp::extend! {network
            def cancel_editing = source();
            def name           = source();
        }
        Self{cancel_editing,name}
    }
}



// ==================
// === FrpOutputs ===
// ==================

#[derive(Debug,Clone,CloneRef)]
#[allow(missing_docs)]
pub struct FrpOutputs {
    pub name : frp::Source<String>
}

impl FrpOutputs {
    /// Create new FrpOutputs.
    pub fn new(network:&frp::Network) -> Self {
        frp::extend! {network
            def name = source();
        }
        Self{name}
    }
}



// ===========
// === Frp ===
// ===========

#[derive(Debug,Clone,CloneRef)]
#[allow(missing_docs)]
pub struct Frp {
    pub inputs  : FrpInputs,
    pub outputs : FrpOutputs,
    pub network : frp::Network,
}

impl Deref for Frp {
    type Target = FrpInputs;
    fn deref(&self) -> &Self::Target {
        &self.inputs
    }
}

impl Frp {
    /// Create new Frp.
    pub fn new() -> Self {
        let network = frp::Network::new();
        let inputs  = FrpInputs::new(&network);
        let outputs = FrpOutputs::new(&network);
        Self{network,inputs,outputs}
    }
}

impl Default for Frp {
    fn default() -> Self {
        Self::new()
    }
}



// ==================
// === Animations ===
// ==================

/// ProjectName's animations handlers.
#[derive(Debug,Clone,CloneRef)]
pub struct Animations {
    opacity  : Animation<f32>,
    position : Animation<Vector3<f32>>
}

impl Animations {
    /// Create new animations handlers.
    pub fn new(network:&frp::Network) -> Self {
        let opacity = Animation::new(&network);
        let position = Animation::new(&network);
        Self{opacity,position}
    }
}



// ========================
// === ProjectNameModel ===
// ========================

#[derive(Debug,Clone,CloneRef)]
#[allow(missing_docs)]
pub struct ProjectNameModel {
    logger         : Logger,
    animations     : Animations,
    display_object : display::object::Instance,
    view           : component::ShapeView<background::Shape>,
    text_field     : TextField,
    project_name   : Rc<RefCell<String>>,
    name_output    : frp::Source<String>,
}

impl ProjectNameModel {
    /// Create new ProjectNameModel.
    pub fn new<'t,S:Into<&'t Scene>>(scene:S,frp:&Frp,focus_manager:&FocusManager) -> Self {
        let scene                 = scene.into();
        let logger                = Logger::new("ProjectName");
        let display_object        = display::object::Instance::new(&logger);
        let font                  = scene.fonts.get_or_load_embedded_font("DejaVuSansMono").unwrap();
        let size                  = Vector2::new(scene.camera().screen().width,TEXT_SIZE);
        let base_color            = TRANSPARENT_TEXT_COLOR;
        let text_size             = TEXT_SIZE;
        let text_field_properties = TextFieldProperties{base_color,font,size,text_size};
        let text_field            = TextField::new(scene,text_field_properties,focus_manager);
        let view_logger           = Logger::sub(&logger,"view_logger");
        let view                  = component::ShapeView::<background::Shape>::new(&view_logger, scene);
        let project_name          = Rc::new(RefCell::new(UNKNOWN_PROJECT_NAME.to_string()));
        let name_output           = frp.outputs.name.clone();
        let animations            = Animations::new(&frp.network);
        Self{logger,view,display_object,text_field,project_name,name_output,animations}.init()
    }

    fn update_center_alignment(&self) {
        let width  = self.text_field.width_of_line(0);
        let offset = Vector3::new(-width/2.0,0.0,0.0);
        let height = self.text_field.line_height();
        self.view.shape.sprite.size.set(Vector2::new(width,height));
        self.view.set_position(Vector3::new(0.0,-height/2.0,0.0));
        self.animations.position.set_target_value(offset);
    }

    fn init(self) -> Self {
        //FIXME:Use add_child(&text_field) when replaced by TextField 2.0
        self.add_child(&self.text_field.display_object());
        self.add_child(&self.view);
        self.update_text_field_content();
        self
    }

    fn reset_name(&self) {
        info!(self.logger, "Resetting project name.");
        self.update_text_field_content();
    }

    fn update_text_field_content(&self) {
        self.text_field.set_content(&self.project_name.borrow());
        self.update_center_alignment();
    }

    fn set_opacity(&self, value:f32) {
        let base_color = linear_interpolation(TRANSPARENT_TEXT_COLOR, TEXT_COLOR, value);
        self.text_field.set_base_color(base_color);
    }

    fn set_position(&self, value:Vector3<f32>) {
        self.text_field.set_position(value);
    }

    fn rename(&self, name:impl Str) {
        let name = name.into();
        self.name_output.emit(&name);
        *self.project_name.borrow_mut() = name;
        self.update_text_field_content();
    }

    fn is_focused(&self) -> bool {
        self.text_field.is_focused()
    }
}

impl display::Object for ProjectNameModel {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}



// ===================
// === ProjectName ===
// ===================

/// The project name's view used for visualizing the project name and renaming it.
#[derive(Debug,Clone,CloneRef,Shrinkwrap)]
#[allow(missing_docs)]
pub struct ProjectName {
    #[shrinkwrap(main_field)]
    model   : Rc<ProjectNameModel>,
    pub frp : Frp
}

impl ProjectName {
    /// Create a new ProjectName view.
    pub fn new<'t,S:Into<&'t Scene>>(scene:S,focus_manager:&FocusManager) -> Self {
        let frp     = Frp::new();
        let model   = Rc::new(ProjectNameModel::new(scene,&frp,focus_manager));
        let network = &frp.network;
        frp::extend! { network
            eval_ model.view.events.mouse_over(model.animations.opacity.set_target_value(1.0));
            eval_ model.view.events.mouse_out({
                //TODO[dg]:Make use of TextField 2.0's frp for getting focus state changes.
                model.animations.opacity.set_target_value(model.is_focused() as i32 as f32);
            });
            eval_ frp.inputs.cancel_editing(model.reset_name());
            eval frp.inputs.name((name) {model.rename(name)});
        }


        // === Animations ===

        frp::extend! {network
            eval model.animations.opacity.value((value) model.set_opacity(*value));
            eval model.animations.position.value((value) model.set_position(*value));
        }

        Self{frp,model}.init()
    }

    fn init(self) -> Self {
        let project_name = Rc::downgrade(&self.model);
        //FIXME[dg]: This section to check newline and keep TextField in a single line is hacky
        // and should be removed once the new TextField is implemented.
        self.text_field.set_text_edit_callback(move |change| {
            if let Some(project_name) = project_name.upgrade() {
                // If the text edit callback is called, the TextEdit must be still alive.
                let field_content = project_name.text_field.get_content();
                let new_name      = field_content.replace("\n", "");
                if change.inserted == "\n" {
                    project_name.rename(&new_name);
                }
                // Keep only one line.
                project_name.text_field.set_content(&new_name);
                project_name.update_center_alignment();
            }
        });
        self
    }
}

impl display::Object for ProjectName {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}
