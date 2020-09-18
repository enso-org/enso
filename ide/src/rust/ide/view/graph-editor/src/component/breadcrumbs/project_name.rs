//! This module provides a view for project's name which can be used to edit it.

use crate::prelude::*;

use crate::component::breadcrumbs::TEXT_SIZE;
use crate::component::breadcrumbs::GLYPH_WIDTH;
use crate::component::breadcrumbs::VERTICAL_MARGIN;
use crate::component::breadcrumbs::breadcrumb;

use enso_frp as frp;
use ensogl::data::color;
use ensogl::display;
use ensogl::display::object::ObjectOps;
use ensogl::display::scene::Scene;
use ensogl::display::shape::text::text_field::FocusManager;
use ensogl::display::shape::text::text_field::TextField;
use ensogl::display::shape::text::text_field::TextFieldProperties;
use ensogl::display::shape::*;
use ensogl::gui::component::Animation;
use ensogl::gui::component;
use ensogl_theme as theme;
use logger::enabled::Logger;
use logger::AnyLogger;



// =================
// === Constants ===
// =================

/// Project name used as a placeholder in `ProjectName` view when it's initialized.
pub const UNKNOWN_PROJECT_NAME:&str = "Unknown";



// ==================
// === Background ===
// ==================

mod background {
    use super::*;

    ensogl::define_shape_system! {
        () {
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
    pub name:frp::Source<String>,
    /// Reset the project name to the one before editing.
    pub cancel_editing:frp::Source,
    /// Commit current project name.
    pub commit        : frp::Source,
    pub outside_press : frp::Source,
    pub select        : frp::Source,
    pub deselect      : frp::Source
}

impl FrpInputs {
    /// Constructor.
    pub fn new(network:&frp::Network) -> Self {
        frp::extend! {network
            cancel_editing <- source();
            name           <- source();
            commit         <- source();
            outside_press  <- source();
            select         <- source();
            deselect       <- source();
        }
        Self{cancel_editing,name,commit,outside_press,select,deselect}
    }
}



// ==================
// === FrpOutputs ===
// ==================

#[derive(Debug,Clone,CloneRef)]
#[allow(missing_docs)]
pub struct FrpOutputs {
    pub name       : frp::Source<String>,
    pub width      : frp::Source<f32>,
    pub mouse_down : frp::Any,
    pub edit_mode  : frp::Source<bool>,
    pub selected   : frp::Source<bool>
}

impl FrpOutputs {
    /// Constructor.
    pub fn new(network:&frp::Network) -> Self {
        frp::extend! {network
            name       <- source();
            width      <- source();
            mouse_down <- any_mut();
            edit_mode  <- source();
            selected   <- source();
        }
        Self{name,width,mouse_down,edit_mode,selected}
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
    /// Constructor.
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

/// Animation handlers.
#[derive(Debug,Clone,CloneRef)]
pub struct Animations {
    color    : Animation<Vector4<f32>>,
    position : Animation<Vector3<f32>>
}

impl Animations {
    /// Constructor.
    pub fn new(network:&frp::Network) -> Self {
        let color    = Animation::new(&network);
        let position = Animation::new(&network);
        Self{color,position}
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
    style          : StyleWatch,
    text_field     : TextField,
    project_name   : Rc<RefCell<String>>,
    outputs        : FrpOutputs
}

impl ProjectNameModel {
    /// Constructor.
    pub fn new<'t,S:Into<&'t Scene>>(scene:S,frp:&Frp,focus_manager:&FocusManager) -> Self {
        let scene                 = scene.into();
        let logger                = Logger::new("ProjectName");
        let display_object        = display::object::Instance::new(&logger);
        let font                  = scene.fonts.get_or_load_embedded_font("DejaVuSansMono").unwrap();
        let size                  = Vector2(scene.camera().screen().width,TEXT_SIZE);
        // FIXME : StyleWatch is unsuitable here, as it was designed as an internal tool for shape system (#795)
        let style                 = StyleWatch::new(&scene.style_sheet);
        let base_color            = style.get_color(theme::vars::graph_editor::breadcrumbs::transparent::color);
        let base_color            = color::Rgba::from(base_color);
        let text_size             = TEXT_SIZE;
        let text_field_properties = TextFieldProperties{base_color,font,size,text_size};
        let text_field            = TextField::new(scene,text_field_properties,focus_manager);
        let view_logger           = Logger::sub(&logger,"view_logger");
        let view                  = component::ShapeView::<background::Shape>::new(&view_logger, scene);
        let project_name          = Rc::new(RefCell::new(UNKNOWN_PROJECT_NAME.to_string()));
        let outputs               = frp.outputs.clone_ref();
        let animations            = Animations::new(&frp.network);
        Self{logger,view,style,display_object,text_field,project_name,animations,outputs}.init()
    }

    /// Get the width of the ProjectName view.
    pub fn width(&self) -> f32 {
        let content = self.text_field.get_content();
        let glyphs  = content.len();
        let width   = glyphs as f32 * GLYPH_WIDTH;
        width + breadcrumb::LEFT_MARGIN + breadcrumb::RIGHT_MARGIN + breadcrumb::PADDING * 2.0
    }

    fn update_alignment(&self) {
        let width       = self.width();
        let line_height = self.text_field.line_height();
        let height      = line_height+VERTICAL_MARGIN*2.0;
        let x_position  = breadcrumb::LEFT_MARGIN + breadcrumb::PADDING;
        let y_position  = -VERTICAL_MARGIN - breadcrumb::TOP_MARGIN - breadcrumb::PADDING;
        self.text_field.set_position(Vector3(x_position,y_position,0.0));
        self.view.shape.sprite.size.set(Vector2(width,height));
        self.view.set_position(Vector3(width,-height,0.0)/2.0);
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
        self.update_alignment();
        self.outputs.width.emit(self.width());
    }

    fn set_color(&self, value:Vector4<f32>) {
        self.text_field.set_base_color(color::Rgba::from(value));
    }

    fn set_position(&self, value:Vector3<f32>) {
        self.text_field.set_position(value);
    }

    fn rename(&self, name:impl Str) {
        let name = name.into();
        self.outputs.name.emit(&name);
        *self.project_name.borrow_mut() = name;
        self.update_text_field_content();
    }

    fn commit(&self) {
        let name = self.text_field.get_content();
        debug!(self.logger, "Committing name: '{name}'.");
        self.outputs.name.emit(&name);
        *self.project_name.borrow_mut() = name;
        self.outputs.edit_mode.emit(false);
    }

    fn select(&self) {
        let styles         = &self.style;
        let selected_color = styles.get_color(theme::vars::graph_editor::breadcrumbs::selected::color);
        let selected_color = color::Rgba::from(selected_color);

        self.animations.color.set_target_value(selected_color.into());
    }

    fn deselect(&self) {
        let styles           = &self.style;
        let deselected_color = styles.get_color(theme::vars::graph_editor::breadcrumbs::deselected::left::color);
        let deselected_color = color::Rgba::from(deselected_color);

        self.animations.color.set_target_value(deselected_color.into());
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

/// The view used for displaying and renaming it.
#[derive(Debug,Clone,CloneRef,Shrinkwrap)]
#[allow(missing_docs)]
pub struct ProjectName {
    #[shrinkwrap(main_field)]
    model   : Rc<ProjectNameModel>,
    pub frp : Frp
}

impl ProjectName {
    /// Constructor.
    pub fn new(scene:&Scene,focus_manager:&FocusManager) -> Self {
        let frp     = Frp::new();
        let model   = Rc::new(ProjectNameModel::new(scene,&frp,focus_manager));
        let network = &frp.network;

        // FIXME : StyleWatch is unsuitable here, as it was designed as an internal tool for shape system (#795)
        let styles            = StyleWatch::new(&scene.style_sheet);
        let hover_color       = styles.get_color(theme::vars::graph_editor::breadcrumbs::hover::color);
        let hover_color       = color::Rgba::from(hover_color);
        let deselected_color  = styles.get_color(theme::vars::graph_editor::breadcrumbs::deselected::left::color);
        let deselected_color  = color::Rgba::from(deselected_color);

        frp::extend! { network
            not_selected               <- frp.outputs.selected.map(|selected| !selected);
            mouse_over_if_not_selected <- model.view.events.mouse_over.gate(&not_selected);
            mouse_out_if_not_selected  <- model.view.events.mouse_out.gate(&not_selected);
            eval_ mouse_over_if_not_selected(
                model.animations.color.set_target_value(hover_color.into());
            );
            eval_ mouse_out_if_not_selected([model] {
                model.animations.color.set_target_value(deselected_color.into());
            });
            eval_ frp.select({
                model.outputs.selected.emit(true);
                model.select();
            });
            eval_ frp.deselect({
                model.outputs.selected.emit(false);
                model.deselect();
            });
            eval_ frp.inputs.cancel_editing(model.reset_name());
            eval  frp.inputs.name((name) {model.rename(name)});
            eval_ frp.inputs.commit(model.commit());
            frp.outputs.mouse_down <+ model.view.events.mouse_down;
            eval_ model.view.events.mouse_down(frp.outputs.edit_mode.emit(true));
            outside_press     <- any(&frp.outside_press,&frp.deselect);
            commit_if_changed <- outside_press.gate(&frp.outputs.edit_mode);
            eval_ commit_if_changed(frp.commit.emit(()));
        }


        // === Animations ===

        frp::extend! {network
            eval model.animations.color.value((value) model.set_color(*value));
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
                // Keep only one line.
                project_name.text_field.set_content(&new_name);
                project_name.outputs.width.emit(project_name.width());
                project_name.update_alignment();
                if change.inserted == "\n" {
                    project_name.commit();
                }
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
