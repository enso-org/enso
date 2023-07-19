//! This module provides a view for project's name that changes color when project is changed since
//! last snapshot.

use ensogl::display::shape::*;
use ensogl::prelude::*;

use enso_frp as frp;
use ensogl::application::Application;
use ensogl::application::View;
use ensogl::data::color;
use ensogl::display;
use ensogl::display::object::ObjectOps;
use ensogl::display::shape::compound::rectangle::Rectangle;
use ensogl::DEPRECATED_Animation;
use ensogl_component::text;
use ensogl_component::text::formatting::Size as TextSize;
use ensogl_hardcoded_theme::application::top_bar::project_name_with_environment_selector::project_name as theme;



// =================
// === Constants ===
// =================

// This is a default value for the project name when it is created. The project name should
// always be initialized externally for the current project. If this value is visible in the UI,
// it was not set to the correct project name due to some bug.
const UNINITIALIZED_PROJECT_NAME: &str = "Initializing project...";



// ===========
// === FRP ===
// ===========

ensogl::define_endpoints_2! {
    Input {
       /// Set the project name.
       set_name (String),
       /// Set whether the project was changed since the last snapshot save.
       set_project_changed(bool),
    }

    Output {
        name          (String),
        mouse_down    (),
        is_hovered    (bool),
    }
}



// ==================
// === Animations ===
// ==================

/// DEPRECATED_Animation handlers.
#[derive(Debug, Clone, CloneRef)]
pub struct Animations {
    color:    DEPRECATED_Animation<color::Rgba>,
    position: DEPRECATED_Animation<Vector3<f32>>,
}

impl Animations {
    /// Constructor.
    pub fn new(network: &frp::Network) -> Self {
        let color = DEPRECATED_Animation::new(network);
        let position = DEPRECATED_Animation::new(network);
        Self { color, position }
    }
}



// ========================
// === ProjectNameModel ===
// ========================

#[derive(Debug, Clone, CloneRef, display::Object)]
struct ProjectNameModel {
    display_object: display::object::Instance,
    overlay:        Rectangle,
    style:          StyleWatch,
    text_field:     text::Text,
    project_name:   Rc<RefCell<String>>,
}

impl ProjectNameModel {
    /// Constructor.
    fn new(app: &Application) -> Self {
        let scene = &app.display.default_scene;
        let display_object = display::object::Instance::new();
        // FIXME : StyleWatch is unsuitable here, as it was designed as an internal tool for shape
        // system (#795)
        let style = StyleWatch::new(&scene.style_sheet);
        let base_color = style.get_color(theme::color);
        let text_size = style.get_number(theme::text_size);
        let text_size: TextSize = text_size.into();
        let text_field = app.new_view::<text::Text>();
        text_field.set_property_default(base_color);
        text_field.set_property_default(text_size);
        text_field.set_single_line_mode(true);

        text_field.add_to_scene_layer(&scene.layers.panel_text);
        text_field.hover();

        let overlay = Rectangle::new().set_color(INVISIBLE_HOVER_COLOR).clone();
        scene.layers.panel.add(&overlay);

        let project_name = default();
        Self { display_object, overlay, style, text_field, project_name }.init()
    }

    fn init(self) -> Self {
        self.add_child(&self.text_field);
        self.add_child(&self.overlay);
        self.update_text_field_content(self.project_name.borrow().as_str());
        self
    }

    /// Update the visible content of the text field.
    fn update_text_field_content(&self, content: &str) {
        self.text_field.set_content(content);
    }

    fn update_size(&self, new_size: Vector2) {
        self.overlay.set_size(new_size);
        self.display_object.set_size(new_size);
        self.text_field.set_y(new_size.y);
    }

    fn set_color(&self, value: color::Rgba) {
        self.text_field.set_property_default(value);
    }

    fn set_position(&self, value: Vector3<f32>) {
        self.text_field.set_position(value);
    }

    /// Change the text field content and commit the given name.
    fn rename(&self, name: impl Str) {
        let name = name.into();
        debug!("Renaming: '{name}'.");
        self.update_text_field_content(&name);
    }
}



// ===================
// === ProjectName ===
// ===================

/// The view used for displaying and renaming it.
#[derive(Debug, Clone, CloneRef, Deref, display::Object)]
#[allow(missing_docs)]
pub struct ProjectName {
    #[display_object]
    model:   Rc<ProjectNameModel>,
    #[deref]
    pub frp: Frp,
}

impl ProjectName {
    /// Constructor.
    fn new(app: &Application) -> Self {
        let frp = Frp::new();
        let model = Rc::new(ProjectNameModel::new(app));
        let network = &frp.network;
        let scene = &app.display.default_scene;
        let text = &model.text_field.frp;
        // FIXME : StyleWatch is unsuitable here, as it was designed as an internal tool for shape
        // system (#795)
        let styles = StyleWatch::new(&scene.style_sheet);
        let saved_hover_color = styles.get_color(theme::color);
        let saved_deselected_color = styles.get_color(theme::color);
        let unsaved_hover_color = styles.get_color(theme::color_unsaved);
        let unsaved_deselected_color = styles.get_color(theme::color_unsaved);
        let animations = Animations::new(network);

        let input = &frp.private.input;
        let output = &frp.private.output;
        frp::extend! { network
            init <- source_();
            // === Mouse IO ===

            output.is_hovered <+ bool(
                &model.overlay.events_deprecated.mouse_out,
                &model.overlay.events_deprecated.mouse_over
            );
            output.mouse_down <+ model.overlay.events_deprecated.mouse_down_primary;

            text_color <- all3(
                &init,
                &output.is_hovered,
                &input.set_project_changed,
            ).map(move |(_, hovered, changed)| match (*hovered, *changed) {
                (false, true) => unsaved_deselected_color,
                (false, false) => saved_deselected_color,
                (true, true) => unsaved_hover_color,
                (true, false) => saved_hover_color,
            });
            eval text_color((&color) animations.color.set_target_value(color));


            // === Text Area ===

            size <- all(text.width, text.height).map(|(w, h)| Vector2(*w, *h));
            eval size([model](size) { model.update_size(*size); });


            // === Input Commands ===

            eval  frp.input.set_name((name) {model.rename(name)});
            output.name <+ frp.input.set_name;


            // === Animations ===

            eval animations.color.value((value) model.set_color(*value));
            eval animations.position.value((value) model.set_position(*value));
        }
        init.emit(());

        frp.input.set_name.emit(UNINITIALIZED_PROJECT_NAME.to_string());

        Self { model, frp }
    }
}

impl FrpNetworkProvider for ProjectName {
    fn network(&self) -> &frp::Network {
        &self.frp.network
    }
}

impl View for ProjectName {
    fn label() -> &'static str {
        "ProjectName"
    }

    fn new(app: &Application) -> Self {
        ProjectName::new(app)
    }
}
