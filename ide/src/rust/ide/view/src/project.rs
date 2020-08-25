//! The main view of single project opened in IDE.

use crate::prelude::*;

use crate::documentation;
use crate::graph_editor::GraphEditor;
use crate::graph_editor::component::visualization;

use enso_frp as frp;
use ensogl::application;
use ensogl::application::Application;
use ensogl::application::shortcut;
use ensogl::display;
use ensogl::display::shape::*;


#[derive(Clone,CloneRef,Debug)]
struct Model {
    logger         : Logger,
    display_object : display::object::Instance,
    graph_editor   : GraphEditor,
    documentation  : documentation::View,
    //TODO[ao] This view should contain also Text Editor; it should be moved here during refactoring
    // planned in task https://github.com/enso-org/ide/issues/597
}

impl Model {
    fn new(app:&Application) -> Self {
        let scene          = app.display.scene();
        let logger         = Logger::new("project::View");
        let display_object = display::object::Instance::new(&logger);
        let graph_editor   = app.new_view::<GraphEditor>();
        let documentation  = documentation::View::new(&scene);
        display_object.add_child(&graph_editor);
        display_object.add_child(&documentation);
        display_object.remove_child(&documentation);
        Self{logger,display_object,graph_editor,documentation}
    }

    fn set_documentation_visibility(&self, is_visible:bool) {
        if is_visible { self.display_object.remove_child(&self.documentation) }
        else          { self.display_object.add_child(&self.documentation)    }
    }

    fn is_documentation_visible(&self) -> bool {
        self.documentation.has_parent()
    }
}

// ===========
// === FRP ===
// ===========

ensogl::def_command_api! { Commands
    /// Documentation open press event. In case the event will be shortly followed by `release_documentation_view_visibility`, the documentation will be shown permanently. In other case, it will be disabled as soon as the `release_documentation_view_visibility` is emitted.
    press_documentation_view_visibility,
    /// Documentation open release event. See `press_documentation_view_visibility` to learn more.
    release_documentation_view_visibility,
}

impl application::command::CommandApi for View {
    fn command_api_docs() -> Vec<application::command::EndpointDocs> {
        Commands::command_api_docs()
    }

    fn command_api(&self) -> Vec<application::command::CommandEndpoint> {
        self.frp.input.command.command_api()
    }
}

ensogl_text::define_endpoints! {
    Commands { Commands }
    Input {
        // resize           (Vector2<f32>),
        set_documentation_data (visualization::Data),
    }
    Output {
        documentation_visible  (bool),
    }
}

/// The main view of single project opened in IDE.
#[allow(missing_docs)]
#[derive(Clone,CloneRef,Debug)]
pub struct View {
    model   : Model,
    pub frp : Frp,
}

impl View {
    /// Constructor.
    pub fn new(app:&Application) -> Self {
        let model = Model::new(app);
        let frp   = Frp::new_network();

        let network = &frp.network;

        frp::extend!{ network
            // === Documentation Set ===

            eval frp.set_documentation_data ((data) model.documentation.frp.send_data.emit(data));


            // === Documentation toggle ===

            let documentation_press_ev = frp.press_documentation_view_visibility.clone_ref();
            let documentation_release  = frp.release_documentation_view_visibility.clone_ref();
            documentation_pressed            <- bool(&documentation_release,&documentation_press_ev);
            documentation_was_pressed        <- documentation_pressed.previous();
            documentation_press              <- documentation_press_ev.gate_not(&documentation_was_pressed);
            documentation_press_on_off       <- documentation_press.map(f_!(model.is_documentation_visible()));
            frp.source.documentation_visible <+ documentation_press_on_off;


            // === OUTPUTS REBIND ===

            eval frp.documentation_visible ((vis) model.set_documentation_visibility(*vis));
        }

        let mock_documentation = visualization::MockDocGenerator::default();
        let data               = mock_documentation.generate_data();
        let content            = serde_json::to_value(data).unwrap_or_default();
        let data               = visualization::Data::from(content);
        frp.set_documentation_data(data);

        Self{model,frp}
    }

    /// Graph Editor View.
    pub fn graph(&self) -> &GraphEditor {
        &self.model.graph_editor
    }
}

impl display::Object for View {
    fn display_object(&self) -> &display::object::Instance { &self.model.display_object }
}

impl application::command::FrpNetworkProvider for View {
    fn network(&self) -> &frp::Network { &self.frp.network }
}

impl application::command::Provider for View {
    fn label() -> &'static str { "ListView" }
}

impl application::View for View {
    fn new(app:&Application) -> Self { View::new(app) }
}

impl application::shortcut::DefaultShortcutProvider for View {
    fn default_shortcuts() -> Vec<application::shortcut::Shortcut> {
        use frp::io::keyboard::Key;
        vec!
        [ Self::self_shortcut(shortcut::Action::press        (&[Key::Control,Key::Character("\\".into())],&[]) , "press_documentation_view_visibility")
        , Self::self_shortcut(shortcut::Action::release      (&[Key::Control,Key::Character("\\".into())],&[]) , "release_documentation_view_visibility")
        ]
    }
}
