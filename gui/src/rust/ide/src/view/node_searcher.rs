#![allow(missing_docs)] // FIXME
#![allow(unused_must_use)] // FIXME

use crate::prelude::*;

use crate::model::module::Position;
use crate::view::node_editor::NodeEditor;

use ensogl::data::color;
use ensogl::display;
use ensogl::display::Scene;
use ensogl::display::shape::text::glyph::font;
use ensogl::display::shape::text::text_field::FocusManager;
use ensogl::display::shape::text::text_field::TextField;
use ensogl::display::shape::text::text_field::TextFieldProperties;
use ensogl::traits::*;


#[derive(Clone,Debug,CloneRef)]
pub struct NodeSearcher {
    display_object : display::object::Instance,
    node_editor    : NodeEditor,
    project        : model::Project,
    controller     : Rc<CloneCell<Option<controller::Searcher>>>,
    text_field     : TextField,
    logger         : Logger,
}

impl NodeSearcher {
    pub fn new<'t,S:Into<&'t Scene>>
    ( scene         : S
    , logger        : impl AnyLogger
    , node_editor   : NodeEditor
    , fonts         : &mut font::Registry
    , focus_manager : &FocusManager
    , project       : model::Project
    ) -> Self {
        let scene          = scene.into();
        let camera         = scene.camera();
        let screen         = camera.screen();
        let logger         = Logger::sub(logger,"NodeSearcher");
        let display_object = display::object::Instance::new(&logger);
        let properties     = TextFieldProperties {
            font       : fonts.get_or_load_embedded_font("DejaVuSansMono").unwrap(),
            text_size  : 16.0,
            base_color : color::Rgba::new(1.0, 1.0, 1.0, 0.7),
            size       : Vector2::new(screen.width,16.0),
        };
        let text_field = TextField::new(scene,properties,focus_manager);
        let controller = default();
        let searcher   = NodeSearcher{node_editor,display_object,project,controller,text_field,
            logger};
        searcher.initialize()
    }

    fn initialize(self) -> Self {
        let mut node_searcher = self.clone_ref();
        self.text_field.set_text_edit_callback(move |change| {
            // If the text edit callback is called, the TextEdit must be still alive.
            let field_content = node_searcher.text_field.get_content();
            let expression    = field_content.split('\n').next().unwrap();
            if let Some(controller) = node_searcher.controller.get() {
                controller.set_input(expression.to_string());
                if change.inserted == "\n" {
                    match controller.commit_node() {
                        Ok(_)    => { node_searcher.hide(); }
                        Err(err) => {
                            error!(node_searcher.logger,"Error when creating node: {err}.")
                        }
                    }
                } else {
                    // Keep only one line.
                    node_searcher.text_field.set_content(expression);
                }
            }
        });
        self
    }

    /// Show NodeSearcher if it is invisible.
    pub fn show(&mut self) {
        if !self.is_shown() {
            let logger     = self.logger.clone_ref();
            let position   = self.position() - self.node_editor.position();
            let position   = Some(Position::new(position.x,position.y));
            let graph      = self.node_editor.graph.controller().clone_ref();
            let mode       = controller::searcher::Mode::NewNode {position};
            let controller = controller::Searcher::new_from_graph_controller(&logger,&self.project,
                graph,mode);
            let weak       = Rc::downgrade(&self.controller);
            match controller {
                Ok(controller) => {
                    executor::global::spawn(controller.subscribe().for_each(move |notification| {
                        let opt_controller = weak.upgrade().and_then(|controller| controller.get());
                        if let Some(controller) = opt_controller {
                            match notification {
                                controller::searcher::Notification::NewSuggestionList => {
                                    let list = controller.suggestions();
                                    info!(logger,"New list in Searcher: {list:?}");
                                }
                            }
                        }
                        futures::future::ready(())
                    }));
                    self.controller.set(Some(controller));
                    //FIXME:Use add_child(&text_field) when replaced by TextField 2.0
                    self.display_object.add_child(&self.text_field.display_object());
                    self.text_field.clear_content();
                    self.text_field.set_focus();
                },
                Err(err) => {
                    error!(logger,"Error while creating Searcher Controller: {err}");
                }
            }
        }
    }

    /// Hide NodeSearcher if it is visible.
    pub fn hide(&mut self) {
        if self.is_shown() {
            self.text_field.clear_content();
            self.controller.set(None);
            //FIXME:Use remove_child(&text_field) when replaced by TextField 2.0
            self.display_object.remove_child(&self.text_field.display_object());
        }
    }

    pub fn is_shown(&self) -> bool {
        self.text_field.display_object().has_parent()
    }
}

impl display::Object for NodeSearcher {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}
