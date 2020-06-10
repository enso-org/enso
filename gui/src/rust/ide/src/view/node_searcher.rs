#![allow(missing_docs)] // FIXME
#![allow(unused_must_use)] // FIXME

use crate::prelude::*;

use crate::controller::graph::LocationHint;
use crate::controller::graph::NewNodeInfo;
use crate::model::module::NodeMetadata;
use crate::model::module::Position;
use crate::view::node_editor::NodeEditor;

use ensogl::data::color;
use ensogl::display::shape::text::glyph::font;
use ensogl::display::shape::text::text_field::TextField;
use ensogl::display::shape::text::text_field::TextFieldProperties;
use ensogl::display::world::World;
use ensogl::display;
use ensogl::traits::*;


#[derive(Clone,Debug,CloneRef)]
pub struct NodeSearcher {
    display_object : display::object::Instance,
    node_editor    : NodeEditor,
    text_field     : TextField,
    controller     : controller::graph::Handle,
    logger         : Logger,
}

impl NodeSearcher {
    pub fn new
    ( world       : &World
    , logger      : impl AnyLogger
    , node_editor : NodeEditor
    , controller  : controller::graph::Handle
    , fonts       : &mut font::Registry)
    -> Self {
        let scene          = world.scene();
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
        let text_field = TextField::new(world,properties);
        display_object.add_child(&text_field.display_object());
        let searcher = NodeSearcher{node_editor,display_object,text_field,controller,logger};
        searcher.initialize()
    }

    fn initialize(self) -> Self {
        let mut node_searcher = self.clone_ref();
        self.text_field.set_text_edit_callback(move |change| {
            // If the text edit callback is called, the TextEdit must be still alive.
            let field_content = node_searcher.text_field.get_content();
            let expression    = field_content.split('\n').next().unwrap();
            if change.inserted == "\n" {
                let position      = node_searcher.display_object.position();
                let position      = position - node_searcher.node_editor.position();
                let position      = Some(Position{vector:Vector2::new(position.x,position.y)});
                let metadata      = Some(NodeMetadata{position});
                let id            = None;
                let location_hint = LocationHint::End;
                let expression    = expression.to_string();
                let new_node      = NewNodeInfo { expression,metadata,id,location_hint };
                node_searcher.controller.add_node(new_node);
                node_searcher.hide();
            } else {
                // Keep only one line.
                node_searcher.text_field.set_content(expression);
            }
        });
        self
    }

    /// Show NodeSearcher if it is invisible.
    pub fn show(&mut self) {
        self.display_object.add_child(&self.text_field.display_object());
        self.text_field.clear_content();
        self.text_field.set_focus();
    }

    /// Hide NodeSearcher if it is visible.
    pub fn hide(&mut self) {
        self.text_field.clear_content();
        self.display_object.remove_child(&self.text_field.display_object());
    }
}

impl display::Object for NodeSearcher {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}
