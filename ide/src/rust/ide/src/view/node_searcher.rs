#![allow(missing_docs)] // FIXME
#![allow(unused_must_use)] // FIXME

use crate::prelude::*;

use ensogl::display;
use ensogl::display::shape::text::text_field::{TextField, TextFieldProperties};
use ensogl::traits::*;
use crate::controller::graph::{NewNodeInfo, LocationHint};
use ensogl::display::world::World;
use ensogl::display::shape::text::glyph::font::FontRegistry;

#[derive(Clone,Debug)]
pub struct NodeSearcher {
    display_object : display::object::Node,
    text_field     : TextField,
    controller     : controller::graph::Handle,
    logger         : Logger,
}

impl NodeSearcher {
    pub fn new
    (world:&World, logger:&Logger, controller:controller::graph::Handle, fonts:&mut FontRegistry)
    -> Self {
        let scene          = world.scene();
        let camera         = scene.camera();
        let screen         = camera.screen();
        let logger         = logger.sub("NodeSearcher");
        let display_object = display::object::Node::new(&logger);
        let properties     = TextFieldProperties {
            font           : fonts.get_or_load_embedded_font("DejaVuSansMono").unwrap(),
            text_size      : 16.0,
            base_color     : Vector4::new(0.0,0.0,0.0,1.0),
            size           : Vector2::new(screen.width,16.0),
        };
        let text_field     = TextField::new(world,properties);
        display_object.add_child(&text_field.display_object());
        let searcher = NodeSearcher{ display_object,text_field,controller,logger};
        searcher.initialize()
    }

    fn initialize(self) -> Self {
        let text_field_weak = self.text_field.downgrade();
        let controller      = self.controller.clone();
        self.text_field.set_text_edit_callback(move |change| {
            // If the text edit callback is called, the TextEdit must be still alive.
            let text_field    = text_field_weak.upgrade().unwrap();
            let field_content = text_field.get_content();
            let expression    = field_content.split('\n').next().unwrap();
            if change.inserted == "\n" {
                let metadata      = default();
                let id            = None;
                let location_hint = LocationHint::End;
                let expression    = expression.to_string();
                let new_node      = NewNodeInfo { expression,metadata,id,location_hint };
                controller.add_node(new_node);
                text_field.clear_content();
            } else {
                // Keep only one line.
                text_field.set_content(expression);
            }
        });
        self
    }
}

impl<'t> From<&'t NodeSearcher> for &'t display::object::Node {
    fn from(graph_editor:&'t NodeSearcher) -> Self {
        &graph_editor.display_object
    }
}
