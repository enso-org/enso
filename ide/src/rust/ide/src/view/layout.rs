//! This module contains implementation of ViewLayout with a single TextEditor temporarily
//! occupying half bottom of the screen as the default layout.

use crate::prelude::*;

use crate::view::temporary_panel::TemporaryPadding;
use crate::view::temporary_panel::TemporaryPanel;
use crate::view::text_editor::TextEditor;

use ensogl::display;
use ensogl::display::traits::*;
use ensogl::display::world::World;
use enso_frp::io::KeyboardActions;
use nalgebra::zero;
use nalgebra::Vector2;
use std::rc::Rc;
use std::cell::RefCell;
use crate::view::node_editor::NodeEditor;
use ensogl::display::shape::text::glyph::font::FontRegistry;
use crate::view::node_searcher::NodeSearcher;


// ==================
// === ViewLayout ===
// ==================

shared! { ViewLayout

/// Initial implementation of ViewLayout with a TextEditor and NodeEditor.
#[derive(Debug)]
pub struct ViewLayoutData {
    text_editor   : TextEditor,
    node_editor   : NodeEditor,
    node_searcher : NodeSearcher,
    size          : Vector2<f32>,
    logger        : Logger
}

impl {

    /// Sets ViewLayout size.
    pub fn set_size(&mut self, size:Vector2<f32>) {
        self.size = size;
        self.recalculate_layout();
    }
}}


// === Private Methods ===

impl ViewLayoutData {

    fn recalculate_layout(&mut self) {
        self.update_text_editor();
        self.update_graph_editor();
        self.update_node_searcher();
    }

    fn update_text_editor(&mut self) {
        let screen_size = self.size;
        let position    = Vector2::new(0.0,screen_size.y / 2.0);
        let size        = Vector2::new(screen_size.x,screen_size.y / 2.0);
        let padding     = TemporaryPadding {
            left   : 10.0,
            top    : 0.0,
            right  : 10.0,
            bottom : 0.0
        };
        self.text_editor.set_padding(padding);
        self.text_editor.set_size(size);
        TemporaryPanel::set_position(&mut self.text_editor,position);
    }

    fn update_graph_editor(&mut self) {
        let screen_size = self.size;
        let position    = Vector3::new(50.0, screen_size.y * 3.0 / 4.0, 0.0);

        let graph_object:&display::object::Node = (&self.node_editor).into();
        graph_object.set_position(position);
    }

    fn update_node_searcher(&mut self) {
        let screen_size = self.size;
        let position    = Vector3::new(screen_size.x*2.0/3.0, screen_size.y - 10.0, 0.0);

        let graph_object:&display::object::Node = (&self.node_searcher).into();
        graph_object.set_position(position);
    }
}

impl ViewLayout {
    /// Creates a new ViewLayout with a single TextEditor.
    pub fn new
    ( logger           : &Logger
    , kb_actions       : &mut KeyboardActions
    , world            : &World
    , text_controller  : controller::text::Handle
    , graph_controller : controller::graph::Handle
    , fonts            : &mut FontRegistry
    ) -> Self {
        let logger        = logger.sub("ViewLayout");
        let text_editor   = TextEditor::new(&logger,world,text_controller,kb_actions,fonts);
        let node_editor   = NodeEditor::new(&logger, world, graph_controller.clone());
        let node_searcher = NodeSearcher::new(world,&logger,graph_controller,fonts);
        world.add_child(&text_editor.display_object());
        world.add_child(&node_editor);
        world.add_child(&node_searcher);
        let size         = zero();
        let data         = ViewLayoutData {text_editor,node_editor,node_searcher,size,logger};
        let rc           = Rc::new(RefCell::new(data));
        Self {rc}.init(world,kb_actions)
    }

    fn init_keyboard(self, _keyboard_actions:&mut KeyboardActions) -> Self {
        // TODO[ao] add here some useful staff (quitting project for example)
        self
    }

    fn init(self, world:&World, keyboard_actions:&mut KeyboardActions) -> Self {
        let screen = world.scene().camera().screen();
        let size   = Vector2::new(screen.width,screen.height);
        self.set_size(size);
        self.init_keyboard(keyboard_actions)
    }
}
