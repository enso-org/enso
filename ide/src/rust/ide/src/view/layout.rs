//! This module contains implementation of ViewLayout with a single TextEditor temporarily
//! occupying half bottom of the screen as the default layout.

use crate::prelude::*;

use crate::view::temporary_panel::TemporaryPadding;
use crate::view::temporary_panel::TemporaryPanel;
use crate::view::text_editor::TextEditor;
use crate::view::node_editor::NodeEditor;
use crate::view::node_searcher::NodeSearcher;

use enso_callback as callback;
use enso_frp as frp;
use frp::io::keyboard;
use ensogl::application::Application;
use ensogl::display::shape::text::glyph::font;
use ensogl::display::traits::*;
use ensogl::display::world::World;
use nalgebra::Vector2;
use nalgebra::zero;
use std::cell::RefCell;
use std::rc::Rc;



// ==================
// === ViewLayout ===
// ==================

shared! { ViewLayout

/// Initial implementation of ViewLayout with a TextEditor and NodeEditor.
#[derive(Debug)]
pub struct ViewLayoutData {
    network                   : frp::Network,
    mouse_position_sampler    : frp::Sampler<Vector2<f32>>,
    text_editor               : TextEditor,
    node_editor               : NodeEditor,
    node_searcher             : NodeSearcher,
    size                      : Vector2<f32>,
    logger                    : Logger,
    node_searcher_show_action : Option<callback::Handle>
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
        let position    = Vector2::new(-screen_size.x / 2.0,0.0);
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
        let screen_size  = self.size;
        let position     = Vector3::new(50.0 - screen_size.x / 2.0, screen_size.y / 4.0, 0.0);
        self.node_editor.set_position(position);
    }

    fn update_node_searcher(&mut self) {
        let screen_size = self.size;
        let position    = Vector3::new(screen_size.x*2.0/3.0, screen_size.y - 10.0, 0.0);
        self.node_searcher.set_position(position);
    }
}

impl ViewLayout {
    /// Creates a new ViewLayout with a single TextEditor.
    pub async fn new
    ( logger                   : impl AnyLogger
    , kb_actions               : &mut keyboard::Actions
    , application              : &Application
    , text_controller          : controller::Text
    , graph_controller         : controller::ExecutedGraph
    , visualization_controller : controller::Visualization
    , fonts                    : &mut font::Registry
    ) -> FallibleResult<Self> {
        let logger        = Logger::sub(logger,"ViewLayout");
        let world         = &application.display;
        let text_editor   = TextEditor::new(&logger,world,text_controller,kb_actions,fonts);
        let graph         = graph_controller.graph.clone_ref();
        let node_editor   = NodeEditor::new
            (&logger,application,graph_controller,visualization_controller).await?;
        let node_searcher = NodeSearcher::new(world,&logger,node_editor.clone_ref(),graph,fonts);
        world.add_child(&text_editor.display_object());
        world.add_child(&node_editor);
        world.add_child(&node_searcher);
        let size  = zero();
        let scene = world.scene();
        let mouse = &scene.mouse.frp;
        frp::new_network! { network def mouse_position_sampler = mouse.position.sampler(); }
        let node_searcher_show_action = None;
        let data = ViewLayoutData{network,text_editor,node_editor,node_searcher,size,logger,
            node_searcher_show_action,mouse_position_sampler};
        let rc   = Rc::new(RefCell::new(data));
        Ok(Self {rc}.init(world,kb_actions))
    }

    fn init_keyboard(self, keyboard_actions:&mut keyboard::Actions) -> Self {
        // TODO[ao] add here some useful staff (quitting project for example)
        let layout                    = self.rc.clone_ref();
        let keys                      = &[keyboard::Key::Tab];
        let node_searcher_show_action = keyboard_actions.add_action(keys, move || {
            let mut layout             = layout.borrow_mut();
            let position               = *layout.mouse_position_sampler.value();
            //TODO[dg]: Test it when graph scene panning is working.
            let node_searcher_position = Vector3::new(position.x,position.y,0.0);
            layout.node_searcher.set_position(node_searcher_position);
            layout.node_searcher.show();
        });
        self.rc.borrow_mut().node_searcher_show_action = Some(node_searcher_show_action);
        self
    }

    fn init(self, world:&World, keyboard_actions:&mut keyboard::Actions) -> Self {
        let screen = world.scene().camera().screen();
        let size   = Vector2::new(screen.width,screen.height);
        self.set_size(size);
        self.init_keyboard(keyboard_actions)
    }
}
