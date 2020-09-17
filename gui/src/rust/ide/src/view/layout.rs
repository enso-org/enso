//! This module contains implementation of ViewLayout with a single TextEditor temporarily
//! occupying half bottom of the screen as the default layout.

use crate::prelude::*;

use crate::view::temporary_panel::TemporaryPadding;
use crate::view::temporary_panel::TemporaryPanel;
use crate::view::text_editor::TextEditor;
use crate::view::node_editor::NodeEditor;

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
    size                      : Vector2<f32>,
    logger                    : Logger,
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
}

impl ViewLayout {
    /// Creates a new ViewLayout with a single TextEditor.
    #[allow(clippy::too_many_arguments)]
    pub async fn new
    ( logger                   : impl AnyLogger
    , kb_actions               : &mut keyboard::Actions
    , application              : &Application
    , text_controller          : controller::Text
    , graph_controller         : controller::ExecutedGraph
    , visualization_controller : controller::Visualization
    , project                  : impl Into<model::Project> // Note [ViewLayout::new]
    , fonts                    : &mut font::Registry
    ) -> FallibleResult<Self> {
        let project       = project.into();
        let logger        = Logger::sub(logger,"ViewLayout");
        let world         = &application.display;
        let scene         = world.scene();
        let focus_manager = world.text_field_focus_manager();
        let text_editor   = TextEditor::new
            (&logger,scene,text_controller,kb_actions,fonts,focus_manager);
        let node_editor   = NodeEditor::new
            (&logger,application,graph_controller,project.clone_ref(),visualization_controller);
        let node_editor   = node_editor.await?;
        world.add_child(&node_editor);
        world.add_child(&text_editor.display_object());
        let size  = zero();
        let scene = world.scene();
        let mouse = &scene.mouse.frp;
        frp::new_network! { network def mouse_position_sampler = mouse.position.sampler(); }
        let data = ViewLayoutData{network,text_editor,node_editor,size,logger,
            mouse_position_sampler};
        let rc = Rc::new(RefCell::new(data));
        Ok(Self {rc}.init(world))
    }

    // Note [ViewLayout::new]
    // ======================
    // We cannot take directly `project:model::Project` as it seems that there's a bug in rustc
    // that cannot properly handle lifetimes when async method is taking dyn Trait as a parameter.
    // We believe it is the same issue as described here:
    // https://github.com/rust-lang/rust/issues/63033
    // Once it it resolved, the method signature should be simplified. For now we use impl Into<...>
    // syntax as a workaround to avoid triggering the bug.

    fn init(self, world:&World) -> Self {
        let screen = world.scene().camera().screen();
        let size   = Vector2::new(screen.width,screen.height);
        self.set_size(size);
        self
    }
}
