//! This module contains ProjectView, the main view, responsible for managing TextEditor and
//! GraphEditor.

use crate::prelude::*;

use crate::view::layout::ViewLayout;

use file_manager_client::Path;
use ensogl::control::callback::CallbackHandle;
use ensogl::control::io::keyboard::listener::KeyboardFrpBindings;
use ensogl::display::world::WorldData;
use ensogl::display::world::World;
use ensogl::system::web;
use enso_frp::Keyboard;
use enso_frp::KeyboardActions;
use nalgebra::Vector2;
use shapely::shared;



// =================
// === Constants ===
// =================

/// Path of the file that is initially opened in project view.
///
/// TODO [mwu] Path of the file that will be initially opened in the text editor.
///      Provisionally the Project View is hardcoded to open with a single text
///      editor and it will be connected with a file under this path.
///      To be replaced with better mechanism once we decide how to describe
///      default initial layout for the project.
const INITIAL_FILE_PATH:&str = "Main.enso";



// ===================
// === ProjectView ===
// ===================

shared! { ProjectView

    /// ProjectView is the main view of the project, holding instances of TextEditor and
    /// GraphEditor.
    #[derive(Debug)]
    pub struct ProjectViewData {
        world             : World,
        layout            : ViewLayout,
        resize_callback   : Option<CallbackHandle>,
        controller        : controller::Project,
        keyboard          : Keyboard,
        keyboard_bindings : KeyboardFrpBindings,
        keyboard_actions  : KeyboardActions
    }

    impl {
        /// Set view size.
        pub fn set_size(&mut self, size:Vector2<f32>) {
            self.layout.set_size(size);
        }
    }
}

impl ProjectView {
    /// Create a new ProjectView.
    pub async fn new(logger:&Logger, controller:controller::Project)
    -> FallibleResult<Self> {
        let path                 = Path::new(INITIAL_FILE_PATH);
        // This touch is to ensure, that our hardcoded module exists (so we don't require
        // additional user/tester action to run IDE. It will be removed once we will support opening
        // any module file.
        controller.file_manager.touch(path.clone()).await?;
        let text_controller      = controller.text_controller(path).await?;
        let world                = WorldData::new(&web::get_html_element_by_id("root").unwrap());
        let logger               = logger.sub("ProjectView");
        let keyboard             = Keyboard::default();
        let keyboard_bindings    = KeyboardFrpBindings::new(&logger,&keyboard);
        let mut keyboard_actions = KeyboardActions::new(&keyboard);
        let resize_callback      = None;
        let layout               = ViewLayout::new
            (&logger,&mut keyboard_actions,&world,text_controller);
        let data = ProjectViewData
            {world,layout,resize_callback,controller,keyboard,keyboard_bindings,keyboard_actions};
        Ok(Self::new_from_data(data).init())
    }

    fn init(self) -> Self {
        let scene = self.with_borrowed(|data| data.world.scene());
        let weak  = self.downgrade();
        let resize_callback = scene.camera().add_screen_update_callback(
            move |size:&Vector2<f32>| {
                if let Some(this) = weak.upgrade() {
                    this.set_size(*size)
                }
            }
        );
        self.with_borrowed(move |data| data.resize_callback = Some(resize_callback));
        self
    }

    /// Forgets ProjectView, so it won't get dropped when it goes out of scope.
    pub fn forget(self) {
        std::mem::forget(self)
    }
}
