//! This module contains ProjectView, the main view, responsible for managing TextEditor and
//! GraphEditor.

use crate::prelude::*;

use super::layout::ViewLayout;

use basegl::display::world::WorldData;
use basegl::display::world::World;
use basegl::system::web;
use basegl::control::callback::CallbackHandle;

use nalgebra::Vector2;
use shapely::shared;



// ===================
// === ProjectView ===
// ===================

shared! { ProjectView

    /// ProjectView is the main view of the project, holding instances of TextEditor and
    /// GraphEditor.
    #[derive(Debug)]
    pub struct ProjectViewData {
        world           : World,
        layout          : ViewLayout,
        resize_callback: Option<CallbackHandle>
    }

    impl {
        /// Set view size.
        pub fn set_size(&mut self, size:Vector2<f32>) {
            self.layout.set_size(size);
        }
    }
}

impl Default for ProjectViewData {
    fn default() -> Self {
        let world           = WorldData::new(&web::body());
        let layout          = ViewLayout::default(&world);
        let resize_callback = None;
        ProjectViewData{world,layout,resize_callback}
    }
}

impl ProjectView {
    /// Create new ProjectView.
    pub fn new() -> Self {
        let data = default();
        Self{rc:data}.init()
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

impl Default for ProjectView {
    fn default() -> Self {
        Self::new()
    }
}
