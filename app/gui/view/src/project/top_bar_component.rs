use ensogl::prelude::*;

use ensogl::application::Application;
use ensogl::display;



#[derive(Clone, CloneRef, Debug, Deref)]
pub struct TopBarComponent<T: CloneRef> {
    root: display::object::Instance,
    #[deref]
    view: T,
}

impl<T: CloneRef + display::Object> TopBarComponent<T> {
    pub fn new(app: &Application, view: T) -> Self {
        let scene = &app.display.default_scene;
        let root = display::object::Instance::new();
        scene.layers.panel.add(&root);
        Self { root, view }
    }

    pub fn show(&self) {
        self.root.add_child(&self.view);
    }
}

impl<T: CloneRef> display::Object for TopBarComponent<T> {
    fn display_object(&self) -> &display::object::Instance {
        &self.root
    }
}
