use crate::prelude::*;

use crate::data::opt_vec::OptVec;
use crate::dirty::Dirty;
use crate::display::workspace;
use crate::display::workspace::Workspace;
use crate::system::web;
use crate::system::web::fmt;
use crate::system::web::Logger;
use wasm_bindgen::prelude::Closure;

// =============
// === Types ===
// =============

type Callback = Rc<RefCell<Option<Closure<dyn FnMut()>>>>;

// =============
// === World ===
// =============

/// World is the top-level structure managing several instances of [Workspace].
/// It is responsible for updating the system on every animation frame.
#[derive(Debug)]
pub struct World {
    pub data:           Rc<RefCell<WorldData>>,
    pub on_every_frame: Callback,
}

impl Default for World {
    fn default() -> Self {
        let data = Rc::new(RefCell::new(WorldData::new()));
        let on_every_frame = Rc::new(RefCell::new(None));
        let data_local = data.clone();
        let on_every_frame_local = on_every_frame.clone();
        *on_every_frame.borrow_mut() = Some(Closure::wrap(Box::new(move || {
            let data_local = data_local.borrow();
            if data_local.started {
                data_local.refresh();
                Self::request_callback(&on_every_frame_local);
            }
        }) as Box<dyn FnMut()>));
        Self { data, on_every_frame }
    }
}

impl World {
    pub fn new() -> Self {
        default()
    }

    pub fn started(&self) -> bool {
        self.data.borrow().started
    }

    pub fn start(&self) {
        if !self.started() {
            self.data.borrow_mut().started = true;
            Self::request_callback(&self.on_every_frame);
        }
    }

    pub fn stop(&self) {
        self.data.borrow_mut().started = false;
    }

    pub fn add_workspace(&self, name: &str) -> workspace::ID {
        self.data.borrow_mut().add_workspace(name)
    }

    pub fn drop_workspace(&self, id: workspace::ID) {
        self.data.borrow_mut().drop_workspace(id)
    }

    pub fn refresh(&self) {
        self.data.borrow().refresh()
    }

    fn request_callback(callback: &Callback) {
        callback.borrow().as_ref().iter().for_each(|f| {
            web::request_animation_frame(f).unwrap();
        });
    }
}

// =================
// === WorldData ===
// =================

#[derive(Debug)]
pub struct WorldData {
    pub workspaces: OptVec<Workspace>,
    pub dirty:      Dirty,
    pub logger:     Logger,
    pub started:    bool,
}

impl Default for WorldData {
    fn default() -> Self {
        let workspaces = OptVec::new();
        let logger = Logger::new("world");
        let dirty = Dirty::new(&logger);
        let started = false;
        Self { workspaces, dirty, logger, started }
    }
}

impl WorldData {
    pub fn new() -> Self {
        default()
    }

    pub fn add_workspace(&mut self, name: &str) -> workspace::ID {
        let logger = &self.logger;
        let dirty = &self.dirty;
        self.workspaces.insert(|id| {
            logger.group(fmt!("Adding workspace {} ({}).", id, name), || {
                dirty.set();
                Workspace::new(id, name, logger, dirty).unwrap()
            })
        })
    }

    pub fn drop_workspace(&mut self, id: workspace::ID) {
        let logger = &self.logger;
        let item = self.workspaces.remove(id);
        match item {
            None => logger.warning("Trying to delete non-existing workspace."),
            Some(item) => logger.group(fmt!("Dropping workspace {}.", id), || {
                let _destruct_it_here = item;
            }),
        }
    }

    pub fn refresh(&self) {
        if self.dirty.is_set() {
            self.logger.group("Refresh.", || {
                self.dirty.unset();
                self.workspaces.iter().filter_map(|opt| opt.as_ref()).for_each(|w| w.refresh());
            });
        }
    }
}
