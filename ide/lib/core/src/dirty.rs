use crate::prelude::*;

use crate::system::web::fmt;
use crate::system::web::Logger;

// ============================
// === TO BE REFACTORED OUT ===
// ============================

trait When {
    fn when<F: FnOnce() -> T, T>(&self, f: F);
}

impl When for bool {
    fn when<F: FnOnce() -> T, T>(&self, f: F) {
        if *self {
            f();
            ()
        }
    }
}

// =============
// === Dirty ===
// =============

#[derive(Clone, Debug)]
pub struct Dirty {
    data: Rc<RefCell<DirtyData>>,
}

impl Dirty {
    pub fn new(logger: &Logger) -> Self {
        let data = Rc::new(RefCell::new(DirtyData::new(logger)));
        Self { data }
    }

    pub fn new_child(&self, logger: &Logger) -> Self {
        let child = Self::new(logger);
        child.set_parent(Some(self.clone()));
        child
    }

    pub fn set_parent(&self, new_parent: Option<Dirty>) {
        self.data.borrow_mut().parent = new_parent;
    }

    pub fn is_set(&self) -> bool {
        self.data.borrow().is_set()
    }

    pub fn change(&self, new_state: bool) {
        self.data.borrow_mut().change(new_state)
    }

    pub fn set(&self) {
        self.change(true)
    }

    pub fn unset(&self) {
        self.change(false)
    }
}

// =================
// === DirtyData ===
// =================

#[derive(Clone, Debug)]
pub struct DirtyData {
    state:  bool,
    parent: Option<Dirty>,
    logger: Logger,
}

impl DirtyData {
    pub fn new(logger: &Logger) -> Self {
        Self { state: false, parent: None, logger: logger.clone() }
    }

    pub fn is_set(&self) -> bool {
        self.state
    }

    pub fn change(&mut self, new_state: bool) {
        if self.state != new_state {
            self.state = new_state;
            self.logger.group(fmt!("Setting dirty to {}.", new_state), || {
                new_state.when(|| self.parent.iter().for_each(|p| p.set()))
            });
        }
    }

    pub fn set(&mut self) {
        self.change(true)
    }

    pub fn unset(&mut self) {
        self.change(false)
    }
}
