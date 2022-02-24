use crate::prelude::*;

#[derive(Debug, Default)]
pub struct Garbage {
    before_pixel_update: Vec<Box<dyn Any>>,
    before_mouse_events: Vec<Box<dyn Any>>,
}

#[derive(Clone, CloneRef, Debug, Default)]
pub struct Collector {
    garbage: Rc<RefCell<Garbage>>,
}

impl Collector {
    pub fn new() -> Self {
        default()
    }

    pub fn collect<T: 'static>(&self, object: T) {
        self.garbage.borrow_mut().before_pixel_update.push(Box::new(object));
    }

    pub fn pixel_updated(&self) {
        let mut garbage = self.garbage.borrow_mut();
        let objects_being_moved = std::mem::take(&mut garbage.before_pixel_update);
        if !objects_being_moved.is_empty() {
            DEBUG!("Pixel updated - {objects_being_moved.len()} objects moved.");
        }
        garbage.before_mouse_events.extend(objects_being_moved);
    }

    pub fn mouse_events_handled(&self) {
        let mut garbage = self.garbage.borrow_mut();
        if !garbage.before_mouse_events.is_empty() {
            DEBUG!("Removing");
        }
        std::mem::drop(std::mem::take(&mut garbage.before_mouse_events));
    }
}
