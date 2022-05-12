//! A module containing the garbage [`Collector`] structure.

use crate::prelude::*;



// ===============
// === Garbage ===
// ===============

/// The structure with collected garbage arranged in three buckets.
///
/// For more info, see the docs of [`Collector`].
#[derive(Debug, Default)]
struct Garbage {
    before_pixel_sync:   Vec<Box<dyn Any>>,
    before_pixel_update: Vec<Box<dyn Any>>,
    before_mouse_events: Vec<Box<dyn Any>>,
}


/// The Garbage Collector
///
/// This structure collects the EnsoGL components structures designated to being drop once the
/// component's hiding will be finished and all related events will be handled. Thanks to that,
/// the library's user don't need to handle "is dropped" events separately from "is hidden" events.
///
/// # Implementation
///
/// Each collected component FRP network and model must be thus kept long enough until the following
/// sequence will happen:
/// 1. We render the scene without the component;
/// 2. On the new rendered scene, we request the pixel value under the mouse cursor (see
/// [`crate::display::render::passes::PixelReadPass`]);
/// 3. The requested pixel value is loaded to CPU memory after creating fences in GPU;
/// 4. The mouse events are handled (see [`crate::display::scene::SceneData::handle_mouse_events`]).
/// This process may span across several frames, during which the "shape under cursor" property may
/// still contain the old value.
///
/// After that the component's FRP network will handle or propagate "object hidden" event, and can
/// be then freely dropped.
///
/// Thus, in the collector, we keep collected objects in three buckets, and when on of the 1-4
/// points above happens we move objects from appropriate bucket to next one (the points 1 and 2
/// happen at the same time, thus we have three buckets instead of four), or, if it is the last
/// bucket, we drop the objects.
#[derive(Clone, CloneRef, Debug, Default)]
pub struct Collector {
    garbage: Rc<RefCell<Garbage>>,
}

impl Collector {
    /// Create new, empty Collector.
    pub fn new() -> Self {
        default()
    }

    /// Collect object.
    ///
    /// The collector is designed to handle EnsoGL component's FRP networks and models, but any
    /// structure with static timeline may be put. See [`Collector`] docs for information when
    /// the object will be finally dropped.
    pub fn collect<T: 'static>(&self, object: T) {
        self.garbage.borrow_mut().before_pixel_sync.push(Box::new(object));
    }

    /// Pixel value requested (the points 1 and 2 in [`Collector`] docs).
    pub fn pixel_synced(&self) {
        let mut garbage = self.garbage.borrow_mut();
        let objects_being_moved = std::mem::take(&mut garbage.before_pixel_sync);
        garbage.before_pixel_update.extend(objects_being_moved);
    }

    /// Pixel value retrieved (the point 3 in [`Collector`] docs).
    pub fn pixel_updated(&self) {
        let mut garbage = self.garbage.borrow_mut();
        let objects_being_moved = std::mem::take(&mut garbage.before_pixel_update);
        garbage.before_mouse_events.extend(objects_being_moved);
    }

    /// Mouse events handled (the point 4 in [`Collector`] docs).
    pub fn mouse_events_handled(&self) {
        let mut garbage = self.garbage.borrow_mut();
        drop(std::mem::take(&mut garbage.before_mouse_events));
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn garbage_lifetime() {
        let collector = Collector::new();
        let garbage = Rc::new(());
        let garbage_weak = Rc::downgrade(&garbage);

        collector.collect(garbage);
        assert!(garbage_weak.upgrade().is_some());
        collector.pixel_synced();
        assert!(garbage_weak.upgrade().is_some());
        collector.pixel_updated();
        assert!(garbage_weak.upgrade().is_some());
        collector.mouse_events_handled();
        assert!(garbage_weak.upgrade().is_none());
    }
}
