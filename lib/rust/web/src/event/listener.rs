#![allow(missing_docs)]

use crate::closure::storage::ClosureFn;
use crate::closure::storage::OptionalFmMutClosure;

use crate::prelude::*;



// ============
// === Slot ===
// ============

/// A Slot stores a callback and manages its connection with JS `EventTarget`.
///
/// Both callback and the target can be set independently using `set_target` and `set_callback`.
/// Additionally, callback can be cleared at any point by `clear_callback`.
///
/// When both target and callback are set, slot ensures that the callback is registered as an
/// event listener in the target.
///
/// When changing target, `Slot` reattaches callback.
///
/// `Slot` owns callback and wraps it into JS closure. `Slot` also keeps reference to the target,
/// so it must not be leaked.
#[derive(Derivative)]
#[derivative(Debug(bound = "EventType::Interface: Debug"))]
pub struct Slot<EventType: crate::event::Type> {
    logger:     Logger,
    #[derivative(Debug = "ignore")]
    target:     Option<EventType::Target>,
    js_closure: OptionalFmMutClosure<EventType::Interface>,
}

impl<EventType: crate::event::Type> Slot<EventType> {
    /// Create a new `Slot`. As the initial target is provided, the listener will register once it
    /// gets a callback (see [[set_callback]]).
    pub fn new(target: &EventType::Target, logger: impl AnyLogger) -> Self {
        Self {
            logger:     Logger::new_sub(logger, EventType::NAME),
            target:     Some(target.clone()),
            js_closure: default(),
        }
    }

    /// Register the event listener if both target and callback are set.
    fn add_if_active(&mut self) {
        if let (Some(target), Some(function)) = (self.target.as_ref(), self.js_closure.js_ref()) {
            debug!(self.logger, "Attaching the callback.");
            EventType::add_listener(target, function)
        }
    }

    /// Unregister the event listener if both target and callback are set.
    fn remove_if_active(&mut self) {
        if let (Some(target), Some(function)) = (self.target.as_ref(), self.js_closure.js_ref()) {
            debug!(self.logger, "Detaching the callback.");
            EventType::remove_listener(target, function)
        }
    }

    /// Set a new target.
    ///
    /// If callback is set, it will be reattached as a listener to a newly set target.
    pub fn set_target(&mut self, target: &EventType::Target) {
        // Prevent spurious reattaching that could affect listeners order.
        if Some(target) != self.target.as_ref() {
            self.remove_if_active();
            self.target = Some(target.clone());
            self.add_if_active()
        }
    }

    /// Clear event target.
    ///
    /// If callback is set, it will be unregistered.
    pub fn clear_target(&mut self, target: &EventType::Target) {
        // Prevent spurious reattaching that could affect listeners order.
        if Some(target) != self.target.as_ref() {
            self.remove_if_active();
            self.target = None;
        }
    }

    /// Assign a new event callback closure and register it in the target.
    ///
    /// If the listener was registered with the previous closure, it will unregister first.
    ///
    /// Caveat: using this method will move the event listener to the end of the registered
    /// callbacks. This will affect the order of callback calls.
    pub fn set_callback(&mut self, f: impl ClosureFn<EventType::Interface>) {
        self.remove_if_active();
        self.js_closure.wrap(f);
        self.add_if_active()
    }

    /// Erase the callback.
    ///
    /// The stored closure will be dropped and event listener unregistered.
    pub fn clear_callback(&mut self) {
        self.remove_if_active();
        self.js_closure.clear();
    }

    /// Detach and attach the listener to the target.
    ///
    /// The purpose is to move this slot to the end of the listeners list.
    pub fn reattach(&mut self) {
        self.remove_if_active();
        self.add_if_active();
    }
}

/// Unregister listener on drop.
impl<EventType: crate::event::Type> Drop for Slot<EventType> {
    fn drop(&mut self) {
        self.remove_if_active();
    }
}
