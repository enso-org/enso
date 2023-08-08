// === Non-Standard Linter Configuration ===
#![allow(missing_docs)]

use crate::prelude::*;

use crate::event::Type;



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
pub struct Slot<EventType: Type> {
    target:   Option<EventType::Target>,
    callback: Option<Rc<RefCell<dyn FnMut(EventType::Interface) + 'static>>>,
    handler:  Option<crate::CleanupHandle>,
}

impl<T: Type> Debug for Slot<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Slot").finish()
    }
}

impl<EventType: Type> Slot<EventType> {
    /// Create a new `Slot`. As the initial target is provided, the listener will register once it
    /// gets a callback (see [[set_callback]]).
    pub fn new(target: &EventType::Target) -> Self {
        Self { target: Some(target.clone()), callback: None, handler: None }
    }

    /// Register the event listener if both target and callback are set.
    fn add_if_active(&mut self) {
        if let (Some(target), Some(callback)) = (self.target.as_ref(), self.callback.as_ref()) {
            let callback = callback.clone();
            let handler =
                crate::add_event_listener(target.as_ref(), EventType::NAME, move |event| {
                    let mut callback = callback.borrow_mut();
                    callback(event.dyn_into().unwrap());
                });
            self.handler = Some(handler);
        }
    }

    /// Unregister the event listener if both target and callback are set.
    fn remove_if_active(&mut self) {
        self.handler.take();
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
    pub fn set_callback(&mut self, f: impl FnMut(EventType::Interface) + 'static) {
        self.remove_if_active();
        self.callback = Some(Rc::new(RefCell::new(f)));
        self.add_if_active()
    }

    /// Erase the callback.
    ///
    /// The stored closure will be dropped and event listener unregistered.
    pub fn clear_callback(&mut self) {
        self.remove_if_active();
        self.callback.take();
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
impl<EventType: Type> Drop for Slot<EventType> {
    fn drop(&mut self) {
        self.remove_if_active();
    }
}
