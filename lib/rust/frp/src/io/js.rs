//! A module with Javascript IO bindings utilities.

use crate::prelude::*;
use enso_web::prelude::*;

use enso_web as web;



// ================
// === Listener ===
// ================

/// Callback for keyboard events.
pub trait KeyboardEventCallback = FnMut(&enso_web::KeyboardEvent) + 'static;

/// Callback for js events.
pub trait EventCallback = FnMut(&enso_web::Event) + 'static;

/// Keyboard event listener which calls the callback function as long it lives.
#[derive(Debug)]
pub struct Listener {
    // The event listener will be removed when handle is dropped.
    _handle: web::EventListenerHandle,
}

impl Listener {
    /// Constructor.
    pub fn new<T: 'static>(
        target: &EventTarget,
        event_type: impl Str,
        callback: Closure<dyn FnMut(T)>,
    ) -> Self {
        let event_type = event_type.as_ref();
        let options = event_listener_options();
        let handle = web::add_event_listener_with_options(target, event_type, callback, &options);
        Self { _handle: handle }
    }
}

impl Listener {
    /// Creates a new key down event listener.
    pub fn new_key_down<F>(target: &EventTarget, f: F) -> Self
    where F: KeyboardEventCallback {
        let boxed = Box::new(f);
        let closure = Closure::<dyn KeyboardEventCallback>::wrap(boxed);
        Self::new(target, "keydown", closure)
    }

    /// Creates a new key up event listener.
    pub fn new_key_up<F>(target: &EventTarget, f: F) -> Self
    where F: KeyboardEventCallback {
        let boxed = Box::new(f);
        let closure = Closure::<dyn KeyboardEventCallback>::wrap(boxed);
        Self::new(target, "keyup", closure)
    }

    /// Creates a blur event listener.
    pub fn new_blur<F>(target: &EventTarget, f: F) -> Self
    where F: EventCallback {
        let boxed = Box::new(f);
        let closure = Closure::<dyn EventCallback>::wrap(boxed);
        Self::new(target, "blur", closure)
    }
}


// === Event Listener Options ===

/// Retrun options for addEventListener function. See also
/// https://developer.mozilla.org/en-US/docs/Web/API/EventTarget/addEventListener
fn event_listener_options() -> enso_web::AddEventListenerOptions {
    let mut options = enso_web::AddEventListenerOptions::new();
    // We want to prevent default action on wheel events, thus listener cannot be passive.
    options.passive(false);
    options
}
