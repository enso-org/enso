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
    pub fn new<T: 'static>(event_type: impl Str, callback: Closure<dyn FnMut(T)>) -> Self {
        let window = &web::window;
        let event_type = event_type.as_ref();
        let options = event_listener_options();
        let handle = web::add_event_listener_with_options(window, event_type, callback, &options);
        Self { _handle: handle }
    }
}

impl Listener {
    /// Creates a new key down event listener.
    pub fn new_key_down<F>(f: F) -> Self
    where F: KeyboardEventCallback {
        let boxed = Box::new(f);
        let closure = Closure::<dyn KeyboardEventCallback>::wrap(boxed);
        Self::new("keydown", closure)
    }

    /// Creates a new key up event listener.
    pub fn new_key_up<F>(f: F) -> Self
    where F: KeyboardEventCallback {
        let boxed = Box::new(f);
        let closure = Closure::<dyn KeyboardEventCallback>::wrap(boxed);
        Self::new("keyup", closure)
    }

    /// Creates a blur event listener.
    pub fn new_blur<F>(f: F) -> Self
    where F: EventCallback {
        let boxed = Box::new(f);
        let closure = Closure::<dyn EventCallback>::wrap(boxed);
        Self::new("blur", closure)
    }
}


// === Event Listener Options ===

/// Retrun options for addEventListener function. See also
/// https://developer.mozilla.org/en-US/docs/Web/API/EventTarget/addEventListener
fn event_listener_options() -> enso_web::AddEventListenerOptions {
    let mut options = enso_web::AddEventListenerOptions::new();
    // We listen for events in capture phase, so we can decide ourself if it should be passed
    // further.
    options.capture(true);
    // We want to prevent default action on wheel events, thus listener cannot be passive.
    options.passive(false);
    options
}
