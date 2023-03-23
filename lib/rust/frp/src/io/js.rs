//! A module with Javascript IO bindings utilities.

use crate::prelude::*;
use enso_web::prelude::*;

use crate as frp;

use enso_profiler as profiler;
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



// ======================
// === JsEventHandler ===
// ======================

/// FRP wrapper for JS events. You can use the [`Self::handler`] method to generate a new event
/// handler closure. When event is fired, it will be emitted as [`Self::event`] stream. After the
/// event stops propagating, [`None`] will be emitted instead.
#[allow(missing_docs)]
#[derive(Clone, CloneRef, Debug)]
pub struct JsEvent {
    pub event:    frp::Stream<Option<enso_web::Event>>,
    event_source: frp::Source<Option<enso_web::Event>>,
    network:      frp::Network,
}

impl Default for JsEvent {
    fn default() -> Self {
        Self::new()
    }
}

impl JsEvent {
    /// Constructor
    pub fn new() -> Self {
        frp::new_network! { network
            event_source <- source();
        }
        let event = event_source.clone().into();
        Self { event, event_source, network }
    }

    /// Creates an event handler which wraps the event in an FRP network. The event will be emitted
    /// on the `event` output stream. After the event is emitted, `None` will be emitted.
    pub fn handler<Event>(&self, mut processing_fn: impl FnMut(&Event)) -> impl FnMut(&Event)
    where Event: AsRef<enso_web::Event> {
        let event_source = &self.event_source;
        f!([event_source] (event) {
            let _profiler = profiler::start_debug!(profiler::APP_LIFETIME, "event_handler");
            let js_event = event.as_ref().clone();
            event_source.emit(Some(js_event));
            processing_fn(event);
            event_source.emit(None);
        })
    }
}
