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

/// Handler of currently processed js event.
///
/// Managed js event by this structure will be NOT propagated further to DOM elements, unless the
/// `pass_to_dom` event will be emitted.
///
/// To make this class manage js event, you should wrap the closure passed as event listener using
/// `make_event_handler` function.
#[derive(Clone, CloneRef, Debug)]
pub struct CurrentJsEvent {
    /// Currently handled js event.
    pub event:       frp::Stream<Option<enso_web::Event>>,
    /// Emitting this signal while handling js event (`current_js_event` is Some) makes this event
    /// pass to the DOM elements. Otherwise the js event propagation will be stopped.
    pub pass_to_dom: frp::Source,
    event_source:    frp::Source<Option<enso_web::Event>>,
    network:         frp::Network,
}

impl Default for CurrentJsEvent {
    fn default() -> Self {
        Self::new()
    }
}

impl CurrentJsEvent {
    /// Constructor
    pub fn new() -> Self {
        frp::new_network! { network
            event_source           <- source();
            pass_to_dom            <- source();
            event_is_passed_to_dom <- any(...);
            event_is_passed_to_dom <+ pass_to_dom.constant(true);
            event                  <- any(...);

            new_event <- event_source.map3(&event,&event_is_passed_to_dom,Self::on_event_change);

            event_is_passed_to_dom <+ new_event.constant(false);
            event                  <+ new_event;
        }
        let event = event.into();
        Self { event, pass_to_dom, event_source, network }
    }

    /// A helper function for creating mouse event handlers.
    ///
    /// This wraps the `processing_fn` so before processing the current js event is
    /// set to the received js event, and after processing it is set back to `None`.
    pub fn make_event_handler<Event>(
        &self,
        mut processing_fn: impl FnMut(&Event),
    ) -> impl FnMut(&Event)
    where
        Event: AsRef<enso_web::Event>,
    {
        let event_source = self.event_source.clone_ref();
        move |event| {
            let _profiler = profiler::start_debug!(profiler::APP_LIFETIME, "event_handler");
            let js_event = event.as_ref().clone();
            event_source.emit(Some(js_event));
            processing_fn(event);
            event_source.emit(None);
        }
    }


    // The bool is passed by reference to match the signatures expected by FRP eval.
    #[allow(clippy::trivially_copy_pass_by_ref)]
    fn on_event_change(
        new: &Option<enso_web::Event>,
        current: &Option<enso_web::Event>,
        is_passed: &bool,
    ) -> Option<enso_web::Event> {
        // Whenever the current js event change, we pass the processed one to the dom if someone
        // asked to.
        if let Some(e) = current {
            if !is_passed {
                // Prevent events from propagating to user agent, so default browser actions will
                // not be triggered.
                e.prevent_default();
                e.stop_propagation();
            }
        }
        new.clone()
    }
}
