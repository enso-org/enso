//! A module with Javascript IO bindings utilities.
use crate::prelude::*;

use crate as frp;

use enso_logger::WarningLogger as Logger;
use enso_web as web;
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;



// ================
// === Listener ===
// ================

/// Callback for keyboard events.
pub trait KeyboardEventCallback = FnMut(&web_sys::KeyboardEvent) + 'static;

/// Callback for js events.
pub trait EventCallback = FnMut(&web_sys::Event) + 'static;

/// Keyboard event listener which calls the callback function as long it lives.
#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub struct Listener<Callback: ?Sized> {
    logger:     Logger,
    callback:   Closure<Callback>,
    element:    web::Window,
    event_type: String,
}

impl<Callback: ?Sized> Listener<Callback> {
    /// Constructor.
    pub fn new(logger: impl AnyLogger, event_type: impl Str, callback: Closure<Callback>) -> Self {
        let element = web::window();
        let js_function = callback.as_ref().unchecked_ref();
        let logger = Logger::new_sub(logger, "Listener");
        let event_type = event_type.as_ref();
        let options = event_listener_options();
        let result = element.add_event_listener_with_callback_and_add_event_listener_options(
            event_type,
            js_function,
            &options,
        );
        if let Err(err) = result {
            warning!(logger, "Couldn't add {event_type} event listener: {err:?}.");
        }
        let event_type = event_type.into();
        Self { logger, callback, element, event_type }
    }
}

impl Listener<dyn KeyboardEventCallback> {
    /// Creates a new key down event listener.
    pub fn new_key_down<F>(logger: impl AnyLogger, f: F) -> Self
    where F: KeyboardEventCallback {
        let boxed = Box::new(f);
        let closure = Closure::<dyn KeyboardEventCallback>::wrap(boxed);
        Self::new(logger, "keydown", closure)
    }

    /// Creates a new key up event listener.
    pub fn new_key_up<F>(logger: impl AnyLogger, f: F) -> Self
    where F: KeyboardEventCallback {
        let boxed = Box::new(f);
        let closure = Closure::<dyn KeyboardEventCallback>::wrap(boxed);
        Self::new(logger, "keyup", closure)
    }
}

impl Listener<dyn EventCallback> {
    /// Creates a blur event listener.
    pub fn new_blur<F>(logger: impl AnyLogger, f: F) -> Self
    where F: EventCallback {
        let boxed = Box::new(f);
        let closure = Closure::<dyn EventCallback>::wrap(boxed);
        Self::new(logger, "blur", closure)
    }
}

impl<Callback: ?Sized> Drop for Listener<Callback> {
    fn drop(&mut self) {
        let callback = self.callback.as_ref().unchecked_ref();
        if self.element.remove_event_listener_with_callback(&self.event_type, callback).is_err() {
            warning!(self.logger, "Couldn't remove event listener.");
        }
    }
}


// === Event Listener Options ===

/// Retrun options for addEventListener function. See also
/// https://developer.mozilla.org/en-US/docs/Web/API/EventTarget/addEventListener
fn event_listener_options() -> web_sys::AddEventListenerOptions {
    let mut options = web_sys::AddEventListenerOptions::new();
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
    pub event:       frp::Stream<Option<web_sys::Event>>,
    /// Emitting this signal while handling js event (`current_js_event` is Some) makes this event
    /// pass to the DOM elements. Otherwise the js event propagation will be stopped.
    pub pass_to_dom: frp::Source,
    event_source:    frp::Source<Option<web_sys::Event>>,
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
        Event: AsRef<web_sys::Event>,
    {
        let event_source = self.event_source.clone_ref();
        move |event| {
            let js_event = event.as_ref().clone();

            event_source.emit(Some(js_event));
            processing_fn(event);
            event_source.emit(None);
        }
    }


    // The bool is passed by reference to match the signatures expected by FRP eval.
    #[allow(clippy::trivially_copy_pass_by_ref)]
    fn on_event_change(
        new: &Option<web_sys::Event>,
        current: &Option<web_sys::Event>,
        is_passed: &bool,
    ) -> Option<web_sys::Event> {
        // Whenever the current js event change, we pass the processed one to the dom if someone
        // asked to.
        if let Some(e) = current {
            if !is_passed {
                // Prevent events from propagating ot user agent, so default browser actions will
                // not be triggered.
                e.prevent_default();
                e.stop_propagation();
            }
        }
        new.clone()
    }
}
