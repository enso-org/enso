//! FIXME DEPRECATED DEPRECATED DEPRECATED DEPRECATED DEPRECATED DEPRECATED DEPRECATED DEPRECATED
//! This file is deprecated and is left only to support `keyboard_old` for the moment.

//! Keyboard listener and related utils.

use crate::prelude::*;
use wasm_bindgen::prelude::*;

use crate::system::web;

use enso_frp::io::keyboard_old::Key;
use enso_frp::io::keyboard_old::Keyboard;
use js_sys::Function;
use wasm_bindgen::JsCast;
use web_sys::HtmlElement;
use web_sys::KeyboardEvent;



// ================
// === Listener ===
// ================

/// Callback for keyboard events.
pub trait ListenerCallback = FnMut(KeyboardEvent) + 'static;

type ListenerClosure = Closure<dyn ListenerCallback>;

/// Keyboard event listener which calls the callback function as long it lives.
#[derive(Debug)]
pub struct Listener {
    logger     : Logger,
    callback   : ListenerClosure,
    element    : HtmlElement,
    event_type : String
}

impl Listener {
    fn new<F:ListenerCallback>(logger:impl AnyLogger,event_type:impl Str, f:F) -> Self {
        let closure     = Box::new(f);
        let callback    = ListenerClosure::wrap(closure);
        let element     = web::body();
        let js_function = callback.as_ref().unchecked_ref();
        let logger      = Logger::sub(logger,"Listener");
        let event_type  = event_type.as_ref();
        if element.add_event_listener_with_callback_and_bool(event_type,js_function,true).is_err() {
            logger.warning(|| format!("Couldn't add {} event listener.",event_type));
        }
        let event_type = event_type.into();
        Self {callback,element,event_type,logger}
    }

    /// Creates a new key down event listener.
    pub fn new_key_down<F:ListenerCallback>(logger:impl AnyLogger, f:F) -> Self {
        Self::new(logger,"keydown",f)
    }

    /// Creates a new key up event listener.
    pub fn new_key_up<F:ListenerCallback>(logger:impl AnyLogger, f:F) -> Self {
        Self::new(logger,"keyup",f)
    }
}

impl Drop for Listener {
    fn drop(&mut self) {
        let callback : &Function = self.callback.as_ref().unchecked_ref();
        if self.element.remove_event_listener_with_callback(&self.event_type, callback).is_err() {
            self.logger.warning("Couldn't remove event listener.");
        }
    }
}

/// A handle of listener emitting events on bound FRP graph.
#[derive(Debug)]
pub struct KeyboardFrpBindings {
    key_down : Listener,
    key_up   : Listener
}

impl KeyboardFrpBindings {
    /// Create new Keyboard and Frp bindings.
    pub fn new(logger:impl AnyLogger, keyboard:&Keyboard) -> Self {
        let key_down = Listener::new_key_down(&logger,enclose!((keyboard.on_pressed => frp)
            move |event:KeyboardEvent| {
                if let Ok(key) = event.key().parse::<Key>() {
                    frp.emit(key);
                }
            }
        ));
        let key_up = Listener::new_key_up(&logger,enclose!((keyboard.on_released => frp)
            move |event:KeyboardEvent| {
                if let Ok(key) = event.key().parse::<Key>() {
                    frp.emit(key);
                }
            }
        ));
        Self {key_down,key_up}
    }
}
