//! This module contains KeyboardListener

use crate::prelude::*;
use wasm_bindgen::prelude::*;

use crate::system::web::document;

use enso_frp::EventEmitterPoly;
use enso_frp::Key;
use enso_frp::Keyboard;
use js_sys::Function;
use wasm_bindgen::JsCast;
use web_sys::HtmlElement;
use web_sys::KeyboardEvent;



// ========================
// === KeyboardListener ===
// ========================

/// KeyboardCallback used in KeyboardListener.
pub trait KeyboardCallback = FnMut(KeyboardEvent) + 'static;

type KeyboardClosure = Closure<dyn KeyboardCallback>;

/// Keyboard event listener which calls the callback function as long it lives.
#[derive(Debug)]
pub struct KeyboardListener {
    logger           : Logger,
    callback_closure : KeyboardClosure,
    element          : HtmlElement,
    event_type       : String
}

impl KeyboardListener {
    fn new<F:KeyboardCallback>(logger:&Logger,event_type:impl Str, f:F) -> Self {
        let closure                 = Box::new(f);
        let callback_closure        = KeyboardClosure::wrap(closure);
        let element                 = document().unwrap().body().unwrap();
        let js_function : &Function = callback_closure.as_ref().unchecked_ref();
        let logger                  = logger.sub("KeyboardListener");
        if element.add_event_listener_with_callback(event_type.as_ref(),js_function).is_err() {
            logger.warning("Couldn't add event listener");
        }
        let event_type = event_type.into();
        Self {callback_closure,element,event_type,logger}
    }

    /// Creates a new key down event listener.
    pub fn new_key_down<F:KeyboardCallback>(logger:&Logger, f:F) -> Self {
        Self::new(logger,"keydown",f)
    }

    /// Creates a new key up event listener.
    pub fn new_key_up<F:KeyboardCallback>(logger:&Logger, f:F) -> Self {
        Self::new(logger,"keyup",f)
    }
}

impl Drop for KeyboardListener {
    fn drop(&mut self) {
        let callback : &Function = self.callback_closure.as_ref().unchecked_ref();
        if self.element.remove_event_listener_with_callback(&self.event_type, callback).is_err() {
            self.logger.warning("Couldn't remove event listener.");
        }
    }
}

/// A handle of listener emitting events on bound FRP graph.
#[derive(Debug)]
pub struct KeyboardFrpBindings {
    key_down : KeyboardListener,
    key_up   : KeyboardListener
}

impl KeyboardFrpBindings {
    /// Create new Keyboard and Frp bindings.
    pub fn new(logger:&Logger,keyboard:&Keyboard) -> Self {
        let key_down = KeyboardListener::new_key_down(logger,enclose!((keyboard.on_pressed => frp)
            move |event:KeyboardEvent| {
                if let Ok(key) = event.key().parse::<Key>() {
                    frp.event.emit(key);
                }
            }
        ));
        let key_up = KeyboardListener::new_key_up(logger,enclose!((keyboard.on_released => frp)
            move |event:KeyboardEvent| {
                if let Ok(key) = event.key().parse::<Key>() {
                    frp.event.emit(key);
                }
            }
        ));
        Self {key_down,key_up}
    }
}
