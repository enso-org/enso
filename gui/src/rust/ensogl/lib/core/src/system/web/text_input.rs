// FIXME[WD] This file should be considered obsolete in favor of `clipboard.rs` implementation.
// FIXME[WD] This implementation steals focus in HTML and will probably cause big problems with
// FIXME[WD] visualization controls. Remove it as soon as new text editor is used everywhere.

//! This module contains Rust wrappers for keyboard event handling.
//!
//! These keyboard events are taken from created invisible textarea element in html document body.
//! We do it this way, because this is the only way for handling clipboard operations.
use crate::prelude::*;

use enso_frp::*;
use enso_frp::io::keyboard_old;
use enso_frp::io::keyboard_old::Keyboard;
use std::fmt::Debug;
use std::fmt::Formatter;
use std::fmt::Error;
use wasm_bindgen::prelude::*;
use web_sys::KeyboardEvent;



// ===========================
// === JavaScript Bindings ===
// ===========================

/// A module with functions imported from JavaScript.
mod js {
    use wasm_bindgen::prelude::*;
    use web_sys::KeyboardEvent;

    #[wasm_bindgen(module = "/src/system/web/text_input/text_input.js")]
    extern "C" {
        pub type TextInputHandlers;

        #[allow(unsafe_code)]
        #[wasm_bindgen(constructor)]
        pub fn new() -> TextInputHandlers;

        #[allow(unsafe_code)]
        #[wasm_bindgen(method)]
        pub fn set_copy_handler
        (this:&TextInputHandlers, handler:&Closure<dyn FnMut(bool) -> String>);

        #[allow(unsafe_code)]
        #[wasm_bindgen(method)]
        pub fn set_paste_handler(this:&TextInputHandlers, handler:&Closure<dyn FnMut(String)>);

        #[allow(unsafe_code)]
        #[wasm_bindgen(method)]
        pub fn set_window_defocus_handler(this:&TextInputHandlers, handler:&Closure<dyn FnMut()>);

        #[allow(unsafe_code)]
        #[wasm_bindgen(method)]
        pub fn set_event_handler
        (this:&TextInputHandlers, name:&str, handler:&Closure<dyn FnMut(KeyboardEvent)>);

        #[allow(unsafe_code)]
        #[wasm_bindgen(method)]
        pub fn stop_handling(this:&TextInputHandlers);
    }
}



// =======================
// === KeyboardBinding ===
// =======================

/// Copy handler takes bool which is true on cut operations, and returns the string should be
/// actually copied to clipboard.
pub trait CopyHandler = FnMut(bool) -> String + 'static;

/// The paste handler takes in its argument the text from clipboard to paste.
pub trait PasteHandler = FnMut(String) + 'static;

/// The window defocus handler is called each time browser window loses its focus.
pub trait WindowDefocusHandler = FnMut() + 'static;

/// Keyboard event handler takes event as an argument.
pub trait KeyboardEventHandler = FnMut(KeyboardEvent) + 'static;

/// The keyboard event bindings.
///
/// This structure wraps the javascript content handling events in a way describing in this module
/// docs.
pub struct KeyboardBinding {
    js_handlers      : js::TextInputHandlers,
    copy_handler     : Option<Closure<dyn CopyHandler>>,
    paste_handler    : Option<Closure<dyn PasteHandler>>,
    defocus_handler  : Option<Closure<dyn WindowDefocusHandler>>,
    key_down_handler : Option<Closure<dyn KeyboardEventHandler>>,
    key_up_handler   : Option<Closure<dyn KeyboardEventHandler>>,
}

impl KeyboardBinding {
    /// Add the textarea element to document body and returns KeyboardBinding structure which uses
    /// it, without any handlers.
    pub fn create() -> Self {
        KeyboardBinding {
            js_handlers      : js::TextInputHandlers::new(),
            copy_handler     : None,
            paste_handler    : None,
            defocus_handler  : None,
            key_down_handler : None,
            key_up_handler   : None
        }
    }

    /// Set copy handler.
    pub fn set_copy_handler<Handler:CopyHandler>(&mut self, handler:Handler) {
        let handler_js : Closure<dyn CopyHandler> = Closure::wrap(Box::new(handler));
        self.js_handlers.set_copy_handler(&handler_js);
        self.copy_handler = Some(handler_js);
    }

    /// Set paste handler.
    pub fn set_paste_handler<Handler:PasteHandler>(&mut self, handler:Handler) {
        let handler_js : Closure<dyn PasteHandler> = Closure::wrap(Box::new(handler));
        self.js_handlers.set_paste_handler(&handler_js);
        self.paste_handler = Some(handler_js);
    }

    /// Set window defocus handler.
    pub fn set_window_defocus_handler<Handler:WindowDefocusHandler>(&mut self, handler:Handler) {
        let handler_js : Closure<dyn WindowDefocusHandler> = Closure::wrap(Box::new(handler));
        self.js_handlers.set_window_defocus_handler(&handler_js);
        self.defocus_handler = Some(handler_js);
    }

    /// Set keydown handler.
    pub fn set_key_down_handler<Handler:KeyboardEventHandler>(&mut self, handler:Handler) {
        let handler_js : Closure<dyn KeyboardEventHandler> = Closure::wrap(Box::new(handler));
        self.js_handlers.set_event_handler("keydown", &handler_js);
        self.key_down_handler = Some(handler_js);
    }

    /// Set keyup handler.
    pub fn set_key_up_handler<Handler:KeyboardEventHandler>(&mut self, handler:Handler) {
        let handler_js : Closure<dyn KeyboardEventHandler> = Closure::wrap(Box::new(handler));
        self.js_handlers.set_event_handler("keyup", &handler_js);
        self.key_up_handler = Some(handler_js);
    }
}

impl Drop for KeyboardBinding {
    fn drop(&mut self) {
        self.js_handlers.stop_handling();
    }
}

impl Debug for KeyboardBinding {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        f.write_str("<KeyboardBindings>")
    }
}


// =======================
// === FRP Integration ===
// =======================

/// Bind this FRP graph to js events.
///
/// Until the returned `KeyboardBinding` structure lives, the js events will emit the proper
/// source events in this graph.
pub fn bind_frp_to_js_keyboard_actions(frp:&Keyboard, binding:&mut KeyboardBinding) {
    binding.set_key_down_handler(enclose!((frp.on_pressed => frp) move |event:KeyboardEvent| {
        if let Ok(key) = event.key().parse::<keyboard_old::Key>() {
            frp.emit(key);
        }
    }));
    binding.set_key_up_handler(enclose!((frp.on_released => frp) move |event:KeyboardEvent| {
        if let Ok(key) = event.key().parse::<keyboard_old::Key>() {
            frp.emit(key);
        }
    }));
    binding.set_window_defocus_handler(enclose!((frp.on_defocus => frp) move || {
        frp.emit(())
    }));
}
