#![allow(missing_docs)]
// FIXME[WD]: THIS FILE IS IN WORK IN PROGRESS STATE. WILL BE FINISHED IN THE NEXT PR.

//! FRP keyboard bindings.

use crate::prelude::*;

use crate as frp;
use ensogl_system_web as web;
use web_sys::KeyboardEvent;
use wasm_bindgen::prelude::*;
use web_sys::HtmlElement;
use wasm_bindgen::JsCast;



// ===========
// === Key ===
// ===========

/// For reference, see the following links:
/// - https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/code/code_values
#[derive(Clone,Debug,Eq,Hash,PartialEq)]
#[allow(missing_docs)]
pub enum Key {
    Alt     (Side),
    Control (Side),
    Meta    (Side),
    Shift   (Side),
    Other   (String)
}

/// The key placement enum.
#[derive(Clone,Copy,Debug,Eq,Hash,PartialEq)]
#[allow(missing_docs)]
pub enum Side {
    Left,Right
}

impl Key {
    pub fn new(label:String, code:String) -> Self {
        let label_ref : &str = &label;
        let code_ref  : &str = &code;
        match (label_ref,code_ref) {
            ( "Alt"     , "AltLeft"      ) => Self::Alt     (Side::Left),
            ( "Alt"     , "AltRight"     ) => Self::Alt     (Side::Right),
            ( "Control" , "ControlLeft"  ) => Self::Control (Side::Left),
            ( "Control" , "ControlRight" ) => Self::Control (Side::Right),
            ( "Meta"    , "MetaLeft"     ) => Self::Meta    (Side::Left),
            ( "Meta"    , "MetaRight"    ) => Self::Meta    (Side::Right),
            ( "Shift"   , "ShiftLeft"    ) => Self::Shift   (Side::Left),
            ( "Shift"   , "ShiftRight"   ) => Self::Shift   (Side::Right),
            _                              => Self::Other   (label)
        }
    }

    pub fn is_meta_independent(&self) -> bool {
        match self {
            Self::Alt     (_) => true,
            Self::Control (_) => true,
            Self::Meta    (_) => true,
            Self::Shift   (_) => true,
            _                 => false
        }
    }
}

impl Default for Key {
    fn default() -> Self {
        Self::Other("".into())
    }
}



// =====================
// === KeyboardModel ===
// =====================

#[derive(Clone,CloneRef,Debug,Default)]
pub struct KeyboardModel {
    set : Rc<RefCell<HashSet<Key>>>,
}

impl KeyboardModel {
    pub fn new() -> Self {
        default()
    }

    pub fn is_meta_down(&self) -> bool {
        self.is_down(&Key::Meta(Side::Left)) || self.is_down(&Key::Meta(Side::Left))
    }

    pub fn is_down(&self, key:&Key) -> bool {
        self.set.borrow().contains(key)
    }

    pub fn set(&self, key:&Key) {
        self.set.borrow_mut().insert(key.clone());
    }

    pub fn unset(&self, key:&Key) {
        self.set.borrow_mut().remove(key);
    }

    #[allow(clippy::unnecessary_filter_map)] // Allows not cloning the element.
    pub fn release_meta_dependent(&self) -> Vec<Key> {
        let mut to_release = Vec::new();
        let new_set        = self.set.borrow_mut().drain().filter_map(|key| {
            if !key.is_meta_independent() {
                to_release.push(key);
                None
            } else {
                Some(key)
            }
        }).collect();
        *self.set.borrow_mut() = new_set;
        to_release
    }
}



// ======================
// === KeyboardSource ===
// ======================

#[derive(Clone,CloneRef,Debug)]
pub struct KeyboardSource {
    pub up   : frp::Source<Key>,
    pub down : frp::Source<Key>,
}

impl KeyboardSource {
    pub fn new(network:&frp::Network) -> Self {
        frp::extend! { network
            down <- source();
            up   <- source();
        }
        Self {up,down}
    }
}



// ================
// === Keyboard ===
// ================

/// Keyboard FRP bindings.
#[derive(Clone,CloneRef,Debug)]
#[allow(missing_docs)]
pub struct Keyboard {
    pub network : frp::Network,
    model       : KeyboardModel,
    pub source  : KeyboardSource,
    pub down    : frp::Stream<Key>,
    pub up      : frp::Stream<Key>,
}

impl Keyboard {
    pub fn new() -> Self {
        let network = frp::Network::new();
        let model   = KeyboardModel::default();
        let source  = KeyboardSource::new(&network);
        frp::extend! { network
            eval source.down ((key) model.set(key));
            eval source.up   ((key) model.unset(key));
            down         <- source.down.map(|t|t.clone());
            meta_down    <- source.down.map(f_!(model.is_meta_down()));
            meta_release <= source.down.gate(&meta_down).map(f_!(model.release_meta_dependent()));
            up           <- any(&source.up,&meta_release);
        }
        Keyboard {network,model,source,down,up}
    }
}

impl Default for Keyboard {
    fn default() -> Self {
        Self::new()
    }
}




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
        let callback = self.callback.as_ref().unchecked_ref();
        if self.element.remove_event_listener_with_callback(&self.event_type, callback).is_err() {
            self.logger.warning("Couldn't remove event listener.");
        }
    }
}

/// A handle of listener emitting events on bound FRP graph.
#[derive(Debug)]
pub struct DomBindings {
    key_down : Listener,
    key_up   : Listener
}

impl DomBindings {
    /// Create new Keyboard and Frp bindings.
    pub fn new(logger:impl AnyLogger, keyboard:&Keyboard) -> Self {
        let key_down = Listener::new_key_down(&logger,f!((event:KeyboardEvent)
            keyboard.source.down.emit(Key::new(event.key(),event.code()))
        ));
        let key_up = Listener::new_key_up(&logger,f!((event:KeyboardEvent)
            keyboard.source.up.emit(Key::new(event.key(),event.code()))
        ));
        Self {key_down,key_up}
    }
}
