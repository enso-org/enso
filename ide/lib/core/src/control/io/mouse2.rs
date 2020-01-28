//! This module contains implementation of a mouse manager and related utilities.

use crate::prelude::*;

use crate::control::callback::*;

use std::cell::RefCell;
use std::rc::Rc;
use wasm_bindgen::JsCast;
use wasm_bindgen::JsValue;
use wasm_bindgen::prelude::Closure;
use web_sys::EventTarget;

pub use crate::control::io::mouse::event;
pub use crate::control::io::mouse::button;
pub use crate::control::io::mouse::button::*;



// =======================
// === EventDispatcher ===
// =======================

// TODO: Consider merging this implementation with crate::control::callback::* ones.

/// Shared event dispatcher.
#[derive(Debug,Derivative)]
#[derivative(Clone(bound=""))]
#[derivative(Default(bound=""))]
pub struct EventDispatcher<T> {
    rc: Rc<RefCell<XCallbackRegistry1<T>>>
}

impl<T> EventDispatcher<T> {
    /// Adds a new callback.
    pub fn add<F:XCallbackMut1Fn<T>>(&self, callback:F) -> CallbackHandle {
        self.rc.borrow_mut().add(callback)
    }

    /// Dispatches event to all callbacks.
    pub fn dispatch(&self, t:&T) {
        self.rc.borrow_mut().run_all(t);
    }
}

impl<T> CloneRef for EventDispatcher<T> {}



// ====================
// === MouseManager ===
// ====================

/// An utility which registers JavaScript handlers for mouse events and translates them to Rust
/// handlers. It is a top level mouse registry hub.
#[derive(Debug,Shrinkwrap)]
pub struct MouseManager {
    #[shrinkwrap(main_field)]
    dispatchers : MouseManagerDispatchers,
    closures    : MouseManagerClosures,
    dom         : EventTarget,
}

/// A JavaScript callback closure for any mouse event.
pub type MouseEventJsClosure = Closure<dyn Fn(JsValue)>;

macro_rules! define_bindings {
    ( $( $js_event:ident :: $js_name:ident => $name:ident ($target:ident) ),* $(,)? ) => {

        /// Keeps references to JavaScript closures in order to keep them alive.
        #[derive(Debug)]
        pub struct MouseManagerClosures {
            $($name : MouseEventJsClosure),*
        }

        /// Set of dispatchers for various mouse events.
        #[derive(Debug,Default)]
        #[allow(missing_docs)]
        pub struct MouseManagerDispatchers {
            $(pub $name : EventDispatcher<event::$target>),*
        }

        impl MouseManager {
            /// Constructor.
            pub fn new (dom:&EventTarget) -> Self {
                let dispatchers = MouseManagerDispatchers::default();
                let dom         = dom.clone();
                $(
                    let dispatcher = dispatchers.$name.clone_ref();
                    let $name : MouseEventJsClosure = Closure::wrap(Box::new(move |event:JsValue| {
                        let event = event.unchecked_into::<web_sys::$js_event>();
                        dispatcher.dispatch(&event.into())
                    }));
                    let js_closure = $name.as_ref().unchecked_ref();
                    let js_name    = stringify!($js_name);
                    let result     = dom.add_event_listener_with_callback(js_name,js_closure);
                    if let Err(e)  = result { panic!("Cannot add event listener. {:?}",e) }
                )*
                let closures = MouseManagerClosures {$($name),*};
                Self {dispatchers,closures,dom}
            }
        }
    };
}

define_bindings! {
    MouseEvent::mousedown  => on_down  (OnDown),
    MouseEvent::mouseup    => on_up    (OnUp),
    MouseEvent::mousemove  => on_move  (OnMove),
    WheelEvent::mousewheel => on_wheel (OnWheel),
}
