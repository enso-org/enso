//! This module contains implementation of a mouse manager and related utilities.

use crate::prelude::*;

pub mod event;

use crate::control::callback;
use crate::system::web;

use std::cell::RefCell;
use std::rc::Rc;
use web::Closure;
use web::JsCast;
use web::JsValue;

pub use crate::frp::io::mouse::*;
pub use event::*;



// =======================
// === EventDispatcher ===
// =======================

// TODO: Consider merging this implementation with crate::control::callback::* ones.

/// Shared event dispatcher.
#[derive(Debug, CloneRef, Derivative)]
#[derivative(Clone(bound = ""))]
#[derivative(Default(bound = ""))]
pub struct EventDispatcher<T> {
    rc: Rc<RefCell<callback::Registry1<T>>>,
}

impl<T> EventDispatcher<T> {
    /// Adds a new callback.
    pub fn add<F: FnMut(&T) + 'static>(&self, f: F) -> callback::Handle {
        self.rc.borrow_mut().add(f)
    }

    /// Dispatches event to all callbacks.
    pub fn dispatch(&self, t: &T) {
        self.rc.borrow_mut().run_all(t);
    }
}



// ====================
// === MouseManager ===
// ====================

/// An utility which registers JavaScript handlers for mouse events and translates them to Rust
/// handlers. It is a top level mouse registry hub.
#[derive(Clone, CloneRef, Debug, Shrinkwrap)]
pub struct MouseManager {
    #[shrinkwrap(main_field)]
    dispatchers: MouseManagerDispatchers,
    handles:     Rc<MouseManagerEventListenerHandles>,
    dom:         web::dom::WithKnownShape<web::EventTarget>,
}

/// A JavaScript callback closure for any mouse event.
pub type MouseEventJsClosure = Closure<dyn FnMut(JsValue)>;

macro_rules! define_bindings {
    ( $( $js_event:ident :: $js_name:ident => $name:ident ($target:ident) ),* $(,)? ) => {

        /// Keeps references to JavaScript closures in order to keep them alive.
        #[derive(Debug)]
        pub struct MouseManagerEventListenerHandles {
            $($name : web::EventListenerHandle),*
        }

        /// Set of dispatchers for various mouse events.
        #[derive(Clone,CloneRef,Debug,Default)]
        #[allow(missing_docs)]
        pub struct MouseManagerDispatchers {
            $(pub $name : EventDispatcher<$target>),*
        }

        impl MouseManager {
            /// Constructor.
            pub fn new(dom:&web::dom::WithKnownShape<web::EventTarget>) -> Self {
                Self::new_separated(dom,dom.deref())
            }

            /// Constructor which takes the exact element to set listener as a separate argument.
            ///
            /// Sometimes we want to listen for mouse event for element without ResizeObserver. Thus
            /// some html element may be passed as a size provider, and another one where we attach
            /// listeners (for example `body` and `window` respectively).
            pub fn new_separated
            (dom:&web::dom::WithKnownShape<web::EventTarget>,target:&web::EventTarget) -> Self {
                let dispatchers = MouseManagerDispatchers::default();
                let dom = dom.clone();
                $(
                    let shape = dom.shape.clone_ref();
                    let dispatcher = dispatchers.$name.clone_ref();
                    let closure : MouseEventJsClosure = Closure::new(move |event:JsValue| {
                        let shape = shape.value();
                        let event = event.unchecked_into::<web::$js_event>();
                        dispatcher.dispatch(&event::$target::new(event,shape))
                    });
                    let js_name = stringify!($js_name);
                    let opt = event_listener_options();
                    let $name = web::add_event_listener_with_options(&target,js_name,closure,&opt);
                )*
                let handles = Rc::new(MouseManagerEventListenerHandles {$($name),*});
                Self {dispatchers,handles,dom}
            }
        }
    };
}

/// Retrun options for addEventListener function. See also
/// https://developer.mozilla.org/en-US/docs/Web/API/EventTarget/addEventListener
fn event_listener_options() -> web::AddEventListenerOptions {
    let mut options = web::AddEventListenerOptions::new();
    // We listen for events in capture phase, so we can decide ourself if it should be passed
    // further.
    options.capture(true);
    // We want to prevent default action on wheel events, thus listener cannot be passive.
    options.passive(false);
    options
}

define_bindings! {
    MouseEvent::mousedown  => on_down  (OnDown),
    MouseEvent::mouseup    => on_up    (OnUp),
    MouseEvent::mousemove  => on_move  (OnMove),
    MouseEvent::mouseleave => on_leave (OnLeave),
    WheelEvent::wheel      => on_wheel (OnWheel),
}
