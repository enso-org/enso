//! This module contains implementation of a mouse manager and related utilities.

use crate::prelude::*;

pub mod event;

use crate::control::callback;
use crate::system::web;

use std::cell::RefCell;
use std::rc::Rc;
use wasm_bindgen::prelude::Closure;
use wasm_bindgen::JsCast;
use wasm_bindgen::JsValue;

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
    closures:    Rc<MouseManagerClosures>,
    dom:         web::dom::WithKnownShape<web::EventTarget>,
}

/// A JavaScript callback closure for any mouse event.
pub type MouseEventJsClosure = Closure<dyn Fn(JsValue)>;

macro_rules! define_bindings {
    ( $( $js_event:ident :: $js_name:ident => $name:ident ($target:ident) ),* $(,)? ) => {

        /// Keeps references to JavaScript closures in order to keep them alive.
        #[derive(Debug)]
        pub struct MouseManagerClosures {
            target  : web::EventTarget,
            $($name : MouseEventJsClosure),*
        }

        impl Drop for MouseManagerClosures {
            fn drop(&mut self) {
            $(
                let target     = &self.target;
                let js_closure = self.$name.as_ref().unchecked_ref();
                let js_name    = stringify!($js_name);
                let result     = target.remove_event_listener_with_callback(js_name,js_closure);
                if let Err(e)  = result { panic!("Cannot add event listener. {:?}",e) }
            )*

            }
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
                let dom         = dom.clone();
                let target      = target.clone();
                $(
                    let shape      = dom.shape.clone_ref();
                    let dispatcher = dispatchers.$name.clone_ref();
                    let $name : MouseEventJsClosure = Closure::wrap(Box::new(move |event:JsValue| {
                        let shape = shape.value();
                        let event = event.unchecked_into::<web_sys::$js_event>();
                        dispatcher.dispatch(&event::$target::new(event,shape))
                    }));
                    let js_closure = $name.as_ref().unchecked_ref();
                    let js_name    = stringify!($js_name);
                    let options    = event_listener_options();
                    let result     =
                        target.add_event_listener_with_callback_and_add_event_listener_options
                        (js_name,js_closure,&options);
                    if let Err(e)  = result { panic!("Cannot add event listener. {:?}",e) }
                )*
                let closures = Rc::new(MouseManagerClosures {target,$($name),*});
                Self {dispatchers,closures,dom}
            }
        }
    };
}

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

define_bindings! {
    MouseEvent::mousedown  => on_down  (OnDown),
    MouseEvent::mouseup    => on_up    (OnUp),
    MouseEvent::mousemove  => on_move  (OnMove),
    MouseEvent::mouseleave => on_leave (OnLeave),
    WheelEvent::wheel      => on_wheel (OnWheel),
}

/// A handles of callbacks emitting events on bound FRP graph. See `callback::Handle`.
#[derive(Debug)]
pub struct MouseFrpCallbackHandles {
    on_move:  callback::Handle,
    on_down:  callback::Handle,
    on_up:    callback::Handle,
    on_wheel: callback::Handle,
}

// FIXME: This is obsolete. Use mouse bindings from scene instead.
/// Bind FRP graph to MouseManager.
pub fn bind_frp_to_mouse(frp: &Mouse, mouse_manager: &MouseManager) -> MouseFrpCallbackHandles {
    let dom_shape = mouse_manager.dom.clone_ref().shape();
    let on_move = enclose!((frp.position => frp) move |e:&OnMove| {
        let position = Vector2(e.client_x() as f32,e.client_y() as f32);
        let position = position - Vector2(dom_shape.width,dom_shape.height) / 2.0;
        frp.emit(position);
    });
    let on_down = enclose!((frp.down  => frp) move |_:&OnDown | frp.emit(Button0));
    let on_up = enclose!((frp.up    => frp) move |_:&OnUp   | frp.emit(Button0));
    let on_wheel = enclose!((frp.wheel => frp) move |_:&OnWheel| frp.emit(()));
    MouseFrpCallbackHandles {
        on_move:  mouse_manager.on_move.add(on_move),
        on_down:  mouse_manager.on_down.add(on_down),
        on_up:    mouse_manager.on_up.add(on_up),
        on_wheel: mouse_manager.on_wheel.add(on_wheel),
    }
}
