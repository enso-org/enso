//! This module contains implementation of a mouse manager and related utilities.

use crate::prelude::*;

pub mod button;
pub mod event;

use crate::control::callback::*;
use crate::display::scene::Shape;

use enso_frp::EventEmitterPoly;
use enso_frp::Position;
use std::cell::RefCell;
use std::rc::Rc;
use wasm_bindgen::JsCast;
use wasm_bindgen::JsValue;
use wasm_bindgen::prelude::Closure;
use web_sys::EventTarget;

pub use button::*;



// =======================
// === EventDispatcher ===
// =======================

// TODO: Consider merging this implementation with crate::control::callback::* ones.

/// Shared event dispatcher.
#[derive(Debug,Derivative)]
#[derivative(Clone(bound=""))]
#[derivative(Default(bound=""))]
pub struct EventDispatcher<T> {
    rc: Rc<RefCell<CallbackRegistry1<T>>>
}

impl<T> EventDispatcher<T> {
    /// Adds a new callback.
    pub fn add<F:CallbackMut1Fn<T>>(&self, callback:F) -> CallbackHandle {
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
    MouseEvent::mouseleave => on_leave (OnLeave),
    WheelEvent::wheel      => on_wheel (OnWheel),
}

/// A handles of callbacks emitting events on bound FRP graph. See `CallbackHandle`.
#[derive(Debug)]
pub struct MouseFrpCallbackHandles {
    on_move  : CallbackHandle,
    on_down  : CallbackHandle,
    on_up    : CallbackHandle,
    on_leave : CallbackHandle,
    on_wheel : CallbackHandle
}

/// Bind FRP graph to MouseManager.
pub fn bind_frp_to_mouse(scene_shape:&Shape, frp:&enso_frp::Mouse, mouse_manager:&MouseManager)
-> MouseFrpCallbackHandles {
    let on_move = enclose!((frp.position.event => frp, scene_shape) move |e:&event::OnMove| {
        let height = scene_shape.screen_shape().height as i32;
        frp.emit(Position::new(e.client_x(),height - e.client_y()));
    });
    let on_down  = enclose!((frp.on_down.event  => frp) move |_:&event::OnDown | frp.emit(()));
    let on_up    = enclose!((frp.on_up.event    => frp) move |_:&event::OnUp   | frp.emit(()));
    let on_wheel = enclose!((frp.on_wheel.event => frp) move |_:&event::OnWheel| frp.emit(()));
    let on_leave = enclose!((frp.on_leave.event => frp) move |_:&event::OnLeave| frp.emit(()));
    MouseFrpCallbackHandles {
        on_move  : mouse_manager.on_move.add(on_move),
        on_down  : mouse_manager.on_down.add(on_down),
        on_up    : mouse_manager.on_up.add(on_up),
        on_leave : mouse_manager.on_leave.add(on_leave),
        on_wheel : mouse_manager.on_wheel.add(on_wheel)
    }
}
