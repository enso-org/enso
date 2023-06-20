//! This module contains implementation of a mouse manager and related utilities.

use crate::control::callback::traits::*;
use crate::prelude::*;

use crate::control::callback;
use crate::system::web;

use std::rc::Rc;
use web::Closure;
use web::JsCast;
use web::JsValue;


// ==============
// === Export ===
// ==============

pub mod event;

pub use crate::frp::io::mouse::*;
pub use event::*;



// ====================
// === MouseManager ===
// ====================

/// A utility which registers JavaScript handlers for mouse events and translates them to Rust
/// handlers. It is a top level mouse registry hub.
#[derive(Clone, CloneRef, Debug, Deref)]
pub struct MouseManager {
    #[deref]
    dispatchers: MouseManagerDispatchers,
    handles:     Rc<MouseManagerEventListenerHandles>,
    dom:         web::dom::WithKnownShape<web::EventTarget>,
}

/// A JavaScript callback closure for any mouse event.
pub type MouseEventJsClosure = Closure<dyn FnMut(JsValue)>;

macro_rules! define_bindings {
    (
        $target:ident,
        $global_target:ident,
        $( $js_event:ident :: $js_name:ident =>
             $name:ident ($event_target:ident, $event:ident)
        ),* $(,)?
    ) => {

        /// Keeps references to JavaScript closures in order to keep them alive.
        #[derive(Debug)]
        pub struct MouseManagerEventListenerHandles {
            $($name : web::EventListenerHandle),*
        }

        /// Set of dispatchers for various mouse events.
        #[derive(Clone,CloneRef,Debug,Default)]
        #[allow(missing_docs)]
        pub struct MouseManagerDispatchers {
            $(pub $name : callback::registry::Ref1<$event>),*
        }

        impl MouseManager {
            /// This is the constructor for mouse listeners which takes three arguments:
            ///
            /// 1. A DOM object to set resize observer on. This object should cover the entire screen.
            /// Since EnsoGL's scene origin is positioned in the left-bottom corner, the size of
            /// the DOM object is used to translate mouse coordinates from HTML to the EnsoGL space.
            ///
            /// 2. A DOM object to set the 'mousedown', 'mousewheel', and 'mouseleave' listeners on.
            /// In most cases, this should be the canvas used by EnsoGL. Alternatively, you can set
            /// this argument to the window object if you want EnsoGL to capture all events, even if
            /// it is placed behind another DOM element.
            ///
            /// 3. A DOM object to set the 'mouseup' and 'mousemove' listeners on. In most cases,
            /// this should be the window object. It is common for the element drag action to be
            /// initiated by a 'mousedown' event on one element and finished by a 'mouseup' event
            /// on another element. Handling these events globally covers such situations.
            pub fn new(
                dom: &web::dom::WithKnownShape<web::EventTarget>,
                $target: &web::EventTarget,
                $global_target: &web::EventTarget,
            ) -> Self {
                let dispatchers = MouseManagerDispatchers::default();
                let dom = dom.clone();
                $(
                    let shape = dom.shape.clone_ref();
                    let dispatcher = dispatchers.$name.clone_ref();
                    let closure : MouseEventJsClosure = Closure::new(move |event:JsValue| {
                        let _profiler = profiler::start_task!(
                            profiler::APP_LIFETIME,
                            concat!("mouse_", stringify!($name))
                        );
                        let shape = shape.value();
                        let event = event.unchecked_into::<web::$js_event>();
                        dispatcher.run_all(&event::$event::new(event,shape))
                    });
                    let js_name = stringify!($js_name);
                    let opt = event_listener_options();
                    let $name = web::add_event_listener_with_options
                        (&$event_target, js_name, closure, opt);
                )*
                let handles = Rc::new(MouseManagerEventListenerHandles {$($name),*});
                Self {dispatchers,handles,dom}
            }
        }
    };
}

/// Return options for addEventListener function. See also
/// https://developer.mozilla.org/en-US/docs/Web/API/EventTarget/addEventListener
fn event_listener_options() -> web::EventListenerHandleOptions {
    // 1. We listen for events in the bubbling phase. If we ever would like to listen in the capture
    // phase, it would need to be set to "bubbling" for the "mouseleave" and "mouseenter" events,
    // as they provide incorrect events for the "capture" phase.
    //
    // 2. We want to prevent default action on wheel events, thus listener cannot be passive.
    web::EventListenerHandleOptions::new().not_passive()
}

define_bindings! { target, gloabl_target,
    MouseEvent::mousedown  => on_down  (target, Down),
    MouseEvent::mouseup    => on_up    (gloabl_target, Up),
    MouseEvent::mousemove  => on_move  (gloabl_target, Move),
    MouseEvent::mouseleave => on_leave (target, Leave),
    MouseEvent::mouseenter => on_enter (target, Enter),
    WheelEvent::wheel      => on_wheel (target, Wheel),
}
