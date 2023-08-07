//! This module contains implementation of a mouse manager and related utilities.

use crate::prelude::*;

use crate::system::web;

use std::rc::Rc;


// ==============
// === Export ===
// ==============

pub mod event;

pub use crate::frp::io::mouse::*;
pub use event::*;



// ==========================
// === DOM Event Bindings ===
// ==========================

macro_rules! define_bindings {
    (
        $target:ident,
        $global_target:ident,
        $( $js_event:ident :: $js_name:ident =>
             $name:ident ($event_target:ident, $event:ident)
        ),* $(,)?
    ) => {
        /// Set of dispatchers for various mouse events.
        #[derive(Clone, CloneRef, Debug, Default)]
        #[allow(missing_docs)]
        pub struct EventDispatchers {
            $(pub $name : $crate::control::callback::registry::Ref1<$event>),*
        }

        impl EventDispatchers {
            fn connect(
                &self,
                dom: &$crate::system::web::dom::WithKnownShape<web::EventTarget>,
                $target: &$crate::system::web::EventTarget,
                $global_target: &$crate::system::web::EventTarget,
            ) -> Rc<[web::CleanupHandle]> {
                use crate::control::callback::traits::*;
                use web::JsCast;
                $(
                    let shape = dom.shape.clone_ref();
                    let dispatcher = self.$name.clone_ref();
                    let $name = web::add_event_listener_with_options(
                        &$event_target,
                        stringify!($js_name),
                        event_listener_options(),
                        move |event| {
                            let _profiler = profiler::start_task!(
                                profiler::APP_LIFETIME,
                                concat!("mouse_", stringify!($name))
                            );
                            let shape = shape.value();
                            let event = event.unchecked_into::<web::$js_event>();
                            dispatcher.run_all(&$event::new(event, shape))
                        }
                    );
                )*
                Rc::new([$($name),*])
            }
        }
    };
}



// =====================
// === Mouse Manager ===
// =====================

/// A utility which registers JavaScript handlers for mouse events and translates them to Rust
/// handlers. It is a top level mouse registry hub.
#[derive(Clone, CloneRef, Debug, Deref)]
pub struct MouseManager {
    #[deref]
    dispatchers: EventDispatchers,
    handles:     Rc<[web::CleanupHandle]>,
}

/// Return options for addEventListener function. See also
/// https://developer.mozilla.org/en-US/docs/Web/API/EventTarget/addEventListener
fn event_listener_options() -> web::ListenerOptions {
    // 1. We listen for events in the bubbling phase. If we ever would like to listen in the capture
    // phase, it would need to be set to "bubbling" for the "mouseleave" and "mouseenter" events,
    // as they provide incorrect events for the "capture" phase.
    //
    // 2. We want to prevent default action on wheel events, thus listener cannot be passive.
    web::ListenerOptions::new().not_passive()
}

define_bindings! { target, global_target,
    MouseEvent::mousedown => on_down (target, Down),
    MouseEvent::mouseup => on_up (global_target, Up),
    MouseEvent::mousemove => on_move (global_target, Move),
    MouseEvent::mouseleave => on_leave (target, Leave),
    MouseEvent::mouseenter => on_enter (target, Enter),
    WheelEvent::wheel => on_wheel (target, Wheel),
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
        target: &web::EventTarget,
        global_target: &web::EventTarget,
    ) -> Self {
        let dispatchers = EventDispatchers::default();
        let handles = dispatchers.connect(dom, target, global_target);
        Self { dispatchers, handles }
    }
}
