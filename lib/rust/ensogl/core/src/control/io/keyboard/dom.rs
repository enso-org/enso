//! This module contains implementation of a keyboard manager and related utilities.

use crate::prelude::*;

use crate::system::web;

use std::rc::Rc;


// ==============
// === Export ===
// ==============

pub mod event;

pub use crate::frp::io::keyboard::*;
pub use event::*;



// ==========================
// === DOM Event Bindings ===
// ==========================

macro_rules! define_bindings {
    (
        $( $js_event:ident :: $js_name:ident =>
             $name:ident ($event:ident)
        ),* $(,)?
    ) => {
        /// Set of dispatchers for various keyboard events.
        #[derive(Clone, CloneRef, Debug, Default)]
        #[allow(missing_docs)]
        pub struct EventDispatchers {
            $(pub $name : $crate::control::callback::registry::Ref1<$event>),*
        }

        impl EventDispatchers {
            fn connect(
                &self,
                target: &$crate::system::web::EventTarget,
            ) -> Rc<[web::EventListenerHandle]> {
                use crate::control::callback::traits::*;
                use web::JsCast;
                type EventJsClosure = web::Closure<dyn FnMut(web::JsValue)>;
                $(
                    let dispatcher = self.$name.clone_ref();
                    let closure = move |event: web::JsValue| {
                        let _profiler = profiler::start_task!(
                            profiler::APP_LIFETIME,
                            concat!("keyboard_", stringify!($name))
                        );
                        let event = event.unchecked_into::<web::$js_event>();
                        dispatcher.run_all(&$event::new(event))
                    };
                    let closure: EventJsClosure = web::Closure::new(closure);
                    let js_name = stringify!($js_name);
                    let opt = event_listener_options();
                    let $name =
                        web::add_event_listener_with_options(&target, js_name, closure, opt);
                )*
                Rc::new([$($name),*])
            }
        }
    };
}



// ========================
// === Keyboard Manager ===
// ========================

/// Top level keyboard registry hub. Registers JavaScript handlers for keyboard events and
/// translates them to Rust handlers.
#[derive(Clone, CloneRef, Debug, Deref)]
pub struct KeyboardManager {
    #[deref]
    dispatchers: EventDispatchers,
    handles:     Rc<[web::EventListenerHandle]>,
}

/// Return options for addEventListener function. See also
/// https://developer.mozilla.org/en-US/docs/Web/API/EventTarget/addEventListener
fn event_listener_options() -> web::EventListenerHandleOptions {
    default()
}

define_bindings! {
    KeyboardEvent::keydown => on_keydown (KeyDown),
    KeyboardEvent::keyup => on_keyup (KeyUp),
    Event::blur => on_blur (Blur),
}

impl KeyboardManager {
    /// Create a new [`KeyboardManager`] handling events received by the specified [`EventTarget`].
    pub fn new(target: &web::EventTarget) -> Self {
        let dispatchers = EventDispatchers::default();
        let handles = dispatchers.connect(target);
        Self { dispatchers, handles }
    }
}
