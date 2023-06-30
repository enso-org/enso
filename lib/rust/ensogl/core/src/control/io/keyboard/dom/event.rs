//! This module defines possible keyboard events.

use crate::prelude::*;

use crate::system::web;



// =============
// === Event ===
// =============

/// Keyboard event wrapper.
#[derive(Clone, Derivative)]
#[derivative(Default(bound = ""))]
pub struct Event<EventType, JsEvent> {
    /// The DOM event that initiated this event. May be absent if this event was synthesized.
    pub js_event: Option<JsEvent>,
    event_type:   ZST<EventType>,
}

impl<EventType, JsEvent> Debug for Event<EventType, JsEvent>
where
    EventType: TypeDisplay,
    JsEvent: AsRef<web::KeyboardEvent>,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct(&type_display::<EventType>()).field("key", &self.key()).finish()
    }
}

/// Trait allowing extracting the phantom type of [`Event`].
#[allow(missing_docs)]
pub trait IsEvent {
    type PhantomType;
}
impl<EventType, JsEvent> IsEvent for Event<EventType, JsEvent> {
    type PhantomType = EventType;
}

/// Extract the phantom type of [`Event`].
pub type EventPhantomType<T> = <T as IsEvent>::PhantomType;

impl<EventType, JsEvent> Event<EventType, JsEvent>
where JsEvent: AsRef<web::Event>
{
    /// Constructor.
    pub fn new(js_event: JsEvent) -> Self {
        let js_event = Some(js_event);
        let event_type = default();
        Self { js_event, event_type }
    }

    /// Prevent the default action of the event.
    pub fn prevent_default(&self) {
        self.js_event.as_ref().map(|t| t.as_ref().prevent_default());
    }

    /// Convert the event to a different type. No checks will be performed during this action.
    pub fn unchecked_convert_to<NewEventType: IsEvent>(
        self,
    ) -> Event<EventPhantomType<NewEventType>, JsEvent> {
        Event { event_type: default(), ..self }
    }
}

impl<EventType, JsEvent> Event<EventType, JsEvent>
where JsEvent: AsRef<web::KeyboardEvent>
{
    /// Return the key that was pressed.
    pub fn key(&self) -> String {
        self.js_event.as_ref().map(|t| t.as_ref().key()).unwrap()
    }

    /// Return whether the `meta` modifier key was active when the event occurred.
    pub fn is_meta_down(&self) -> bool {
        self.js_event.as_ref().map(|event| event.as_ref().meta_key()).unwrap_or_default()
    }

    /// Return whether the `ctrl`/`control` modifier key was active when the event occurred.
    pub fn is_ctrl_down(&self) -> bool {
        self.js_event.as_ref().map(|event| event.as_ref().ctrl_key()).unwrap_or_default()
    }
}



// ==============
// === Events ===
// ==============

macro_rules! define_events {
    ( $( $(#$meta:tt)* $name:ident <$js_event:ident> ),* $(,)? ) => {paste!{
        $(
            /// Distinguishes a specific type of keyboard event.
            $(#$meta)*
            #[derive(Copy, Clone, Debug, Default)]
            pub struct [<Phantom $name>];

            impl TypeDisplay for [<Phantom $name>] {
                fn type_display() -> String {
                    stringify!($name).to_string()
                }
            }

            /// A keyboard event.
            $(#$meta)*
            pub type $name = Event<[<Phantom $name>], web::$js_event>;
        )*
    }};
}

define_events! {
    KeyDown<KeyboardEvent>,
    KeyUp<KeyboardEvent>,
    Blur<Event>,
}
