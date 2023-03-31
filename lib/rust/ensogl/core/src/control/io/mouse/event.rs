//! This module defines possible mouse events.

use crate::prelude::*;
use web::traits::*;

use crate::system::web;

use enso_frp::io::mouse;
use web::dom::Shape;



// =============
// === Event ===
// =============

/// Mouse event wrapper.
#[derive(Debug, Clone, Derivative)]
#[derivative(Default(bound = ""))]
pub struct Event<EventType, JsEvent> {
    js_event:   Option<JsEvent>,
    shape:      Shape,
    event_type: PhantomData<EventType>,
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
where JsEvent: AsRef<web::MouseEvent>
{
    /// Constructor.
    pub fn new(js_event: JsEvent, shape: Shape) -> Self {
        let js_event = Some(js_event);
        let event_type = default();
        Self { js_event, shape, event_type }
    }

    /// The horizontal coordinate within the application's viewport at which the event occurred (as
    /// opposed to the coordinate within the page).
    ///
    /// For example, clicking on the left edge of the viewport will always result in a mouse event
    /// with a [`client_x`] value of 0, regardless of whether the page is scrolled horizontally.
    pub fn client_x(&self) -> i32 {
        self.js_event.as_ref().map(|t| t.as_ref().client_x()).unwrap_or_default()
    }

    /// The vertical coordinate within the application's viewport at which the event occurred (as
    /// opposed to the coordinate within the page).
    ///
    /// For example, clicking on the bottom edge of the viewport will always result in a mouse event
    /// with a [`client_y`] value of 0, regardless of whether the page is scrolled horizontally.
    pub fn client_y(&self) -> i32 {
        self.shape.height as i32
            - self.js_event.as_ref().map(|t| t.as_ref().client_y()).unwrap_or_default()
    }

    /// The horizontal coordinate (offset) of the mouse pointer in global (screen) coordinates.
    pub fn screen_x(&self) -> i32 {
        self.js_event.as_ref().map(|t| t.as_ref().screen_x()).unwrap_or_default()
    }

    /// The vertical coordinate (offset) of the mouse pointer in global (screen) coordinates.
    pub fn screen_y(&self) -> i32 {
        self.shape.height as i32
            - self.js_event.as_ref().map(|t| t.as_ref().screen_y()).unwrap_or_default()
    }

    /// Indicates which button was pressed on the mouse to trigger the event.
    pub fn button(&self) -> mouse::Button {
        mouse::Button::from_code(
            self.js_event.as_ref().map(|t| t.as_ref().button().into()).unwrap_or_default(),
        )
    }

    /// Return the position relative to the event handler that was used to catch the event. If the
    /// event handler does not have a position in the DOM, the returned position will be relative to
    /// the viewport. This can happen if the event handler is, for example, the window.
    ///
    /// Note: may cause reflow of the JS layout.
    pub fn position_relative_to_event_handler(&self) -> Vector2<f32> {
        if let Some(element) = self.try_get_current_target_element() {
            self.relative_position_with_reflow(&element)
        } else {
            Vector2::new(self.client_x() as f32, self.client_y() as f32)
        }
    }

    /// Return the event handler that caught this event if it exists and if it is an HTML element.
    /// Returns  `None`  if the event was caught, for example, byt the window.
    fn try_get_current_target_element(&self) -> Option<web::Element> {
        let target = self.js_event.as_ref().and_then(|t| t.as_ref().current_target())?;
        target.value_of().dyn_into::<web::Element>().ok()
    }

    /// Return the position relative to the given element.
    ///
    /// Note: causes reflow of the JS layout.
    pub fn relative_position_with_reflow(&self, element: &web::Element) -> Vector2<f32> {
        let rect = element.get_bounding_client_rect();
        let x = self.client_x() as f64 - rect.left();
        let y = self.client_y() as f64 - rect.top();
        Vector2::new(x as f32, y as f32)
    }

    /// Check whether the `ctrl` key was pressed when the event was triggered.
    pub fn ctrl_key(&self) -> bool {
        self.js_event.as_ref().map(|t| t.as_ref().ctrl_key()).unwrap_or_default()
    }

    /// Prevent the default action of the event.
    pub fn prevent_default(&self) {
        self.js_event.as_ref().map(|t| t.as_ref().prevent_default());
    }

    /// Convert the event to a different type. No checks will be performed during this action.
    pub fn unchecked_convert_to<NewEventType: IsEvent>(
        self,
    ) -> Event<EventPhantomType<NewEventType>, JsEvent> {
        let js_event = self.js_event;
        let shape = self.shape;
        let event_type = default();
        Event { js_event, shape, event_type }
    }
}



// ==============
// === Events ===
// ==============

macro_rules! define_events {
    ( $( $(#$meta:tt)* $name:ident <$js_event:ident> ),* $(,)? ) => {paste!{
        $(
            $(#$meta)*
            #[derive(Copy, Clone, Debug, Default)]
            pub struct [<Phantom $name>];

            $(#$meta)*
            pub type $name = Event<[<Phantom $name>], web::$js_event>;
        )*
    }};
}

define_events! {
    // ======================
    // === JS-like Events ===
    // ======================
    // These events are counterpart of the JavaScript events. They have the same behavior in the
    // EnsoGL display object hierarchy. To learn more about them, see:
    // - https://developer.mozilla.org/en-US/docs/Web/API/Element/mousedown_event
    // - https://developer.mozilla.org/en-US/docs/Web/API/Element/mouseenter_event
    // - https://developer.mozilla.org/en-US/docs/Web/API/Element/mouseleave_event
    // - https://developer.mozilla.org/en-US/docs/Web/API/Element/mousemove_event
    // - https://developer.mozilla.org/en-US/docs/Web/API/Element/mouseout_event
    // - https://developer.mozilla.org/en-US/docs/Web/API/Element/mouseover_event
    // - https://developer.mozilla.org/en-US/docs/Web/API/Element/mouseup_event
    // - https://developer.mozilla.org/en-US/docs/Web/API/Element/wheel_event

    /// The [`Down`] event is fired at an element when a button on a pointing device (such as a
    /// mouse or trackpad) is pressed while the pointer is inside the element.
    ///
    /// The [`Down`] event is the counterpoint to the [`Up`] event.
    Down<MouseEvent>,

    /// The [`Up`] event is fired at an element when a button on a pointing device (such as a mouse
    /// or trackpad) is released while the pointer is located inside it.
    ///
    /// The [`Up`] event is the counterpoint to the [`Down`] event.
    Up<MouseEvent>,

    /// The [`Move`] event is fired at an element when a pointing device (such as a mouse or
    /// trackpad) is moved while the cursor's hotspot is inside it.
    Move<MouseEvent>,

    /// The [`Enter`] event is fired at an element when the cursor of a pointing device (such as a
    /// mouse or trackpad) is initially moved so that its hotspot is within the element at which the
    /// event was fired.
    ///
    /// Both [`Enter`] and [`Over`] events are similar but differ in that [`Enter`] does not bubble
    /// and [`Over`] does. This means that [`Enter`] is fired when the pointer has entered the
    /// element and all of its descendants, whereas [`Over`] is fired when the pointer enters the
    /// element or enters one of the element's descendants (even if the pointer was already within
    /// the element).
    Enter<MouseEvent>,

    /// The [`Leave`] event is fired at an element when the cursor of a pointing device (such as a
    /// mouse or trackpad) is moved out of it.
    ///
    /// Both [`Leave`] and [`Out`] events are similar but differ in that [`Leave`] does not bubble
    /// and [`Out`] does. This means that [`Leave`] is fired when the pointer has exited the element
    /// and all of its descendants, whereas [`Out`] is fired when the pointer leaves the element or
    /// leaves one of the element's descendants (even if the pointer is still within the element).
    Leave<MouseEvent>,

    /// The [`Over`] event is fired at an element when the cursor of a pointing device (such as a
    /// mouse or trackpad) is moved onto the element or one of its child elements
    ///
    /// Both [`Enter`] and [`Over`] events are similar but differ in that [`Enter`] does not bubble
    /// and [`Over`] does. This means that [`Enter`] is fired when the pointer has entered the
    /// element and all of its descendants, whereas [`Over`] is fired when the pointer enters the
    /// element or enters one of the element's descendants (even if the pointer was already within
    /// the element).
    Over<MouseEvent>,

    /// The [`Out`] event is fired at an element when the cursor of a pointing device (such as a
    /// mouse or trackpad) is moved so that it is no longer contained within the element or one of
    /// its children.
    ///
    /// Both [`Leave`] and [`Out`] events are similar but differ in that [`Leave`] does not bubble
    /// and [`Out`] does. This means that [`Leave`] is fired when the pointer has exited the element
    /// and all of its descendants, whereas [`Out`] is fired when the pointer leaves the element or
    /// leaves one of the element's descendants (even if the pointer is still within the element).
    Out<MouseEvent>,

    /// The wheel event fires when the user rotates a wheel button on a pointing device
    /// (typically a mouse).
    Wheel<WheelEvent>,



    // ==========================
    // === Non JS-like Events ===
    // ==========================
    // These events do not have their JavaScript counterpart and are EnsoGL-specific extensions to
    // the mouse event family

    /// The [`Release`] event is fired at an element when a button on a pointing device (such as a
    /// mouse or trackpad) is released anywhere in the scene, if it was previously pressed on that
    /// element.
    ///
    /// The [`Release`] event is similar to the [`Up`] event, but fires even if the mouse is outside
    /// of the element it was initially pressed on.
    Release<MouseEvent>,
}

impl Wheel {
    /// The horizontal scroll amount.
    pub fn delta_x(&self) -> f64 {
        self.js_event.as_ref().map(|t| t.delta_x()).unwrap_or_default()
    }

    /// The vertical scroll amount.
    pub fn delta_y(&self) -> f64 {
        self.js_event.as_ref().map(|t| t.delta_y()).unwrap_or_default()
    }
}
