//! This module defines possible mouse events.

use crate::prelude::*;
use web::traits::*;

use crate::system::web;

use enso_frp::io::mouse;
use std::borrow::Borrow;
use web::dom::Shape;



// =============
// === Event ===
// =============

/// Mouse event wrapper.
#[derive(Clone, Derivative)]
#[derivative(Default(bound = ""))]
pub struct Event<EventType, JsEvent: ToEventData> {
    js_event:   Option<JsEvent>,
    data:       JsEvent::Data,
    shape:      Shape,
    event_type: ZST<EventType>,
}

impl<EventType, JsEvent> Debug for Event<EventType, JsEvent>
where
    EventType: TypeDisplay,
    JsEvent: ToEventData,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct(&type_display::<EventType>())
            .field("data", &self.data)
            .field("shape", &self.shape)
            .finish()
    }
}

/// Trait allowing extracting the phantom type of [`Event`].
#[allow(missing_docs)]
pub trait IsEvent {
    type PhantomType;
}
impl<EventType, JsEvent: ToEventData> IsEvent for Event<EventType, JsEvent> {
    type PhantomType = EventType;
}

/// Extract the phantom type of [`Event`].
pub type EventPhantomType<T> = <T as IsEvent>::PhantomType;

impl<EventType, JsEvent> Event<EventType, JsEvent>
where JsEvent: ToEventData
{
    /// Constructor.
    pub fn new(js_event: JsEvent, shape: Shape) -> Self {
        Self {
            data: js_event.to_data(shape),
            js_event: Some(js_event),
            shape,
            event_type: default(),
        }
    }

    /// Constructor for simulated event. Those events have provided data, but no actual JS event
    /// object. This is useful for testing.
    pub fn simulated(data: JsEvent::Data, shape: Shape) -> Self {
        Self { data, js_event: None, shape, event_type: default() }
    }
}

impl<EventType, JsEvent> Event<EventType, JsEvent>
where
    JsEvent: ToEventData + AsRef<web::Event>,
    JsEvent::Data: Borrow<MouseEventData>,
{
    /// The horizontal coordinate within the application's viewport at which the event occurred (as
    /// opposed to the coordinate within the page).
    ///
    /// For example, clicking on the left edge of the viewport will always result in a mouse event
    /// with a [`client_x`] value of 0, regardless of whether the page is scrolled horizontally.
    pub fn client_x(&self) -> f32 {
        self.client().x
    }

    /// The vertical coordinate within the application's viewport at which the event occurred (as
    /// opposed to the coordinate within the page).
    ///
    /// For example, clicking on the bottom edge of the viewport will always result in a mouse event
    /// with a [`client_y`] value of 0, regardless of whether the page is scrolled horizontally.
    pub fn client_y(&self) -> f32 {
        self.client().y
    }

    /// The coordinate within the application's viewport at which the event occurred (as opposed to
    /// the coordinate within the page).
    ///
    /// For example, clicking on the bottom edge of the viewport will always result in a mouse event
    /// with a [`client`] value of (0,0), regardless of whether the page is scrolled horizontally.
    pub fn client(&self) -> Vector2 {
        self.data.borrow().client
    }

    /// The coordinate within the application's viewport at which the event occurred (as opposed to
    /// the coordinate within the page), measured from the center of the viewport.
    pub fn client_centered(&self) -> Vector2 {
        let x = self.client_x() - self.shape.width / 2.0;
        let y = self.client_y() - self.shape.height / 2.0;
        Vector2(x, y)
    }

    /// The horizontal coordinate (offset) of the mouse pointer in global (screen) coordinates.
    pub fn screen_x(&self) -> f32 {
        self.screen().x
    }

    /// The vertical coordinate (offset) of the mouse pointer in global (screen) coordinates.
    pub fn screen_y(&self) -> f32 {
        self.screen().y
    }

    /// The coordinate (offset) of the mouse pointer in global (screen) coordinates.
    pub fn screen(&self) -> Vector2 {
        self.data.borrow().screen
    }

    /// The difference in the X coordinate of the mouse pointer between the given event and the
    /// previous mousemove event. In other words, the value of the property is computed like this:
    /// `current_event.movement_x = current_event.screen_x() - previous_event.screen_x()`.
    pub fn movement_x(&self) -> f32 {
        self.movement().x
    }

    /// The difference in the Y coordinate of the mouse pointer between the given event and the
    /// previous mousemove event. In other words, the value of the property is computed like this:
    /// `current_event.movement_y = current_event.screen_y() - previous_event.screen_y()`.
    pub fn movement_y(&self) -> f32 {
        self.movement().y
    }

    /// The difference in the coordinate of the mouse pointer between the given event and the
    /// previous mousemove event. In other words, the value of the property is computed like this:
    /// `current_event.movement = current_event.screen() - previous_event.screen()`.
    pub fn movement(&self) -> Vector2 {
        self.data.borrow().movement
    }

    /// Indicates which button was pressed on the mouse to trigger the event.
    pub fn button(&self) -> mouse::Button {
        self.data.borrow().button
    }

    /// Return the position relative to the event handler that was used to catch the event. If the
    /// event handler does not have a position in the DOM, the returned position will be relative to
    /// the viewport. This can happen if the event handler is, for example, the window.
    ///
    /// Note: may cause reflow of the JS layout.
    pub fn position_relative_to_event_handler(&self) -> Vector2 {
        if let Some(element) = self.try_get_current_target_element() {
            self.relative_position_with_reflow(&element)
        } else {
            Vector2::new(self.client_x(), self.client_y())
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
    pub fn relative_position_with_reflow(&self, element: &web::Element) -> Vector2 {
        let rect = element.get_bounding_client_rect();
        let x = self.client_x() - rect.left() as f32;
        let y = self.client_y() - rect.top() as f32;
        Vector2(x, y)
    }

    /// Check whether the `ctrl` key was pressed when the event was triggered.
    pub fn ctrl_key(&self) -> bool {
        self.data.borrow().ctrl_key
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
        let data = self.data;
        let shape = self.shape;
        let event_type = default();
        Event { js_event, data, shape, event_type }
    }
}

// ===============
// === Filters ===
// ===============

type FanMouseEvent<EventType> = crate::event::Event<Event<EventType, web::MouseEvent>>;

/// Indicates whether the primary mouse button was pressed when the event was triggered.
pub fn is_primary<T>(event: &FanMouseEvent<T>) -> bool {
    event.button() == mouse::PrimaryButton
}

/// Indicates whether the primary mouse button was pressed when the event was triggered.
pub fn is_middle<T>(event: &FanMouseEvent<T>) -> bool {
    event.button() == mouse::MiddleButton
}

/// Indicates whether the primary mouse button was pressed when the event was triggered.
pub fn is_secondary<T>(event: &FanMouseEvent<T>) -> bool {
    event.button() == mouse::SecondaryButton
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

            impl TypeDisplay for [<Phantom $name>] {
                fn type_display() -> String {
                    stringify!($name).to_string()
                }
            }

            $(#$meta)*
            pub type $name = Event<[<Phantom $name>], web::$js_event>;
        )*
    }};
}

/// An JS event type that is convertible to a light copyable struct containing its associated data.
#[allow(missing_docs)]
pub trait ToEventData {
    type Data: Default + Debug + Copy;
    fn to_data(&self, shape: Shape) -> Self::Data;
}

/// The data associated with a mouse event. In web environment, it is derived from the mouse event
/// itself. In test environment, it is passed as a parameter in order to simulate a particular
/// mouse event.
#[derive(Copy, Clone, Debug, Default)]
pub struct MouseEventData {
    /// Mouse client position. See [`Event<EventType,JsEvent>::client()`].
    pub client:   Vector2,
    /// Mouse screen position. See [`Event<EventType,JsEvent>::screen()`].
    pub screen:   Vector2,
    /// Mouse movement. See [`Event<EventType,JsEvent>::movement()`].
    pub movement: Vector2,
    /// See [`Event<EventType,JsEvent>::button()`].
    pub button:   mouse::Button,
    /// See [`Event<EventType,JsEvent>::ctrl_key()`].
    pub ctrl_key: bool,
}

impl MouseEventData {
    /// Convenience constructor for primary mouse button events. Used in testing.
    pub fn primary_at(pos: Vector2) -> Self {
        Self { client: pos, screen: pos, ..default() }
    }
}

/// The data associated with a mouse wheel event. See [`MouseEventData`] for more information.
#[derive(Copy, Clone, Debug, Default, Deref)]
pub struct WheelEventData {
    /// Each wheel event is also a mouse event, therefore it also contains the mouse event data.
    #[deref]
    pub base:  MouseEventData,
    /// The amount of scrolling that was performed in the x and y direction. The unit is not
    /// specified and browser dependent, but the sign will always match the direction of scroll.
    pub delta: Vector2,
}

impl Borrow<MouseEventData> for WheelEventData {
    fn borrow(&self) -> &MouseEventData {
        &self.base
    }
}

impl ToEventData for web::MouseEvent {
    type Data = MouseEventData;
    fn to_data(&self, shape: Shape) -> Self::Data {
        MouseEventData {
            client:   Vector2(self.client_x() as f32, shape.height - self.client_y() as f32),
            screen:   Vector2(self.screen_x() as f32, shape.height - self.screen_y() as f32),
            movement: Vector2(self.movement_x() as f32, -self.movement_y() as f32),
            button:   mouse::Button::from_code(self.button().into()),
            ctrl_key: self.ctrl_key(),
        }
    }
}


impl ToEventData for web::WheelEvent {
    type Data = WheelEventData;
    fn to_data(&self, shape: Shape) -> Self::Data {
        WheelEventData {
            base:  web::MouseEvent::to_data(self, shape),
            delta: Vector2(self.delta_x() as f32, self.delta_y() as f32),
        }
    }
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
    pub fn delta_x(&self) -> f32 {
        self.data.delta.x
    }

    /// The vertical scroll amount.
    pub fn delta_y(&self) -> f32 {
        self.data.delta.y
    }
}
