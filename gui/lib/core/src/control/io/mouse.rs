//! This module contains the `MouseManager` implementation, its associated structs such as
//! `MousePositionEvent`, `MouseClickEvent` and `MouseWheelEvent`.

use crate::system::web::dom::DomContainer;
use crate::system::web::dyn_into;
use crate::system::web::Result;
use crate::system::web::Error;
use crate::system::web::ignore_context_menu;

use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::MouseEvent;
use web_sys::WheelEvent;
use web_sys::EventTarget;
use web_sys::AddEventListenerOptions;
use js_sys::Function;
use nalgebra::Vector2;
use std::rc::Rc;
use std::cell::RefCell;



// =====================
// === EventListener ===
// =====================

/// This struct keeps the register of the event listener and unregisters it when it's dropped.
pub struct EventListener<T:?Sized> {
    target   : EventTarget,
    name     : String,
    callback : Closure<T>
}

impl<T:?Sized> EventListener<T> {
    fn new<Str:AsRef<str>>(target:EventTarget, name:Str, callback:Closure<T>) -> Self {
        let name = name.as_ref().to_string();
        Self { target,name,callback }
    }
}

impl<T:?Sized> Drop for EventListener<T> {
    fn drop(&mut self) {
        let callback: &Function = self.callback.as_ref().unchecked_ref();
        remove_event_listener_with_callback(&self.target,&self.name,&callback).ok();
    }
}



// =============================
// === Mouse Event Listeners ===
// =============================

/// EventListener for Mouse events.
pub type MouseEventListener = EventListener<dyn FnMut(MouseEvent)>;

/// EventListener for Wheel events.
pub type WheelEventListener = EventListener<dyn FnMut(WheelEvent)>;



// ===================
// === MouseButton ===
// ===================

/// An enumeration representing the mouse buttons.
pub enum MouseButton {
    /// Left mouse button.
    LEFT,

    /// Middle mouse button.
    MIDDLE,

    /// Right mouse button.
    RIGHT,

    /// For unknown mouse buttons IDs.
    UNKNOWN
}



// =======================
// === MouseClickEvent ===
// =======================

/// Mouse click callback used by `MouseManager`.
pub trait MouseClickCallback = FnMut(MouseClickEvent) + 'static;

/// A struct storing information about mouse down and mouse up events.
pub struct MouseClickEvent {
    /// The position where the MouseClickEvent occurred.
    pub position : Vector2<f32>,

    /// The button which triggered the event.
    pub button : MouseButton
}

impl MouseClickEvent {
    fn from(event:MouseEvent, data:&Rc<MouseManagerData>) -> Self {
        let position  = Vector2::new(event.x() as f32, event.y() as f32);
        let position  = position - data.dom().position_with_style_reflow();
        let button    = match event.button() {
            LEFT_MOUSE_BUTTON      => MouseButton::LEFT,
            MIDDLE_MOUSE_BUTTON    => MouseButton::MIDDLE,
            RIGHT_MOUSE_BUTTON     => MouseButton::RIGHT,
            _                      => MouseButton::UNKNOWN
        };
        Self { position, button }
    }
}



// ==========================
// === MousePositionEvent ===
// ==========================

/// Mouse position callback used by `MouseManager`.
pub trait MousePositionCallback = FnMut(MousePositionEvent)  + 'static;

/// A struct storing information about mouse move, mouse enter and mouse leave events.
pub struct MousePositionEvent {
    /// The previous position where the mouse was.
    pub previous_position : Vector2<f32>,

    /// The current position where the mouse is.
    pub position : Vector2<f32>
}

impl MousePositionEvent {
    fn from(event:MouseEvent, data:&Rc<MouseManagerData>) -> Self {
        let position          = Vector2::new(event.x() as f32,event.y() as f32);
        let position          = position - data.dom().position_with_style_reflow();
        let previous_position = match data.mouse_position() {
            Some(position) => position,
            None           => position
        };
        data.set_mouse_position(Some(position));
        Self { previous_position, position }
    }
}



// =======================
// === MouseWheelEvent ===
// =======================

/// Mouse wheel callback used by `MouseManager`.
pub trait MouseWheelCallback = FnMut(MouseWheelEvent) + 'static;

/// A struct storing information about mouse wheel events.
pub struct MouseWheelEvent {
    /// A boolean indicating if the keyboard ctrl button is pressed.
    pub is_ctrl_pressed : bool,

    /// The horizontal movement in pixels.
    pub movement_x : f32,

    /// The vertical movement in pixels.
    pub movement_y : f32
}

impl MouseWheelEvent {
    fn from(event:WheelEvent) -> Self {
        let movement_x      = event.delta_x() as f32;
        let movement_y      = event.delta_y() as f32;
        let is_ctrl_pressed = event.ctrl_key();
        Self { movement_x,movement_y,is_ctrl_pressed }
    }
}



// ==============================
// === MouseManagerProperties ===
// ==============================

struct MouseManagerProperties {
    dom                    : DomContainer,
    mouse_position         : Option<Vector2<f32>>,
    target                 : EventTarget,
    stop_tracking_listener : Option<MouseEventListener>
}



// ========================
// === MouseManagerData ===
// ========================

/// A struct used for storing shared MouseManager's mutable data.
struct MouseManagerData {
    properties : RefCell<MouseManagerProperties>
}

impl MouseManagerData {
    fn new(target:EventTarget, dom: DomContainer) -> Rc<Self> {
        let mouse_position         = None;
        let stop_tracking_listener = None;
        let p = MouseManagerProperties{dom,mouse_position,target,stop_tracking_listener};
        let properties = RefCell::new(p);
        Rc::new(Self { properties })
    }
}


// === Setters ===

impl MouseManagerData {
    fn set_mouse_position(&self, position:Option<Vector2<f32>>) {
        self.properties.borrow_mut().mouse_position = position
    }

    fn set_stop_mouse_tracking(&self, listener:Option<MouseEventListener>) {
        self.properties.borrow_mut().stop_tracking_listener = listener;
    }
}


// === Getters ===

impl MouseManagerData {
    fn target(&self) -> EventTarget {
        self.properties.borrow().target.clone()
    }

    fn mouse_position(&self) -> Option<Vector2<f32>> {
        self.properties.borrow().mouse_position
    }

    fn dom(&self) -> DomContainer {
        self.properties.borrow().dom.clone()
    }
}



// ==========================
// === add_callback macro ===
// ==========================

/// Creates an add_callback method implementation.
/// ```compile_fail
/// add_callback!(add_mouse_down_callback, MouseClick, "mousedown")
/// ```
/// expands to
/// ```compile_fail
/// fn add_mouse_down_callback
/// <F:MouseClickEvent>(&mut self, mut f:F) -> Result<MouseEventListener> {
///     let data = Rc::downgrade(&self.data);
///     let closure = move |event:MouseEvent| {
///     if let Some(data) = data.upgrade() {
///         f(MouseClickEvent::from(event, &data));
///     }
/// };
/// ```
macro_rules! add_callback {
    ($name:ident, $event_type:ident, $target:literal) => { paste::item! {
        /// Adds $name event callback and returns its listener object.
        pub fn [<add_ $name _callback>]
        <F:[<$event_type Callback>]>(&mut self, mut f:F) -> Result<MouseEventListener> {
            let data = Rc::downgrade(&self.data);
            let closure = move |event:MouseEvent| {
                if let Some(data) = data.upgrade() {
                    f([<$event_type Event>]::from(event, &data));
                }
            };
            add_mouse_event(&self.data.target(), $target, closure)
        }
    } };
}



// ====================
// === MouseManager ===
// ====================

/// This structs manages mouse events in a specified DOM object.
pub struct MouseManager {
    data : Rc<MouseManagerData>
}

const   LEFT_MOUSE_BUTTON: i16 = 0;
const MIDDLE_MOUSE_BUTTON: i16 = 1;
const  RIGHT_MOUSE_BUTTON: i16 = 2;

impl MouseManager {
    /// Creates a new instance to manage mouse events in the specified DOMContainer.
    pub fn new(dom:&DomContainer) -> Result<Self> {
        let target              = dyn_into::<_, EventTarget>(dom.dom.clone())?;
        let dom                 = dom.clone();
        let data                = MouseManagerData::new(target,dom);
        let mut mouse_manager   = Self { data };
        mouse_manager.stop_tracking_mouse_when_it_leaves_dom()?;
        Ok(mouse_manager)
    }

    /// Sets context menu state to enabled or disabled.
    pub fn disable_context_menu(&mut self) -> Result<MouseEventListener> {
        let listener = ignore_context_menu(&self.data.target())?;
        Ok(EventListener::new(self.data.target(), "contextmenu", listener))
    }

    add_callback!(mouse_down, MouseClick, "mousedown");

    add_callback!(mouse_up, MouseClick, "mouseup");

    add_callback!(mouse_move, MousePosition, "mousemove");

    add_callback!(mouse_leave, MousePosition, "mouseleave");

    /// Adds MouseWheel event callback and returns its listener object.
    pub fn add_mouse_wheel_callback
    <F:MouseWheelCallback>(&mut self, mut f:F) -> Result<WheelEventListener> {
        let closure = move |event:WheelEvent| {
            event.prevent_default();
            f(MouseWheelEvent::from(event));
        };
        add_wheel_event(&self.data.target(), closure, false)
    }

    fn stop_tracking_mouse_when_it_leaves_dom(&mut self) -> Result<()> {
        let data    = Rc::downgrade(&self.data);
        let closure = move |_| {
            if let Some(data) = data.upgrade() {
                data.set_mouse_position(None);
            }
        };
        let listener = add_mouse_event(&self.data.target(), "mouseleave", closure)?;
        self.data.set_stop_mouse_tracking(Some(listener));
        Ok(())
    }
}



// =============
// === Utils ===
// =============

fn add_event_listener_with_callback
(target:&EventTarget, name:&str, function:&Function) -> Result<()> {
    match target.add_event_listener_with_callback(name, function) {
        Ok(_)  => Ok(()),
        Err(_) => Err(Error::FailedToAddEventListener)
    }
}

fn remove_event_listener_with_callback
(target:&EventTarget, name:&str, function:&Function) -> Result<()> {
    match target.remove_event_listener_with_callback(name, function) {
        Ok(_)  => Ok(()),
        Err(_) => Err(Error::FailedToRemoveEventListener)
    }
}

/// Adds mouse event callback and returns its listener.
fn add_mouse_event<T>(target:&EventTarget, name:&str, closure: T) -> Result<MouseEventListener>
where T : FnMut(MouseEvent) + 'static {
    let closure : Closure<dyn FnMut(MouseEvent)> = Closure::wrap(Box::new(closure));
    let callback : &Function = closure.as_ref().unchecked_ref();
    add_event_listener_with_callback(target, name, callback)?;
    Ok(EventListener::new(target.clone(), name.to_string(), closure))
}

/// Adds wheel event callback and returns its listener.
fn add_wheel_event<T>(target:&EventTarget, closure: T, passive:bool) -> Result<WheelEventListener>
where T : FnMut(WheelEvent) + 'static {
    let closure : Closure<dyn FnMut(WheelEvent)> = Closure::wrap(Box::new(closure));
    let callback    = closure.as_ref().unchecked_ref();
    let mut options = AddEventListenerOptions::new();
    options.passive(passive);
    match target.add_event_listener_with_callback_and_add_event_listener_options
    ("wheel", callback, &options) {
        Ok(_)  => {
            let target = target.clone();
            let name = "wheel".to_string();
            let listener = EventListener::new(target, name, closure);
            Ok(listener)
        },
        Err(_) => Err(Error::FailedToAddEventListener)
    }
}
