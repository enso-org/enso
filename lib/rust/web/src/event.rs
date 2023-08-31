//! Utilities for DOM events.

use crate::Event;
use crate::EventTarget;
use crate::JsCast;
use crate::JsValue;


// ==============
// === Export ===
// ==============

pub mod listener;



// =============
// === Event ===
// =============

/// This trait represents a type of event that may fire from some specific JS `EventTarget`.
///
/// For example, `WebSocket.close` is such an event, where `close` is event type and `WebSocket` is
/// the `EventTarget`.
///
/// The purpose is to increase type safety by grouping event type name, event target type and event
/// value type together.
///
/// Typically this trait is to be implemented for uncreatable types, created for the sole
/// purpose of denoting a particular event type within a context of an event target.
pub trait Type {
    /// The event value -- i.e. the Rust type of a value that will be passed as an argument
    /// to the listener.
    /// For example `CloseEvent`.
    type Interface: AsRef<Event> + JsCast + 'static;

    /// The type of the EventTarget object that fires this type of event, e.g. `WebSocket`.
    type Target: AsRef<EventTarget> + AsRef<JsValue> + JsCast + Clone + PartialEq;

    /// The type of the event as a string. For example `"close"`.
    const NAME: &'static str;
}
