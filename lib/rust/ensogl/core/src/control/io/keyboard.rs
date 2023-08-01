//! Keyboard utilities:
//! - [`event`]: Defines application-level keyboard events.
//! - [`dom`]: A DOM Event keyboard manager.


// ==============
// === Export ===
// ==============

pub mod dom;



/// Keyboard events.
pub mod event {
    use crate::frp::io::keyboard as frp_keyboard;
    use crate::prelude::*;

    /// A keyboard event.
    #[derive(Debug, Clone, Deref, Derivative)]
    #[derivative(Default(bound = "T: Default"))]
    pub struct Event<Marker, T> {
        /// The event.
        #[deref]
        pub event:  T,
        event_type: ZST<Marker>,
    }

    impl<Marker, T> From<T> for Event<Marker, T> {
        fn from(event: T) -> Self {
            let event_type = ZST();
            Self { event, event_type }
        }
    }

    /// Marker types.
    pub mod marker {
        /// Marker type for [`KeyUp`] events.
        #[derive(Debug, Copy, Clone)]
        pub enum Up {}
        /// Marker type for [`KeyDown`] events.
        #[derive(Debug, Copy, Clone)]
        pub enum Down {}
    }
    /// A key press event.
    pub type KeyDown = Event<marker::Down, KeyEvent>;
    /// A key release event.
    pub type KeyUp = Event<marker::Up, KeyEvent>;

    /// A keyboard event (press or release).
    #[derive(Debug, Clone, Default)]
    pub struct KeyEvent {
        key:  frp_keyboard::Key,
        meta: bool,
        ctrl: bool,
        alt:  bool,
    }

    impl KeyEvent {
        fn new(key: frp_keyboard::Key, meta: bool, ctrl: bool, alt: bool) -> Self {
            Self { key, meta, ctrl, alt }
        }

        /// Return the key that was pressed or released.
        pub fn key(&self) -> &frp_keyboard::Key {
            &self.key
        }

        /// Return whether the `meta` modifier key was active while the event occurred.
        pub fn meta(&self) -> bool {
            self.meta
        }

        /// Return whether the `ctrl`/`control` modifier key was active while the event occurred.
        pub fn ctrl(&self) -> bool {
            self.ctrl
        }

        /// Return whether the `alt` modifier key was active while the event occurred.
        pub fn alt(&self) -> bool {
            self.alt
        }
    }

    macro_rules! new_from_key_event {
        () => {
            /// Create a new event object.
            pub fn new(key: frp_keyboard::Key, meta: bool, ctrl: bool, alt: bool) -> Self {
                Self::from(KeyEvent::new(key, meta, ctrl, alt))
            }
        };
    }
    impl KeyDown {
        new_from_key_event!();
    }
    impl KeyUp {
        new_from_key_event!();
    }
}
pub use event::*;
