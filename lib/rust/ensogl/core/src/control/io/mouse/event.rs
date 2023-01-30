//! This module defines possible mouse events.

use crate::prelude::*;
use web::traits::*;

use crate::system::web;

use enso_frp::io::mouse;
use web::dom::Shape;



// =============
// === Event ===
// =============

macro_rules! define_events {
    ( $( $js_event:ident :: $name:ident ),* $(,)? ) => {$(
        /// Mouse event wrapper.
        #[derive(Debug, Clone, From, Deref, AsRef)]
        pub struct $name {
            #[deref]
            raw   : web::$js_event,
            shape : Shape,
        }
        impl $name {

            /// Constructor.
            pub fn new(raw:web::$js_event,shape:Shape) -> Self {
                Self {raw,shape}
            }

            /// The Y coordinate of the mouse pointer relative to the position of the padding edge
            /// of the target node.
            pub fn offset_y(&self) -> i32 {
                self.shape.height as i32 - self.raw.offset_y()
            }

            /// The Y coordinate of the mouse pointer in local (DOM content) coordinates.
            pub fn client_y(&self) -> i32 {
                self.shape.height as i32 - self.raw.client_y()
            }

            /// The Y coordinate of the mouse pointer in global (screen) coordinates.
            pub fn screen_y(&self) -> i32 {
                self.shape.height as i32 - self.raw.screen_y()
            }

            /// Translation of the button property to Rust `Button` enum.
            pub fn button(&self) -> mouse::Button {
                mouse::Button::from_code(self.raw.button().into())
            }

            /// Return the position relative to the event handler that was used to catch the event.
            /// If the event handler does not have a position in the DOM, the returned position
            /// will be relative to the viewport. This can happen if the event handler is, for
            /// example, the window.
            ///
            /// Note: may cause reflow of the JS layout.
            pub fn position_relative_to_event_handler(&self) -> Vector2<f32> {
                if let Some(element) = self.try_get_current_target_element() {
                    self.relative_position_with_reflow(&element)
                } else {
                    Vector2::new(self.client_x() as f32,self.client_y() as f32)
                }
            }

            /// Return the event handler that caught this event if it exists and if it is an
            /// html element. Returns `None` if the event was caught, for example, byt the window.
            fn try_get_current_target_element(&self) -> Option<web::Element> {
                let target  = self.current_target()?;
                target.value_of().dyn_into::<web::Element>().ok()
            }

            /// Return the position relative to the given element.
            ///
            /// Note: causes reflow of the JS layout.
            pub fn relative_position_with_reflow(&self, element:&web::Element) -> Vector2<f32> {
                let rect = element.get_bounding_client_rect();
                let x    = self.client_x() as f64 - rect.left();
                let y    = self.client_y() as f64 - rect.top();
                Vector2::new(x as f32,y as f32)
            }

        }

        impl AsRef<web::Event> for $name {
            fn as_ref(&self) -> &web::Event {
                let js_event = AsRef::<web::$js_event>::as_ref(self);
                js_event.as_ref()
            }
        }
    )*};
}

define_events! {
    MouseEvent::OnDown,
    MouseEvent::OnUp,
    MouseEvent::OnMove,
    MouseEvent::OnLeave,
    WheelEvent::OnWheel,
}
