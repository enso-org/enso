//! This module implements an Functional Reactive Programming system. It is an advanced event
//! handling framework which allows describing events and actions by creating declarative event
//! flow diagrams.
//!
//! Please read this document as the initial introduction to FRP concepts:
//! https://github.com/hansroland/reflex-dom-inbits/blob/master/tutorial.md

#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]

#![feature(specialization)]
#![feature(trait_alias)]
#![feature(weak_into_raw)]
#![feature(associated_type_defaults)]

pub mod data;
pub mod debug;
pub mod io;
pub mod macros;
pub mod core;

pub use data::*;
pub use debug::*;
pub use io::*;
pub use macros::*;
pub use crate::core::*;

pub use crate::core::dynamic::*;

use enso_prelude      as prelude;
use ensogl_system_web as web;



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn drag_and_drop() {
        let mouse = Mouse::new();

        frp_def! { mouse_down_position    = mouse.position.sample (&mouse.on_down)    }
        frp_def! { mouse_position_if_down = mouse.position.gate   (&mouse.is_down) }

        let final_position_ref_event  = Recursive::<EventData<Position>>::new_named("final_position_ref");
        let final_position_ref        = Dynamic::from(&final_position_ref_event);

        frp_def! { pos_diff_on_down = mouse_down_position.map2    (&final_position_ref,|m,f|{m-f}) }
        frp_def! { final_position   = mouse_position_if_down.map2 (&pos_diff_on_down  ,|m,f|{m-f}) }

        final_position_ref_event.initialize(&final_position);

        final_position_ref.event.set_display_id(final_position.event.display_id());
        final_position_ref.behavior.set_display_id(final_position.event.display_id());

        assert_eq!(final_position.behavior.current_value(),Position::new(0,0));
        mouse.position.event.emit(Position::new(3,4));
        assert_eq!(final_position.behavior.current_value(),Position::new(0,0));
        mouse.on_down.event.emit(());
        assert_eq!(final_position.behavior.current_value(),Position::new(0,0));
        mouse.position.event.emit(Position::new(4,6));
        assert_eq!(final_position.behavior.current_value(),Position::new(1,2));
        mouse.position.event.emit(Position::new(4,7));
        assert_eq!(final_position.behavior.current_value(),Position::new(1,3));
        mouse.on_up.event.emit(());
        mouse.position.event.emit(Position::new(4,0));
        assert_eq!(final_position.behavior.current_value(),Position::new(1,3));
        mouse.on_down.event.emit(());
        mouse.position.event.emit(Position::new(0,0));
        assert_eq!(final_position.behavior.current_value(),Position::new(-3,3));
    }
}
