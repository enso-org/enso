//! A FRP definitions for mouse event handling, with biding this FRP graph to js events.

use crate::prelude::*;

use crate::control::io::mouse::{MouseFrpCallbackHandles, bind_frp_to_mouse};
use crate::display::shape::text::text_field::frp::keyboard::TextFieldKeyboardFrp;
use crate::display::shape::text::text_field::WeakTextField;
use crate::display::world::World;

use enso_frp::io::keyboard;
use enso_frp::io::Mouse;
use enso_frp::Position;
use enso_frp as frp;
use nalgebra::Vector2;



/// All nodes of FRP graph related to TextField operations.
#[derive(Debug)]
pub struct TextFieldMouseFrp {
    /// A "Mouse" common part of this graph from FRP library.
    pub mouse: Mouse,
    pub network : frp::Network,
    /// Event emitted on click inside the TextField.
    pub click_in: frp::Stream,
    /// Node giving `true` value during selection (clicked inside TextField and keeping pressed).
    pub selecting: frp::Stream<bool>,
    /// Node giving `true` when using keyboard modifiers for multicursor edit.
    pub multicursor: frp::Stream<bool>,
    /// A node setting cursor after mouse click.
    pub set_cursor_action: frp::Stream,
    /// A node modifying selection on mouse drag.
    pub select_action: frp::Stream,
}

impl TextFieldMouseFrp {
    /// Create FRP graph doing actions on given TextField.
    pub fn new(text_field_ptr:WeakTextField, keyboard:&TextFieldKeyboardFrp)
    -> Self {
        use keyboard::Key::*;
        let mouse               = Mouse::default();
        let is_inside           = Self::is_inside_text_field_lambda(text_field_ptr.clone());
        let is_multicursor_mode = |mask:&keyboard::KeyMask| mask == &[Alt,Shift].iter().collect();
        let is_block_selection  = |mask:&keyboard::KeyMask| mask == &[Alt].iter().collect();
        let set_cursor_action   = Self::set_cursor_lambda(text_field_ptr.clone());
        let select_action       = Self::select_lambda(text_field_ptr);
        frp::new_network! { text_field
            def is_inside         = mouse.position.map(is_inside);
            def click_in          = mouse.press.gate(&is_inside);
            def click_in_bool     = click_in.constant(true);
            def mouse_up_bool     = mouse.release.constant(false);
            def selecting         = click_in_bool.merge(&mouse_up_bool);
            def multicursor       = keyboard.keyboard.key_mask.map(is_multicursor_mode);
            def block_selection   = keyboard.keyboard.key_mask.map(is_block_selection);
            def click_in_pos      = mouse.position.sample(&click_in);
            def select_pos        = mouse.position.gate(&selecting);
            def set_cursor_action = click_in_pos.map2(&multicursor,set_cursor_action);
            def select_action     = select_pos.map2(&block_selection,select_action);
        }
        let network = text_field;
        Self {mouse,network,click_in,selecting,multicursor,set_cursor_action,select_action}
    }

    /// Bind this FRP graph to js events.
    pub fn bind_frp_to_mouse(&self, world:&World) -> MouseFrpCallbackHandles  {
        let mouse_manager = &world.scene().mouse.mouse_manager;
        bind_frp_to_mouse(&self.mouse,mouse_manager)
    }
}

// === Private functions ===

impl TextFieldMouseFrp {
    fn is_inside_text_field_lambda(text_field:WeakTextField) -> impl Fn(&Position) -> bool {
        move |position| {
            let position = Vector2::new(position.x,position.y);
            text_field.upgrade().map_or(false, |tf| tf.is_inside(position))
        }
    }

    fn set_cursor_lambda(text_field:WeakTextField) -> impl Fn(&Position,&bool) {
        move |position,multicursor| {
            let position = Vector2::new(position.x,position.y);
            if let Some(text_field) = text_field.upgrade() {
                text_field.set_focus();
                if *multicursor {
                    text_field.add_cursor(position);
                } else {
                    text_field.set_cursor(position);
                }
            }
        }
    }

    fn select_lambda(text_field:WeakTextField) -> impl Fn(&Position,&bool) {
        move |position,block_selection| {
            let position = Vector2::new(position.x,position.y);
            if let Some(text_field) = text_field.upgrade() {
                text_field.set_focus();
                if *block_selection {
                    text_field.block_selection(position);
                } else {
                    text_field.jump_cursor(position,true);
                }
            }
        }
    }
}
