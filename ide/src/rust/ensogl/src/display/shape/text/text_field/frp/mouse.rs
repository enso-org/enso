//! A FRP definitions for mouse event handling, with biding this FRP graph to js events.

use crate::prelude::*;

use crate::control::io::mouse::{MouseFrpCallbackHandles, bind_frp_to_mouse};
use crate::display::shape::text::text_field::frp::keyboard::TextFieldKeyboardFrp;
use crate::display::shape::text::text_field::WeakTextField;
use crate::display::world::World;

use enso_frp::*;
use nalgebra::Vector2;



/// All nodes of FRP graph related to TextField operations.
#[derive(Debug)]
pub struct TextFieldMouseFrp {
    /// A "Mouse" common part of this graph from FRP library.
    pub mouse: Mouse,
    /// Event emitted on click inside the TextField.
    pub click_in: Dynamic<()>,
    /// Node giving `true` value during selection (clicked inside TextField and keeping pressed).
    pub selecting: Dynamic<bool>,
    /// Node giving `true` when using keyboard modifiers for multicursor edit.
    pub multicursor: Dynamic<bool>,
    /// A node setting cursor after mouse click.
    pub set_cursor_action: Dynamic<()>,
    /// A node modifying selection on mouse drag.
    pub select_action: Dynamic<()>
}

impl TextFieldMouseFrp {
    /// Create FRP graph doing actions on given TextField.
    pub fn new(text_field_ptr:WeakTextField, keyboard:&TextFieldKeyboardFrp)
    -> Self {
        use Key::*;
        let mouse               = Mouse::default();
        let is_inside           = Self::is_inside_text_field_lambda(text_field_ptr.clone());
        let is_multicursor_mode = |mask:&KeyMask| mask == &[Alt,Shift].iter().collect();
        let is_block_selection  = |mask:&KeyMask| mask == &[Alt].iter().collect();
        let set_cursor_action   = Self::set_cursor_lambda(text_field_ptr.clone());
        let select_action       = Self::select_lambda(text_field_ptr);
        frp! {
            text_field.is_inside       = mouse.position.map(is_inside);
            text_field.click_in        = mouse.on_down.gate(&is_inside);
            text_field.click_in_bool   = click_in.constant(true);
            text_field.mouse_up_bool   = mouse.on_up.constant(false);
            text_field.selecting       = click_in_bool.merge(&mouse_up_bool);
            text_field.multicursor     = keyboard.keyboard.key_mask.map(is_multicursor_mode);
            text_field.block_selection = keyboard.keyboard.key_mask.map(is_block_selection);

            text_field.click_in_pos = mouse.position.sample(&click_in);
            text_field.select_pos   = mouse.position.gate(&selecting);

            text_field.set_cursor_action   = click_in_pos.map2(&multicursor,set_cursor_action);
            text_field.select_action       = select_pos.map2(&block_selection,select_action);
        }
        Self {mouse,click_in,selecting,multicursor,set_cursor_action,select_action}
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
            let position = Vector2::new(position.x as f32,position.y as f32);
            text_field.upgrade().map_or(false, |tf| tf.is_inside(position))
        }
    }

    fn set_cursor_lambda(text_field:WeakTextField) -> impl Fn(&Position,&bool) {
        move |position,multicursor| {
            let position = Vector2::new(position.x as f32,position.y as f32);
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
            let position = Vector2::new(position.x as f32,position.y as f32);
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
