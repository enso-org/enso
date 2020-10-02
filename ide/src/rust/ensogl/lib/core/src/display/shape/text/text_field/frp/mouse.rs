//! A FRP definitions for mouse event handling, with biding this FRP graph to js events.

use crate::prelude::*;

use crate::control::io::mouse::bind_frp_to_mouse;
use crate::control::io::mouse::MouseFrpCallbackHandles;
use crate::display::Scene;
use crate::display::shape::text::text_field::frp::keyboard::TextFieldKeyboardFrp;
use crate::display::shape::text::text_field::WeakTextField;

use enso_frp::io::keyboard_old;
use enso_frp::io::Mouse;
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
        use keyboard_old::Key::*;
        let mouse               = Mouse::default();
        let loc_text_field_ptr  = text_field_ptr.clone();
        let is_inside           = move |t:Vector2<f32>| Self::is_inside_text_field(&loc_text_field_ptr,t);
        let is_multicursor_mode = |mask:&keyboard_old::KeyMask| mask == &[Alt,Shift].iter().collect();
        let is_block_selection  = |mask:&keyboard_old::KeyMask| mask == &[Alt].iter().collect();
        let loc_text_field_ptr  = text_field_ptr.clone();
        let set_cursor_action   = move |p,m| Self::set_cursor(&loc_text_field_ptr,p,m);
        let select_action       = move |p,s| Self::select(&text_field_ptr,p,s);
        frp::new_network! { text_field
            is_inside         <- mouse.position.map(move |t|is_inside(*t));
            click_in          <- mouse.down.gate(&is_inside).constant(());
            click_in_bool     <- click_in.constant(true);
            mouse_up_bool     <- mouse.up.constant(false);
            selecting         <- any (click_in_bool,mouse_up_bool);
            multicursor       <- keyboard.keyboard.key_mask.map(is_multicursor_mode);
            block_selection   <- keyboard.keyboard.key_mask.map(is_block_selection);
            click_in_pos      <- mouse.position.sample(&click_in);
            select_pos        <- mouse.position.gate(&selecting);
            set_cursor_action <- click_in_pos.map2(&multicursor,move|p,m|set_cursor_action(*p,*m));
            select_action     <- select_pos.map2(&block_selection,move|p,s|select_action(*p,*s));
        }
        let network = text_field;
        Self {mouse,network,click_in,selecting,multicursor,set_cursor_action,select_action}
    }

    /// Bind this FRP graph to js events.
    pub fn bind_frp_to_mouse<'t,S:Into<&'t Scene>>(&self, scene:S) -> MouseFrpCallbackHandles  {
        let mouse_manager = &scene.into().mouse.mouse_manager;
        bind_frp_to_mouse(&self.mouse,mouse_manager)
    }
}

// === Private functions ===

impl TextFieldMouseFrp {
    fn is_inside_text_field(text_field:&WeakTextField,position:Vector2<f32>) -> bool {
        text_field.upgrade().map_or(false, |tf| tf.is_inside(position))
    }

    fn set_cursor(text_field:&WeakTextField,position:Vector2<f32>,multicursor:bool) {
        if let Some(text_field) = text_field.upgrade() {
            text_field.set_focus();
            if multicursor {
                text_field.add_cursor(position);
            } else {
                text_field.set_cursor(position);
            }
        }
    }

    fn select(text_field:&WeakTextField,position:Vector2<f32>,block_selection:bool) {
        if let Some(text_field) = text_field.upgrade() {
            text_field.set_focus();
            if block_selection {
                text_field.block_selection(position);
            } else {
                text_field.jump_cursor(position,true);
            }
        }
    }
}
