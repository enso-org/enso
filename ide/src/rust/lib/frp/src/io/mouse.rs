//! Mouse FRP bindings.

use crate::prelude::*;

use crate::nodes::*;
use crate::frp_def;



// ================
// === Position ===
// ================

/// A 2-dimensional position. Used for storing the mouse position on the screen.
#[derive(Clone,Copy,Debug,Default,PartialEq,Eq)]
#[allow(missing_docs)]
pub struct Position {
    pub x:i32,
    pub y:i32,
}

impl Position {
    /// Constructor.
    pub fn new(x:i32, y:i32) -> Self {
        Self {x,y}
    }
}

impl Sub<&Position> for &Position {
    type Output = Position;
    fn sub(self, rhs: &Position) -> Self::Output {
        let x = self.x - rhs.x;
        let y = self.y - rhs.y;
        Position {x,y}
    }
}



// =============
// === Mouse ===
// =============

/// Mouse FRP bindings.
#[derive(Clone,CloneRef,Debug)]
pub struct Mouse {
    /// The mouse up event.
    pub on_up : Dynamic<()>,
    /// The mouse down event.
    pub on_down : Dynamic<()>,
    /// The mouse wheel event.
    pub on_wheel : Dynamic<()>,
    /// The mouse leave event.
    pub on_leave : Dynamic<()>,
    /// Mouse button press status.
    pub is_down : Dynamic<bool>,
    /// Current mouse position.
    pub position : Dynamic<Position>,
}

impl Default for Mouse {
    fn default() -> Self {
        frp_def! { mouse.on_up     = source() }
        frp_def! { mouse.on_down   = source() }
        frp_def! { mouse.on_wheel  = source() }
        frp_def! { mouse.on_leave  = source() }
        frp_def! { mouse.position  = source() }
        frp_def! { mouse.down_bool = on_down.constant(true) }
        frp_def! { mouse.up_bool   = on_up.constant(false) }
        frp_def! { mouse.is_down   = down_bool.merge(&up_bool) }
        Self {on_up,on_down,on_leave,on_wheel,is_down,position}
    }
}

impl Mouse {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }
}
