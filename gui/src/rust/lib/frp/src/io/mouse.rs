//! Mouse FRP bindings.

use crate::prelude::*;

use crate as frp;



// ================
// === Position ===
// ================

/// A 2-dimensional position. Used for storing the mouse position on the screen.
#[derive(Clone,Copy,Debug,Default,PartialEq)]
#[allow(missing_docs)]
pub struct Position {
    pub x:f32,
    pub y:f32,
}

impl Position {
    /// Constructor.
    pub fn new(x:f32, y:f32) -> Self {
        Self {x,y}
    }

    /// Length of a vector from the origin to a point of the position.
    pub fn length(self) -> f32 {
        (self.x * self.x + self.y * self.y).sqrt()
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

impl From<&Position> for Position {
    fn from(t:&Position) -> Self {
        *t
    }
}



// =============
// === Mouse ===
// =============

/// Mouse FRP bindings.
#[derive(Clone,CloneRef,Debug)]
#[allow(missing_docs)]
pub struct Mouse {
    pub network       : frp::Network,
    pub release       : frp::Source,
    pub press         : frp::Source,
    pub wheel         : frp::Source,
    pub leave         : frp::Source,
    pub down          : frp::Stream<bool>,
    pub up            : frp::Stream<bool>,
    pub position      : frp::Source<Position>,
    pub prev_position : frp::Stream<Position>,
    pub translation   : frp::Stream<Position>,
    pub distance      : frp::Stream<f32>,
}

impl Default for Mouse {
    fn default() -> Self {
        frp::new_network! { mouse
            def release       = source_();
            def press         = source_();
            def wheel         = source_();
            def leave         = source_();
            def position      = source();
            def down_const    = press.constant(true);
            def up_const      = release.constant(false);
            def down          = down_const.merge(&up_const);
            def up            = down.map(|t| !t);
            def prev_position = position.previous();
            def translation   = position.map2(&prev_position,|t,s| t - s);
            def distance      = translation.map(|t:&Position| t.length());
        };
        let network = mouse;
        Self {network,release,press,leave,wheel,down,up,position,prev_position,translation,distance}
    }
}

impl Mouse {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }
}
