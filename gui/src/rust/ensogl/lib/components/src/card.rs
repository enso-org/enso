//! This module provides the [`Card`] component which constists of a white rectangle with rounded
//! corners and a shadow.

use crate::prelude::*;

use crate::shadow;

use ensogl_core::display;
use ensogl_core::display::shape::*;
use ensogl_core::display::object::{ObjectOps, Instance};
use ensogl_theme as theme;



// ============
// === Card ===
// ============

/// The estimated margin that we add on all sides of the card to make sure that the shadow is
/// included within the shape's boundary. If we didn't use this then the shadow would get clipped.
/// We use and estimate because the precise number is hard to determine with our current
/// implementation.
const ESTIMATED_SHADOW_MARGIN:f32 = 10.0;

ensogl_core::define_shape_system! {
    (style:Style,corner_radius:f32) {
        let sprite_width  : Var<Pixels> = "input_size.x".into();
        let sprite_height : Var<Pixels> = "input_size.y".into();

        let width  = sprite_width - ESTIMATED_SHADOW_MARGIN.px() * 2.0;
        let height = sprite_height - ESTIMATED_SHADOW_MARGIN.px() * 2.0;
        let color  = style.get_color(theme::card::background);
        let rect   = Rect((&width,&height)).corners_radius(corner_radius);
        let shape  = rect.fill(color);

        let shadow = shadow::from_shape(rect.into(),style);

        (shadow + shape).into()
    }
}

/// A simple display object consisting of a white rectangle with rounded corners and a shadow. This
/// can be used as a background for other components.
#[derive(Debug,Clone,CloneRef)]
pub struct Card(View);

impl Card {
    /// Create a new card.
    pub fn new() -> Self {
        Card(View::new(Logger::new("Card")))
    }

    /// Set the size of the card's base shape. The shadow can reach beyound this.
    pub fn resize(&self,size:Vector2) {
        let shadow_margin = Vector2(ESTIMATED_SHADOW_MARGIN,ESTIMATED_SHADOW_MARGIN);
        self.0.size.set(size+2.0*shadow_margin);
    }

    /// Set the card's corner radius.
    pub fn set_corner_radius(&self,radius:f32) {
        self.0.corner_radius.set(radius);
    }
}

impl display::Object for Card {
    fn display_object(&self) -> &Instance {
        self.0.display_object()
    }
}

impl Default for Card {
    fn default() -> Self {
        Self::new()
    }
}
