use crate::prelude::*;

use crate::entry;

use ensogl_core::data::color;



pub struct Style {
    pub contour: entry::Contour,
    pub color:   color::Rgba,
}

impl Style {
    pub fn update_entry_shape(&self, shape: &entry::shape::View) {
        shape.set_contour(self.contour);
        shape.color.set(self.color.into());
    }
}
