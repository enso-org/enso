#![allow(missing_docs)]

use crate::prelude::*;

use crate::display::camera::Camera2d;
use crate::display::object::DisplayObjectData;
use crate::system::gpu::data::uniform::UniformScope;



// =============
// === Scene ===
// =============

#[derive(Derivative)]
#[derivative(Debug(bound=""))]
pub struct Scene {
    pub root   : DisplayObjectData,
    pub camera : Camera2d
}


// === Implementation ===

impl Scene {
    pub fn new(logger:Logger, globals:&UniformScope) -> Self {
        let root   = DisplayObjectData::new(logger.sub("root"));
        let camera = Camera2d::new(logger.sub("camera"),globals);
        Self {root,camera}
    }
}
