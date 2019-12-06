use crate::prelude::*;

use crate::display::rendering::Object;

use nalgebra::base::Matrix4;
use nalgebra::geometry::Perspective3;
use std::f32::consts::PI;

// ==============
// === Camera ===
// ==============

/// A 3D camera representation with its own 3D `Transform` and
/// projection matrix.
#[derive(Shrinkwrap, Debug)]
#[shrinkwrap(mutable)]
pub struct Camera {
    #[shrinkwrap(main_field)]
    pub object     : Object,
    pub projection : Matrix4<f32>,
}

impl Camera {
    /// Creates a Camera with perspective projection.
    pub fn perspective(fov:f32, aspect:f32, z_near:f32, z_far:f32) -> Self {
        let fov = fov / 180.0 * PI;
        let projection = Perspective3::new(aspect, fov, z_near, z_far);
        let projection = *projection.as_matrix();
        let object     = default();
        Self { object, projection }
    }

    pub fn get_y_scale(&self) -> f32 { self.projection.m11 }
}

#[cfg(test)]
mod test {
    #[test]
    fn perspective() {
        use super::Camera;
        use nalgebra::Matrix4;
        let camera   = Camera::perspective(45.0, 1920.0 / 1080.0, 1.0, 1000.0);
        let expected = Matrix4::new
            ( 1.357995,       0.0,       0.0,       0.0
            , 0.0     , 2.4142134,       0.0,       0.0
            , 0.0     ,       0.0, -1.002002, -2.002002
            , 0.0     ,       0.0,      -1.0,       0.0
            );
       assert_eq!(camera.projection, expected);
    }
}
