#![allow(missing_docs)]

use crate::prelude::*;

use crate::display::render::css3d::Object;

use nalgebra::base::Matrix4;
use nalgebra::geometry::Perspective3;
use nalgebra::geometry::Orthographic3;
use std::f32::consts::PI;



// ===================
// === Perspective ===
// ===================

/// Perspective projection properties.
#[derive(Debug, Clone, Copy)]
pub struct Perspective {
    pub fov    : f32,
    pub aspect : f32,
    pub near   : f32,
    pub far    : f32
}



// ====================
// === Orthographic ===
// ====================

/// Orthographic projection properties.
#[derive(Debug, Clone, Copy)]
pub struct Orthographic {
    pub left   : f32,
    pub right  : f32,
    pub top    : f32,
    pub bottom : f32,
    pub near   : f32,
    pub far    : f32
}



// ==================
// === CameraType ===
// ==================

/// CameraType enum.
#[derive(Debug, Clone, Copy)]
pub enum CameraType {
    Perspective(Perspective),
    Orthographic(Orthographic)
}



// ==================
// === CameraData ===
// ==================

struct CameraData {
    camera_type : CameraType,
    projection  : Matrix4<f32>
}

// ==============
// === Camera ===
// ==============

/// A 3D camera representation with its own 3D Transform and projection matrix.
#[derive(Shrinkwrap, Clone)]
#[shrinkwrap(mutable)]
pub struct Camera {
    #[shrinkwrap(main_field)]
    pub object  : Object,
    data        : Rc<RefCell<CameraData>>
}

impl Camera {
    /// Creates a perspective projection Camera.
    pub fn perspective(fov:f32, aspect:f32, near:f32, far:f32) -> Self {
        let fov = fov / 180.0 * PI;
        let projection  = Perspective3::new(aspect, fov, near, far);
        let projection  = *projection.as_matrix();
        let object      = default();
        let camera_type = Perspective { fov, aspect, near, far };
        let camera_type = CameraType::Perspective(camera_type);
        let data        = Rc::new(RefCell::new(CameraData { camera_type, projection }));
        Self { object, data }
    }

    /// Creates an orthographic projection Camera.
    pub fn orthographic
        (left    : f32
        , right  : f32
        , bottom : f32
        , top    : f32
        , near   : f32
        , far    : f32) -> Self {
        let projection  = Orthographic3::new(left, right, bottom, top, near, far);
        let projection  = *projection.as_matrix();
        let object      = default();
        let camera_type = Orthographic { left, right, bottom, top, near, far };
        let camera_type = CameraType::Orthographic(camera_type);
        let data        = Rc::new(RefCell::new(CameraData { camera_type, projection }));
        Self { object, data }
    }

    /// Gets CameraType.
    pub fn camera_type(&self) -> CameraType { self.data.borrow().camera_type }

    /// Gets projection's y scaling.
    pub fn get_y_scale(&self) -> f32 { self.data.borrow().projection.m11 }

    /// Gets Camera's projection matrix.
    pub fn projection(&self) -> Matrix4<f32> { self.data.borrow().projection }
}

// =============
// === Tests ===
// =============

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
        assert_eq!(camera.projection(), expected);
    }
}
