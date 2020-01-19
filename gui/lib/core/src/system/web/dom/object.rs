#![allow(missing_docs)]

use crate::prelude::*;

use crate::system::web::dom::Transform;

use nalgebra::UnitQuaternion;
use nalgebra::Vector3;

use std::rc::Rc;
use std::cell::RefCell;



// ==============
// === Object ===
// ==============

/// Base structure for representing a 3D object in a `Scene`.
#[derive(Default, Debug, Clone)]
pub struct Object {
    transform : Rc<RefCell<Transform>>
}

impl Object {
    /// Creates a Default Object.
    pub fn new() -> Object { default() }

    /// Gets object's transform.
    pub fn transform(&self) -> Transform { *self.transform.borrow() }

    /// Gets object's position.
    pub fn position(&self) -> Vector3<f32> {
        *self.transform.borrow().translation()
    }

    /// Sets object's position.
    pub fn set_position(&mut self, position:Vector3<f32>) {
        self.transform.borrow_mut().set_translation(position)
    }

    /// Sets the object's rotation in YXZ (yaw -> roll -> pitch) order.
    pub fn set_rotation(&mut self, roll:f32, pitch:f32, yaw:f32) {
        self.transform.borrow_mut().set_rotation(roll, pitch, yaw)
    }

    /// Gets the object's rotation UnitQuaternion.
    pub fn rotation(&self) -> UnitQuaternion<f32> {
        *self.transform.borrow().rotation()
    }

    /// Sets the object's scale.
    pub fn set_scale(&mut self, x: f32, y: f32, z: f32) {
        self.transform.borrow_mut().set_scale(x, y, z);
    }

    /// Gets the object's scale.
    pub fn scale(&self) -> Vector3<f32> {
        *self.transform.borrow().scale()
    }
}

#[cfg(test)]
mod test {
    use super::Object;
    use nalgebra::Vector3;
    use nalgebra::Quaternion;
    use std::f32::consts::PI;

    #[test]
    fn set_transform() {
        let mut object = Object::new();
        object.set_position(Vector3::new(1.0, 2.0, 3.0));
        object.set_scale(3.0, 2.0, 1.0);
        object.set_rotation(PI * 2.0, PI, PI / 2.0);

        assert_eq!(object.position(), Vector3::new(1.0, 2.0, 3.0));
        assert_eq!(object.scale(), Vector3::new(3.0, 2.0, 1.0));

        let expected = Quaternion::new
            ( 0.00000009272586
            , -0.7071068
            , -0.7071068
            , -0.000000030908623 );
        assert_eq!(*object.rotation().quaternion(), expected);
    }
}
