//! Additional methods for [`nalgebra::Vector`] structs.

use nalgebra::*;



// ===================
// === IntoVector2 ===
// ===================

/// Additional conversions from some struct to [`Vector2`].
#[allow(missing_docs)]
pub trait IntoVector2<T> {
    fn into_vector2(self) -> Vector2<T>;
}

impl<T> IntoVector2<T> for Vector2<T> {
    fn into_vector2(self) -> Vector2<T> {
        self
    }
}

impl<T, S: Into<T>> IntoVector2<T> for (S, S) {
    fn into_vector2(self) -> Vector2<T> {
        Vector2::new(self.0.into(), self.1.into())
    }
}



// ===================
// === IntoVector2 ===
// ===================

/// Additional conversions from some struct to [`Vector3`].
#[allow(missing_docs)]
pub trait IntoVector3<T> {
    fn into_vector3(self) -> Vector3<T>;
}

impl<T> IntoVector3<T> for Vector3<T> {
    fn into_vector3(self) -> Vector3<T> {
        self
    }
}

impl<T, S: Into<T>> IntoVector3<T> for (S, S, S) {
    fn into_vector3(self) -> Vector3<T> {
        Vector3::new(self.0.into(), self.1.into(), self.2.into())
    }
}
