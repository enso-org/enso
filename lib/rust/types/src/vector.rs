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

impl<T, S> IntoVector2<T> for Vector2<S>
where S: Scalar + Copy + Into<T>
{
    fn into_vector2(self) -> Vector2<T> {
        Vector2::new(self.x.into(), self.y.into())
    }
}

impl<T, T1, T2> IntoVector2<T> for (T1, T2)
where
    T1: Into<T>,
    T2: Into<T>,
{
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

impl<T, S> IntoVector3<T> for Vector3<S>
where S: Scalar + Copy + Into<T>
{
    fn into_vector3(self) -> Vector3<T> {
        Vector3::new(self.x.into(), self.y.into(), self.z.into())
    }
}

impl<T, T1, T2, T3> IntoVector3<T> for (T1, T2, T3)
where
    T1: Into<T>,
    T2: Into<T>,
    T3: Into<T>,
{
    fn into_vector3(self) -> Vector3<T> {
        Vector3::new(self.0.into(), self.1.into(), self.2.into())
    }
}
