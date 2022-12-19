//! Additional methods for [`nalgebra::Vector`] structs.

use nalgebra::*;



// ===================
// === IntoVector1 ===
// ===================

/// Dummy implementation, mostly used by macros. See [`IntoVector2`] for a more interesting case.
#[allow(missing_docs)]
pub trait IntoVector1<T> {
    fn into_vector(self) -> T;
}

impl<T> IntoVector1<T> for T {
    fn into_vector(self) -> T {
        self
    }
}



// ===================
// === IntoVector2 ===
// ===================

/// Additional conversions from some struct to [`Vector2`].
#[allow(missing_docs)]
pub trait IntoVector2<T> {
    fn into_vector(self) -> Vector2<T>;
}

impl<T> IntoVector2<T> for Vector2<T>
where T: Scalar + Copy
{
    fn into_vector(self) -> Vector2<T> {
        Vector2::new(self.x, self.y)
    }
}

impl<T> IntoVector2<T> for (T, T) {
    fn into_vector(self) -> Vector2<T> {
        Vector2::new(self.0, self.1)
    }
}



// ===================
// === IntoVector3 ===
// ===================

/// Additional conversions from some struct to [`Vector3`].
#[allow(missing_docs)]
pub trait IntoVector3<T> {
    fn into_vector(self) -> Vector3<T>;
}

impl<T> IntoVector3<T> for Vector3<T>
where T: Scalar + Copy
{
    fn into_vector(self) -> Vector3<T> {
        Vector3::new(self.x, self.y, self.z)
    }
}

impl<T> IntoVector3<T> for (T, T, T) {
    fn into_vector(self) -> Vector3<T> {
        Vector3::new(self.0, self.1, self.2)
    }
}



// ========================
// === IntoVectorTrans2 ===
// ========================

/// Additional conversions from some struct to [`Vector2`]. Similar to [`IntoVector2`], but allows
/// the provided arguments to contain values of different type than the resulting vector. This has
/// a greater flexibility, but weakens the type inferencer capabilities. If you do not need the
/// automatic conversion, use [`IntoVector2`] instead.
#[allow(missing_docs)]
pub trait IntoVectorTrans2<T> {
    fn into_vector_trans(self) -> Vector2<T>;
}

impl<T, S> IntoVectorTrans2<T> for Vector2<S>
where S: Scalar + Copy + Into<T>
{
    fn into_vector_trans(self) -> Vector2<T> {
        Vector2::new(self.x.into(), self.y.into())
    }
}

impl<T, T1, T2> IntoVectorTrans2<T> for (T1, T2)
where
    T1: Into<T>,
    T2: Into<T>,
{
    fn into_vector_trans(self) -> Vector2<T> {
        Vector2::new(self.0.into(), self.1.into())
    }
}



// ========================
// === IntoVectorTrans3 ===
// ========================

/// Additional conversions from some struct to [`Vector3`]. Similar to [`IntoVector3`], but allows
// /// the provided arguments to contain values of different type than the resulting vector. This
// has /// a greater flexibility, but weakens the type inferencer capabilities. If you do not need
// the /// automatic conversion, use [`IntoVector3`] instead.
#[allow(missing_docs)]
pub trait IntoVectorTrans3<T> {
    fn into_vector_trans(self) -> Vector3<T>;
}

impl<T, S> IntoVectorTrans3<T> for Vector3<S>
where S: Scalar + Copy + Into<T>
{
    fn into_vector_trans(self) -> Vector3<T> {
        Vector3::new(self.x.into(), self.y.into(), self.z.into())
    }
}

impl<T, T1, T2, T3> IntoVectorTrans3<T> for (T1, T2, T3)
where
    T1: Into<T>,
    T2: Into<T>,
    T3: Into<T>,
{
    fn into_vector_trans(self) -> Vector3<T> {
        Vector3::new(self.0.into(), self.1.into(), self.2.into())
    }
}
