use crate::prelude::*;

use nalgebra::Matrix4;
use nalgebra::RealField;
use js_sys::Float32Array;
use std::marker::PhantomData;


// =====================
// === IntoCSSMatrix ===
// =====================

pub trait IntoCSSMatrix {
    fn into_css_matrix(&self) -> String;
}

impl<T : RealField> IntoCSSMatrix for Matrix4<T> {
    fn into_css_matrix(&self) -> String {
        let mut iter = self.iter();
        let item     = iter.next().expect("Matrix4 should have the first item");
        let acc      = format!("{}", item);
        let ret      = iter.fold(acc, |acc, item| format!("{}, {}", acc, item));
        format!("matrix3d({})", ret)
    }
}


// ========================
// === Float32ArrayView ===
// ========================

/// A Float32Array view created from `IntoFloat32ArrayView`.
#[derive(Shrinkwrap)]
pub struct Float32ArrayView<'a> {
    #[shrinkwrap(main_field)]
    array   : Float32Array,
    phantom : PhantomData<&'a Float32Array>
}

pub trait IntoFloat32ArrayView {
    /// # Safety
    /// Views into WebAssembly memory are only valid so long as the backing buffer isn't resized in
    /// JS. Once this function is called any future calls to Box::new (or malloc of any form) may
    /// cause the returned value here to be invalidated. Use with caution!
    ///
    /// Additionally the returned object can be safely mutated but the input slice isn't guaranteed
    /// to be mutable.
    unsafe fn into_float32_array_view(&self) -> Float32ArrayView<'_>;
}

impl IntoFloat32ArrayView for Matrix4<f32> {
    unsafe fn into_float32_array_view(&self) -> Float32ArrayView<'_> {
        let matrix = matrix4_to_array(&self);
        let array = Float32Array::view(matrix);
        let phantom = PhantomData;
        Float32ArrayView { array, phantom }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn into_css_matrix() {
        use super::Matrix4;
        use super::IntoCSSMatrix;

        let matrix = Matrix4::new
            ( 1.0, 5.0,  9.0, 13.0
            , 2.0, 6.0, 10.0, 14.0
            , 3.0, 7.0, 11.0, 15.0
            , 4.0, 8.0, 12.0, 16.0 );
        let column_major = matrix.into_css_matrix();
        let expected = "matrix3d(1, 2, 3, 4, \
                                 5, 6, 7, 8, \
                                 9, 10, 11, 12, \
                                13, 14, 15, 16)";
        assert_eq!(column_major, expected);
    }

    #[test]
    fn matrix4_memory_layout() {
        use super::Matrix4;
        use super::matrix4_to_array;

        let matrix = Matrix4::<f32>::new
            ( 1.0, 5.0,  9.0, 13.0
            , 2.0, 6.0, 10.0, 14.0
            , 3.0, 7.0, 11.0, 15.0
            , 4.0, 8.0, 12.0, 16.0 );

        let layout = matrix4_to_array(&matrix);
        for (i, n) in layout.iter().enumerate() {
            assert_eq!((i + 1) as f32, *n);
        }
    }
}

// ============
// === Misc ===
// ============

pub fn matrix4_to_array(matrix:&Matrix4<f32>) -> &[f32; 16] {
    // To interpret [[f32; 4]; 4] as [f32; 16].
    // The memory layout is the same, so this operation is safe.
    unsafe {
        &*(matrix.as_ref() as *const [[f32; 4]; 4] as *const [f32; 16])
    }
}

// eps is used to round very small values to 0.0 for numerical stability
pub fn eps(value: f32) -> f32 {
    if value.abs() < 1e-10 { 0.0 } else { value }
}

// Inverts Matrix Y coordinates.
// It's equivalent to scaling by (1.0, -1.0, 1.0).
pub fn invert_y(mut m: Matrix4<f32>) -> Matrix4<f32> {
    // Negating the second column to invert Y.
    m.row_part_mut(1, 4).iter_mut().for_each(|a| *a = -*a);
    m
}