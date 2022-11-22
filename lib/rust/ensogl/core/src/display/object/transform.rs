// === Non-Standard Linter Configuration ===
#![allow(missing_docs)]

use crate::prelude::*;



// =================
// === AxisOrder ===
// =================

/// Defines the order in which particular axis coordinates are processed. Used for example to define
/// the rotation order in `DisplayObject`.
#[derive(Clone, Copy, Debug)]
pub enum AxisOrder {
    XYZ,
    XZY,
    YXZ,
    YZX,
    ZXY,
    ZYX,
}

impl Default for AxisOrder {
    fn default() -> Self {
        Self::XYZ
    }
}



// ======================
// === TransformOrder ===
// ======================

/// Defines the order in which transformations (scale, rotate, translate) are applied to a
/// particular object.
#[derive(Clone, Copy, Debug)]
pub enum TransformOrder {
    ScaleRotateTranslate,
    ScaleTranslateRotate,
    RotateScaleTranslate,
    RotateTranslateScale,
    TranslateRotateScale,
    TranslateScaleRotate,
}

impl Default for TransformOrder {
    fn default() -> Self {
        Self::ScaleRotateTranslate
    }
}



// =================
// === Transform ===
// =================

/// Structure describing transform of an object, in particular its position, scale, and rotation.
/// You can use methods like `matrix` to get a combined transformation matrix. Bear in mind that
/// the matrix will always be recomputed from scratch. This structure does not contain any caching
/// mechanisms.
#[derive(Clone, Copy, Debug)]
pub struct Transform {
    pub position:        Vector3<f32>,
    pub scale:           Vector3<f32>,
    pub rotation:        Vector3<f32>,
    pub transform_order: TransformOrder,
    pub rotation_order:  AxisOrder,
}

impl Default for Transform {
    fn default() -> Self {
        let position = Vector3::new(0.0, 0.0, 0.0);
        let scale = Vector3::new(1.0, 1.0, 1.0);
        let rotation = Vector3::new(0.0, 0.0, 0.0);
        let transform_order = default();
        let rotation_order = default();
        Self { position, scale, rotation, transform_order, rotation_order }
    }
}

impl Transform {
    /// Creates a new transformation object.
    pub fn new() -> Self {
        default()
    }

    /// Computes transformation matrix from the provided scale, rotation, and
    /// translation components, based on the transformation and rotation orders.
    pub fn matrix(&self) -> Matrix4<f32> {
        let mut matrix = Matrix4::identity();
        let matrix_ref = &mut matrix;
        match self.transform_order {
            TransformOrder::ScaleRotateTranslate => {
                self.append_scale(matrix_ref);
                self.append_rotation(matrix_ref);
                self.append_translation(matrix_ref);
            }
            TransformOrder::ScaleTranslateRotate => {
                self.append_scale(matrix_ref);
                self.append_translation(matrix_ref);
                self.append_rotation(matrix_ref);
            }
            TransformOrder::RotateScaleTranslate => {
                self.append_rotation(matrix_ref);
                self.append_scale(matrix_ref);
                self.append_translation(matrix_ref);
            }
            TransformOrder::RotateTranslateScale => {
                self.append_rotation(matrix_ref);
                self.append_translation(matrix_ref);
                self.append_scale(matrix_ref);
            }
            TransformOrder::TranslateRotateScale => {
                self.append_translation(matrix_ref);
                self.append_rotation(matrix_ref);
                self.append_scale(matrix_ref);
            }
            TransformOrder::TranslateScaleRotate => {
                self.append_translation(matrix_ref);
                self.append_scale(matrix_ref);
                self.append_rotation(matrix_ref);
            }
        }
        matrix
    }

    /// Computes a rotation matrix from the provided rotation values based on
    /// the rotation order.
    pub fn rotation_matrix(&self) -> Matrix4<f32> {
        let rx = Matrix4::from_scaled_axis(Vector3::x() * self.rotation.x);
        let ry = Matrix4::from_scaled_axis(Vector3::y() * self.rotation.y);
        let rz = Matrix4::from_scaled_axis(Vector3::z() * self.rotation.z);
        match self.rotation_order {
            AxisOrder::XYZ => rz * ry * rx,
            AxisOrder::XZY => ry * rz * rx,
            AxisOrder::YXZ => rz * rx * ry,
            AxisOrder::YZX => rx * rz * ry,
            AxisOrder::ZXY => ry * rx * rz,
            AxisOrder::ZYX => rx * ry * rz,
        }
    }

    fn append_translation(&self, m: &mut Matrix4<f32>) {
        m.append_translation_mut(&self.position);
    }

    fn append_rotation(&self, m: &mut Matrix4<f32>) {
        *m = self.rotation_matrix() * (*m);
    }

    fn append_scale(&self, m: &mut Matrix4<f32>) {
        m.append_nonuniform_scaling_mut(&self.scale);
    }
}



// =======================
// === CachedTransform ===
// =======================

/// The same as `Transform` but with caching. It contains cached transformation matrix and dirty
/// flags which are set after fields are modified. You can use the `update` function to recompute
/// the matrix.
#[derive(Clone, Debug)]
#[allow(missing_copy_implementations)]
pub struct CachedTransform {
    transform:        Transform,
    transform_matrix: Matrix4<f32>,
    origin:           Matrix4<f32>,
    pub matrix:       Matrix4<f32>,
    pub dirty:        bool,
}

impl Default for CachedTransform {
    fn default() -> Self {
        let transform = default();
        let transform_matrix = Matrix4::identity();
        let origin = Matrix4::identity();
        let matrix = Matrix4::identity();
        let dirty = default();
        Self { transform, transform_matrix, origin, matrix, dirty }
    }
}

impl CachedTransform {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }

    /// Update the transformation matrix and return information if the data was really updated.
    pub fn update(&mut self, new_origin: Option<Matrix4<f32>>) -> bool {
        let origin_changed = new_origin.is_some();
        let changed = self.dirty || origin_changed;
        if changed {
            if self.dirty {
                self.transform_matrix = self.transform.matrix();
                self.dirty = false;
            }
            new_origin.into_iter().for_each(|t| self.origin = t);
            self.matrix = self.origin * self.transform_matrix;
        }
        changed
    }
}


// === Getters ===

impl CachedTransform {
    pub fn position(&self) -> Vector3<f32> {
        self.transform.position
    }

    pub fn rotation(&self) -> Vector3<f32> {
        self.transform.rotation
    }

    pub fn scale(&self) -> Vector3<f32> {
        self.transform.scale
    }

    pub fn matrix(&self) -> Matrix4<f32> {
        self.matrix
    }

    pub fn global_position(&self) -> Vector3<f32> {
        (self.matrix * Vector4::new(0.0, 0.0, 0.0, 1.0)).xyz()
    }
}


// === Setters ===

impl CachedTransform {
    pub fn position_mut(&mut self) -> &mut Vector3<f32> {
        self.dirty = true;
        &mut self.transform.position
    }

    pub fn rotation_mut(&mut self) -> &mut Vector3<f32> {
        self.dirty = true;
        &mut self.transform.rotation
    }

    pub fn scale_mut(&mut self) -> &mut Vector3<f32> {
        self.dirty = true;
        &mut self.transform.scale
    }

    pub fn set_position(&mut self, t: Vector3<f32>) {
        *self.position_mut() = t;
    }

    pub fn set_rotation(&mut self, t: Vector3<f32>) {
        *self.rotation_mut() = t;
    }

    pub fn set_scale(&mut self, t: Vector3<f32>) {
        *self.scale_mut() = t;
    }

    pub fn mod_position<F: FnOnce(&mut Vector3<f32>)>(&mut self, f: F) {
        f(self.position_mut());
    }

    pub fn mod_rotation<F: FnOnce(&mut Vector3<f32>)>(&mut self, f: F) {
        f(self.rotation_mut());
    }

    pub fn mod_scale<F: FnOnce(&mut Vector3<f32>)>(&mut self, f: F) {
        f(self.scale_mut());
    }
}
