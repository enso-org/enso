//! This module defines abstraction for uniform uploading. WebGL defines a lot of functions for
//! uploading particular data shapes. Fortunately, Rust is strongly typed, so we can establish a
//! single abstraction for data uploading.

use crate::types::*;

use crate::system::gpu::Context;

use nalgebra::storage::Storage;
use web_sys::WebGlUniformLocation;



// =====================
// === UniformUpload ===
// =====================

/// Abstraction for uploading uniforms to GPU based on their types.
pub trait UniformUpload {
    fn upload_uniform(&self, context: &Context, location: &WebGlUniformLocation);
}

impl UniformUpload for bool {
    fn upload_uniform(&self, context: &Context, location: &WebGlUniformLocation) {
        let value = if *self { 1 } else { 0 };
        context.uniform1i(Some(location), value);
    }
}

impl UniformUpload for i32 {
    fn upload_uniform(&self, context: &Context, location: &WebGlUniformLocation) {
        context.uniform1i(Some(location), *self);
    }
}

impl UniformUpload for u32 {
    fn upload_uniform(&self, context: &Context, location: &WebGlUniformLocation) {
        context.uniform1ui(Some(location), *self);
    }
}

impl UniformUpload for f32 {
    fn upload_uniform(&self, context: &Context, location: &WebGlUniformLocation) {
        context.uniform1f(Some(location), *self);
    }
}


// === Vector ===

impl UniformUpload for Vector2<f32> {
    fn upload_uniform(&self, context: &Context, location: &WebGlUniformLocation) {
        context.uniform2fv_with_f32_array(Some(location), self.data.as_slice());
    }
}

impl UniformUpload for Vector3<f32> {
    fn upload_uniform(&self, context: &Context, location: &WebGlUniformLocation) {
        context.uniform3fv_with_f32_array(Some(location), self.data.as_slice());
    }
}

impl UniformUpload for Vector4<f32> {
    fn upload_uniform(&self, context: &Context, location: &WebGlUniformLocation) {
        context.uniform4fv_with_f32_array(Some(location), self.data.as_slice());
    }
}

impl UniformUpload for Vector2<i32> {
    fn upload_uniform(&self, context: &Context, location: &WebGlUniformLocation) {
        context.uniform2iv_with_i32_array(Some(location), self.data.as_slice());
    }
}

impl UniformUpload for Vector3<i32> {
    fn upload_uniform(&self, context: &Context, location: &WebGlUniformLocation) {
        context.uniform3iv_with_i32_array(Some(location), self.data.as_slice());
    }
}

impl UniformUpload for Vector4<i32> {
    fn upload_uniform(&self, context: &Context, location: &WebGlUniformLocation) {
        context.uniform4iv_with_i32_array(Some(location), self.data.as_slice());
    }
}

impl UniformUpload for Vector2<u32> {
    fn upload_uniform(&self, context: &Context, location: &WebGlUniformLocation) {
        context.uniform2uiv_with_u32_array(Some(location), self.data.as_slice());
    }
}

impl UniformUpload for Vector3<u32> {
    fn upload_uniform(&self, context: &Context, location: &WebGlUniformLocation) {
        context.uniform3uiv_with_u32_array(Some(location), self.data.as_slice());
    }
}

impl UniformUpload for Vector4<u32> {
    fn upload_uniform(&self, context: &Context, location: &WebGlUniformLocation) {
        context.uniform4uiv_with_u32_array(Some(location), self.data.as_slice());
    }
}

impl UniformUpload for Vector2<bool> {
    fn upload_uniform(&self, context: &Context, location: &WebGlUniformLocation) {
        let v: Vec<i32> =
            self.data.as_slice().iter().cloned().map(|t| if t { 1 } else { 0 }).collect();
        context.uniform2iv_with_i32_array(Some(location), &v);
    }
}

impl UniformUpload for Vector3<bool> {
    fn upload_uniform(&self, context: &Context, location: &WebGlUniformLocation) {
        let v: Vec<i32> =
            self.data.as_slice().iter().cloned().map(|t| if t { 1 } else { 0 }).collect();
        context.uniform3iv_with_i32_array(Some(location), &v);
    }
}

impl UniformUpload for Vector4<bool> {
    fn upload_uniform(&self, context: &Context, location: &WebGlUniformLocation) {
        let v: Vec<i32> =
            self.data.as_slice().iter().cloned().map(|t| if t { 1 } else { 0 }).collect();
        context.uniform4iv_with_i32_array(Some(location), &v);
    }
}


// === Matrix ===

impl UniformUpload for Matrix2<f32> {
    fn upload_uniform(&self, context: &Context, location: &WebGlUniformLocation) {
        context.uniform_matrix2fv_with_f32_array(Some(location), false, self.data.as_slice());
    }
}

impl UniformUpload for Matrix3<f32> {
    fn upload_uniform(&self, context: &Context, location: &WebGlUniformLocation) {
        context.uniform_matrix3fv_with_f32_array(Some(location), false, self.data.as_slice());
    }
}

impl UniformUpload for Matrix4<f32> {
    fn upload_uniform(&self, context: &Context, location: &WebGlUniformLocation) {
        context.uniform_matrix4fv_with_f32_array(Some(location), false, self.data.as_slice());
    }
}

impl UniformUpload for Matrix2x3<f32> {
    fn upload_uniform(&self, _context: &Context, _location: &WebGlUniformLocation) {
        todo!() // FIXME: https://github.com/rustwasm/wasm-bindgen/issues/1956
                // context.uniform_matrix2x3fv_with_f32_array(Some(location),false,self.data.
                // as_slice());
    }
}

impl UniformUpload for Matrix2x4<f32> {
    fn upload_uniform(&self, _context: &Context, _location: &WebGlUniformLocation) {
        todo!() // FIXME: https://github.com/rustwasm/wasm-bindgen/issues/1956
                // context.uniform_matrix2x4fv_with_f32_array(Some(location),false,self.data.
                // as_slice());
    }
}

impl UniformUpload for Matrix3x2<f32> {
    fn upload_uniform(&self, _context: &Context, _location: &WebGlUniformLocation) {
        todo!() // FIXME: https://github.com/rustwasm/wasm-bindgen/issues/1956
                // context.uniform_matrix3x2fv_with_f32_array(Some(location),false,self.data.
                // as_slice());
    }
}

impl UniformUpload for Matrix3x4<f32> {
    fn upload_uniform(&self, _context: &Context, _location: &WebGlUniformLocation) {
        todo!() // FIXME: https://github.com/rustwasm/wasm-bindgen/issues/1956
                // context.uniform_matrix3x4fv_with_f32_array(Some(location),false,self.data.
                // as_slice());
    }
}

impl UniformUpload for Matrix4x2<f32> {
    fn upload_uniform(&self, _context: &Context, _location: &WebGlUniformLocation) {
        todo!() // FIXME: https://github.com/rustwasm/wasm-bindgen/issues/1956
                // context.uniform_matrix4x2fv_with_f32_array(Some(location),false,self.data.
                // as_slice());
    }
}

impl UniformUpload for Matrix4x3<f32> {
    fn upload_uniform(&self, _context: &Context, _location: &WebGlUniformLocation) {
        todo!() // FIXME: https://github.com/rustwasm/wasm-bindgen/issues/1956
                // context.uniform_matrix4x3fv_with_f32_array(Some(location),false,self.data.
                // as_slice());
    }
}
