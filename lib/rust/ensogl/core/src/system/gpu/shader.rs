// === Non-Standard Linter Configuration ===
#![allow(missing_docs)]

use enso_prelude::*;

use crate::system::Context;

use js_sys::Float32Array;
use web_sys::WebGlBuffer;
use web_sys::WebGlProgram;
use web_sys::WebGlShader;



// ===============
// === Exports ===
// ===============

#[warn(missing_docs)]
pub mod glsl;
pub mod compiler;
/// Common types.
pub mod types {
    pub use super::glsl;
    pub use glsl::traits::*;
    pub use glsl::Glsl;
}
pub use types::*;
pub use compiler::Compiler;



// =============
// === Types ===
// =============

pub type Shader = WebGlShader;
pub type Program = WebGlProgram;



// ==============
// === VfPair ===
// ==============

#[derive(Copy, Clone, Debug)]
pub struct VfPair<T> {
    pub vertex:   T,
    pub fragment: T,
}

impl<T> VfPair<T> {
    /// Apply a function to both shaders.
    pub fn for_each<F>(self, mut f: F) where F: FnMut(T) {
        f(self.vertex);
        f(self.fragment);
    }

    /// Apply a function to both shaders, stopping if an error is encountered.
    pub fn try_for_each<F, E>(self, mut f: F) -> Result<(), E>
        where F: FnMut(T) -> Result<(), E> {
        f(self.vertex)?;
        f(self.fragment)?;
        Ok(())
    }

    /// Apply a function to both shaders; return the pair of results.
    pub fn map<F, U>(self, mut f: F) -> VfPair<U> where F: FnMut(T) -> U {
        let Self { vertex, fragment } = self;
        let vertex = f(vertex);
        let fragment = f(fragment);
        VfPair { vertex, fragment }
    }

    /// Apply a function that returns an option to both shaders; return the pair of results, unless
    /// the function returned None for either shader.
    pub fn maybe_map<F, U>(self, mut f: F) -> Option<VfPair<U>> where F: FnMut(T) -> Option<U> {
        let Self { vertex, fragment } = self;
        let vertex = f(vertex)?;
        let fragment = f(fragment)?;
        Some(VfPair { vertex, fragment })
    }

    /// Combine the vertex components from each value, and the fragment components from each value,
    /// into a new pair of tuples.
    pub fn zip<U>(self, other: VfPair<U>) -> VfPair<(T, U)> {
        let vertex = (self.vertex, other.vertex);
        let fragment = (self.fragment, other.fragment);
        VfPair { vertex, fragment }
    }

    /// Return a pair borrowing its values from this pair.
    pub fn as_ref(&self) -> VfPair<&T> {
        let vertex = &self.vertex;
        let fragment = &self.fragment;
        VfPair { vertex, fragment }
    }
}



// ==================
// === ShaderCode ===
// ==================

/// GLSL source code of a shader program.
pub type ShaderCode = VfPair<String>;



// ========================
// === Managing buffers ===
// ========================

// TODO: The functions below might be obsolete after text is fully integrated to buffer management.

/// Set the array buffer data with floats.
pub fn set_buffer_data(gl_context: &Context, buffer: &WebGlBuffer, data: &[f32]) {
    let target = Context::ARRAY_BUFFER;
    gl_context.bind_buffer(target, Some(buffer));
    set_bound_buffer_data(gl_context, target, data);
}

/// Set data in currently bound buffer.
///
/// # Safety
/// The Float32Array::view is safe as long there are no allocations done
/// until it is destroyed. This way of creating buffers were taken from
/// wasm-bindgen examples
/// (https://rustwasm.github.io/wasm-bindgen/examples/webgl.html).
#[allow(unsafe_code)]
fn set_bound_buffer_data(gl_context: &Context, target: u32, data: &[f32]) {
    let usage = Context::STATIC_DRAW;
    unsafe {
        let float_array = Float32Array::view(data);
        gl_context.buffer_data_with_array_buffer_view(target, &float_array, usage);
    }
}

/// Set the array buffer fragment with with floats.
pub fn set_buffer_subdata(gl_context: &Context, buffer: &WebGlBuffer, offset: usize, data: &[f32]) {
    let target = Context::ARRAY_BUFFER;
    gl_context.bind_buffer(target, Some(buffer));
    set_bound_buffer_subdata(gl_context, target, offset as i32, data);
}

/// Set subdata in currently bound buffer.
///
/// # Safety
/// The Float32Array::view is safe as long there are no allocations done
/// until it is destroyed. This way of creating buffers were taken from
/// wasm-bindgen examples
/// (https://rustwasm.github.io/wasm-bindgen/examples/webgl.html).
#[allow(unsafe_code)]
fn set_bound_buffer_subdata(gl_context: &Context, target: u32, offset: i32, data: &[f32]) {
    unsafe {
        let float_array = Float32Array::view(data);
        gl_context.buffer_sub_data_with_i32_and_array_buffer_view(target, offset, &float_array);
    }
}
