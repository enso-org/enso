//! Abstractions for GPU shaders and shader programs.

use enso_prelude::*;

use crate::display::GlEnum;
use crate::display::ToGlEnum;

use enso_shapely::define_singleton_enum;
use web_sys::WebGl2RenderingContext;
use web_sys::WebGlProgram;
use web_sys::WebGlShader;


// ==============
// === Export ===
// ==============

pub mod compiler;
pub mod glsl;

pub use compiler::Compiler;
pub use types::*;



/// Common types.
pub mod types {
    pub use super::glsl;
    pub use glsl::traits::*;
    pub use glsl::Glsl;
}



// ============
// === Type ===
// ============

define_singleton_enum! {
    /// The shader type. Currently, only vertex and fragment shaders are supported (in WebGL 2.0).
    Type {
        Vertex, Fragment
    }
}

impl ToGlEnum for Type {
    fn to_gl_enum(&self) -> GlEnum {
        match self {
            Self::Vertex => GlEnum(WebGl2RenderingContext::VERTEX_SHADER),
            Self::Fragment => GlEnum(WebGl2RenderingContext::FRAGMENT_SHADER),
        }
    }
}



// ===============
// === Sources ===
// ===============

/// Abstractions for shader sources (vertex and fragment one).
#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub struct Sources<Vertex, Fragment> {
    pub vertex:   Vertex,
    pub fragment: Fragment,
}

/// Shader sources as a code.
pub type Code = Sources<String, String>;

/// Shader sources as compiled shaders that are not linked yet.
pub type CompiledCode = Sources<Shader<Vertex>, Shader<Fragment>>;

impl<V, F> Sources<V, F> {
    /// Apply [`Into::into`] to the [`vertex`] and [`fragment`] elements and return the result.
    pub fn into<V1, F1>(self) -> Sources<V1, F1>
    where
        V: Into<V1>,
        F: Into<F1>, {
        let Sources { vertex, fragment } = self;
        let vertex = vertex.into();
        let fragment = fragment.into();
        Sources { vertex, fragment }
    }

    /// Return references to the [`vertex`] and [`fragment`] elements.
    pub fn as_ref(&self) -> Sources<&V, &F> {
        let Sources { vertex, fragment } = self;
        let vertex = &vertex;
        let fragment = &fragment;
        Sources { vertex, fragment }
    }
}

impl<VF> Sources<VF, VF> {
    /// Apply a function to the [`vertex`] and [`fragment`] elements and return the result.
    pub fn map<VF1, F>(self, f: F) -> Sources<VF1, VF1>
    where F: Fn(VF) -> VF1 {
        let Sources { vertex, fragment } = self;
        let vertex = f(vertex);
        let fragment = f(fragment);
        Sources { vertex, fragment }
    }
}



// ==============
// === Shader ===
// ==============

/// A compiled shader of a given type.
#[allow(missing_docs)]
#[derive(AsRef, Clone, Debug, Deref, Eq, PartialEq)]
pub struct Shader<T> {
    #[as_ref]
    #[deref]
    pub native: WebGlShader,
    pub code:   ImString,
    tp:         ZST<T>,
}

impl<T> Shader<T> {
    /// Constructor.
    pub fn new(code: ImString, native: WebGlShader) -> Self {
        let tp = default();
        Self { native, code, tp }
    }
}



// ===============
// === Program ===
// ===============

/// A compiled shader program.
#[allow(missing_docs)]
#[derive(AsRef, Clone, Debug, Deref, Eq, PartialEq)]
pub struct Program {
    #[as_ref]
    #[deref]
    pub native: WebGlProgram,
    pub shader: CompiledCode,
}

impl Program {
    /// Constructor.
    pub fn new(shader: CompiledCode, native: WebGlProgram) -> Self {
        Self { native, shader }
    }
}
