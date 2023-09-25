//! This module implements GPU-based texture support. Proper texture handling is a complex topic.
//! Follow the link to learn more about many assumptions this module was built upon:
//! https://developer.mozilla.org/en-US/docs/Web/API/WebGLRenderingContext/texImage2D

use crate::prelude::*;
use crate::system::gpu::data::gl_enum::traits::*;
use crate::system::gpu::data::gl_enum::*;

use crate::system::gpu::context::ContextLost;
use crate::system::gpu::data::buffer::item::JsBufferViewArr;
use crate::system::gpu::Context;

use web_sys::WebGlTexture;


// ==============
// === Export ===
// ==============

pub mod types;

pub use types::*;



/// Provides smart scope for item types.
pub mod item_type {
    pub use super::types::item_type::AnyItemType::*;
}



// ===============
// === Texture ===
// ===============

/// Texture bound to a GL context.
#[derive(Debug)]
pub struct Texture {
    width:           i32,
    height:          i32,
    layers:          i32,
    gl_texture:      WebGlTexture,
    context:         Context,
    item_type:       AnyItemType,
    internal_format: AnyInternalFormat,
}

impl Texture {
    /// Allocate a texture on the GPU, and set its parameters.
    pub fn new(
        context: &Context,
        internal_format: AnyInternalFormat,
        item_type: AnyItemType,
        width: i32,
        height: i32,
        layers: i32,
        parameters: Parameters,
    ) -> Result<Self, ContextLost> {
        let context = context.clone();
        let gl_texture = context.create_texture()?;
        let this = Self { width, height, layers, gl_texture, context, item_type, internal_format };
        this.init(parameters);
        Ok(this)
    }

    /// Returns the texture target.
    pub fn target(&self) -> GlEnum {
        match self.layers {
            0 => Context::TEXTURE_2D,
            _ => Context::TEXTURE_2D_ARRAY,
        }
    }

    /// Returns the width, in pixels.
    pub fn width(&self) -> i32 {
        self.width
    }

    /// Returns the height, in pixels.
    pub fn height(&self) -> i32 {
        self.height
    }

    /// Returns the number of layers.
    pub fn layers(&self) -> i32 {
        self.layers
    }

    /// Reloads gpu texture with data from given slice.
    pub fn reload_with_content<T: JsBufferViewArr + bytemuck::Pod>(&self, data: &[T]) {
        let target = self.target();
        let level = 0;
        let (xoffset, yoffset, zoffset) = default();
        let format = self.internal_format.format().to_gl_enum().into();
        let elem_type = self.item_type.to_gl_enum().into();
        let data: &[u8] = bytemuck::cast_slice(data);
        self.context.bind_texture(*target, Some(&self.gl_texture));
        let error = match self.layers {
            0 => self
                .context
                .tex_sub_image_2d_with_i32_and_i32_and_u32_and_type_and_opt_u8_array(
                    *target,
                    level,
                    xoffset,
                    yoffset,
                    self.width,
                    self.height,
                    format,
                    elem_type,
                    Some(data),
                )
                .err(),
            _ => self
                .context
                .tex_sub_image_3d_with_opt_u8_array(
                    *target,
                    level,
                    xoffset,
                    yoffset,
                    zoffset,
                    self.width,
                    self.height,
                    self.layers,
                    format,
                    elem_type,
                    Some(data),
                )
                .err(),
        };
        if let Some(error) = error {
            if !self.context.is_context_lost() {
                error!("Error in `texSubImage`: {error:?}.");
            }
        }
    }

    /// Bind this texture to the specified texture unit on the GPU.
    pub fn bind_texture_unit(&self, unit: TextureUnit) -> TextureBindGuard {
        let context = self.context.clone();
        let target = self.target();
        context.active_texture(*Context::TEXTURE0 + unit.to::<u32>());
        context.bind_texture(*target, Some(&self.gl_texture));
        context.active_texture(*Context::TEXTURE0);
        TextureBindGuard { context, target, unit }
    }

    /// Access the raw WebGL texture object.
    pub fn as_gl_texture(&self) -> &WebGlTexture {
        &self.gl_texture
    }

    /// Get the format of the texture.
    pub fn get_format(&self) -> AnyFormat {
        self.internal_format.format()
    }

    /// Get the texture's item type.
    pub fn get_item_type(&self) -> AnyItemType {
        self.item_type
    }
}


// === Internal API ===

impl Texture {
    /// Allocate GPU memory for the texture, and apply the specified parameters.
    fn init(&self, parameters: Parameters) {
        let levels = 1;
        let internal_format = self.internal_format.to_gl_enum().into();
        let target = self.target();
        self.context.bind_texture(*target, Some(&self.gl_texture));
        match self.layers {
            0 => {
                self.context.tex_storage_2d(
                    *target,
                    levels,
                    internal_format,
                    self.width,
                    self.height,
                );
            }
            _ => {
                self.context.tex_storage_3d(
                    *target,
                    levels,
                    internal_format,
                    self.width,
                    self.height,
                    self.layers,
                );
            }
        }
        parameters.apply_parameters(&self.context, self.target());
    }
}

impl Drop for Texture {
    fn drop(&mut self) {
        self.context.delete_texture(&self.gl_texture);
    }
}



// ===================
// === TextureUnit ===
// ===================

/// A texture unit representation in WebGl.
#[derive(Copy, Clone, Debug, Display, From, Into)]
pub struct TextureUnit(u32);



// ========================
// === TextureBindGuard ===
// ========================

/// Guard which unbinds texture in specific texture unit on drop.
#[derive(Debug)]
pub struct TextureBindGuard {
    context: Context,
    target:  GlEnum,
    unit:    TextureUnit,
}

impl Drop for TextureBindGuard {
    fn drop(&mut self) {
        self.context.active_texture(*Context::TEXTURE0 + self.unit.to::<u32>());
        self.context.bind_texture(*self.target, None);
        self.context.active_texture(*Context::TEXTURE0);
    }
}



// ==================
// === Parameters ===
// ==================

/// Helper struct to specify texture parameters that need to be set when binding a texture.
///
/// The essential parameters that need to be set are about how the texture will be sampled, i.e.,
/// how the values of the texture are interpolated at various resolutions, and how out of bounds
/// samples are handled.
///
/// For more background see:
/// https://developer.mozilla.org/en-US/docs/Web/API/WebGLRenderingContext/texParameter
#[derive(Copy, Clone, Debug, Default)]
pub struct Parameters {
    /// Specifies the setting for the texture minification filter (`Context::TEXTURE_MIN_FILTER`).
    pub min_filter: MinFilter,
    /// Specifies the setting for the texture magnification filter (`Context::TEXTURE_MAG_FILTER`).
    pub mag_filter: MagFilter,
    /// Specifies the setting for the wrapping function for texture coordinate s
    /// (`Context::TEXTURE_WRAP_S`).
    pub wrap_s:     Wrap,
    /// Specifies the setting for the wrapping function for texture coordinate t
    /// (`Context::TEXTURE_WRAP_T`).
    pub wrap_t:     Wrap,
}

impl Parameters {
    /// Applies the context parameters in the given context.
    pub fn apply_parameters(self, context: &Context, target: GlEnum) {
        context.tex_parameteri(*target, *Context::TEXTURE_MIN_FILTER, *self.min_filter as i32);
        context.tex_parameteri(*target, *Context::TEXTURE_MAG_FILTER, *self.mag_filter as i32);
        context.tex_parameteri(*target, *Context::TEXTURE_WRAP_S, *self.wrap_s as i32);
        context.tex_parameteri(*target, *Context::TEXTURE_WRAP_T, *self.wrap_t as i32);
    }
}


// === Parameter Types ===

/// Define a type that can represent a subset of `GlEnum` values defined in the `Context`. The
/// resulting type is not an enum, so it cannot be exhaustively pattern-matched, but conversion to a
/// `GlEnum` is zero-cost.
macro_rules! gl_enum_subset {
    ($(#[$($attrs:tt)*])* $ty:ident: $base:ty, [$($value:ident),*]) => {
        $(#[$($attrs)*])*
        #[derive(Copy, Clone, Debug, PartialEq, Eq)]
        pub struct $ty($base);

        #[allow(missing_docs)]
        impl $ty {
            $(pub const $value: $ty = $ty(Context::$value);)*
        }
    }
}

gl_enum_subset!(
    /// Valid Parameters for the `gl.TEXTURE_MAG_FILTER` texture setting.
    ///
    /// Specifies how values are interpolated if the texture is rendered at a resolution that is
    /// *higher* than its native resolution.
    MagFilter: GlEnum,
    [LINEAR, NEAREST]
);

impl Deref for MagFilter {
    type Target = u32;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

// Note: The parameters implement our own default, not the WebGL one.
impl Default for MagFilter {
    fn default() -> Self {
        Self::LINEAR
    }
}

gl_enum_subset!(
    /// Valid Parameters for the `gl.TEXTURE_MIN_FILTER` texture setting.
    ///
    /// Specifies how values are interpolated if the texture is rendered at a resolution that is
    /// *lower* than its native resolution.
    MinFilter: GlEnum,
    [
        LINEAR,
        NEAREST,
        NEAREST_MIPMAP_NEAREST,
        LINEAR_MIPMAP_NEAREST,
        NEAREST_MIPMAP_LINEAR,
        LINEAR_MIPMAP_LINEAR
    ]
);

impl Deref for MinFilter {
    type Target = u32;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

// Note: The parameters implement our own default, not the WebGL one.
impl Default for MinFilter {
    fn default() -> Self {
        Self::LINEAR
    }
}

gl_enum_subset!(
    /// Valid Parameters for the `gl.TEXTURE_WRAP_S` and `gl.TEXTURE_WRAP_T` texture setting.
    ///
    /// Specifies what happens if a texture is sampled out of bounds.
    Wrap: GlEnum,
    [REPEAT, CLAMP_TO_EDGE, MIRRORED_REPEAT]
);

impl Deref for Wrap {
    type Target = u32;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

// Note: The parameters implement our own default, not the WebGL one.
impl Default for Wrap {
    fn default() -> Self {
        Self::CLAMP_TO_EDGE
    }
}
