//! This module implements GPU-based texture support. Proper texture handling is a complex topic.
//! Follow the link to learn more about many assumptions this module was built upon:
//! https://developer.mozilla.org/en-US/docs/Web/API/WebGLRenderingContext/texImage2D

use crate::prelude::*;
use crate::system::gpu::data::gl_enum::traits::*;
use crate::system::gpu::data::gl_enum::*;

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
        if self.context.is_valid() {
            self.context.active_texture(*Context::TEXTURE0 + self.unit.to::<u32>());
            self.context.bind_texture(*self.target, None);
            self.context.active_texture(*Context::TEXTURE0);
        }
    }
}



// ==================
// === Parameters ===
// ==================

/// Helper struct to specify texture parameters that need to be set when binding a texture.
///
/// The essential parameters that need to be set are about how the texture will be samples, i.e.,
/// how the values of the texture are interpolated at various resolutions, and how out of bounds
/// samples are handled.
///
/// For more background see: https://developer.mozilla.org/en-US/docs/Web/API/WebGLRenderingContext/texParameter
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

/// Valid Parameters for the `gl.TEXTURE_MAG_FILTER` texture setting.
///
/// Specifies how values are interpolated if the texture is rendered at a resolution that is
/// lower than its native resolution.
#[derive(Copy, Clone, Debug)]
pub struct MagFilter(GlEnum);

impl Deref for MagFilter {
    type Target = u32;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[allow(missing_docs)]
impl MagFilter {
    pub const LINEAR: MagFilter = MagFilter(Context::LINEAR);
    pub const NEAREST: MagFilter = MagFilter(Context::NEAREST);
}

// Note: The parameters implement our own default, not the WebGL one.
impl Default for MagFilter {
    fn default() -> Self {
        Self::LINEAR
    }
}

/// Valid Parameters for the `gl.TEXTURE_MIN_FILTER` texture setting.
///
/// Specifies how values are interpolated if the texture is rendered at a resolution that is
/// lower than its native resolution.
#[derive(Copy, Clone, Debug)]
pub struct MinFilter(GlEnum);

impl Deref for MinFilter {
    type Target = u32;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[allow(missing_docs)]
impl MinFilter {
    pub const LINEAR: MinFilter = MinFilter(Context::LINEAR);
    pub const NEAREST: MinFilter = MinFilter(Context::NEAREST);
    pub const NEAREST_MIPMAP_NEAREST: MinFilter = MinFilter(Context::NEAREST_MIPMAP_NEAREST);
    pub const LINEAR_MIPMAP_NEAREST: MinFilter = MinFilter(Context::LINEAR_MIPMAP_NEAREST);
    pub const NEAREST_MIPMAP_LINEAR: MinFilter = MinFilter(Context::NEAREST_MIPMAP_LINEAR);
    pub const LINEAR_MIPMAP_LINEAR: MinFilter = MinFilter(Context::LINEAR_MIPMAP_LINEAR);
}

// Note: The parameters implement our own default, not the WebGL one.
impl Default for MinFilter {
    fn default() -> Self {
        Self::LINEAR
    }
}

/// Valid Parameters for the `gl.TEXTURE_WRAP_S` and `gl.TEXTURE_WRAP_T` texture setting.
///
/// Specifies what happens if a texture is sampled out of bounds.
#[derive(Copy, Clone, Debug)]
pub struct Wrap(GlEnum);

impl Deref for Wrap {
    type Target = u32;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[allow(missing_docs)]
impl Wrap {
    pub const REPEAT: Wrap = Wrap(Context::REPEAT);
    pub const CLAMP_TO_EDGE: Wrap = Wrap(Context::CLAMP_TO_EDGE);
    pub const MIRRORED_REPEAT: Wrap = Wrap(Context::MIRRORED_REPEAT);
}

// Note: The parameters implement our own default, not the WebGL one.
impl Default for Wrap {
    fn default() -> Self {
        Self::CLAMP_TO_EDGE
    }
}



// ===============
// === Texture ===
// ===============

/// Texture bound to GL context.
#[derive(Debug)]
pub struct Texture<InternalFormat, ItemType> {
    storage:         GpuData,
    gl_texture:      WebGlTexture,
    context:         Context,
    parameters:      Parameters,
    internal_format: PhantomData<InternalFormat>,
    item_type:       PhantomData<ItemType>,
}


// === Traits ===

/// Reloading functionality for textured. It is also used for initial data population.
pub trait TextureReload {
    /// Loads or re-loads the texture data from provided source.
    fn reload(&self);
}


// === Type Level Utils ===

impl<I, T> Texture<I, T>
where
    I: InternalFormat,
    T: ItemType,
{
    /// Internal format instance of this texture.
    pub fn internal_format() -> AnyInternalFormat {
        <I>::default().into()
    }

    /// Format instance of this texture.
    pub fn format() -> AnyFormat {
        <I>::Format::default().into()
    }

    /// Internal format of this texture as `GlEnum`.
    pub fn gl_internal_format() -> i32 {
        let GlEnum(u) = Self::internal_format().to_gl_enum();
        u as i32
    }

    /// Format of this texture as `GlEnum`.
    pub fn gl_format() -> GlEnum {
        Self::format().to_gl_enum()
    }

    /// Element type of this texture as `GlEnum`.
    pub fn gl_elem_type() -> u32 {
        <T>::gl_enum().into()
    }

    /// Element type of this texture.
    pub fn item_type() -> AnyItemType {
        ZST::<T>().into()
    }
}


// === Getters ===

impl<I, T> Texture<I, T> {
    /// Getter.
    pub fn gl_texture(&self) -> &WebGlTexture {
        &self.gl_texture
    }

    /// Getter.
    pub fn context(&self) -> &Context {
        &self.context
    }

    /// Getter.
    pub fn storage(&self) -> &GpuData {
        &self.storage
    }

    /// Texture target getter.
    pub fn target(&self) -> GlEnum {
        match self.storage.layers {
            0 => Context::TEXTURE_2D,
            _ => Context::TEXTURE_2D_ARRAY,
        }
    }

    /// Getter.
    pub fn parameters(&self) -> &Parameters {
        &self.parameters
    }
}


// === Setters ===

impl<I, T> Texture<I, T> {
    /// Setter.
    pub fn set_parameters(&mut self, parameters: Parameters) {
        self.parameters = parameters;
    }
}


// === Constructors ===

impl<I: InternalFormat, T: ItemType> Texture<I, T>
where Self: TextureReload
{
    /// Constructor.
    pub fn new<P: Into<GpuData>>(context: &Context, provider: P) -> Self {
        let this = Self::new_uninitialized(context, provider);
        this.reload();
        this
    }
}


// === Destructos ===

impl<I, T> Drop for Texture<I, T> {
    fn drop(&mut self) {
        // Check before dropping; otherwise, WebGL will log an error when we delete a texture from a
        // previous context.
        if self.context.is_valid() && self.context.is_texture(Some(&self.gl_texture)) {
            self.context.delete_texture(Some(&self.gl_texture));
        }
    }
}


// === Internal API ===

impl<I, T> Texture<I, T> {
    /// New, uninitialized constructor. If you are not implementing a custom texture format, you
    /// should probably use `new` instead.
    pub fn new_uninitialized<X: Into<GpuData>>(context: &Context, storage: X) -> Self {
        let storage = storage.into();
        let context = context.clone();
        let gl_texture = context.create_texture().unwrap();
        let parameters = default();
        let internal_format = default();
        let item_type = default();
        Self { storage, gl_texture, context, parameters, internal_format, item_type }
    }

    /// Applies this textures' parameters in the given context.
    pub fn apply_texture_parameters(&self, context: &Context) {
        self.parameters.apply_parameters(context, self.target());
    }
}

impl<I, T> Texture<I, T>
where
    I: InternalFormat,
    T: ItemType + JsBufferViewArr,
{
    /// Reloads gpu texture with data from given slice.
    pub fn reload_from_memory(&self, data: &[T], width: i32, height: i32, depth: i32) {
        let target = self.target();
        let level = 0;
        let border = 0;
        let internal_format = Self::gl_internal_format();
        let format = Self::gl_format().into();
        let elem_type = Self::gl_elem_type();
        let data: &[u8] = bytemuck::cast_slice(data);
        self.context.bind_texture(*target, Some(&self.gl_texture));
        match target {
            Context::TEXTURE_2D => {
                self.context
                    .tex_image_2d_with_i32_and_i32_and_i32_and_format_and_type_and_opt_u8_array(
                        *target,
                        level,
                        internal_format,
                        width,
                        height,
                        border,
                        format,
                        elem_type,
                        Some(data),
                    )
                    .unwrap();
            }
            Context::TEXTURE_2D_ARRAY | Context::TEXTURE_3D => {
                self.context
                    .tex_image_3d_with_opt_u8_array(
                        *target,
                        level,
                        internal_format,
                        width,
                        height,
                        depth,
                        border,
                        format,
                        elem_type,
                        Some(data),
                    )
                    .unwrap();
            }
            _ => {
                panic!("Unsupported texture target: {target:?}");
            }
        }
        self.apply_texture_parameters(&self.context);
    }
}


// === Instances ===

impl<I, T> HasItem for Texture<I, T> {
    type Item = Texture<I, T>;
}

impl<I, T> ItemRef for Texture<I, T> {
    fn item(&self) -> &Self::Item {
        self
    }
}



// ==================
// === TextureOps ===
// ==================

/// API of the texture. It is defined as trait and uses the `WithContent` mechanism in order for
/// uniforms to easily redirect the methods.
pub trait TextureOps {
    /// Bind texture to a specific unit.
    fn bind_texture_unit(&self, context: &Context, unit: TextureUnit) -> TextureBindGuard;

    /// Accessor.
    fn gl_texture(&self) -> WebGlTexture;

    /// Accessor.
    fn get_format(&self) -> AnyFormat;

    /// Accessor.
    fn get_item_type(&self) -> AnyItemType;
}

impl<P: WithItemRef<Item = Texture<I, T>>, I: InternalFormat, T: ItemType> TextureOps for P {
    fn bind_texture_unit(&self, context: &Context, unit: TextureUnit) -> TextureBindGuard {
        self.with_item(|this| {
            let context = context.clone();
            let target = this.target();
            context.active_texture(*Context::TEXTURE0 + unit.to::<u32>());
            context.bind_texture(*target, Some(&this.gl_texture));
            context.active_texture(*Context::TEXTURE0);
            TextureBindGuard { context, target, unit }
        })
    }

    fn gl_texture(&self) -> WebGlTexture {
        self.with_item(|this| this.gl_texture.clone())
    }

    fn get_format(&self) -> AnyFormat {
        self.with_item(|_| <Texture<I, T>>::format())
    }

    fn get_item_type(&self) -> AnyItemType {
        self.with_item(|_| <Texture<I, T>>::item_type())
    }
}



// ===============
// === GpuData ===
// ===============

/// Sized, uninitialized texture.
#[derive(Clone, Copy, Debug)]
pub struct GpuData {
    /// Texture width.
    pub width:  i32,
    /// Texture height.
    pub height: i32,
    /// Number of texture layers. When the texture is not layered, this value is 0.
    pub layers: i32,
}


// === Instances ===

impl From<(i32, i32)> for GpuData {
    fn from(t: (i32, i32)) -> Self {
        let (width, height) = t;
        Self { width, height, layers: 0 }
    }
}

impl From<((i32, i32), i32)> for GpuData {
    fn from(t: ((i32, i32), i32)) -> Self {
        let ((width, height), layers) = t;
        Self { width, height, layers }
    }
}



// === API ===

impl GpuData {
    fn new(width: i32, height: i32) -> Self {
        Self { width, height, layers: 0 }
    }
}

impl<I: InternalFormat, T: ItemType> TextureReload for Texture<I, T> {
    fn reload(&self) {
        let GpuData { width, height, layers } = *self.storage();
        let level = 0;
        let border = 0;
        let internal_format = Self::gl_internal_format();
        let format = Self::gl_format().into();
        let elem_type = Self::gl_elem_type();

        let target = self.target();
        self.context().bind_texture(*target, Some(self.gl_texture()));
        match target {
            Context::TEXTURE_2D => {
                self.context()
                    .tex_image_2d_with_i32_and_i32_and_i32_and_format_and_type_and_opt_u8_array(
                        *target,
                        level,
                        internal_format,
                        width,
                        height,
                        border,
                        format,
                        elem_type,
                        None,
                    )
                    .unwrap();
            }
            Context::TEXTURE_2D_ARRAY | Context::TEXTURE_3D => {
                self.context()
                    .tex_image_3d_with_opt_u8_array(
                        *target,
                        level,
                        internal_format,
                        width,
                        height,
                        layers,
                        border,
                        format,
                        elem_type,
                        None,
                    )
                    .unwrap();
            }
            _ => {
                panic!("Unsupported texture target: {target:?}");
            }
        }

        self.apply_texture_parameters(self.context());
    }
}

impl<I: InternalFormat, T: ItemType + JsBufferViewArr> Texture<I, T> {
    /// Reload texture with given content. The data will be copied to gpu, but the texture will not
    /// take ownership.
    pub fn reload_with_content(&self, data: &[T]) {
        let GpuData { width, height, layers } = *self.storage();
        self.reload_from_memory(data, width, height, layers);
    }
}
