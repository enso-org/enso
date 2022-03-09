//! This module defines a texture storage which do not keep local data. It only keeps a reference
//! to GPU texture of a given size.

use crate::system::gpu::data::texture::class::*;
use crate::system::gpu::data::texture::storage::*;
use crate::system::gpu::data::texture::types::*;

use crate::system::gpu::data::buffer::item::JsBufferViewArr;
use crate::system::gpu::Context;



// ===============
// === GpuOnly ===
// ===============

/// Sized, uninitialized texture.
#[derive(Clone, Copy, Debug)]
pub struct GpuOnlyData {
    /// Texture width.
    pub width:  i32,
    /// Texture height.
    pub height: i32,
}


// === Instances ===

impl<I, T> StorageRelation<I, T> for GpuOnly {
    type Storage = GpuOnlyData;
}

impl From<(i32, i32)> for GpuOnlyData {
    fn from(t: (i32, i32)) -> Self {
        Self::new(t.0, t.1)
    }
}


// === API ===

impl GpuOnlyData {
    fn new(width: i32, height: i32) -> Self {
        Self { width, height }
    }
}

impl<I: InternalFormat, T: ItemType> TextureReload for Texture<GpuOnly, I, T> {
    fn reload(&self) {
        let width = self.storage().width;
        let height = self.storage().height;
        let target = Context::TEXTURE_2D;
        let level = 0;
        let border = 0;
        let internal_format = Self::gl_internal_format();
        let format = Self::gl_format().into();
        let elem_type = Self::gl_elem_type();

        self.context().bind_texture(target, Some(self.gl_texture()));
        self.context()
            .tex_image_2d_with_i32_and_i32_and_i32_and_format_and_type_and_opt_u8_array(
                target,
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

        self.apply_texture_parameters(self.context());
    }
}

impl<I: InternalFormat, T: ItemType + JsBufferViewArr> Texture<GpuOnly, I, T> {
    /// Reload texture with given content. The data will be copied to gpu, but the texture will not
    /// take ownership.
    pub fn reload_with_content(&self, data: &[T]) {
        let width = self.storage().width;
        let height = self.storage().height;
        self.reload_from_memory(data, width, height);
    }
}
