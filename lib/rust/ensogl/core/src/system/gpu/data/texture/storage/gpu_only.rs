//! This module defines a texture storage which do not keep local data. It only keeps a reference
//! to GPU texture of a given size.

use crate::system::gpu::data::texture::class::*;
use crate::system::gpu::data::texture::storage::*;
use crate::system::gpu::data::texture::types::*;

use crate::system::gpu::data::buffer::item::JsBufferViewArr;
use crate::system::gpu::Context;
use crate::system::gpu::GlEnum;



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
    /// Number of texture layers. When the texture is not layered, this value is 0.
    pub layers: i32,
}


// === Instances ===

impl<I, T> StorageRelation<I, T> for GpuOnly {
    type Storage = GpuOnlyData;

    fn target(storage: &GpuOnlyData) -> GlEnum {
        match storage.layers {
            0 => Context::TEXTURE_2D,
            _ => Context::TEXTURE_2D_ARRAY,
        }
    }
}

impl From<(i32, i32)> for GpuOnlyData {
    fn from(t: (i32, i32)) -> Self {
        let (width, height) = t;
        Self { width, height, layers: 0 }
    }
}

impl From<((i32, i32), i32)> for GpuOnlyData {
    fn from(t: ((i32, i32), i32)) -> Self {
        let ((width, height), layers) = t;
        Self { width, height, layers }
    }
}



// === API ===

impl GpuOnlyData {
    fn new(width: i32, height: i32) -> Self {
        Self { width, height, layers: 0 }
    }
}

impl<I: InternalFormat, T: ItemType> TextureReload for Texture<GpuOnly, I, T> {
    fn reload(&self) {
        let GpuOnlyData { width, height, layers } = *self.storage();
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

impl<I: InternalFormat, T: ItemType + JsBufferViewArr> Texture<GpuOnly, I, T> {
    /// Reload texture with given content. The data will be copied to gpu, but the texture will not
    /// take ownership.
    pub fn reload_with_content(&self, data: &[T]) {
        let GpuOnlyData { width, height, layers } = *self.storage();
        self.reload_from_memory(data, width, height, layers);
    }
}
