//! This module defines an owned texture storage type. It keeps the texture data in a local memory.

use crate::prelude::*;

use crate::system::gpu::Context;
use crate::system::gpu::data::buffer::item::JsBufferViewArr;
use crate::system::gpu::data::texture::class::*;
use crate::system::gpu::data::texture::storage::*;
use crate::system::gpu::data::texture::types::*;



// =============
// === Owned ===
// =============

/// Texture plain data.
#[derive(Debug)]
pub struct OwnedData<T> {
    /// An array containing texture data.
    pub data: Vec<T>,
    /// Texture width.
    pub width: i32,
    /// Texture height.
    pub height: i32,
}


// === Instances ===

impl<I,T:Debug> StorageRelation<I,T> for Owned {
    type Storage = OwnedData<T>;
}

impl<T> OwnedData<T> {
    fn new(data:Vec<T>, width:i32, height:i32) -> Self {
        Self {data,width,height}
    }
}


// === API ===

impl<I:InternalFormat,T:ItemType+JsBufferViewArr>
TextureReload for Texture<Owned,I,T> {
    fn reload(&self) {
        let width           = self.storage().width;
        let height          = self.storage().height;
        let target          = Context::TEXTURE_2D;
        let level           = 0;
        let border          = 0;
        let internal_format = Self::gl_internal_format();
        let format          = Self::gl_format().into();
        let elem_type       = Self::gl_elem_type();

        self.context().bind_texture(target,Some(&self.gl_texture()));
        unsafe {
            // We use unsafe array view which is used immediately, so no allocations should happen
            // until we drop the view.
            let view   = self.storage().data.js_buffer_view();
            let result = self.context()
                .tex_image_2d_with_i32_and_i32_and_i32_and_format_and_type_and_opt_array_buffer_view
                (target,level,internal_format,width,height,border,format,elem_type,Some(&view));
            result.unwrap();
        }
        Self::set_texture_parameters(&self.context());
    }
}
