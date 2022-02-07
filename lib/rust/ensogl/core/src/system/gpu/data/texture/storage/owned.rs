//! This module defines an owned texture storage type. It keeps the texture data in a local memory.

use crate::prelude::*;

use crate::system::gpu::data::buffer::item::JsBufferViewArr;
use crate::system::gpu::data::texture;
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
    pub data:   Vec<T>,
    /// Texture width.
    pub width:  i32,
    /// Texture height.
    pub height: i32,
}


// === Instances ===

impl<I, T: Debug> StorageRelation<I, T> for texture::storage::Owned {
    type Storage = OwnedData<T>;
}

impl<T> OwnedData<T> {
    fn new(data: Vec<T>, width: i32, height: i32) -> Self {
        Self { data, width, height }
    }
}


// === API ===

impl<I: InternalFormat, T: ItemType + JsBufferViewArr> TextureReload
    for Texture<texture::storage::Owned, I, T>
{
    #[allow(unsafe_code)]
    fn reload(&self) {
        let storage = &self.storage();
        let data = storage.data.as_slice();
        let width = storage.width;
        let height = storage.height;
        self.reload_from_memory(data, width, height);
    }
}
