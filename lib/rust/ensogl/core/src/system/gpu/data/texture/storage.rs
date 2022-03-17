//! This is a root module for all texture storage definitions.

use crate::prelude::*;


// ==============
// === Export ===
// ==============

pub mod gpu_only;
pub mod owned;
pub mod remote_image;

pub use gpu_only::*;
pub use owned::*;
pub use remote_image::*;



// ===============
// === Storage ===
// ===============

/// Trait describing any storage texture type.
pub trait Storage = Debug + Default + Into<AnyStorage> + PhantomInto<AnyStorage> + 'static;

/// Type level accessor of the storage implementation for a given set of texture parameters.
pub type StorageOf<S, I, T> = <S as StorageRelation<I, T>>::Storage;

/// The storage implementation type family.
pub trait StorageRelation<InternalFormat, ElemType>: Storage {
    /// The storage implementation.
    type Storage: Debug;
}

enso_shapely::define_singleton_enum! {
    AnyStorage {RemoteImage,GpuOnly,Owned}
}
