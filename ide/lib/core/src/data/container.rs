// ===========
// === Add ===
// ===========

/// An abstraction for container which can be provided with new elements. The
/// element type is polymorphic, allowing the container to reuse the function
/// for different item types.
pub trait Add<T> {
    type Result = ();
    fn add(&mut self, component:T) -> Self::Result;
}

pub type AddResult<T,S> = <T as Add<S>>::Result;
