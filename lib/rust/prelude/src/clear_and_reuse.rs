//! Definition of [`Clearable`] and [`Reusable`] traits, allowing performant way of clearing and
//! reusing data structures.

use std::cell::Cell;



// =================
// === Clearable ===
// =================

/// Abstraction for structures that can be cleared and reused.
///
/// # Performance
/// In most cases, clearing is more performance efficient than re-initializing. For example,
/// [`Vec::clear`] is practically zero-cost and does not drop the allocated memory, allowing for
/// fast following insertions. However, in some cases, clearing is less memory efficient than struct
/// re-initialization. For example, clearing a long [`Vec`] and never filling it again would prevent
/// the previously allocated memory from being re-used. Thus, clearing is recommended in situations
/// when you plan to fill the cleared memory in a similar way as before.
///
/// # Reusing Cleared Values
/// Clearing does not have to reset all fields of a value. In such a case, it should be combined
/// with the [`Reusable`] trait. For example, a struct might represent a node in a graph that
/// contains vec of input connections, vec of output connections, and metadata. Clearing might be
/// implemented as clearing the input and output connections only, without resetting the metadata.
/// This is because when creating a new node, we would need to set the metadata anyway, so by not
/// clearing it, we can save a little bit time.
#[allow(missing_docs)]
pub trait Clearable {
    fn clear(&mut self);
}

/// Version of [`Clearable`] with internal mutability pattern.
#[allow(missing_docs)]
pub trait ImClearable {
    fn clear_im(&self);
}



// ================
// === Reusable ===
// ================

/// Abstraction for structures that can be cleared and reused. This does not have to set all fields
/// of the given struct, only the ones that were not cleared by [`Clearable`]. You should not use
/// this abstraction if the struct was not cleared beforehand. See the docs of [`Clearable`] to
/// learn more.
#[allow(missing_docs)]
pub trait Reusable {
    type Args;
    fn reuse(&mut self, args: Self::Args);
}



// =================
// === Std Impls ===
// =================

impl<T: Default> ImClearable for Cell<T> {
    #[inline(always)]
    fn clear_im(&self) {
        self.set(Default::default())
    }
}

impl<T> Clearable for Vec<T> {
    #[inline(always)]
    fn clear(&mut self) {
        self.clear()
    }
}
