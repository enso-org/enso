//! Generic representation of data types. Refer to the crate documentation to learn more.

use super::hlist;



// ==============
// === Traits ===
// ==============

/// Common traits.
pub mod traits {
    pub use super::HasRepr        as _TRAIT_HasRepr;
    pub use super::HasFieldsCount as _TRAIT_HasFieldsCount;
}



// ===============
// === HasRepr ===
// ===============

/// Association of a given type with its generic representation.
#[allow(missing_docs)]
pub trait HasRepr {
    type GenericRepr : hlist::HList;
}

/// Type level accessor of a generic representation of the given type.
pub type Repr<T> = <T as HasRepr>::GenericRepr;

/// Converts the type to its generic representation. Please note that this trait is implemented
/// automatically for every type which implements `Into<Repr<Self>>`.
#[allow(missing_docs)]
pub trait IntoGeneric : HasRepr + Into<Repr<Self>> {
    fn into_generic(self) -> Repr<Self> {
        self.into()
    }
}
impl<T> IntoGeneric for T where T : HasRepr + Into<Repr<T>> {}



// ======================
// === HasFieldsCount ===
// ======================

/// Information of field count of any structure implementing `Generics`. This trait is implemented
/// automatically.
#[allow(missing_docs)]
pub trait HasFieldsCount {
    const FIELDS_COUNT : usize;
    fn fields_count() -> usize {
        Self::FIELDS_COUNT
    }
}

impl<T> HasFieldsCount for T
where T:HasRepr {
    const FIELDS_COUNT : usize = <Repr<T> as hlist::HasLength>::LEN;
}
