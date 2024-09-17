//! This module defines utilities for working with the [`std::vec::Vec`] type.

use enso_reflect::prelude::*;

use serde::Deserialize;
use serde::Deserializer;
use serde::Serialize;
use serde::Serializer;



// =====================
// === VecAllocation ===
// =====================

/// Owns a storage allocation for a [`std::vec::Vec`], but no elements.
///
/// # Usage
///
/// This data structure implements an optimization when creating temporary vectors. The use case
/// occurs when:
/// - Within some scope, a `Vec` is created, added to, and discarded.
/// - The scope may be entered multiple times.
///
/// The optimization is to reuse an allocation between entries to the scope. This is sometimes done
/// by storing and reusing the `Vec`, but that pattern is misleading; owning a `Vec` suggests that
/// values may be retained between entries to the scope. This type explicitly has only one logical
/// state (empty).
///
/// ```
/// # use enso_prelude::*;
/// #[derive(Default)]
/// struct NumberAdder {
///     // In a more complex struct it would be important to be able to tell what state the object
///     // retains from its fields.
///     temporary_nums: VecAllocation<f64>,
/// }
///
/// impl NumberAdder {
///     /// Add some numbers, with better precision than simply adding `f32` values in a loop.
///     /// (For the sake of example, ignore that this is not a fast or accurate approach.)
///     ///
///     /// Because we reuse an allocation, if this method is called repeatedly it will only have to
///     /// allocate enough space to accommodate the largest single input it processes. Thus, rather
///     /// than performing a number of reallocations that scales linearly in the number of batches
///     /// of input (assuming batch size has some constant geometric mean), it performs a number of
///     /// allocations that scales with the log of the size of the largest batch; the worst case of
///     /// this implementation has the same performance as the best case of an implementation that
///     /// doesn't reuse its allocation.
///     pub fn add_nums(&mut self, inputs: impl IntoIterator<Item = f32>) -> f32 {
///         let mut extended_precision = self.temporary_nums.take();
///         extended_precision.extend(inputs.into_iter().map(f64::from));
///         let result = extended_precision.drain(..).fold(0.0, f64::add);
///         self.temporary_nums.set_from(extended_precision);
///         result as f32
///     }
/// }
/// ```
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct VecAllocation<T> {
    data: Vec<T>,
}

impl<T> Default for VecAllocation<T> {
    fn default() -> Self {
        Self { data: Vec::new() }
    }
}

impl<T> VecAllocation<T> {
    /// Create a new, empty allocation.
    pub fn new() -> Self {
        Self::default()
    }

    /// Drop any elements from the given `Vec`, keeping its allocated memory. It can be retrieved
    /// later with `take`.
    pub fn set_from(&mut self, mut data: Vec<T>) {
        data.clear();
        self.data = data;
    }

    /// Return a `Vec` containing no elements, whose allocated storage comes from the most recent
    /// call to `set_from`, unless `take` has been called since then. Any subsequent call before the
    /// next `set_from` would return a newly-created `Vec` with no allocated memory.
    pub fn take(&mut self) -> Vec<T> {
        std::mem::take(&mut self.data)
    }
}


// ================
// === Cold Vec ===
// ================

/// A vector optimized to be unused.
///
/// If it has never contained any elements, it will be stored more efficiently than a [`Vec`].
#[derive(Clone, Debug, Eq, Reflect)]
#[reflect(transparent)]
pub struct ColdVec<T> {
    #[allow(clippy::box_collection)]
    #[reflect(as = Vec<T>)]
    elements: Option<Box<Vec<T>>>,
}

impl<T> ColdVec<T> {
    pub fn push(&mut self, element: T) {
        self.elements_mut().push(element);
    }

    pub fn append(&mut self, other: &mut Self) {
        if let Some(other_elements) = other.elements.as_mut() {
            self.elements_mut().append(other_elements)
        }
    }

    pub fn iter(&self) -> std::slice::Iter<T> {
        match self.elements.as_ref() {
            Some(elements) => elements.iter(),
            None => [].iter(),
        }
    }

    fn elements_mut(&mut self) -> &mut Vec<T> {
        if self.elements.is_none() {
            self.elements = Some(Default::default());
        }
        self.elements.as_mut().unwrap()
    }
}

impl<T: PartialEq<T>> PartialEq<ColdVec<T>> for ColdVec<T> {
    fn eq(&self, other: &ColdVec<T>) -> bool {
        match (&self.elements, &other.elements) {
            (Some(a), Some(b)) => a.eq(b),
            (Some(x), None) | (None, Some(x)) => x.is_empty(),
            (None, None) => true,
        }
    }
}
impl<T> Default for ColdVec<T> {
    fn default() -> Self {
        Self { elements: None }
    }
}

impl<T: Serialize> Serialize for ColdVec<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where S: Serializer {
        match &self.elements {
            Some(elements) => elements.serialize(serializer),
            None => Vec::<T>::new().serialize(serializer),
        }
    }
}

impl<'de, T: Deserialize<'de>> Deserialize<'de> for ColdVec<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where D: Deserializer<'de> {
        let elements = Vec::deserialize(deserializer)?;
        Ok(Self { elements: (!elements.is_empty()).then(|| Box::new(elements)) })
    }
}
