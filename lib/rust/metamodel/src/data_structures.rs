//! Data structures used in the crate implementation.

use vecmap::*;

use derivative::Derivative;
use std::marker::PhantomData;



// ===========
// === IDs ===
// ===========

/// A globally unique identifier, with a type-tag.
#[derive(Derivative)]
#[derivative(Copy(bound = ""))]
#[derivative(Clone(bound = ""))]
#[derivative(Debug(bound = ""))]
#[derivative(Eq(bound = ""))]
#[derivative(PartialEq(bound = ""))]
#[derivative(Ord(bound = ""))]
#[derivative(PartialOrd(bound = ""))]
#[derivative(Hash(bound = ""))]
pub struct Id<T> {
    value:  u32,
    marker: PhantomData<*const T>,
}

impl<T> Id<T> {
    /// Assign a new ID.
    pub fn new() -> Self {
        use std::sync::atomic;
        static NEXT_ID: atomic::AtomicU32 = atomic::AtomicU32::new(0);
        let value = NEXT_ID.fetch_add(1, atomic::Ordering::Relaxed);
        let marker = Default::default();
        Self { value, marker }
    }
}

impl<T> Default for Id<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> std::fmt::Display for Id<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}



// ==========================
// === Densely-stored map ===
// ==========================

/// Densely-stored map from internally-produced keys.
///
/// This is similar in implementation to `enso_data_structures::OptVec`, however there is a core
/// design difference: `OptVec` is a self-keying map created to be *more efficient* than the
/// standard map types; this is a self-keying map created to be *safer* than the standard map types,
/// and also efficient for the expected workload.
///
/// `OptVec` uses a freelist to reuse keys and remain dense during mixed remove/create workloads;
/// `VecMap` statically disallows key reuse--values can be explicitly mutated, but once removed
/// cannot be rebound. This improves the failure mode of broken references: Rather than likely
/// become apparently-valid references to the wrong values, attempts to access removed elements will
/// fail, and be detected.
#[derive(Debug, Derivative, Clone)]
#[derivative(Default(bound = ""))]
pub struct VecMap<T> {
    data: Vec<Option<T>>,
}

impl<T> VecMap<T> {
    /// Obtain a new key, with no bound value.
    pub fn unbound_key(&mut self) -> Key<T, Unbound> {
        let id = Key::new(self.data.len());
        self.data.push(None);
        id
    }

    /// Set the value bound to a key.
    pub fn bind(&mut self, key: Key<T, Unbound>, value: T) -> Key<T> {
        assert!(self.data[key.index].is_none());
        self.data[key.index] = Some(value);
        Key::new(key.index)
    }

    /// Add a value; return its newly-assigned key.
    pub fn insert(&mut self, value: T) -> Key<T> {
        let key = self.unbound_key();
        self.bind(key, value)
    }

    /// Remove a value from the graph; its ID will be permanently unoccupied.
    pub fn remove(&mut self, key: Key<T>) -> T {
        self.data[key.index].take().unwrap()
    }

    /// Get a reference to a value, if present.
    pub fn get(&self, key: Key<T>) -> Option<&T> {
        self.data[key.index].as_ref()
    }

    /// Get a mutable reference to a value, if present.
    pub fn get_mut(&mut self, key: Key<T>) -> Option<&mut T> {
        self.data[key.index].as_mut()
    }

    /// Iterate all key with values set.
    pub fn keys(&self) -> impl Iterator<Item = Key<T>> + '_ {
        self.data.iter().enumerate().filter_map(|(i, val)| val.as_ref().map(|_| Key::new(i)))
    }

    /// Iterate values.
    pub fn values(&self) -> impl Iterator<Item = &T> {
        self.data.iter().filter_map(|val| val.as_ref())
    }

    /// Iterate values mutably.
    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.data.iter_mut().filter_map(|val| val.as_mut())
    }

    /// Iterate entries.
    pub fn iter<'s>(&'s self) -> impl Iterator<Item = (Key<T>, &'s T)> {
        let map_key = |(i, val): (usize, &'s Option<T>)| val.as_ref().map(|val| (Key::new(i), val));
        self.data.iter().enumerate().filter_map(map_key)
    }

    /// Iterate entries mutably.
    pub fn iter_mut<'s>(&'s mut self) -> impl Iterator<Item = (Key<T>, &'s mut T)> {
        let map_key =
            |(i, val): (usize, &'s mut Option<T>)| val.as_mut().map(|val| (Key::new(i), val));
        self.data.iter_mut().enumerate().filter_map(map_key)
    }
}

impl<T> std::ops::Index<Key<T, MaybeBound>> for VecMap<T> {
    type Output = T;
    fn index(&self, key: Key<T, MaybeBound>) -> &Self::Output {
        self.get(key).unwrap()
    }
}
impl<T> std::ops::Index<&Key<T, MaybeBound>> for VecMap<T> {
    type Output = T;
    fn index(&self, key: &Key<T, MaybeBound>) -> &Self::Output {
        &self[*key]
    }
}
impl<T> std::ops::IndexMut<Key<T, MaybeBound>> for VecMap<T> {
    fn index_mut(&mut self, key: Key<T, MaybeBound>) -> &mut Self::Output {
        self.get_mut(key).unwrap()
    }
}
impl<T> std::ops::IndexMut<&Key<T, MaybeBound>> for VecMap<T> {
    fn index_mut(&mut self, key: &Key<T, MaybeBound>) -> &mut Self::Output {
        &mut self[*key]
    }
}

/// Types used by `VecMap`.
pub mod vecmap {
    use super::*;

    /// Marker indicating a key that may or may not currently be bound.
    #[derive(Copy, Clone, Debug)]
    pub struct MaybeBound;
    /// Marker indicating a key that is not yet bound.
    #[allow(missing_copy_implementations)] // Type is one-shot promise.
    #[derive(Debug)]
    pub struct Unbound;

    /// Identifies a location within a `VecMap`.
    #[derive(Derivative)]
    #[derivative(Copy(bound = "State: Copy"))]
    #[derivative(Clone(bound = "State: Clone"))]
    #[derivative(Debug(bound = ""))]
    #[derivative(Eq(bound = ""))]
    #[derivative(PartialEq(bound = ""))]
    #[derivative(Ord(bound = ""))]
    #[derivative(PartialOrd(bound = ""))]
    #[derivative(Hash(bound = ""))]
    pub struct Key<T, State = MaybeBound> {
        pub(super) index: usize,
        #[derivative(Debug = "ignore")]
        marker:           PhantomData<*const T>,
        #[derivative(Debug = "ignore")]
        state:            PhantomData<*const State>,
    }

    impl<T, State> Key<T, State> {
        pub(super) fn new(index: usize) -> Self {
            let marker = Default::default();
            let state = Default::default();
            Self { index, marker, state }
        }
    }

    /// Identifies a location within a `VecMap` that does not yet have a value bound.
    pub type UnboundKey<T> = Key<T, Unbound>;

    impl<T> From<&'_ Key<T, Unbound>> for Key<T, MaybeBound> {
        fn from(key: &Key<T, Unbound>) -> Self {
            Self::new(key.index)
        }
    }

    impl<T, State> std::fmt::Display for Key<T, State> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.index)
        }
    }
}
