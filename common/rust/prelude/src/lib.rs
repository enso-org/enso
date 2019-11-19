#![feature(trait_alias)]

pub use boolinator::Boolinator;
pub use core::{any::type_name, fmt::Debug};
pub use derivative::Derivative;
pub use derive_more::*;
pub use failure::Fail;
pub use itertools::Itertools;
pub use num::Num;
pub use shrinkwraprs::Shrinkwrap;
pub use std::{
    cell::{Ref, RefCell},
    collections::{HashMap, HashSet},
    convert::{identity, TryFrom, TryInto},
    fmt::Display,
    hash::Hash,
    iter,
    iter::FromIterator,
    marker::PhantomData,
    ops::{Deref, DerefMut, Index, IndexMut},
    rc::{Rc, Weak},
    slice,
    slice::SliceIndex,
};

pub trait Str = AsRef<str>;

pub fn default<T: Default>() -> T {
    Default::default()
}

#[derive(Derivative, Shrinkwrap)]
#[shrinkwrap(mutable)]
#[derivative(Clone(bound = "T: Clone"))]
pub struct WithPhantomType<T, P = ()> {
    #[shrinkwrap(main_field)]
    pub t: T,
    phantom: PhantomData<P>,
}

impl<T, P> WithPhantomType<T, P> {
    pub fn new(t: T) -> Self {
        let phantom = PhantomData;
        Self { t, phantom }
    }
}

pub fn with<T, F: FnOnce(T) -> Out, Out>(t: T, f: F) -> Out {
    f(t)
}
