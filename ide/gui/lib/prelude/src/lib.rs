#![feature(trait_alias)]

pub use boolinator::Boolinator;
pub use core::any::type_name;
pub use core::fmt::Debug;
pub use derivative::Derivative;
pub use derive_more::*;
pub use failure::Fail;
pub use itertools::Itertools;
pub use num::Num;
pub use shrinkwraprs::Shrinkwrap;
pub use std::cell::Ref;
pub use std::cell::RefCell;
pub use std::collections::HashMap;
pub use std::collections::HashSet;
pub use std::convert::identity;
pub use std::convert::TryFrom;
pub use std::convert::TryInto;
pub use std::fmt::Display;
pub use std::hash::Hash;
pub use std::iter;
pub use std::iter::FromIterator;
pub use std::marker::PhantomData;
pub use std::ops::Deref;
pub use std::ops::DerefMut;
pub use std::ops::Index;
pub use std::ops::IndexMut;
pub use std::rc::Rc;
pub use std::rc::Weak;
pub use std::slice;
pub use std::slice::SliceIndex;

pub trait Str = AsRef<str>;

pub fn default<T: Default>() -> T {
    Default::default()
}

#[derive(Derivative)]
#[derive(Shrinkwrap)]
#[shrinkwrap(mutable)]
#[derivative(Clone(bound="T: Clone"))]
pub struct WithPhantomType<T, P=()> {
    #[shrinkwrap(main_field)]
    pub t: T,
    phantom: PhantomData<P>
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