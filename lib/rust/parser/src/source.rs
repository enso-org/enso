use crate::location;
use crate::prelude::*;

pub mod traits {
    pub use super::HasRepr;
}

#[derive(Deref)]
pub struct DebugLeaf<T>(pub T);

#[derive(Deref)]
pub struct With<'s, T> {
    #[deref]
    pub data:   T,
    pub source: &'s str,
}

impl<'s, T> With<'s, T> {
    pub fn new(source: &'s str, data: T) -> Self {
        Self { source, data }
    }

    pub fn trans<S>(&self, f: impl FnOnce(&T) -> S) -> With<S> {
        let source = self.source;
        let data = f(&self.data);
        With { source, data }
    }

    pub fn with_data<S>(&self, data: S) -> With<'s, S> {
        let source = self.source;
        With { source, data }
    }
}

pub trait HasRepr<'s> {
    fn repr(&self) -> &'s str;
}

impl<'s, T> HasRepr<'s> for With<'s, location::With<T>> {
    fn repr(&self) -> &'s str {
        self.source_slice(&self.source)
    }
}

impl<'s, T> HasRepr<'s> for With<'s, &location::With<T>> {
    fn repr(&self) -> &'s str {
        self.source_slice(&self.source)
    }
}
