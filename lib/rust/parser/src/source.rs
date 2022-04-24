use crate::prelude::*;

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
}
