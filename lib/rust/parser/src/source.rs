use crate::prelude::*;

#[derive(Deref)]
pub struct DebugLeaf<T>(pub T);

#[derive(Deref)]
pub struct WithSources<'s, T> {
    #[deref]
    pub data:   T,
    pub source: &'s str,
}

impl<'s, T> WithSources<'s, T> {
    pub fn new(source: &'s str, data: T) -> Self {
        Self { source, data }
    }

    pub fn trans<S>(&self, f: impl FnOnce(&T) -> S) -> WithSources<S> {
        let source = self.source;
        let data = f(&self.data);
        WithSources { source, data }
    }
}
