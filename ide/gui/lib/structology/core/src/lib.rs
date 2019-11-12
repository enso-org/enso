#![feature(generators, generator_trait)]

pub use structology_macros::*;

use std::ops::Generator;
use std::ops::GeneratorState;
use std::pin::Pin;
use basegl_prelude::PhantomData;

// ========================
// === IterForGenerator ===
// ========================

pub struct IterForGenerator<G: Generator>(pub G);

impl<G> Iterator for IterForGenerator<G>
where G: Generator<Return = ()> + Unpin {
    type Item = G::Yield;
    fn next(&mut self) -> Option<Self::Item> {
        match { Pin::new(&mut self.0).resume() } {
            GeneratorState::Yielded(element) => Some(element),
            _ => None,
        }
    }
}


// ======================
// === EmptyGenerator ===
// ======================

pub struct EmptyGenerator<T>(PhantomData<T>);

impl<T> EmptyGenerator<T> {
    pub fn new() -> Self {
        Self(PhantomData)
    }
}

impl<T> Iterator for EmptyGenerator<T> {
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        None
    }
}