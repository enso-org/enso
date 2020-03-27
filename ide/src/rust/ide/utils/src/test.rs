//! Module with general purpose utilities meant to be used in tests.

use futures::Stream;
use std::future::Future;
use std::pin::Pin;
use std::task::Context;
use std::task::Poll;



// ======================
// === Task Execution ===
// ======================

/// Polls the future, performing any available work.
///
/// If future is complete, returns result. Otherwise, returns control when
/// stalled.
/// It is not legal to call this on future that already completed.
pub fn poll_future_output<F : Future>(f:&mut Pin<Box<F>>) -> Option<F::Output> {
    let mut ctx = Context::from_waker(futures::task::noop_waker_ref());
    match f.as_mut().poll(&mut ctx) {
        Poll::Ready(result) => Some(result),
        Poll::Pending       => None,
    }
}

/// Polls the stream, performing any available work. If a new value is
/// ready, returns it.
///
/// Note that this API hides the difference between value not being available
/// yet and stream being finished.
pub fn poll_stream_output<S : Stream + ?Sized>(f:&mut Pin<Box<S>>) -> Option<S::Item> {
    let mut ctx = Context::from_waker(futures::task::noop_waker_ref());
    match f.as_mut().poll_next(&mut ctx) {
        Poll::Ready(result) => result,
        Poll::Pending       => None,
    }
}



// ===================
// === ExpectTuple ===
// ===================

// === Trait ===
/// Helper allowing converting between collections and tuples. Unwraps internally,
/// will panic on failure. For test environment only.
pub trait ExpectTuple<T> {
    /// Convert Self to tuple `T`. Panic if collection has different count of elements.
    fn expect_tuple(self) -> T;
}


// === Implementations ===
// TODO [MWU] boilerplate below should be generated with macro

impl<Collection:IntoIterator>
ExpectTuple<(Collection::Item,)> for Collection {
    fn expect_tuple(self) -> (Collection::Item,) {
        let mut iter = self.into_iter();
        let     v1   = iter.next().unwrap();
        assert!(iter.next().is_none());
        (v1,)
    }
}

impl<Collection: IntoIterator>
ExpectTuple<(Collection::Item,Collection::Item)>
for Collection {
    fn expect_tuple(self) -> (Collection::Item,Collection::Item) {
        let mut iter = self.into_iter();
        let     v1   = iter.next().unwrap();
        let     v2   = iter.next().unwrap();
        assert!(iter.next().is_none());
        (v1,v2)
    }
}

impl<Collection: IntoIterator>
ExpectTuple<(Collection::Item,Collection::Item,Collection::Item)>
for Collection {
    fn expect_tuple
    (self) -> (Collection::Item,Collection::Item,Collection::Item) {
        let mut iter = self.into_iter();
        let     v1   = iter.next().unwrap();
        let     v2   = iter.next().unwrap();
        let     v3   = iter.next().unwrap();
        assert!(iter.next().is_none());
        (v1,v2,v3)
    }
}

impl<Collection: IntoIterator>
ExpectTuple<(Collection::Item,Collection::Item,Collection::Item,Collection::Item)>
for Collection {
    fn expect_tuple
    (self) -> (Collection::Item,Collection::Item,Collection::Item,Collection::Item) {
        let mut iter = self.into_iter();
        let     v1   = iter.next().unwrap();
        let     v2   = iter.next().unwrap();
        let     v3   = iter.next().unwrap();
        let     v4   = iter.next().unwrap();
        assert!(iter.next().is_none());
        (v1,v2,v3,v4)
    }
}

#[allow(clippy::type_complexity)]
impl<Collection: IntoIterator>
ExpectTuple<(Collection::Item,Collection::Item,Collection::Item,Collection::Item,Collection::Item)>
for Collection {
    fn expect_tuple
    (self)
     -> (Collection::Item,Collection::Item,Collection::Item,Collection::Item,Collection::Item) {
        let mut iter = self.into_iter();
        let     v1   = iter.next().unwrap();
        let     v2   = iter.next().unwrap();
        let     v3   = iter.next().unwrap();
        let     v4   = iter.next().unwrap();
        let     v5   = iter.next().unwrap();
        assert!(iter.next().is_none());
        (v1,v2,v3,v4,v5)
    }
}
