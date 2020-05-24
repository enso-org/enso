//! Module with general purpose utilities meant to be used in tests.

use futures::Stream;

use std::future::Future;
use std::fmt::Debug;
use std::ops::DerefMut;
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
pub fn poll<F,R>(fut:&mut Pin<F>) -> Option<R>
where F:DerefMut<Target:Future<Output=R>> {
    let mut ctx = std::task::Context::from_waker(futures::task::noop_waker_ref());
    match fut.as_mut().poll(&mut ctx) {
        std::task::Poll::Ready(result) => Some(result),
        std::task::Poll::Pending       => None,
    }
}

/// Polls the future and asserts that the result remains not available yet.
pub fn expect_not_ready<F,R>(fut:&mut Pin<F>)
where F:DerefMut<Target:Future<Output=R>> {
    assert!(poll(fut).is_none(), "expected future to not be ready")
}

/// Polls the future and asserts that the result is available yet - and returns it.
pub fn expect_ready<F,R>(fut:&mut Pin<F>) -> R
where F:DerefMut<Target:Future<Output=R>> {
    poll(fut).expect("expected future to be ready")
}

/// Polls the future and asserts that the result is Ok(_) - and returns it after unwrapping.
pub fn expect_ok<F,R,E>(fut:&mut Pin<F>) -> R
where F:DerefMut<Target:Future<Output=Result<R,E>>>,
          E:Debug {
    expect_ready(fut).expect("expected future to yield an Ok(_) result")
}

/// Polls the future and asserts that the result is Err(_) - and returns the error after unwrapping.
pub fn expect_err<F,R,E>(fut:&mut Pin<F>) -> E
where F:DerefMut<Target:Future<Output=Result<R,E>>>,
          R:Debug {
    expect_ready(fut).expect_err("expected future to yield an Err(_) result")
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
