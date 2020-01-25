//! General purpose functions to be reused between components, not belonging to
//! any other crate and yet not worth of being split into their own creates.

#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]
#![warn(unsafe_code)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]

#[allow(unused)]
use prelude::*;

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
