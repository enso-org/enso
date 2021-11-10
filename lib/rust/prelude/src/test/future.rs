//! Utilities for dealing with `Future` values in test code.

use crate::*;

use std::future::Future;
use std::pin::Pin;
use std::task::Context;
use std::task::Poll;

/// Extensions to the `Future` trait allowing manual control of the future execution by subsequent
/// polling.
pub trait FutureTestExt<F: Future + ?Sized> {
    /// Access the underlying `Future` in its pinned form, so that it can be `poll`ed.
    fn get_pinned_future(&mut self) -> Pin<&mut F>;

    /// Polls the future and asserts that the result remains not available yet.
    ///
    /// Same caveats apply as for `manual_poll`.
    fn expect_pending(&mut self) {
        assert!(self.manual_poll().is_pending());
    }

    /// Polls the future and asserts that the result is available yet - and returns it.
    ///
    /// Same caveats apply as for `manual_poll`.
    fn expect_ready(&mut self) -> F::Output {
        if let Poll::Ready(output) = self.manual_poll() {
            output
        } else {
            panic!("The future does not have a ready value.")
        }
    }

    /// Polls the future, performing any available work.
    ///
    /// If future is complete, returns result. Otherwise, returns control when
    /// stalled. It is not legal to call this on future that has been already completed.
    ///
    /// As this manually polls the future while skipping the executor, the Future won't
    /// automatically wake up later and may be finished only by subsequent calls to this function.
    /// As such, this shouldn't be used anywhere except for testing the behavior of the future
    /// value.
    fn manual_poll(&mut self) -> Poll<F::Output> {
        let mut ctx = Context::from_waker(futures::task::noop_waker_ref());
        self.get_pinned_future().poll(&mut ctx)
    }
}

impl<P, F> FutureTestExt<F> for Pin<P>
where
    P: Unpin + DerefMut<Target = F>,
    F: ?Sized + Future,
{
    fn get_pinned_future(&mut self) -> Pin<&mut F> {
        self.as_mut()
    }
}

/// Convenience extensions for testing `Future`s that yields `Result` type.
pub trait FutureResultTestExt<F, R, E>: FutureTestExt<F>
where F: ?Sized + Future<Output = Result<R, E>> {
    /// Polls the future and asserts that the result is Ok(_) - and returns it after unwrapping.
    ///
    /// Same caveats apply as for `manual_poll`.
    fn expect_ok(&mut self) -> R
    where E: Debug {
        self.expect_ready().expect("Expected future to yield an Ok(_) result.")
    }

    /// Polls the future and asserts that the result is Err(_) - and returns the error after
    /// unwrapping.
    ///
    /// Same caveats apply as for `manual_poll`.
    fn expect_err(&mut self) -> E
    where R: Debug {
        self.expect_ready().expect_err("Expected future to yield an Err(_) result.")
    }
}

impl<T, F, R, E> FutureResultTestExt<F, R, E> for T
where
    T: FutureTestExt<F>,
    F: ?Sized + Future<Output = Result<R, E>>,
{
}
