use std::pin::Pin;
use std::task::{Context, Poll};
use std::future::Future;
use crate::{ is_emscripten_runtime_initialized, run_once_initialized };

/// The future for running test after initialization
pub struct TestAfterInit<F:Fn()> {
    test : F
}

impl<F:Fn()> TestAfterInit<F> {
    pub fn schedule(test:F) -> TestAfterInit<F> {
        TestAfterInit{test}
    }
}

impl<F:Fn()> Future for TestAfterInit<F> {

    type Output = ();

    fn poll(self:Pin<&mut Self>, cx:&mut Context<'_>) -> Poll<Self::Output> {
        if is_emscripten_runtime_initialized() {
            (self.test)();
            Poll::Ready(())
        } else {
            let waker = cx.waker().clone();
            run_once_initialized(move || waker.wake());
            Poll::Pending
        }
    }
}