//! Utilities for dealing with `Stream` values in test code.

use crate::*;

use futures::Stream;
use std::pin::Pin;
use std::task::Context;
use std::task::Poll;



/// Extensions to the `Stream` trait allowing manual control of the execution by subsequent
/// polling.
pub trait StreamTestExt<S: ?Sized + Stream> {
    /// Access the underlying `Stream` in its pinned form, so that it can be `poll`ed.
    fn get_pinned_stream(&mut self) -> Pin<&mut S>;

    /// Polls the stream, performing any available work. If a new value is ready, returns it.
    ///
    /// The stream polled with this method might not be properly waken later.
    /// Once stream finishes, this method must not be called again (unless stream is fused).
    fn manual_poll_next(&mut self) -> Poll<Option<S::Item>> {
        let mut ctx = Context::from_waker(futures::task::noop_waker_ref());
        self.get_pinned_stream().poll_next(&mut ctx)
    }

    /// Asserts that stream has next value ready and returns it.
    ///
    /// Same caveats apply as for `test_poll_next`.
    fn expect_next(&mut self) -> S::Item {
        match self.manual_poll_next() {
            Poll::Pending => panic!("Stream has no next item available yet."),
            Poll::Ready(Some(item)) => item,
            Poll::Ready(None) => panic!("Stream ended instead of yielding an expected value."),
        }
    }

    /// Asserts that stream has exactly one value ready and returns it.
    ///
    /// Same caveats apply as for `test_poll_next`.
    fn expect_one(&mut self) -> S::Item
    where S::Item: Debug {
        let ret = self.expect_next();
        self.expect_pending();
        ret
    }

    /// Asserts that stream has terminated.
    ///
    /// Same caveats apply as for `test_poll_next`.
    fn expect_terminated(&mut self) {
        match self.manual_poll_next() {
            Poll::Ready(None) => {}
            _ => panic!("Stream has not terminated."),
        }
    }

    /// Asserts that the next value in the stream is not ready yet.
    ///
    /// Same caveats apply as for `test_poll_next`.
    fn expect_pending(&mut self)
    where S::Item: Debug {
        match self.manual_poll_next() {
            Poll::Pending => {}
            Poll::Ready(Some(item)) =>
                panic!("There should be no value ready, yet the stream yielded {item:?}"),
            Poll::Ready(None) => {
                panic!("Stream has terminated, while it should be waiting for the next value.")
            }
        }
    }

    /// Asserts that the stream has two values ready. First needs to match against any of the
    /// given predicates, the second one against the other predicate.
    ///
    /// The order of these two values is irrelevant.
    fn expect_both(&mut self, one: impl Fn(&S::Item) -> bool, other: impl Fn(&S::Item) -> bool)
    where S::Item: Debug {
        self.expect_many(vec![Box::new(one), Box::new(other)])
    }

    /// Expect many items being ready but in arbitrary order.
    ///
    /// Takes a list of predicates. Items are matched against them, after predicate succeeds match
    /// it is removed from the list.
    fn expect_many<'a>(&mut self, mut expected: Vec<Box<dyn Fn(&S::Item) -> bool + 'a>>)
    where S::Item: Debug {
        while !expected.is_empty() {
            let item = self.expect_next();
            match expected.iter().find_position(|expected_predicate| expected_predicate(&item)) {
                Some((index, _)) => {
                    let _ = expected.remove(index);
                }
                _ => panic!(
                    "Stream yielded item that did not match to any of the given predicates. \
                    Item: {item:?}"
                ),
            }
        }
    }
}

impl<P, S> StreamTestExt<S> for Pin<P>
where
    P: Unpin + DerefMut<Target = S>,
    S: ?Sized + Stream,
{
    fn get_pinned_stream(&mut self) -> Pin<&mut S> {
        self.as_mut()
    }
}
