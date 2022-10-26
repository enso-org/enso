//! Extension methods for `Iterator` and `Iterator`-like types.

use crate::prelude::*;

use std::iter::Rev;
use std::iter::Take;



/// Extension methods for `Iterator` and `Iterator`-like types.
pub trait IteratorExt: Iterator {
    /// try_filter
    /// Transforms an [Iterator]'s items into `Result`s, and filters out the `Err` variants.
    fn try_filter<R>(mut self, mut f: impl FnMut(&Self::Item) -> Result<bool>) -> Result<R>
    where
        Self: Sized,
        R: Default + Extend<Self::Item> + Sized, {
        self.try_fold(default(), |mut acc: R, item| {
            acc.extend(f(&item)?.then_some(item));
            Ok(acc)
        })
    }

    /// Transforms an [Iterator]'s items into `Result`s, and filters out the `Err` variants.
    fn try_map<R, U>(mut self, mut f: impl FnMut(Self::Item) -> Result<U>) -> Result<R>
    where
        Self: Sized,
        R: Default + Extend<U> + Sized, {
        self.try_fold(default(), |mut acc: R, item| {
            acc.extend_one(f(item)?);
            Ok(acc)
        })
    }
}

impl<I: Iterator> IteratorExt for I {}

/// Extension methods for `Iterator` and `Iterator`-like types.s
pub trait TryIteratorExt: Iterator {
    /// The result of successful iteration.
    type Ok;

    /// Collects the results of the iterator into a `Result<Vec<_>>`.
    fn try_collect_vec(self) -> Result<Vec<Self::Ok>>;
}

impl<T, U, E> TryIteratorExt for T
where
    T: Iterator<Item = std::result::Result<U, E>>,
    E: Into<anyhow::Error>,
{
    type Ok = U;
    fn try_collect_vec(self) -> Result<Vec<U>> {
        self.map(|i| i.anyhow_err()).collect::<Result<Vec<_>>>()
    }
}

#[allow(missing_docs)]
pub trait ExactDoubleEndedIteratorExt: ExactSizeIterator + DoubleEndedIterator + Sized {
    /// Take the last n elements of the iterator.
    fn take_last_n(self, n: usize) -> Rev<Take<Rev<Self>>> {
        self.rev().take(n).rev()
    }
}

impl<T> ExactDoubleEndedIteratorExt for T where T: ExactSizeIterator + DoubleEndedIterator {}
