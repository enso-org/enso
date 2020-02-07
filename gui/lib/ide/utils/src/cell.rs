//! This module implements utilities that allow for a safer usage pattern of
//! a shared data stored under `RefCell`. The primary goal is to allow accessing
//! the data without any direct calls to `borrow_mut`.
//!
//! Any type that implements `IsWeakHandle` or `IsStrongHandle` (types provided
//! out-of-the-box are Weak<RefCell<T>> and Strong<RefCell<T>> responsively)
//! gets an extension method names `with`. It allows accessing a data through
//! callback function that will get &mut access to the `T`.
//!
//! The safety is ensured by the execution model. The `with` function returns
//! a `Future` that yields a result of callback. Because `borrow_mut` only
//! applied in the `Future` implementation (being called by the executor), it
//! is guaranteed to be safe (as executors do not allow self-entry and callback
//! must be a synchronous function).

use crate::prelude::*;

use std::future::Future;
use std::pin::Pin;
use std::task::Context;
use std::task::Poll;

/// Value or error if the data under handle was not available.
pub type Result<T> = std::result::Result<T,Error>;



// ===========
// == Error ==
// ===========

/// Error that may occur when trying to access data through a `WeakHandle`.
#[derive(Clone,Copy,Display,Debug,Fail)]
pub enum Error {
    /// Happens when `Weak` cannot be upgraded.
    /// Likely the object (if there was an object) has been already dropped.
    HandleExpired,
    /// RefCell was already borrowed, cannot `borrow_mut`. Should never happen.
    AlreadyBorrowed,
}



// ================
// == WeakHandle ==
// ================

/// A weak handle to some internally mutable data.
#[derive(Debug,Derivative)]
#[derivative(Clone(bound=""))]
pub struct WeakHandle<T>(Weak<RefCell<T>>);

/// A type that can provide `Weak` handle to a value under `RefCell`.
impl<T> WeakHandle<T> {
    /// Obtain strong handle to the data.
    pub fn upgrade(&self) -> Option<StrongHandle<T>> {
        self.0.upgrade().map(StrongHandle)
    }

    /// Returns `Future` that shall try calling `Callback` with the access to mutably
    /// borrowed data. Safe, as long as the returned `Future` is run only
    /// through the executor and not manually polled.
    pub fn with<Callback,R> (&self, callback:Callback) -> WeakWith<T,Callback>
    where for<'a> Callback: FnOnce(&'a mut T) -> R {
        WeakWith::new(self.0.clone(),callback)
    }

    /// Clones the value from the handle.
    pub async fn load(&self) -> Result<T> where T:Clone {
        self.with(|data| data.clone()).await
    }

    /// Stores the value into the handle.
    pub async fn store(&self, value:T) -> Result<()> {
        self.with(|data| *data = value).await
    }
}



// ==================
// == StrongHandle ==
// ==================

/// A strong, shared owning handle to some internally mutable data.
#[derive(Debug,Derivative)]
#[derivative(Clone(bound=""))]
pub struct StrongHandle<T>(Rc<RefCell<T>>);

impl<T> StrongHandle<T> {
    /// Packs given value into a shared owning handle.
    pub fn new(t:T) -> StrongHandle<T> {
        StrongHandle(Rc::new(RefCell::new(t)))
    }

    /// Obtain weak handle to the data.
    pub fn downgrade(&self) -> WeakHandle<T> {
        WeakHandle(Rc::downgrade(&self.0))
    }

    /// Returns `Future` that shall call `Callback` with the access to mutably
    /// borrowed data. Safe, as long as the returned `Future` is run only
    /// through the executor and not manually polled.
    pub fn with<Callback,R> (&self, callback:Callback) -> StrongWith<T,Callback>
    where Callback: FnOnce(&mut T) -> R {
        StrongWith::new(self.0.clone(),callback)
    }

    /// Clones the value from the handle.
    pub async fn load(&self) -> T where T:Clone {
        self.with(|data| data.clone()).await
    }

    /// Stores the value into the handle.
    pub async fn store(&self, value:T) {
        self.with(|data| *data = value).await
    }
}



// ==============
// == WeakWith ==
// ==============

/// Future that shall call `callback` with the data under `handle`.
#[derive(Debug)]
pub struct WeakWith<Data,Callback> {
    handle   : Weak<RefCell<Data>>,
    callback : Option<Callback>,
}

impl<Data,Callback> WeakWith<Data,Callback> {
    /// Creates the value.
    pub fn new(handle:Weak<RefCell<Data>>, callback:Callback) -> WeakWith<Data,Callback> {
        WeakWith {handle,callback:Some(callback)}
    }

    // Explanations pertaining this pattern can be found on SO:
    // https://stackoverflow.com/questions/56058494/
    pin_utils::unsafe_unpinned!(callback:Option<Callback>);
}

impl <Data,Callback,R> Future for WeakWith<Data,Callback>
where Callback: FnOnce(&mut Data) -> R, {
    type Output = std::result::Result<R,Error>;
    fn poll(self:Pin<&mut Self>, _cx:&mut Context<'_>) -> Poll<Self::Output> {
        let result = if let Some(handle) = self.handle.upgrade() {
            let callback = self.callback().take().unwrap();
            let result   = with(handle.borrow_mut(), |mut data| callback(&mut data));
            Ok(result)
        } else {
            Err(Error::HandleExpired)
        };
        Poll::Ready(result)
    }
}



// ================
// == StrongWith ==
// ================

/// A `Future` structure that allows processing data under the handle with
/// given callback. As the handle is owning (`Rc`) the underlying data is always
/// accessible.
#[derive(Debug)]
pub struct StrongWith<Data,Callback> {
    handle   : Rc<RefCell<Data>>,
    callback : Option<Callback>,
}

impl<Data,Callback> StrongWith<Data,Callback> {
    /// Create new `StrongWith` value.
    pub fn new(handle:Rc<RefCell<Data>>, callback:Callback) -> StrongWith<Data,Callback> {
        StrongWith {handle,callback:Some(callback)}
    }

    // Explanations pertaining this pattern can be found on SO:
    // https://stackoverflow.com/questions/56058494/
    pin_utils::unsafe_unpinned!(callback:Option<Callback>);
}

impl <Data,Callback,R> Future for StrongWith<Data,Callback>
where Callback : FnOnce(&mut Data) -> R, {
    type Output = R;

    fn poll(self:Pin<&mut Self>, _cx:&mut Context<'_>) -> Poll<Self::Output> {
        let handle   = self.handle.clone();
        let callback = self.callback().take().unwrap();
        let result   = with(handle.borrow_mut(), |mut data| callback(&mut data));
        Poll::Ready(result)
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    use futures::executor::block_on;

    #[test]
    fn strong_handle() {
        const INITIAL_VALUE: i32 = 1000;
        let data = StrongHandle::new(INITIAL_VALUE);
        let data2 = data.clone();
        let fut = async move {
            let unboxed = data2.load().await;
            assert_eq!(unboxed, INITIAL_VALUE);
            const NEW_VALUE: i32 = INITIAL_VALUE + 1;
            data2.store(NEW_VALUE).await;
            let unboxed = data2.load().await;
            assert_eq!(unboxed, NEW_VALUE);
        };
        block_on(fut);
    }

    #[test]
    fn weak_handle() {
        const INITIAL_VALUE: i32 = 1000;
        let data = StrongHandle::new(INITIAL_VALUE);
        let data2 = data.downgrade();
        let fut = async move {
            let unboxed = data2.load().await.unwrap();
            assert_eq!(unboxed, INITIAL_VALUE);
            const NEW_VALUE: i32 = INITIAL_VALUE + 1;
            data2.store(NEW_VALUE).await.unwrap();
            let unboxed = data2.load().await.unwrap();
            assert_eq!(unboxed, NEW_VALUE);
        };
        block_on(fut);
    }
}
