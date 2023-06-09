//! A common utilities for synchronization many asynchronous tasks.

use crate::*;

use std::thread;


// ==============
// === Export ===
// ==============

pub use std::sync::PoisonError;
pub use std::sync::TryLockError;
pub use std::sync::TryLockResult;



// =========================
// === SingleThreadMutex ===
// =========================

// === MutexGuard ===

/// A guard keeping the lock on a [`SingleThreadMutex`] and leaving it once dropped.
#[derive(Debug)]
pub struct MutexGuard<'a, T> {
    mutex:                  &'a SingleThreadMutex<T>,
    data:                   RefMut<'a, T>,
    created_when_panicking: bool,
}

impl<'a, T> Deref for MutexGuard<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<'a, T> DerefMut for MutexGuard<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
}

impl<'a, T> MutexGuard<'a, T> {
    fn new(mutex: &'a SingleThreadMutex<T>, data: RefMut<'a, T>) -> Self {
        Self { mutex, data, created_when_panicking: thread::panicking() }
    }
}

impl<'a, T> Drop for MutexGuard<'a, T> {
    fn drop(&mut self) {
        if !self.created_when_panicking && thread::panicking() {
            self.mutex.poisoned.set(true);
        }
    }
}


// === SingleThreadMutex ===

/// A Mutex designed to be used in a single-thread environment.
///
/// It is much simpler version of [`std::sync::Mutex`]. It does not have a blocking [`lock`] as it
/// would always result in a deadlock.
///
/// # Poisoning
///
/// When panic occurs while keeping lock on the mutex, it goes to "poisoned" state. It's implemented
/// the same way as in [`std::sync::Mutex`] - see its documentation for details.
#[derive(Debug, Default)]
pub struct SingleThreadMutex<T> {
    data:     RefCell<T>,
    poisoned: Cell<bool>,
}

impl<T> SingleThreadMutex<T> {
    /// Create new mutex.
    pub fn new(data: T) -> Self {
        Self { data: RefCell::new(data), poisoned: default() }
    }

    /// Try to lock mutex. Returns lock guard if succeeded, which unlocks the mutex on drop.
    pub fn try_lock(&self) -> TryLockResult<MutexGuard<T>> {
        let data = self.data.try_borrow_mut().map_err(|_| TryLockError::WouldBlock)?;
        let guard = MutexGuard::new(self, data);
        if self.poisoned.get() {
            Err(TryLockError::Poisoned(PoisonError::new(guard)))
        } else {
            Ok(guard)
        }
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn locking() {
        let mutex = SingleThreadMutex::new(13);
        let lock = mutex.try_lock().expect("First lock should be successful");
        assert_eq!(*lock, 13);
        mutex.try_lock().expect_err("Locking while already lock should fail");
        drop(lock);
        mutex.try_lock().expect("Locking after dropping previous lock should succeed");
    }
}
