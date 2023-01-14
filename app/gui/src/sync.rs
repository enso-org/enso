//! Synchronization facilities.

use crate::prelude::*;



/// Wrapper for a value that allows asynchronous observation of its updates.
#[derive(Derivative, CloneRef, Debug, Default)]
#[clone_ref(bound = "")]
#[derivative(Clone(bound = ""))]
pub struct Synchronized<T> {
    value:    Rc<RefCell<T>>,
    notifier: notification::Publisher<()>,
}

impl<T> Synchronized<T> {
    /// Wrap a value into `Synchronized`.
    pub fn new(t: T) -> Self {
        Self { value: Rc::new(RefCell::new(t)), notifier: default() }
    }

    /// Replace the value with a new one. Return the previous value.
    pub fn replace(&self, new_value: T) -> T {
        let previous = std::mem::replace(self.value.borrow_mut().deref_mut(), new_value);
        self.notifier.notify(());
        previous
    }

    /// Take the value out and replace it with a default-constructed one.
    pub fn take(&self) -> T
    where T: Default {
        self.replace(default())
    }

    /// Uses a given function to update the stored value.
    ///
    /// The function is invoked with value borrowed mutably, it must not use this value in any wya.
    pub fn update<R>(&self, f: impl FnOnce(&mut T) -> R) -> R {
        f(self.value.borrow_mut().deref_mut())
    }

    /// Get a copy of the stored value.
    pub fn get_cloned(&self) -> T
    where T: Clone {
        self.borrow().clone()
    }

    /// Borrow the store value.
    pub fn borrow(&self) -> Ref<T> {
        self.value.borrow()
    }

    /// Run given tester function on each value update. Stop when function returns `Some` value.
    /// Forwards the returned value from the tester or `None` if the value was dropped before the
    /// tester returned `Some`.
    pub fn when_map<Condition, R>(
        &self,
        mut tester: Condition,
    ) -> impl Future<Output = Option<R>> + 'static
    where
        Condition: FnMut(&T) -> Option<R> + 'static,
        R: 'static,
        T: 'static,
    {
        if let Some(ret) = tester(self.value.borrow().deref()) {
            futures::future::ready(Some(ret)).left_future()
        } else {
            // We pass strong value reference to the future, because we want to able to notify
            // our observers about changes, even if these notifications are processed after this
            // object is dropped.
            let value = self.value.clone_ref();
            let tester = move |_: ()| {
                let result = tester(value.borrow().deref());
                futures::future::ready(result)
            };
            self.notifier
                .subscribe()
                .filter_map(tester)
                .boxed_local()
                .into_future()
                .map(|(head, _tail)| head)
                .right_future()
        }
    }

    /// Get a Future that resolves once the value satisfies the condition checked by a given
    /// function.
    pub fn when<Condition>(
        &self,
        mut tester: Condition,
    ) -> impl Future<Output = Option<()>> + 'static
    where
        Condition: FnMut(&T) -> bool + 'static,
        T: 'static,
    {
        self.when_map(move |value| tester(value).as_some(()))
    }

    /// Get a Future that resolves once the value is equal to a given argument.
    pub fn when_eq<U>(&self, u: U) -> impl Future<Output = Option<()>> + 'static
    where
        for<'a> &'a T: PartialEq<U>,
        U: 'static,
        T: 'static, {
        self.when(move |val_ref| val_ref == u)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::executor::test_utils::TestWithLocalPoolExecutor;

    #[test]
    fn synchronized() {
        let mut fixture = TestWithLocalPoolExecutor::set_up();

        let flag = Synchronized::new(false);
        assert!(!*flag.borrow());

        // If condition was already met, be immediately ready.
        let mut on_false = flag.when(|&f| !f).boxed_local();
        assert_eq!(on_false.expect_ready(), Some(()));

        // Otherwise not ready.
        let mut on_true = flag.when(|&f| f).boxed_local();
        on_true.expect_pending();

        // Faux no-op change. Should not spawn a new task.
        flag.replace(false);
        fixture.expect_finished();
        on_true.expect_pending();

        // Real change, future now should complete.
        flag.replace(true);
        fixture.expect_finished();
        assert_eq!(on_true.expect_ready(), Some(()));

        // After dropping the flag, pending future should complete with None.
        let mut on_false = flag.when(|&f| !f).boxed_local();
        on_false.expect_pending();
        drop(flag);
        assert_eq!(on_false.expect_ready(), None);
    }


    #[test]
    fn some_on_drop_before_notification() {
        let mut fixture = TestWithLocalPoolExecutor::set_up();
        let number = Synchronized::new(10);
        let mut fut = number.when_map(|v| (*v == 0).then_some(())).boxed_local();
        fixture.run_until_stalled();
        fut.expect_pending();
        number.replace(0);
        drop(number);
        assert_eq!(fixture.expect_completion(&mut fut), Some(()));
    }
}
