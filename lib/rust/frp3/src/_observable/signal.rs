use crate::{
    runtime::{with_runtime, Runtime},
    utils::{downcast_mut_unchecked, downcast_ref_unchecked},
};
use std::marker::PhantomData;

slotmap::new_key_type! { pub(crate) struct SignalId; }

#[derive(Clone, Copy)]
pub struct Signal<T> {
    id: SignalId,
    marker: PhantomData<fn(T) -> T>,
}

impl<T> Signal<T> {
    pub(super) fn new_with_id(id: SignalId) -> Self {
        Signal {
            id,
            marker: PhantomData,
        }
    }
}

pub(super) fn with_signal_mut_untracked<T: 'static, U>(
    rt: &Runtime,
    sig: Signal<T>,
    f: impl FnOnce(&mut T) -> U,
) -> U {
    let data = rt.get_signal_data(sig.id);
    let mut borrow = data.borrow_mut();
    let value: &mut T = unsafe { downcast_mut_unchecked(&mut *borrow) };
    f(value)
}

fn with_signal_ref_untracked<T: 'static, U>(
    rt: &Runtime,
    sig: Signal<T>,
    f: impl FnOnce(&T) -> U,
) -> U {
    let data = rt.get_signal_data(sig.id);
    let borrow = data.borrow();
    let value: &T = unsafe { downcast_ref_unchecked(&*borrow) };
    f(value)
}

impl<T: 'static> Signal<T> {
    fn depend(self) {
        with_runtime(|rt| {
            rt.depend(self.id);
        });
    }

    fn notify(self) {
        with_runtime(|rt| {
            rt.notify(self.id);
        });
    }

    pub fn with<U>(self, f: impl FnOnce(&T) -> U) -> U {
        with_runtime(|rt| {
            rt.depend(self.id);
            with_signal_ref_untracked(rt, self, f)
        })
    }

    pub fn with_untracked<U>(self, f: impl FnOnce(&T) -> U) -> U {
        with_runtime(|rt| with_signal_ref_untracked(rt, self, f))
    }

    pub fn with_mut_untracked<U>(self, f: impl FnOnce(&mut T) -> U) -> U {
        with_runtime(|rt| with_signal_mut_untracked(rt, self, f))
    }

    pub fn get(self) -> T
    where
        T: Clone,
    {
        self.with(|v| v.clone())
    }

    /// Modify signal value based on previous value. Does not depend on the signal value.
    pub fn modify(self, f: impl FnOnce(T) -> T)
    where
        T: Clone,
    {
        with_runtime(|rt| {
            let id = self.id;
            let ret = with_signal_mut_untracked(rt, self, |v| *v = f(v.clone()));
            rt.notify(id);
            ret
        });
    }

    pub fn get_untracked(self) -> T
    where
        T: Clone,
    {
        self.with_untracked(|v| v.clone())
    }

    pub fn set(self, value: T) {
        with_runtime(|rt| {
            let id = self.id;
            with_signal_mut_untracked(rt, self, |v| *v = value);
            rt.notify(id);
        })
    }

    pub fn set_changed(self, value: T)
    where
        T: PartialEq,
    {
        with_runtime(|rt| {
            let id = self.id;
            with_signal_mut_untracked(rt, self, |v| {
                if v != &value {
                    *v = value;
                    rt.notify(id);
                }
            });
        })
    }

    pub fn set_untracked(self, value: T) {
        self.with_mut_untracked(|v| *v = value);
    }
}
