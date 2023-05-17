#![allow(dead_code)]

use std::cell::RefCell;
use std::rc::Rc;

slotmap::new_key_type! { pub(crate) struct CollectorId; }

thread_local! {
    static COLLECTED_DATA: Rc<RefCell<slotmap::SlotMap<CollectorId, Vec<i32>>>> = Default::default();
}

pub struct CollectorGuard {
    id: CollectorId,
}

impl Drop for CollectorGuard {
    fn drop(&mut self) {
        COLLECTED_DATA.with(|data| {
            data.borrow_mut().remove(self.id);
        });
    }
}

#[macro_export]
macro_rules! collect {
    ($($name:ident),*) => {
        $(let (_g, $name) = $crate::common::Collector::new();)*
    };
}

#[derive(Clone, Copy)]
pub struct Collector {
    id: CollectorId,
}

impl Collector {
    pub fn new() -> (CollectorGuard, Self) {
        let id = COLLECTED_DATA.with(|data| data.borrow_mut().insert(Vec::new()));
        (CollectorGuard { id }, Collector { id })
    }
    pub fn push(&self, value: i32) {
        COLLECTED_DATA.with(|data| data.borrow_mut()[self.id].push(value))
    }

    #[track_caller]
    pub fn assert_empty(&self) {
        self.assert_eq(&[])
    }

    #[track_caller]
    pub fn assert_chunk(&self, expected: &[i32]) {
        let rc = COLLECTED_DATA.with(|data| data.clone());
        let vec = &mut rc.borrow_mut()[self.id];
        assert_eq!(vec, expected);
        vec.clear();
    }

    #[track_caller]
    pub fn assert_eq(&self, expected: &[i32]) {
        let rc = COLLECTED_DATA.with(|data| data.clone());
        assert_eq!(&rc.borrow()[self.id], expected);
    }
}
