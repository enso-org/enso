use std::cell::RefCell;
use std::ops::Deref;

pub trait RefCellOps {
    type Borrowed;
    fn with_borrowed<T>(&self, f: impl FnOnce(&Self::Borrowed) -> T) -> T;
    fn with_borrowed_mut<T>(&self, f: impl FnOnce(&mut Self::Borrowed) -> T) -> T;
}

impl<T> RefCellOps for RefCell<T> {
    type Borrowed = T;
    fn with_borrowed<U>(&self, f: impl FnOnce(&Self::Borrowed) -> U) -> U {
        f(&*self.borrow())
    }
    fn with_borrowed_mut<U>(&self, f: impl FnOnce(&mut Self::Borrowed) -> U) -> U {
        f(&mut *self.borrow_mut())
    }
}


auto trait NotRefCell {}
impl<T> !NotRefCell for RefCell<T> {}

impl<T> RefCellOps for T
where
    T: NotRefCell + Deref,
    <T as Deref>::Target: RefCellOps,
{
    type Borrowed = <<T as Deref>::Target as RefCellOps>::Borrowed;
    fn with_borrowed<U>(&self, f: impl FnOnce(&Self::Borrowed) -> U) -> U {
        self.deref().with_borrowed(f)
    }
    fn with_borrowed_mut<U>(&self, f: impl FnOnce(&mut Self::Borrowed) -> U) -> U {
        self.deref().with_borrowed_mut(f)
    }
}
