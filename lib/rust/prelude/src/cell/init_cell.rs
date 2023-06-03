//! A zero-cost abstraction allowing initialization of some structures without requiring mutable
//! access to them.

use crate::*;



// ================
// === InitCell ===
// ================

/// A type that can be used to parametrize [`InitCell`]. It has to be any container that can store
/// zero or one item, that can be constructed from an item, and that allows getting an optional
/// reference to the stored item.
pub trait InitCellContent = HasSizedItem + OptItemRef + FromItem;

/// A zero-cost abstraction allowing initialization of some structures without requiring mutable
/// access to them. For example, `my_var: InitCell<Option<T>>` can be initialized with a  default
/// `T` value, even if there exists an immutable reference to `my_var`.
#[derive(Default, Zeroable)]
#[repr(transparent)]
pub struct InitCell<T> {
    // # Safety
    // Please note that the implementation never exposes reference to this field. If it was
    // exposed, the implementation of [`Self::init_if_empty`] would be unsound. For example, given
    // the `InitCell<Option<usize>>` parametrization, the user might acquire a reference to a
    // [`None`] value and then have it changed with [`Self::init_if_empty`] while keeping the
    // reference, causing undefined behavior.
    not_exposed: UnsafeCell<T>,
}

impl<T: Debug> Debug for InitCell<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(unsafe { self.not_exposed.unchecked_borrow() }, f)
    }
}

impl<T> InitCell<T> {
    pub fn into_inner(self) -> T {
        self.not_exposed.into_inner()
    }
}


impl<T: InitCellContent> InitCell<T> {
    /// Initialize the value stored in this cell if it is empty. It is impossible to re-initialize
    /// the value without requiring mutable access to self, as there might exist a reference to the
    /// value.
    pub fn init_if_empty(&self, f: impl FnOnce() -> T::Item) {
        if !self.has_item() {
            // # Safety
            // We checked that the current value does not contain an item. We also know that no one
            // has a reference to [`Self::not_exposed`] (see the docs of [`Self`] to
            // learn more). Therefore, it is safe to overwrite the current value.
            #[allow(unsafe_code)]
            unsafe {
                *self.not_exposed.unchecked_borrow_mut() = T::from_item(f())
            };
        }
    }

    /// Set the internal data of this cell.
    pub fn set_value(&mut self, internal: T) {
        self.not_exposed = UnsafeCell::new(internal);
    }

    /// Set this value to its default.
    pub fn set_default(&mut self)
    where T: Default {
        self.set_value(default())
    }
}

impl<T: HasItem> HasItem for InitCell<T> {
    type Item = T::Item;
}

impl<T: InitCellContent> OptItemRef for InitCell<T> {
    fn opt_item(&self) -> Option<&Self::Item> {
        // # Safety
        // Every mutable access to the value stored in [`Self::not_exposed`] requires a mutable
        // access to self. The only exception is [`Self::init_if_empty`], which is safe, as
        // explained there.
        #[allow(unsafe_code)]
        let not_exposed = unsafe { self.not_exposed.unchecked_borrow() };
        not_exposed.opt_item()
    }
}

impl<T: InitCellContent + OptItemRefMut> OptItemRefMut for InitCell<T> {
    fn opt_item_mut(&mut self) -> Option<&mut Self::Item> {
        // # Safety
        // Every mutable access to the value stored in [`Self::not_exposed`] requires a mutable
        // access to self. The only exception is [`Self::init_if_empty`], which is safe, as
        // explained there.
        #[allow(unsafe_code)]
        let not_exposed = unsafe { self.not_exposed.unchecked_borrow_mut() };
        not_exposed.opt_item_mut()
    }
}

impl<T: InitCellContent> FromItem for InitCell<T> {
    fn from_item(item: Self::Item) -> Self {
        Self { not_exposed: UnsafeCell::new(T::from_item(item)) }
    }
}
