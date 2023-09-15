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
    not_exposed: UnsafeCell<T>,
}

impl<T: Debug> Debug for InitCell<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "InitCell")
    }
}

impl<T> InitCell<T> {
    #[inline(always)]
    pub fn into_inner(self) -> T {
        self.not_exposed.into_inner()
    }
}


impl<T: InitCellContent> InitCell<T> {
    /// Initialize the value stored in this cell if it is empty. Returns `true` if it was empty. It
    /// is impossible to re-initialize the value without requiring mutable access to self, as
    /// there might exist a reference to the value.
    #[inline(always)]
    pub fn init_if_empty(&self, f: impl FnOnce() -> T::Item) -> bool {
        let was_empty = !self.has_item();
        if was_empty {
            // # Safety
            // We checked that the current value does not contain an item. We also know that no one
            // has a reference to [`Self::not_exposed`] (see the docs of [`Self`] to
            // learn more). Therefore, it is safe to overwrite the current value.
            #[allow(unsafe_code)]
            unsafe {
                *self.not_exposed.unchecked_borrow_mut() = T::from_item(f())
            };
        }
        was_empty
    }

    /// Initialize the value stored in this cell with its default value if it is empty. Returns
    /// `true` if it was empty. It is impossible to re-initialize the value without requiring
    /// mutable access to self, as there might exist a reference to the value.
    #[inline(always)]
    pub fn init_default_if_empty(&self) -> bool
    where T::Item: Default {
        self.init_if_empty(default)
    }

    /// Set the internal data of this cell.
    #[inline(always)]
    pub fn set_value(&mut self, internal: T) {
        self.not_exposed = UnsafeCell::new(internal);
    }

    /// Set this value to its default.
    #[inline(always)]
    pub fn set_default(&mut self)
    where T: Default {
        self.set_value(default())
    }

    /// Return a reference to the value stored in this cell, initializing it if it does not exist.
    #[inline(always)]
    pub fn get_or_init(&self, f: impl FnOnce() -> T::Item) -> &T::Item {
        self.init_if_empty(f);
        self.opt_item().unwrap()
    }

    /// Return a reference to the value stored in this cell, initializing it if it does not exist.
    #[inline(always)]
    pub fn get_or_init_default(&self) -> &T::Item
    where T::Item: Default {
        self.get_or_init(default)
    }
}

impl<T: HasItem> HasItem for InitCell<T> {
    type Item = T::Item;
}

impl<T: InitCellContent> OptItemRef for InitCell<T> {
    #[inline(always)]
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
    #[inline(always)]
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
    #[inline(always)]
    fn from_item(item: Self::Item) -> Self {
        Self { not_exposed: UnsafeCell::new(T::from_item(item)) }
    }
}
