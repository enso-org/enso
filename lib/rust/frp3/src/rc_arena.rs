use bumpalo::Bump;
use std::any::TypeId;
use std::cell::Cell;
use std::marker::PhantomData;
use std::marker::Unsize;
use std::mem;
use std::mem::ManuallyDrop;
use std::ops::CoerceUnsized;
use std::ops::Deref;
use std::ptr::NonNull;
use std::ptr::{self};
use std::rc::Rc;

use crate::Data;

pub struct RcArena {
    arena: Rc<Bump>,
}

impl RcArena {
    pub fn new() -> Self {
        RcArena { arena: Rc::new(Bump::new()) }
    }

    pub fn alloc<T>(&self, value: T) -> BumpRc<T> {
        let arena = Rc::clone(&self.arena);
        let arena = Rc::into_raw(arena);
        let inner = self
            .arena
            .alloc(RcBox { strong: Cell::new(1), arena, value });
        BumpRc { ptr: inner.into(), phantom: PhantomData }
    }
}

#[repr(C)]
struct RcBox<T: ?Sized> {
    strong: Cell<usize>,
    arena:  *const Bump,
    value:  T,
}

#[repr(transparent)]
pub struct BumpRc<T: ?Sized> {
    ptr:     NonNull<RcBox<T>>,
    phantom: PhantomData<RcBox<T>>,
}

impl<T: ?Sized + std::fmt::Debug> std::fmt::Debug for BumpRc<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.deref().fmt(f)
    }
}

impl<T: ?Sized + Unsize<U>, U: ?Sized> CoerceUnsized<BumpRc<U>> for BumpRc<T> {}

impl<T: ?Sized> Clone for BumpRc<T> {
    fn clone(&self) -> Self {
        unsafe {
            self.inner().inc_strong();
            BumpRc::from_inner(self.ptr)
        }
    }
}

impl<T: ?Sized> Deref for BumpRc<T> {
    type Target = T;

    #[inline(always)]
    fn deref(&self) -> &T {
        &self.inner().value
    }
}

impl<T: ?Sized + Eq> Eq for BumpRc<T> {}
impl<T: ?Sized + PartialEq> PartialEq for BumpRc<T> {
    #[inline]
    fn eq(&self, other: &BumpRc<T>) -> bool {
        BumpRc::ptr_eq(self, other) || **self == **other
    }

    #[inline]
    fn ne(&self, other: &BumpRc<T>) -> bool {
        !BumpRc::ptr_eq(self, other) && **self != **other
    }
}

impl<T> BumpRc<T> {
    /// Create a new `BumpRc` with its own dedicated allocation, not tied to any arena. This is
    /// useful in cases when an unbounded number of `BumpRc`s is needed in rare situations, and we
    /// don't want to fragment the arena.
    pub fn alloc_standalone(value: T) -> Self {
        let arena = ptr::null();
        let inner = Box::new(RcBox { strong: Cell::new(1), arena, value });
        let ptr = unsafe { NonNull::new_unchecked(Box::into_raw(inner)) };
        BumpRc { ptr, phantom: PhantomData }
    }

    #[must_use]
    pub fn alloc_standalone_uninit() -> BumpRc<mem::MaybeUninit<T>> {
        BumpRc::alloc_standalone(mem::MaybeUninit::uninit())
    }
}

impl BumpRc<dyn Data> {
    /// Writes into a dynamic mutable BumpRc.
    ///
    /// If there are other BumpRc pointers to the same allocation, then `write_mut_and_write_dyn`
    /// will create new allocation to ensure unique ownership, and replace the passed `BumpRc` with
    /// new one. This is also referred to as clone-on-write. The new `BumpRc` is no longer
    /// associated with original Rc's arena, but instead is a standalone allocation, as created
    /// with `BumpRc::alloc_standalone`.
    ///
    /// However, if there are no other BumpRc pointers to this allocation, then it will be reused.
    ///
    /// If the stored value is not a `T`, then `Err` is returned with the passed in value.
    pub fn make_write_dyn<T: Data>(this: &mut Self, value: T) -> Result<(), T> {
        if (&*this).as_any().type_id() == TypeId::of::<T>() {
            return Err(value);
        }

        if BumpRc::strong_count(this) != 1 {
            // There are other Rcs, need to create a new allocation.
            // Pre-allocate memory to allow writing the cloned value directly.
            let mut rc = BumpRc::<T>::alloc_standalone_uninit();
            unsafe {
                BumpRc::get_mut_unchecked(&mut rc).write(value);
                *this = rc.assume_init();
            }
        } else {
            // This unsafety is ok because we're guaranteed that the pointer
            // returned is the *only* pointer that will ever be returned to T. Our
            // reference count is guaranteed to be 1 at this point, and we required
            // the `BumpRc<T>` itself to be `mut`, so we're returning the only possible
            // reference to the allocation, and we know that it is of the correct type.
            let box_ptr = this.ptr.cast::<RcBox<T>>();
            let mut_ref = unsafe { &mut (*box_ptr.as_ptr()).value };
            *mut_ref = value;
        }

        Ok(())
    }

    /// Attempt to downcast the `BumpRc<dyn Data>` to a concrete type.
    #[allow(dead_code)]
    pub(crate) fn downcast_ref_data<T: Data>(&self) -> Result<&BumpRc<T>, &BumpRc<dyn Data>> {
        if (&**self).as_any().type_id() == TypeId::of::<T>() {
            // # Safety
            //
            // [`Data`] trait requires [`Any`], and we just checked that `data` is of type `T`.
            // Therefore it is safe to downcast it.
            unsafe { Ok(self.downcast_ref_data_unchecked()) }
        } else {
            Err(self)
        }
    }

    /// Downcasts the `BumpRc<dyn Data>` to a concrete type.
    /// # Safety
    ///
    /// The contained value must be of type `T`. Calling this method with the incorrect type is
    /// *undefined behavior*.
    pub(crate) unsafe fn downcast_ref_data_unchecked<T: Data>(&self) -> &BumpRc<T> {
        debug_assert!(
            (&**self).as_any().type_id() == TypeId::of::<T>(),
            "Incorrect Data type. Expected {}",
            std::any::type_name::<T>(),
        );

        let ptr = self as *const Self as *const BumpRc<T>;
        &*ptr
    }

    /// Attempt to downcast the `BumpRc<dyn Data>` to a concrete type.
    #[allow(dead_code)]
    pub(crate) fn downcast_data<T: Data>(self) -> Result<BumpRc<T>, Self> {
        if (&*self).as_any().type_id() == TypeId::of::<T>() {
            // # Safety
            //
            // [`Data`] trait requires [`Any`], and we just checked that `data` is of type `T`.
            // Therefore it is safe to downcast it.
            unsafe { Ok(self.downcast_data_unchecked()) }
        } else {
            Err(self)
        }
    }

    /// Downcasts the `BumpRc<dyn Data>` to a concrete type.
    /// # Safety
    ///
    /// The contained value must be of type `T`. Calling this method with the incorrect type is
    /// *undefined behavior*.
    pub(crate) unsafe fn downcast_data_unchecked<T: Data>(self) -> BumpRc<T> {
        debug_assert!(
            (&*self).as_any().type_id() == TypeId::of::<T>(),
            "Incorrect Data type. Expected {}",
            std::any::type_name::<T>(),
        );

        let ptr = self.ptr.cast::<RcBox<T>>();
        mem::forget(self);
        BumpRc::from_inner(ptr)
    }
}

impl<T> BumpRc<mem::MaybeUninit<T>> {
    pub unsafe fn assume_init(self) -> BumpRc<T> {
        unsafe { BumpRc::from_inner(mem::ManuallyDrop::new(self).ptr.cast()) }
    }
}

impl<T: ?Sized> BumpRc<T> {
    #[inline]
    fn inner(&self) -> &RcBox<T> {
        // This unsafety is ok because while this Rc is alive we're guaranteed that the inner
        // pointer is valid.
        unsafe { self.ptr.as_ref() }
    }

    #[inline]
    pub fn strong_count(this: &Self) -> usize {
        this.inner().strong()
    }

    unsafe fn from_inner(ptr: NonNull<RcBox<T>>) -> Self {
        Self { ptr, phantom: PhantomData }
    }

    #[inline]
    fn is_unique(this: &Self) -> bool {
        BumpRc::strong_count(this) == 1
    }

    #[inline]
    pub fn get_mut(this: &mut Self) -> Option<&mut T> {
        if BumpRc::is_unique(this) {
            unsafe { Some(BumpRc::get_mut_unchecked(this)) }
        } else {
            None
        }
    }

    #[inline]
    pub unsafe fn get_mut_unchecked(this: &mut Self) -> &mut T {
        // We are careful to *not* create a reference covering the "count" fields, as
        // this would conflict with accesses to the reference counts.
        unsafe { &mut (*this.ptr.as_ptr()).value }
    }

    pub fn ptr_eq(this: &Self, other: &Self) -> bool {
        this.ptr.as_ptr() == other.ptr.as_ptr()
    }

    pub fn as_ptr(this: &Self) -> *const T {
        let ptr: *mut RcBox<T> = NonNull::as_ptr(this.ptr);

        // SAFETY: This cannot go through Deref::deref or BumpRc::inner because
        // this is required to retain raw/mut provenance such that e.g. `get_mut` can
        // write through the pointer after the BumpRc is recovered through `from_raw`.
        unsafe { ptr::addr_of_mut!((*ptr).value) }
    }

    pub fn into_raw(this: Self) -> *const T {
        let ptr = Self::as_ptr(&this);
        mem::forget(this);
        ptr
    }

    unsafe fn from_ptr(ptr: *mut RcBox<T>) -> Self {
        unsafe { Self::from_inner(NonNull::new_unchecked(ptr)) }
    }

    pub unsafe fn from_raw(ptr: *const T) -> Self {
        let offset = unsafe { data_offset(ptr) };

        // Reverse the offset to find the original RcBox.
        let rc_ptr = unsafe { ptr.byte_sub(offset) as *mut RcBox<T> };

        unsafe { Self::from_ptr(rc_ptr) }
    }
}

/// Get the offset within an `RcBox` for the payload behind a pointer.
///
/// # Safety
///
/// The pointer must point to (and have valid metadata for) a previously
/// valid instance of T, but the T is allowed to be dropped.
unsafe fn data_offset<T: ?Sized>(ptr: *const T) -> usize {
    // Align the unsized value to the end of the RcBox.
    // Because RcBox is repr(C), it will always be the last field in memory.
    // SAFETY: since the only unsized types possible are slices, trait objects,
    // and extern types, the input safety requirement is currently enough to
    // satisfy the requirements of align_of_val_raw; this is an implementation
    // detail of the language that must not be relied upon outside of std.
    unsafe { data_offset_align(mem::align_of_val_raw(ptr)) }
}

#[inline]
fn data_offset_align(align: usize) -> usize {
    let layout = std::alloc::Layout::new::<RcBox<()>>();
    layout.size().wrapping_add(align).wrapping_sub(1) & !align.wrapping_sub(1)
}

impl<T: ?Sized> RcBox<T> {
    fn strong(&self) -> usize {
        self.strong.get()
    }

    fn inc_strong(&self) {
        self.strong.set(self.strong() + 1);
    }

    fn dec_strong(&self) {
        self.strong.set(self.strong() - 1);
    }
}

impl<T: ?Sized> Drop for BumpRc<T> {
    fn drop(&mut self) {
        unsafe {
            self.inner().dec_strong();
            if self.inner().strong() == 0 {
                // Destroy the contained object.
                ptr::drop_in_place(&mut (*self.ptr.as_ptr()).value);

                // Drop arena Rc, if this Rc was allocated from an arena.
                let arena_ptr = (*self.ptr.as_ptr()).arena;
                if !arena_ptr.is_null() {
                    drop(Rc::from_raw(arena_ptr));
                } else {
                    // Otherwise, it was allocated as box. Free the memory, but don't drop its
                    // contents second time, as it was already dropped.
                    drop(Box::from_raw(self.ptr.as_ptr() as *mut ManuallyDrop<T>))
                }
            }
        }
    }
}
