// ========================
// === LazyInvariantVec ===
// ========================

/// Contiguous, ordered collection of elements with an invariant, determined by a parameter. The
/// invariant is always restored before the elements are observed, but new elements can be added
/// without immediately restoring the invariant.
///
/// # Safety:
///
/// `elements`:
/// The only `unsafe` mutation performed is to restore the invariant, when `elements` is not clean.
/// At that time, we can be sure no borrows of `elements` exist, because:
/// - We never give out a borrow of `elements` unless it is clean.
/// - `elements` never goes from clean to dirty without `&mut` access to `self`.
///
/// `restore_invariant`: No borrow of this escapes its scope.
#[derive(Default)]
pub struct LazyInvariantVec<T, F> {
    elements:          core::cell::UnsafeCell<Vec<T>>,
    clean_up_to:       core::cell::Cell<usize>,
    restore_invariant: core::cell::UnsafeCell<F>,
}

impl<T, F> LazyInvariantVec<T, F> {
    pub fn push(&mut self, t: T) {
        self.elements.get_mut().push(t);
    }
}

impl<T, F> LazyInvariantVec<T, F>
where F: RestoreInvariant<T>
{
    pub fn get_mut(&mut self, i: usize) -> Option<&mut T> {
        self.clean();
        let elements = self.elements.get_mut();
        if i < elements.len() {
            let t = elements.remove(i);
            let i = elements.len();
            self.clean_up_to = i.into();
            elements.push(t);
            elements.get_mut(i)
        } else {
            None
        }
    }

    pub fn as_slice(&self) -> &[T] {
        self.clean()
    }

    pub fn as_mut_slice(&mut self) -> &mut [T] {
        self.clean();
        self.elements.get_mut().as_mut_slice()
    }

    #[allow(unsafe_code)] // See docs for [`Self`].
    fn clean(&self) -> &Vec<T> {
        unsafe {
            let elements = &mut *self.elements.get();
            let clean_up_to = self.clean_up_to.get();
            let restore_invariant = &mut *self.restore_invariant.get();
            // Note: Although `restore_invariant` *should* be a no-op when the whole is already
            // clean, we check that condition here so that memory safety doesn't depend on that
            // property.
            if clean_up_to != elements.len() {
                restore_invariant.restore_invariant(clean_up_to, elements);
                self.clean_up_to.set(elements.len());
            }
            elements
        }
    }
}


// === Trait implementations ===

impl<T, F> core::fmt::Debug for LazyInvariantVec<T, F>
where
    T: core::fmt::Debug,
    F: core::fmt::Debug,
{
    #[allow(unsafe_code)] // Short-lived borrows.
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        unsafe {
            let elements = &*self.elements.get();
            let restore_invariant = &*self.restore_invariant.get();
            f.debug_struct("LazyInvariantVec")
                .field("elements", elements)
                .field("clean_up_to", &self.clean_up_to.get())
                .field("restore_invariant", restore_invariant)
                .finish()
        }
    }
}

impl<T, F> Clone for LazyInvariantVec<T, F>
where
    T: Clone,
    F: Clone + RestoreInvariant<T>,
{
    #[allow(unsafe_code)] // See docs for [`Self`].
    fn clone(&self) -> Self {
        let elements: Vec<_> = self.clean().clone();
        let elements = elements.into();
        let clean_up_to = self.clean_up_to.get().into();
        let restore_invariant = unsafe { (*self.restore_invariant.get()).clone().into() };
        Self { elements, clean_up_to, restore_invariant }
    }
}

impl<T, F> From<Vec<T>> for LazyInvariantVec<T, F>
where F: Default
{
    fn from(elements: Vec<T>) -> Self {
        let elements = elements.into();
        let clean_up_to = Default::default();
        let restore_invariant = Default::default();
        Self { elements, clean_up_to, restore_invariant }
    }
}

impl<T, F> FromIterator<T> for LazyInvariantVec<T, F>
where F: Default
{
    fn from_iter<I>(elements: I) -> Self
    where I: IntoIterator<Item = T> {
        let elements: Vec<_> = elements.into_iter().collect();
        elements.into()
    }
}

impl<T, F> From<LazyInvariantVec<T, F>> for Vec<T>
where F: RestoreInvariant<T>
{
    fn from(mut vec: LazyInvariantVec<T, F>) -> Self {
        vec.clean();
        core::mem::take(vec.elements.get_mut())
    }
}

impl<T, F> IntoIterator for LazyInvariantVec<T, F>
where F: RestoreInvariant<T>
{
    type Item = T;
    type IntoIter = <Vec<T> as IntoIterator>::IntoIter;
    fn into_iter(self) -> Self::IntoIter {
        Vec::from(self).into_iter()
    }
}

impl<T, F> core::ops::Deref for LazyInvariantVec<T, F>
where F: RestoreInvariant<T>
{
    type Target = Vec<T>;
    fn deref(&self) -> &Self::Target {
        self.clean()
    }
}


// === RestoreInvariant ===

pub trait RestoreInvariant<T> {
    fn restore_invariant(&mut self, clean: usize, elements: &'_ mut Vec<T>);
}
