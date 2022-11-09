//! HList provides many operations to create and manipulate heterogenous lists (HLists) whose length
//! and element types are known at compile-time. HLists can be used to implement records, variants,
//! type-indexed products (TIP), type-indexed co-products (TIC), or keyword arguments.



// =============
// === HList ===
// =============

/// Type of every `HList`.
pub trait HList = HasLength;

/// Empty `HList` value.
#[derive(Debug, Clone, Copy)]
pub struct Nil;

/// Non-empty `HList` with head and tail.
#[derive(Debug, Clone, Copy)]
#[allow(missing_docs)]
pub struct Cons<Head, Tail>(pub Head, pub Tail);



// === Smart Constructors ===

/// Creates new `HList` from the provided elements, similar to `vec!`. In order to provide type for
/// the list, use the `ty` macro. In order to pattern match on it, use the `pat` macro.
///
/// ```text
/// let HList::pat![t1,t2] : HList::ty![&str,usize] = HList::new!["hello",7];
/// ```
#[macro_export]
macro_rules! new {
    ($(,)*) => { $crate::Nil };
    ($t:expr $(,$($ts:expr),*)?) => {
        $crate::Cons($t,$crate::new!{$($($ts),*)?})
    }
}

/// Pattern matches on a `HList`. See docs of `new` to learn more.
#[macro_export]
macro_rules! pat {
    ($(,)*) => { $crate::Nil };
    ($t:pat $(,$($ts:pat),*)?) => {
        $crate::Cons($t,$crate::pat!{$($($ts),*)?})
    }
}

/// Smart `HList` type constructor. See docs of `new` to learn more.
#[macro_export]
macro_rules! ty {
    ($(,)*) => { $crate::Nil };
    ($t:ty $(,$($ts:ty),*)?) => {
        $crate::Cons<$t,$crate::ty!{$($($ts),*)?}>
    }
}



// ==============
// === Length ===
// ==============

/// Compile-time known length value.
#[allow(missing_docs)]
pub trait HasLength {
    const LEN: usize;
    fn len() -> usize {
        Self::LEN
    }
}

/// Compile-time known length value.
pub const fn len<T: HasLength>() -> usize {
    <T as HasLength>::LEN
}

impl HasLength for Nil {
    const LEN: usize = 0;
}
impl<H, T: HasLength> HasLength for Cons<H, T> {
    const LEN: usize = 1 + len::<T>();
}



// ============
// === Head ===
// ============

/// Head element accessor.
#[allow(missing_docs)]
pub trait KnownHead {
    type Head;
}

/// Head element type accessor.
pub type Head<T> = <T as KnownHead>::Head;

/// Head element accessor.
#[allow(missing_docs)]
pub trait GetHead: KnownHead {
    fn head(&self) -> &Self::Head;
}

/// Mutable head element accessor.
#[allow(missing_docs)]
pub trait GetHeadMut: KnownHead {
    fn head_mut(&mut self) -> &mut Self::Head;
}

/// Head element clone.
#[allow(missing_docs)]
pub trait GetHeadClone: KnownHead {
    fn head_clone(&self) -> Self::Head;
}

impl<T> GetHeadClone for T
where
    T: GetHead,
    Head<T>: Clone,
{
    default fn head_clone(&self) -> Self::Head {
        self.head().clone()
    }
}


// === Impls ===

impl<H, T> KnownHead for Cons<H, T> {
    type Head = H;
}

impl<H, T> GetHead for Cons<H, T> {
    fn head(&self) -> &Self::Head {
        &self.0
    }
}

impl<H, T> GetHeadMut for Cons<H, T> {
    fn head_mut(&mut self) -> &mut Self::Head {
        &mut self.0
    }
}



// ============
// === Tail ===
// ============

/// Tail element accessor.
#[allow(missing_docs)]
pub trait KnownTail {
    type Tail;
}

/// Tail element type accessor.
pub type Tail<T> = <T as KnownTail>::Tail;

/// Tail element accessor.
#[allow(missing_docs)]
pub trait GetTail: KnownTail {
    fn tail(&self) -> &Self::Tail;
}

/// Mutable tail element accessor.
#[allow(missing_docs)]
pub trait GetTailMut: KnownTail {
    fn tail_mut(&mut self) -> &mut Self::Tail;
}

/// Tail element clone.
#[allow(missing_docs)]
pub trait GetTailClone: KnownTail {
    fn tail_clone(&self) -> Self::Tail;
}

impl<T> GetTailClone for T
where
    T: GetTail,
    Tail<T>: Clone,
{
    default fn tail_clone(&self) -> Self::Tail {
        self.tail().clone()
    }
}


// === Impls ===

impl<H, T> KnownTail for Cons<H, T> {
    type Tail = T;
}

impl<H, T> GetTail for Cons<H, T> {
    fn tail(&self) -> &Self::Tail {
        &self.1
    }
}

impl<H, T> GetTailMut for Cons<H, T> {
    fn tail_mut(&mut self) -> &mut Self::Tail {
        &mut self.1
    }
}



// ============
// === Last ===
// ============

/// Last element accessor.
#[allow(missing_docs)]
pub trait KnownLast {
    type Last;
}

/// Last element type accessor.
pub type Last<T> = <T as KnownLast>::Last;

/// Last element accessor.
#[allow(missing_docs)]
pub trait GetLast: KnownLast {
    fn last(&self) -> &Self::Last;
}

/// Mutable last element accessor.
#[allow(missing_docs)]
pub trait GetLastMut: KnownLast {
    fn last_mut(&mut self) -> &mut Self::Last;
}

/// Last element clone.
#[allow(missing_docs)]
pub trait GetLastClone: KnownLast {
    fn last_clone(&self) -> Self::Last;
}

impl<T> GetLastClone for T
where
    T: GetLast,
    Last<T>: Clone,
{
    default fn last_clone(&self) -> Self::Last {
        self.last().clone()
    }
}


// === Impls ===

impl<H> KnownLast for Cons<H, Nil> {
    type Last = H;
}
impl<H, T: KnownLast> KnownLast for Cons<H, T> {
    type Last = Last<T>;
}

impl<H> GetLast for Cons<H, Nil> {
    fn last(&self) -> &Self::Last {
        &self.0
    }
}

impl<H> GetLastMut for Cons<H, Nil> {
    fn last_mut(&mut self) -> &mut Self::Last {
        &mut self.0
    }
}

impl<H, T: GetLast> GetLast for Cons<H, T> {
    fn last(&self) -> &Self::Last {
        self.tail().last()
    }
}

impl<H, T: GetLastMut> GetLastMut for Cons<H, T> {
    fn last_mut(&mut self) -> &mut Self::Last {
        self.tail_mut().last_mut()
    }
}



// ============
// === Init ===
// ============

/// Init elements accessor (all but last).
#[allow(missing_docs)]
pub trait KnownInit {
    type Init;
}

/// Init elements type accessor.
pub type Init<T> = <T as KnownInit>::Init;

/// Init element clone.
#[allow(missing_docs)]
pub trait GetInitClone: KnownInit {
    fn init_clone(&self) -> Self::Init;
}


// === Impls ===

impl<H> KnownInit for Cons<H, Nil> {
    type Init = Nil;
}
impl<H, T: KnownInit> KnownInit for Cons<H, T> {
    type Init = Cons<H, Init<T>>;
}

impl<H> GetInitClone for Cons<H, Nil> {
    fn init_clone(&self) -> Self::Init {
        Nil
    }
}

impl<H: Clone, T: GetInitClone> GetInitClone for Cons<H, T> {
    fn init_clone(&self) -> Self::Init {
        Cons(self.head().clone(), self.tail().init_clone())
    }
}



// ================
// === PushBack ===
// ================

// TODO: Consider implementing PushBack for everything that converts to and from HList.

/// Add a new element to the back of the list.
#[allow(missing_docs)]
pub trait PushBack<T>: Sized {
    type Output: KnownLast<Last = T> + KnownInit<Init = Self>;
    fn push_back(self, t: T) -> Self::Output;
}

impl<X> PushBack<X> for Nil {
    type Output = Cons<X, Nil>;
    #[inline(always)]
    fn push_back(self, x: X) -> Self::Output {
        Cons(x, Nil)
    }
}

impl<X, H, T> PushBack<X> for Cons<H, T>
where T: PushBack<X>
{
    type Output = Cons<H, <T as PushBack<X>>::Output>;
    #[inline(always)]
    fn push_back(self, x: X) -> Self::Output {
        let Cons(head, tail) = self;
        Cons(head, tail.push_back(x))
    }
}



// ===============
// === PopBack ===
// ===============

// TODO: Consider implementing PopBack for everything that converts to and from HList.

/// Remove the last element of the list and return it and the new list.
#[allow(missing_docs)]
pub trait PopBack: KnownLast + KnownInit {
    fn pop_back(self) -> (Self::Last, Self::Init);
}

impl<H> PopBack for Cons<H, Nil> {
    fn pop_back(self) -> (Self::Last, Self::Init) {
        (self.0, Nil)
    }
}

impl<H, T> PopBack for Cons<H, T>
where T: PopBack
{
    fn pop_back(self) -> (Self::Last, Self::Init) {
        let (last, tail) = self.1.pop_back();
        (last, Cons(self.0, tail))
    }
}
