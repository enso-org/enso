//! This module contains implementation of various dirty flags. A dirty flag is a structure which
//! remembers that something was changed, but not updated yet. For example, dirty flags are useful
//! when defining OpenGL buffer management. When a data in CPU-buffer changes, dirty flags can keep
//! a set of changed indexes and bulk-update the GPU-buffers every animation frame. You can think
//! of dirty flags like about a way to introduce laziness to the program evaluation mechanisms.

use crate::data::function::traits::*;
use crate::prelude::*;

use rustc_hash::FxHashSet;
use std::hash::Hash;
use std::mem;



// ==================
// === Operations ===
// ==================

/// Common traits for dirty flags.
pub mod traits {
    use super::*;

    // === Arg ===

    /// Abstraction for dirty flags which accept an argument.
    #[allow(missing_docs)]
    pub trait HasArg {
        type Arg;
    }
    /// The argument type-level getter.
    pub type Arg<T> = <T as HasArg>::Arg;


    // === Global Operations ===

    /// Abstraction for dirty flags which can be checked for being dirty.
    #[allow(missing_docs)]
    pub trait HasCheckAll {
        fn check_all(&self) -> bool;
    }

    /// Abstraction for dirty flags which can be unset (set to clean).
    #[allow(missing_docs)]
    pub trait HasUnsetAll {
        fn unset_all(&mut self);
    }


    // === Arity-0 Operations ===

    /// Abstraction for dirty flags which can perform a dirty check without requiring an argument.
    #[allow(missing_docs)]
    pub trait HasCheck0 {
        fn check(&self) -> bool;
    }

    /// Abstraction for dirty flags which can be set without requiring an argument.
    #[allow(missing_docs)]
    pub trait HasSet0 {
        fn set(&mut self);
    }

    /// Abstraction for dirty flags which can be unset without requiring an argument.
    #[allow(missing_docs)]
    pub trait HasUnset0 {
        fn unset(&mut self);
    }


    // === Arity-1 Operations ===

    /// Abstraction for dirty flags which can perform a dirty check by providing a single argument.
    #[allow(missing_docs)]
    pub trait HasCheck1: HasArg {
        fn check(&self, arg: &Self::Arg) -> bool;
    }

    /// Abstraction for dirty flags which can be set by providing a single argument.
    #[allow(missing_docs)]
    pub trait HasSet1: HasArg {
        fn set(&mut self, arg: Self::Arg);
    }

    /// Abstraction for dirty flags which can be unset by providing a single argument.
    #[allow(missing_docs)]
    pub trait HasUnset1: HasArg {
        fn unset(&mut self, arg: &Self::Arg);
    }


    // === Shared Global Operations ===

    /// Abstraction for dirty flags which can be unset (set to clean) without requiring mutable
    /// access to self.
    #[allow(missing_docs)]
    pub trait SharedHasUnsetAll {
        fn unset_all(&self);
    }


    // === Shared Arity-0 Operations ===

    /// Abstraction for dirty flags which can be set without requiring an argument and without
    /// requiring mutable access to self.
    #[allow(missing_docs)]
    pub trait SharedHasSet0 {
        fn set(&self);
    }

    /// Abstraction for dirty flags which can be unset without requiring an argument and without
    /// requiring mutable access to self.
    #[allow(missing_docs)]
    pub trait SharedHasUnset0 {
        fn unset(&self);
    }


    // === Shared Arity-1 Operations ===

    /// Abstraction for dirty flags which can be set by providing a single argument without
    /// requiring mutable access to self.
    #[allow(missing_docs)]
    pub trait SharedHasSet1: HasArg {
        fn set(&self, arg: Self::Arg);
    }

    /// Abstraction for dirty flags which can be unset by providing a single argument without
    /// requiring mutable access to self.
    #[allow(missing_docs)]
    pub trait SharedHasUnset1: HasArg {
        fn unset(&self, arg: &Self::Arg);
    }

    // === Type Aliases ===

    /// Trait alias for bounds required by all dirty flags.
    pub trait FlagOps = Debug + HasCheckAll + HasUnsetAll;

    /// Trait alias for bounds required by all dirty flags which does not accept an argument.
    pub trait FlagOps0 = FlagOps + HasCheck0 + HasSet0;

    /// Trait alias for bounds required by all dirty flags which accept an argument.
    pub trait FlagOps1 = FlagOps + HasCheck1 + HasSet1 where Arg<Self>: Debug;
}

pub use traits::*;



// ============
// === Flag ===
// ============

// === Definition ===

/// Abstraction for every dirty flag implementation. It is a smart struct adding
/// logging and callback utilities to the underlying data. Moreover, it
/// implements public API for working with dirty flags.
#[derive(Derivative)]
#[derivative(Debug(bound = "T:Debug"))]
#[allow(missing_docs)]
pub struct Flag<T, OnMut> {
    pub data: T,
    #[derivative(Debug = "ignore")]
    on_set:   OnMut,
}


// === Basics ===

impl<OnMut, T: Default> Flag<T, OnMut> {
    /// Constructor.
    pub fn new(on_set: OnMut) -> Self {
        let data = default();
        Self { data, on_set }
    }

    /// Unsets the flag and returns its dirty value.
    pub fn take(&mut self) -> T {
        mem::take(&mut self.data)
    }
}


// === Arguments ===

impl<T: HasArg, OnMut> HasArg for Flag<T, OnMut> {
    type Arg = Arg<T>;
}


// === Global Operations ===

impl<T: HasCheckAll, OnMut> HasCheckAll for Flag<T, OnMut> {
    fn check_all(&self) -> bool {
        self.data.check_all()
    }
}

impl<T: HasUnsetAll, OnMut> HasUnsetAll for Flag<T, OnMut> {
    fn unset_all(&mut self) {
        self.data.unset_all()
    }
}


// === Check ===

impl<T: FlagOps0, OnMut> HasCheck0 for Flag<T, OnMut> {
    fn check(&self) -> bool {
        self.data.check()
    }
}

impl<T: FlagOps1, OnMut> HasCheck1 for Flag<T, OnMut> {
    fn check(&self, arg: &Self::Arg) -> bool {
        self.data.check(arg)
    }
}


// === Set ===

impl<T: FlagOps0, OnMut: FnMut0> HasSet0 for Flag<T, OnMut> {
    fn set(&mut self) {
        let is_set = self.data.check_all();
        if !is_set {
            self.data.set();
            debug_span!("Setting.").in_scope(|| {
                self.on_set.call();
            })
        }
    }
}

impl<T: FlagOps1, OnMut: FnMut0> HasSet1 for Flag<T, OnMut> {
    fn set(&mut self, arg: Self::Arg) {
        let first_set = !self.check_all();
        let is_set = self.data.check(&arg);
        if !is_set {
            self.data.set(arg);
            debug_span!("Setting to {:?}.", self.data).in_scope(|| {
                if first_set {
                    self.on_set.call();
                }
            })
        }
    }
}


// === Unset ===

impl<T: HasUnset0, OnMut> HasUnset0 for Flag<T, OnMut> {
    fn unset(&mut self) {
        trace!("Unsetting.");
        self.data.unset()
    }
}

impl<T: HasUnset1, OnMut> HasUnset1 for Flag<T, OnMut>
where Arg<T>: Display
{
    fn unset(&mut self, arg: &Self::Arg) {
        trace!("Unsetting {arg}.");
        self.data.unset(arg)
    }
}



// ==================
// === RefCellFlag ===
// ==================

// === Definition ===

/// A version of `Flag` which uses internal mutability pattern. It is meant to expose the same
/// API but without requiring `self` reference to be mutable. This version does not allow for
/// cloning the flag. If you want to clone it you either need to put it in something like [`Rc`] or
/// use the [`SharedFlag`] instead.
#[derive(Derivative, From)]
#[derivative(Debug(bound = "T:Debug"))]
#[repr(transparent)]
pub struct RefCellFlag<T, OnMut> {
    data: RefCell<Flag<T, OnMut>>,
}


// === API ===

impl<T: Default, OnMut> RefCellFlag<T, OnMut> {
    /// Constructor.
    pub fn new(on_set: OnMut) -> Self {
        Self { data: RefCell::new(Flag::new(on_set)) }
    }

    /// Unsets the flag and returns its dirty value.
    pub fn take(&self) -> T {
        self.data.borrow_mut().take()
    }
}

impl<T, OnMut> RefCellFlag<T, OnMut> {
    /// Replace the callback of the flag.
    pub fn set_callback(&self, on_set: OnMut) {
        self.data.borrow_mut().on_set = on_set;
    }
}


// === Arg ===

impl<T: HasArg, OnMut> HasArg for RefCellFlag<T, OnMut> {
    type Arg = Arg<T>;
}


// === Global Operations ===

impl<T: HasUnsetAll, OnMut> SharedHasUnsetAll for RefCellFlag<T, OnMut> {
    fn unset_all(&self) {
        self.data.borrow_mut().unset_all()
    }
}

impl<T: HasCheckAll, OnMut> HasCheckAll for RefCellFlag<T, OnMut> {
    fn check_all(&self) -> bool {
        self.data.borrow().check_all()
    }
}

// === Check ===

impl<T: FlagOps0, OnMut> HasCheck0 for RefCellFlag<T, OnMut> {
    fn check(&self) -> bool {
        self.data.borrow().check()
    }
}

impl<T: FlagOps1, OnMut> HasCheck1 for RefCellFlag<T, OnMut> {
    fn check(&self, arg: &Arg<T>) -> bool {
        self.data.borrow().check(arg)
    }
}

// === Set ===

impl<T: FlagOps0, OnMut: FnMut0> SharedHasSet0 for RefCellFlag<T, OnMut> {
    fn set(&self) {
        self.data.borrow_mut().set()
    }
}

impl<T: FlagOps1, OnMut: FnMut0> SharedHasSet1 for RefCellFlag<T, OnMut> {
    fn set(&self, arg: Arg<T>) {
        self.data.borrow_mut().set(arg)
    }
}

// === Unset ===

impl<T: HasUnset0, OnMut> SharedHasUnset0 for RefCellFlag<T, OnMut> {
    fn unset(&self) {
        self.data.borrow_mut().unset()
    }
}

impl<T: HasUnset1, OnMut> SharedHasUnset1 for RefCellFlag<T, OnMut>
where Arg<T>: Display
{
    fn unset(&self, arg: &Self::Arg) {
        self.data.borrow_mut().unset(arg)
    }
}



// ==================
// === SharedFlag ===
// ==================

// === Definition ===

/// A version of `Flag` which uses internal mutability pattern. It is meant to expose the same
/// API but without requiring `self` reference to be mutable. This version implements cloning. If
/// you don't need it, or you want to store a bunch of flags enclosed in a single [`Rc`], use the
/// [`RefCellFlag`] instead.
#[derive(Derivative, CloneRef, From, Deref)]
#[derivative(Debug(bound = "T:Debug"))]
#[derivative(Clone(bound = ""))]
#[repr(transparent)]
pub struct SharedFlag<T, OnMut> {
    rc: Rc<RefCellFlag<T, OnMut>>,
}


// === API ===

impl<T: Default, OnMut> SharedFlag<T, OnMut> {
    /// Constructor.
    pub fn new(on_set: OnMut) -> Self {
        Self { rc: Rc::new(RefCellFlag::new(on_set)) }
    }
}


// === Arg ===

impl<T: HasArg, OnMut> HasArg for SharedFlag<T, OnMut> {
    type Arg = Arg<T>;
}


// === Global Operations ===

impl<T: HasUnsetAll, OnMut> SharedHasUnsetAll for SharedFlag<T, OnMut> {
    fn unset_all(&self) {
        self.rc.unset_all()
    }
}

impl<T: HasCheckAll, OnMut> HasCheckAll for SharedFlag<T, OnMut> {
    fn check_all(&self) -> bool {
        self.rc.check_all()
    }
}

// === Check ===

impl<T: FlagOps0, OnMut> HasCheck0 for SharedFlag<T, OnMut> {
    fn check(&self) -> bool {
        self.rc.check()
    }
}

impl<T: FlagOps1, OnMut> HasCheck1 for SharedFlag<T, OnMut> {
    fn check(&self, arg: &Arg<T>) -> bool {
        self.rc.check(arg)
    }
}

// === Set ===

impl<T: FlagOps0, OnMut: FnMut0> SharedHasSet0 for SharedFlag<T, OnMut> {
    fn set(&self) {
        self.rc.set()
    }
}

impl<T: FlagOps1, OnMut: FnMut0> SharedHasSet1 for SharedFlag<T, OnMut> {
    fn set(&self, arg: Arg<T>) {
        self.rc.set(arg)
    }
}

// === Unset ===

impl<T: HasUnset0, OnMut> SharedHasUnset0 for SharedFlag<T, OnMut> {
    fn unset(&self) {
        self.rc.unset()
    }
}

impl<T: HasUnset1, OnMut> SharedHasUnset1 for SharedFlag<T, OnMut>
where Arg<T>: Display
{
    fn unset(&self, arg: &Self::Arg) {
        self.rc.unset(arg)
    }
}



// ======================
// === WeakSharedFlag ===
// ======================

/// A weak version of [`SharedFlag`].
#[derive(Derivative)]
#[derivative(Debug(bound = "T:Debug"))]
#[derivative(Clone(bound = ""))]
#[repr(transparent)]
pub struct WeakSharedFlag<T, OnMut> {
    weak: Weak<RefCellFlag<T, OnMut>>,
}
impl<T, OnMut> SharedFlag<T, OnMut> {
    /// Downgrade the flag to its weak version.
    pub fn downgrade(&self) -> WeakSharedFlag<T, OnMut> {
        let weak = self.rc.downgrade();
        WeakSharedFlag { weak }
    }
}
impl<T, OnMut> WeakSharedFlag<T, OnMut> {
    /// Upgrade the flag to its strong version.
    pub fn upgrade(&self) -> Option<SharedFlag<T, OnMut>> {
        self.weak.upgrade().map(|rc| SharedFlag { rc })
    }
}



// =================================================================================================
// === Flag Definitions ============================================================================
// =================================================================================================

macro_rules! define_flag {
    ($(#$meta:tt)* $name:ident $(< $($param:tt),* $(,)? >)?) => { paste! {
        $(#$meta)*
        pub type $name< $($($param,)*)? OnMut = ()> =
            Flag<[<$name Data>] $(<$($param,)*>)?, OnMut>;

        /// A version with an internal mutability pattern.
        $(#$meta)*
        pub type [<RefCell $name>]< $($($param,)*)? OnMut = ()> =
            RefCellFlag<[<$name Data>] $(<$($param,)*>)?, OnMut>;

        /// A version with an internal mutability pattern and a reference counting mechanism.
        /// Can be cloned and downgraded to a weak reference.
        $(#$meta)*
        pub type [<Shared $name>]< $($($param,)*)? OnMut = ()> =
            SharedFlag<[<$name Data>] $(<$($param,)*>)?, OnMut>;

        /// A weak version of the shared flag.
        $(#$meta)*
        pub type [<WeakShared $name>]< $($($param,)*)? OnMut = ()> =
            WeakSharedFlag<[<$name Data>] $(<$($param,)*>)?, OnMut>;
    }};
}



// ============
// === Bool ===
// ============

define_flag! {
    /// The on / off dirty flag. If you need a simple dirty / clean switch, this one
    /// is the right choice.
    Bool
}

/// Internal representation of the [`Bool`] flag.
#[derive(Clone, Copy, Debug, Display, Default)]
pub struct BoolData {
    is_dirty: bool,
}

impl HasCheckAll for BoolData {
    fn check_all(&self) -> bool {
        self.is_dirty
    }
}

impl HasUnsetAll for BoolData {
    fn unset_all(&mut self) {
        self.is_dirty = false
    }
}

impl HasCheck0 for BoolData {
    fn check(&self) -> bool {
        self.is_dirty
    }
}

impl HasSet0 for BoolData {
    fn set(&mut self) {
        self.is_dirty = true
    }
}

impl HasUnset0 for BoolData {
    fn unset(&mut self) {
        self.is_dirty = false
    }
}



// =============
// === Range ===
// =============

define_flag! {
    /// Dirty flag which keeps information about a range of dirty items. It does not track items
    /// separately, nor you are allowed to keep multiple ranges in it. Just a single value range.
    Range<Ix>
}

pub trait RangeIx = PartialOrd + Copy + Debug;

/// Internal representation of the [`Range`] flag.
#[derive(Debug, Default)]
#[allow(missing_docs)]
pub struct RangeData<Ix = usize> {
    pub range: Option<RangeInclusive<Ix>>,
}

impl<Ix> HasArg for RangeData<Ix> {
    type Arg = Ix;
}

impl<Ix> HasCheckAll for RangeData<Ix> {
    fn check_all(&self) -> bool {
        self.range.is_some()
    }
}

impl<Ix> HasUnsetAll for RangeData<Ix> {
    fn unset_all(&mut self) {
        self.range = None
    }
}

impl<Ix: RangeIx> HasCheck1 for RangeData<Ix> {
    fn check(&self, ix: &Ix) -> bool {
        self.range.as_ref().map(|r| r.contains(ix)) == Some(true)
    }
}

impl<Ix: RangeIx> HasSet1 for RangeData<Ix> {
    fn set(&mut self, ix: Ix) {
        self.range = match &self.range {
            None => Some(ix..=ix),
            Some(r) =>
                if ix < *r.start() {
                    Some(ix..=*r.end())
                } else if ix > *r.end() {
                    Some(*r.start()..=ix)
                } else {
                    Some(r.clone())
                },
        };
    }
}

impl<Ix: RangeIx> HasUnset1 for RangeData<Ix> {
    fn unset(&mut self, _arg: &Self::Arg) {}
}

impl<Ix: RangeIx> Display for RangeData<Ix> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            self.range
                .as_ref()
                .map(|t| format!("[{:?}...{:?}]", t.start(), t.end()))
                .unwrap_or_else(|| "false".into())
        )
    }
}



// ===========
// === Set ===
// ===========

define_flag! {
    /// Dirty flag which keeps a set of dirty values. The `HashSet` dirty flag counterpart. Please
    /// note that it uses `FxHashSet` under the hood, so there are no guarantees regarding
    /// attack-proof hashing algorithm here.
    Set<Ix>
}

pub trait SetItem = Eq + Hash + Debug;

/// Internal representation of the [`Set`] flag.
#[derive(Derivative, Shrinkwrap)]
#[derivative(Debug(bound = "Item:SetItem"))]
#[derivative(Default(bound = "Item:SetItem"))]
#[allow(missing_docs)]
pub struct SetData<Item> {
    pub set: FxHashSet<Item>,
}

impl<Item> HasArg for SetData<Item> {
    type Arg = Item;
}

impl<Item> HasCheckAll for SetData<Item> {
    fn check_all(&self) -> bool {
        !self.set.is_empty()
    }
}

impl<Item> HasUnsetAll for SetData<Item> {
    fn unset_all(&mut self) {
        self.set.clear();
    }
}

impl<Item: SetItem> HasCheck1 for SetData<Item> {
    fn check(&self, a: &Item) -> bool {
        self.set.contains(a)
    }
}

impl<Item: SetItem> HasSet1 for SetData<Item> {
    fn set(&mut self, a: Item) {
        self.set.insert(a);
    }
}

impl<Item: SetItem> HasUnset1 for SetData<Item> {
    fn unset(&mut self, a: &Item) {
        self.set.remove(a);
    }
}

impl<Item: SetItem> Display for SetData<Item> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.set)
    }
}

impl<'t, Item: SetItem> IntoIterator for &'t SetData<Item> {
    type Item = &'t Item;
    type IntoIter = <&'t FxHashSet<Item> as IntoIterator>::IntoIter;
    fn into_iter(self) -> Self::IntoIter {
        (self.set).iter()
    }
}



// ==============
// === Vector ===
// ==============

define_flag! {
    /// Dirty flag which keeps a vector of dirty values.
    Vector<Item>
}

pub trait VectorItem = Debug + PartialEq;

/// Internal representation of the [`Vector`] flag.
#[derive(Derivative, Debug, Shrinkwrap)]
#[derivative(Default(bound = ""))]
#[allow(missing_docs)]
pub struct VectorData<Item> {
    pub vec: Vec<Item>,
}

impl<Item> HasArg for VectorData<Item> {
    type Arg = Item;
}

impl<Item> HasCheckAll for VectorData<Item> {
    fn check_all(&self) -> bool {
        !self.vec.is_empty()
    }
}

impl<Item> HasUnsetAll for VectorData<Item> {
    fn unset_all(&mut self) {
        self.vec.clear();
    }
}

impl<Item: PartialEq> HasCheck1 for VectorData<Item> {
    fn check(&self, a: &Item) -> bool {
        self.vec.contains(a)
    }
}

impl<Item> HasSet1 for VectorData<Item> {
    fn set(&mut self, a: Item) {
        self.vec.push(a);
    }
}

impl<Item: PartialEq> HasUnset1 for VectorData<Item> {
    fn unset(&mut self, a: &Item) {
        self.vec.remove_item(a);
    }
}

impl<Item: Debug> Display for VectorData<Item> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.vec)
    }
}

impl<'t, Item> IntoIterator for &'t VectorData<Item> {
    type Item = &'t Item;
    type IntoIter = <&'t Vec<Item> as IntoIterator>::IntoIter;
    fn into_iter(self) -> Self::IntoIter {
        (self.vec).iter()
    }
}



// ================
// === BitField ===
// ================

use bit_field::BitField as BF;

define_flag! {
    /// Dirty flag which keeps information about a set of enumerator values. The items must be a
    /// plain enumerators implementing `Into<usize>`. The data is stored as an efficient `BitField`
    /// under the hood.
    Enum<Prim, T>
}

pub trait EnumBase = Default + PartialEq + Copy + BF;
pub trait EnumElem = Copy + Into<usize>;

/// Dirty flag which keeps dirty indexes in a `BitField` under the hood.
pub type BitField<Prim, OnMut> = Enum<Prim, usize, OnMut>;

/// Shared version of the [`BitField`] flag.
pub type SharedBitField<Prim, OnMut> = SharedEnum<Prim, usize, OnMut>;

/// Internal representation of the [`Enum`] flag.
#[derive(Derivative)]
#[derivative(Debug(bound = "Prim:Debug"))]
#[derivative(Default(bound = "Prim:Default"))]
#[allow(missing_docs)]
pub struct EnumData<Prim = u32, T = usize> {
    pub bits: Prim,
    phantom:  PhantomData<T>,
}

impl<Prim, T> HasArg for EnumData<Prim, T> {
    type Arg = T;
}

impl<Prim: EnumBase, T> HasCheckAll for EnumData<Prim, T> {
    fn check_all(&self) -> bool {
        self.bits != default()
    }
}

impl<Prim: EnumBase, T> HasUnsetAll for EnumData<Prim, T> {
    fn unset_all(&mut self) {
        self.bits = default()
    }
}

impl<Prim: EnumBase, T: EnumElem> HasCheck1 for EnumData<Prim, T> {
    fn check(&self, t: &T) -> bool {
        self.bits.get_bit((*t).into())
    }
}

impl<Prim: EnumBase, T: EnumElem> HasSet1 for EnumData<Prim, T> {
    fn set(&mut self, t: T) {
        self.bits.set_bit(t.into(), true);
    }
}

impl<Prim: EnumBase, T: EnumElem> HasUnset1 for EnumData<Prim, T> {
    fn unset(&mut self, t: &T) {
        self.bits.set_bit((*t).into(), false);
    }
}

impl<Prim: EnumBase, T: EnumElem> Display for EnumData<Prim, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.check_all())
    }
}
