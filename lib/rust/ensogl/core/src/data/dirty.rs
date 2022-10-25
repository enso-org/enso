//! This module contains implementation of various dirty flags. A dirty flag is a structure which
//! remembers that something was changed, but not updated yet. For example, dirty flags are useful
//! when defining OpenGL buffer management. When a data in CPU-buffer changes, dirty flags can keep
//! a set of changed indexes and bulk-update the GPU-buffers every animation frame. You can think
//! of dirty flags like about a way to introduce laziness to the program evaluation mechanisms.

// === Non-Standard Linter Configuration ===
#![allow(missing_docs)]

use crate::data::function::traits::*;
use crate::prelude::*;

use rustc_hash::FxHashSet;
use std::hash::Hash;
use std::mem;



// ==================
// === Operations ===
// ==================

pub mod traits {
    use super::*;

    // === Arg ===
    pub trait HasArg {
        type Arg;
    }
    pub type Arg<T> = <T as HasArg>::Arg;

    // === Global Operations ===
    pub trait HasCheckAll {
        fn check_all(&self) -> bool;
    }
    pub trait HasUnsetAll {
        fn unset_all(&mut self);
    }

    // === Arity-0 Operations ===
    pub trait HasCheck0 {
        fn check(&self) -> bool;
    }
    pub trait HasSet0 {
        fn set(&mut self);
    }
    pub trait HasUnset0 {
        fn unset(&mut self);
    }

    // === Arity-1 Operations ===
    pub trait HasCheck1: HasArg {
        fn check(&self, arg: &Self::Arg) -> bool;
    }
    pub trait HasSet1: HasArg {
        fn set(&mut self, arg: Self::Arg);
    }
    pub trait HasUnset1: HasArg {
        fn unset(&mut self, arg: &Self::Arg);
    }

    // === Shared Operations ===
    pub trait SharedHasUnsetAll {
        fn unset_all(&self);
    }
    pub trait SharedHasSet0 {
        fn set(&self);
    }
    pub trait SharedHasUnset0 {
        fn unset(&self);
    }
    pub trait SharedHasSet1: HasArg {
        fn set(&self, arg: Self::Arg);
    }
    pub trait SharedHasUnset1: HasArg {
        fn unset(&self, arg: &Self::Arg);
    }

    // === Type Aliases ===
    pub trait DirtyFlagOps = Debug + HasCheckAll + HasUnsetAll;
    pub trait DirtyFlagOps0 = DirtyFlagOps + HasCheck0 + HasSet0;
    pub trait DirtyFlagOps1 = DirtyFlagOps + HasCheck1 + HasSet1 where Arg<Self>: Debug;
}

pub use traits::*;



// =================
// === DirtyFlag ===
// =================

// === Definition ===

/// Abstraction for every dirty flag implementation. It is a smart struct adding
/// logging and callback utilities to the underlying data. Moreover, it
/// implements public API for working with dirty flags.
#[derive(Derivative)]
#[derivative(Debug(bound = "T:Debug"))]
pub struct DirtyFlag<T, OnMut> {
    pub data: T,
    #[derivative(Debug = "ignore")]
    on_set:   OnMut,
}


// === Basics ===

impl<OnMut, T: Default> DirtyFlag<T, OnMut> {
    pub fn new(on_set: OnMut) -> Self {
        let data = default();
        Self { data, on_set }
    }

    pub fn take(&mut self) -> T {
        mem::take(&mut self.data)
    }
}


// === Arguments ===

impl<T: HasArg, OnMut> HasArg for DirtyFlag<T, OnMut> {
    type Arg = Arg<T>;
}


// === Global Operations ===

impl<T: HasCheckAll, OnMut> HasCheckAll for DirtyFlag<T, OnMut> {
    fn check_all(&self) -> bool {
        self.data.check_all()
    }
}

impl<T: HasUnsetAll, OnMut> HasUnsetAll for DirtyFlag<T, OnMut> {
    fn unset_all(&mut self) {
        self.data.unset_all()
    }
}


// === Check ===

impl<T: DirtyFlagOps0, OnMut> HasCheck0 for DirtyFlag<T, OnMut> {
    fn check(&self) -> bool {
        self.data.check()
    }
}

impl<T: DirtyFlagOps1, OnMut> HasCheck1 for DirtyFlag<T, OnMut> {
    fn check(&self, arg: &Self::Arg) -> bool {
        self.data.check(arg)
    }
}


// === Set ===

impl<T: DirtyFlagOps0, OnMut: FnMut0> HasSet0 for DirtyFlag<T, OnMut> {
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

impl<T: DirtyFlagOps1, OnMut: FnMut0> HasSet1 for DirtyFlag<T, OnMut> {
    fn set(&mut self, arg: Self::Arg) {
        let first_set = !self.check_all();
        let is_set = self.data.check(&arg);
        if !is_set {
            self.data.set(arg);
            debug_span!("Setting to {self.data:?}.").in_scope(|| {
                if first_set {
                    self.on_set.call();
                }
            })
        }
    }
}


// === Unset ===

impl<T: HasUnset0, OnMut> HasUnset0 for DirtyFlag<T, OnMut> {
    fn unset(&mut self) {
        trace!("Unsetting.");
        self.data.unset()
    }
}

impl<T: HasUnset1, OnMut> HasUnset1 for DirtyFlag<T, OnMut>
where Arg<T>: Display
{
    fn unset(&mut self, arg: &Self::Arg) {
        trace!("Unsetting {arg}.");
        self.data.unset(arg)
    }
}



// =======================
// === SharedDirtyFlag ===
// =======================

// === Definition ===

/// A version of `DirtyFlag` which uses internal mutability pattern. It is meant to expose the same
/// API but without requiring `self` reference to be mutable.
#[derive(Derivative)]
#[derivative(Debug(bound = "T:Debug"))]
#[derivative(Clone(bound = ""))]
pub struct SharedDirtyFlag<T, OnMut> {
    rc: Rc<RefCell<DirtyFlag<T, OnMut>>>,
}


// === API ===

impl<T: Default, OnMut> SharedDirtyFlag<T, OnMut> {
    pub fn new(on_set: OnMut) -> Self {
        Self { rc: Rc::new(RefCell::new(DirtyFlag::new(on_set))) }
    }

    pub fn take(&self) -> T {
        self.rc.borrow_mut().take()
    }
}

impl<T, OnMut> SharedDirtyFlag<T, OnMut> {
    pub fn clone_ref(&self) -> Self {
        self.clone()
    }
}

impl<T, OnMut> SharedDirtyFlag<T, OnMut> {
    pub fn set_callback(&self, on_set: OnMut) {
        self.rc.borrow_mut().on_set = on_set;
    }
}

impl<T, OnMut> From<Rc<RefCell<DirtyFlag<T, OnMut>>>> for SharedDirtyFlag<T, OnMut> {
    fn from(rc: Rc<RefCell<DirtyFlag<T, OnMut>>>) -> Self {
        Self { rc }
    }
}


// === Arg ===

impl<T: HasArg, OnMut> HasArg for SharedDirtyFlag<T, OnMut> {
    type Arg = Arg<T>;
}


// === Global Operations ===

impl<T: HasUnsetAll, OnMut> SharedHasUnsetAll for SharedDirtyFlag<T, OnMut> {
    fn unset_all(&self) {
        self.rc.borrow_mut().unset_all()
    }
}

impl<T: HasCheckAll, OnMut> HasCheckAll for SharedDirtyFlag<T, OnMut> {
    fn check_all(&self) -> bool {
        self.rc.borrow().check_all()
    }
}

// === Check ===

impl<T: DirtyFlagOps0, OnMut> HasCheck0 for SharedDirtyFlag<T, OnMut> {
    fn check(&self) -> bool {
        self.rc.borrow().check()
    }
}

impl<T: DirtyFlagOps1, OnMut> HasCheck1 for SharedDirtyFlag<T, OnMut> {
    fn check(&self, arg: &Arg<T>) -> bool {
        self.rc.borrow().check(arg)
    }
}

// === Set ===

impl<T: DirtyFlagOps0, OnMut: FnMut0> SharedHasSet0 for SharedDirtyFlag<T, OnMut> {
    fn set(&self) {
        self.rc.borrow_mut().set()
    }
}

impl<T: DirtyFlagOps1, OnMut: FnMut0> SharedHasSet1 for SharedDirtyFlag<T, OnMut> {
    fn set(&self, arg: Arg<T>) {
        self.rc.borrow_mut().set(arg)
    }
}

// === Unset ===

impl<T: HasUnset0, OnMut> SharedHasUnset0 for SharedDirtyFlag<T, OnMut> {
    fn unset(&self) {
        self.rc.borrow_mut().unset()
    }
}

impl<T: HasUnset1, OnMut> SharedHasUnset1 for SharedDirtyFlag<T, OnMut>
where Arg<T>: Display
{
    fn unset(&self, arg: &Self::Arg) {
        self.rc.borrow_mut().unset(arg)
    }
}


// === Weak References ===

#[derive(Derivative)]
#[derivative(Debug(bound = "T:Debug"))]
#[derivative(Clone(bound = ""))]
pub struct WeakSharedDirtyFlag<T, OnMut> {
    weak: Weak<RefCell<DirtyFlag<T, OnMut>>>,
}
impl<T, OnMut> SharedDirtyFlag<T, OnMut> {
    pub fn downgrade(&self) -> WeakSharedDirtyFlag<T, OnMut> {
        let weak = self.rc.downgrade();
        WeakSharedDirtyFlag { weak }
    }
}
impl<T, OnMut> WeakSharedDirtyFlag<T, OnMut> {
    pub fn upgrade(&self) -> Option<SharedDirtyFlag<T, OnMut>> {
        self.weak.upgrade().map(|rc| SharedDirtyFlag { rc })
    }
}



// =============================================================================
// === Flags ===================================================================
// =============================================================================

// ============
// === Bool ===
// ============

/// The on / off dirty flag. If you need a simple dirty / clean switch, this one
/// is the right choice.

pub type Bool<OnMut = ()> = DirtyFlag<BoolData, OnMut>;
pub type SharedBool<OnMut = ()> = SharedDirtyFlag<BoolData, OnMut>;
pub type WeakSharedBool<OnMut = ()> = WeakSharedDirtyFlag<BoolData, OnMut>;
pub trait BoolCtx<OnMut> = where OnMut: FnMut0;

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

/// Dirty flag which keeps information about a range of dirty items. It does not track items
/// separately, nor you are allowed to keep multiple ranges in it. Just a single value range.

pub type Range<Ix, OnMut> = DirtyFlag<RangeData<Ix>, OnMut>;
pub type SharedRange<Ix, OnMut> = SharedDirtyFlag<RangeData<Ix>, OnMut>;
pub trait RangeCtx<OnMut> = where OnMut: FnMut0;
pub trait RangeIx = PartialOrd + Copy + Debug;

#[derive(Debug, Default)]
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

/// Dirty flag which keeps a set of dirty values. The `HashSet` dirty flag
/// counterpart. Please note that it uses `FxHashSet` under the hood, so there
/// are no guarantees regarding attack-proof hashing algorithm here.

pub type Set<Ix, OnMut = ()> = DirtyFlag<SetData<Ix>, OnMut>;
pub type SharedSet<Ix, OnMut = ()> = SharedDirtyFlag<SetData<Ix>, OnMut>;
pub trait SetCtx<OnMut> = where OnMut: FnMut0;
pub trait SetItem = Eq + Hash + Debug;

#[derive(Derivative, Shrinkwrap)]
#[derivative(Debug(bound = "Item:SetItem"))]
#[derivative(Default(bound = "Item:SetItem"))]
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

/// Dirty flag which keeps a vector of dirty values.
pub type Vector<Item, OnMut = ()> = DirtyFlag<VectorData<Item>, OnMut>;
pub type SharedVector<Item, OnMut = ()> = SharedDirtyFlag<VectorData<Item>, OnMut>;
pub trait VectorItem = Debug + PartialEq;

#[derive(Derivative, Debug, Shrinkwrap)]
#[derivative(Default(bound = ""))]
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

/// Dirty flag which keeps information about a set of enumerator values. The
/// items must be a plain enumerator implementing `Into<usize>`. The data is
/// stored as an efficient `BitField` under the hood.

pub type Enum<Prim, T, OnMut> = DirtyFlag<EnumData<Prim, T>, OnMut>;
pub type SharedEnum<Prim, T, OnMut> = SharedDirtyFlag<EnumData<Prim, T>, OnMut>;
pub trait EnumCtx<OnMut> = where OnMut: FnMut0;
pub trait EnumBase = Default + PartialEq + Copy + BF;
pub trait EnumElem = Copy + Into<usize>;

/// Dirty flag which keeps dirty indexes in a `BitField` under the hood.

pub type BitField<Prim, OnMut> = Enum<Prim, usize, OnMut>;
pub type SharedBitField<Prim, OnMut> = SharedEnum<Prim, usize, OnMut>;

#[derive(Derivative)]
#[derivative(Debug(bound = "Prim:Debug"))]
#[derivative(Default(bound = "Prim:Default"))]
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
