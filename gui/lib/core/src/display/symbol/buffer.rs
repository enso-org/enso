pub mod item;
pub mod data;

use crate::prelude::*;

use crate::closure;
use crate::data::function::callback::*;
use crate::dirty;
use crate::dirty::traits::*;
use crate::system::web::Logger;
use crate::system::web::fmt;
use crate::system::web::group;
use crate::tp::debug::TypeDebugName;
use item::Item;
use nalgebra::Vector2;
use nalgebra::Vector3;
use nalgebra::Vector4;
use std::iter::Extend;


// ==============
// === Buffer ===
// ==============

// === Definition ===

/// Please refer to the 'Buffer management pipeline' doc to learn more about
/// attributes, scopes, geometries, meshes, scenes, and other relevant concepts.
///
/// Buffers are values stored in geometry. Under the hood they are stored in
/// vectors and are synchronised with GPU buffers on demand.
#[derive(Derivative,Shrinkwrap)]
#[shrinkwrap(mutable)]
#[derivative(Debug(bound="T:Debug"))]
pub struct Buffer<T,OnSet,OnResize> {
    #[shrinkwrap(main_field)]
    pub buffer       : Data        <T,OnSet,OnResize>,
    pub set_dirty    : SetDirty    <OnSet>,
    pub resize_dirty : ResizeDirty <OnResize>,
    pub logger       : Logger
}

// === Types ===

pub type Data <T,S,R> = data::Data <T,DataOnSet<S>,DataOnResize<R>>;

#[macro_export]
macro_rules! promote_buffer_types { ($callbacks:tt $module:ident) => {
    promote! { $callbacks $module [Var<T>,Buffer<T>,SharedBuffer<T>,AnyBuffer] }
};}

// === Callbacks ===

pub type SetDirty    <Callback> = dirty::SharedRange<usize,Callback>;
pub type ResizeDirty <Callback> = dirty::SharedBool<Callback>;

closure! {
fn buffer_on_resize<C:Callback0> (dirty:ResizeDirty<C>) ->
    DataOnResize { || dirty.set() }
}

closure! {
fn buffer_on_set<C:Callback0> (dirty:SetDirty<C>) ->
    DataOnSet { |ix: usize| dirty.set(ix) }
}

// === Instances ===

impl<T,OnSet:Callback0, OnResize:Callback0>
Buffer<T,OnSet,OnResize> {
    /// Creates new buffer from provided explicit buffer object.
    pub fn new_from
    (vec:Vec<T>, logger:Logger, on_set:OnSet, on_resize:OnResize) -> Self {
        logger.info(fmt!("Creating new {} buffer.", T::type_debug_name()));
        let set_logger     = logger.sub("set_dirty");
        let resize_logger  = logger.sub("resize_dirty");
        let set_dirty      = SetDirty::new(set_logger,on_set);
        let resize_dirty   = ResizeDirty::new(resize_logger,on_resize);
        let buff_on_resize = buffer_on_resize(resize_dirty.clone_rc());
        let buff_on_set    = buffer_on_set(set_dirty.clone_rc());
        let buffer         = Data::new_from(vec, buff_on_set, buff_on_resize);
        Self {buffer,set_dirty,resize_dirty,logger}
    }
    /// Creates a new empty buffer.
    pub fn new(logger:Logger, on_set:OnSet, on_resize:OnResize) -> Self {
        Self::new_from(default(),logger,on_set,on_resize)
    }
    /// Build the buffer from the provider configuration builder.
    pub fn build(bldr:Builder<T>, on_set:OnSet, on_resize:OnResize) -> Self {
        let buffer = bldr._buffer.unwrap_or_else(default);
        let logger = bldr._logger.unwrap_or_else(default);
        Self::new_from(buffer,logger,on_set,on_resize)
    }
}

impl<T,OnSet,OnResize>
Buffer<T,OnSet,OnResize> {
    /// Returns a new buffer `Builder` object.
    pub fn builder() -> Builder<T> {
        default()
    }
    /// Returns the number of elements in the buffer.
    pub fn len(&self) -> usize {
        self.buffer.len()
    }
    /// Checks if the buffer is empty.
    pub fn is_empty(&self) -> bool {
        self.buffer.is_empty()
    }
    /// Check dirty flags and update the state accordingly.
    pub fn update(&mut self) {
        group!(self.logger, "Updating.", {
            self.set_dirty.unset();
            self.resize_dirty.unset();
            // TODO finish
        })
    }
}

pub trait AddElementCtx<T,OnResize> = where
    T: Item + Clone,
    OnResize: Callback0;

impl<T,OnSet,OnResize>
Buffer<T,OnSet,OnResize> where Self: AddElementCtx<T,OnResize> {
    /// Adds a single new element initialized to default value.
    pub fn add_element(&mut self) {
        self.add_elements(1);
    }
    /// Adds multiple new elements initialized to default values.
    pub fn add_elements(&mut self, elem_count: usize) {
        self.extend(iter::repeat(T::empty()).take(elem_count));
    }
}

impl<T,OnSet,OnResize>
Index<usize> for Buffer<T,OnSet,OnResize> {
    type Output = T;
    fn index(&self, index: usize) -> &Self::Output {
        self.buffer.index(index)
    }
}

impl<T,OnSet:Callback0,OnResize>
IndexMut<usize> for Buffer<T,OnSet,OnResize> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        self.buffer.index_mut(index)
    }
}


// ====================
// === SharedBuffer ===
// ====================

/// Shared view for `Buffer`.
#[derive(Derivative,Shrinkwrap)]
#[derivative(Debug(bound="T:Debug"))]
#[derivative(Clone(bound=""))]
pub struct SharedBuffer<T,OnSet,OnResize> {
    pub rc: Rc<RefCell<Buffer<T,OnSet,OnResize>>>
}

impl<T, OnSet:Callback0, OnResize:Callback0>
SharedBuffer<T,OnSet,OnResize> {
    /// Creates a new empty buffer.
    pub fn new(logger:Logger, on_set:OnSet, on_resize:OnResize) -> Self {
        let rc = Rc::new(RefCell::new(Buffer::new(logger,on_set,on_resize)));
        Self {rc}
    }
    /// Build the buffer from the provider configuration builder.
    pub fn build(bldr:Builder<T>, on_set:OnSet, on_resize:OnResize) -> Self {
        let rc = Rc::new(RefCell::new(Buffer::build(bldr,on_set,on_resize)));
        Self {rc}
    }
}

impl<T,OnSet,OnResize>
SharedBuffer<T,OnSet,OnResize> {
    /// Check dirty flags and update the state accordingly.
    pub fn update(&self) {
        self.borrow_mut().update()
    }
    /// Get the variable by given index.
    pub fn get(&self, index:usize) -> Var<T,OnSet,OnResize> {
        Var::new(index,self.clone_rc())
    }
    /// Returns the number of elements in the buffer.
    pub fn len(&self) -> usize {
        self.borrow().len()
    }
    /// Checks if the buffer is empty.
    pub fn is_empty(&self) -> bool {
        self.borrow().is_empty()
    }
}

impl<T,OnSet,OnResize>
SharedBuffer<T,OnSet,OnResize> where (): AddElementCtx<T,OnResize> {
    /// Adds a single new element initialized to default value.
    pub fn add_element(&self){
        self.borrow_mut().add_element()
    }
}

impl <T,OnSet,OnResize>
From<Rc<RefCell<Buffer<T,OnSet,OnResize>>>> for SharedBuffer<T,OnSet,OnResize> {
    fn from(rc: Rc<RefCell<Buffer<T, OnSet, OnResize>>>) -> Self {
        Self {rc}
    }
}


// ===========
// === Var ===
// ===========

/// View for a particular buffer. Allows reading and writing buffer data
/// via the internal mutability pattern. It is implemented as a view on
/// a selected `SharedBuffer` element under the hood.
#[derive(Derivative)]
#[derivative(Debug(bound="T:Debug"))]
pub struct Var<T,OnSet,OnResize> {
    index  : usize,
    buffer : SharedBuffer<T,OnSet,OnResize>
}

impl<T,OnSet,OnResize>
Var<T,OnSet,OnResize> {
    /// Creates a new variable as an indexed view over provided buffer.
    pub fn new(index:usize, buffer: SharedBuffer<T,OnSet,OnResize>) -> Self {
        Self {index, buffer}
    }
    /// Gets immutable reference to the underlying data.
    // [1] Please refer to `Prelude::drop_lifetime` docs to learn why it is safe
    // to use it here.
    pub fn get(&self) -> IndexGuard<Buffer<T,OnSet,OnResize>> {
        let _borrow = self.buffer.borrow();
        let target  = _borrow.index(self.index);
        let target  = unsafe { drop_lifetime(target) }; // [1]
        IndexGuard {target,_borrow}
    }
}

impl<T,OnSet:Callback0,OnResize>
Var<T,OnSet,OnResize> {
    /// Gets mutable reference to the underlying data.
    // [1] Please refer to `Prelude::drop_lifetime` docs to learn why it is safe
    // to use it here.
    pub fn get_mut(&self) -> IndexGuardMut<Buffer<T,OnSet,OnResize>> {
        let mut _borrow = self.buffer.borrow_mut();
        let target      = _borrow.index_mut(self.index);
        let target      = unsafe { drop_lifetime_mut(target) }; // [1]
        IndexGuardMut {target,_borrow}
    }
    /// Modifies the underlying data by using the provided function.
    pub fn modify<F: FnOnce(&mut T)>(&self, f:F) {
        f(&mut self.buffer.borrow_mut()[self.index]);
    }
}

#[derive(Shrinkwrap)]
pub struct IndexGuard<'t,T> where T:Index<usize> {
    #[shrinkwrap(main_field)]
    pub target : &'t <T as Index<usize>>::Output,
    _borrow    : Ref<'t,T>
}

#[derive(Shrinkwrap)]
pub struct IndexGuardMut<'t,T> where T:Index<usize> {
    #[shrinkwrap(main_field)]
    pub target : &'t mut <T as Index<usize>>::Output,
    _borrow    : RefMut<'t,T>
}


// ===============
// === Builder ===
// ===============

/// Buffer builder.
#[derive(Derivative)]
#[derivative(Default(bound=""))]
pub struct Builder<T> {
    pub _buffer : Option <Vec<T>>,
    pub _logger : Option <Logger>
}

impl<T> Builder<T> {
    /// Creates a new builder object.
    pub fn new() -> Self {
        default()
    }
    /// Sets the underlying buffer data.
    pub fn buffer(self, val: Vec <T>) -> Self {
        Self { _buffer: Some(val), _logger: self._logger }
    }
    /// Sets the logger.
    pub fn logger(self, val: Logger) -> Self {
        Self { _buffer: self._buffer, _logger: Some(val) }
    }
}

// ========================
// === TO BE REFACTORED ===
// ========================

// TODO The following code should be refactored to use the new macro `eval-tt`
// TODO engine. Some utils, like `cartesian` macro should also be refactored
// TODO out.

macro_rules! cartesian_impl {
    ($out:tt [] $b:tt $init_b:tt, $f:ident) => {
        $f!{ $out }
    };
    ($out:tt [$a:ident, $($at:tt)*] [] $init_b:tt, $f:ident) => {
        cartesian_impl!{ $out [$($at)*] $init_b $init_b, $f }
    };
    ([$($out:tt)*] [$a:ident, $($at:tt)*] [$b:ident, $($bt:tt)*] $init_b:tt
    ,$f:ident) => {
        cartesian_impl!{
            [$($out)* ($a, $b),] [$a, $($at)*] [$($bt)*] $init_b, $f
        }
    };
}

macro_rules! cartesian {
    ([$($a:tt)*], [$($b:tt)*], $f:ident) => {
        cartesian_impl!{ [] [$($a)*,] [$($b)*,] [$($b)*,], $f }
    };
}

// =================
// === AnyBuffer ===
// =================

use enum_dispatch::*;

// === Macros ===

#[derive(Debug)]
pub struct BadVariant;

macro_rules! mk_any_buffer_impl {
([$(($base:ident, $param:ident)),*,]) => { paste::item! {

    /// An enum with a variant per possible buffer type (i32, f32, Vector<f32>,
    /// and many, many more). It provides a faster alternative to dyn trait one:
    /// `Buffer<dyn Item, OnSet, OnResize>`.
    #[enum_dispatch(IsBuffer)]
    #[derive(Derivative)]
    #[derivative(Debug(bound=""))]
    pub enum AnyBuffer<OnSet, OnResize> {
        $(  [<Variant $base For $param>]
                (SharedBuffer<$base<$param>, OnSet, OnResize>),
        )*
    }

    $( // ======================================================================

    impl<'t, T, S>
    TryFrom<&'t AnyBuffer<T, S>>
    for &'t SharedBuffer<$base<$param>, T, S> {
        type Error = BadVariant;
        fn try_from(v: &'t AnyBuffer<T, S>)
        -> Result <&'t SharedBuffer<$base<$param>, T, S>, Self::Error> {
            match v {
                AnyBuffer::[<Variant $base For $param>](a) => Ok(a),
                _ => Err(BadVariant)
            }
        }
    }

    impl<'t, T, S>
    TryFrom<&'t mut AnyBuffer<T, S>>
    for &'t mut SharedBuffer<$base<$param>, T, S> {
        type Error = BadVariant;
        fn try_from(v: &'t mut AnyBuffer<T, S>)
        -> Result <&'t mut SharedBuffer<$base<$param>, T, S>, Self::Error> {
            match v {
                AnyBuffer::[<Variant $base For $param>](a) => Ok(a),
                _ => Err(BadVariant)
            }
        }
    }

    )* // ======================================================================
}
}}

macro_rules! mk_any_buffer {
    ($bases:tt, $params:tt) => {
        cartesian!($bases, $params, mk_any_buffer_impl);
    }
}

// === Definition ===

type Identity<T> = T;
mk_any_buffer!([Identity, Vector2, Vector3, Vector4], [f32, i32]);

/// Collection of all methods common to every buffer variant.
#[enum_dispatch]
pub trait IsBuffer<OnSet: Callback0, OnResize: Callback0> {
    fn add_element(&self);
    fn len(&self) -> usize;
    fn is_empty(&self) -> bool;
    fn update(&self);
}