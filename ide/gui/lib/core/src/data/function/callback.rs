use crate::data::function::nop::NOP;
use crate::prelude::*;
use std::fmt;

// =====================
// === Callback Type ===
// =====================

pub type NoCallback = ();

#[derive(Shrinkwrap)]
#[shrinkwrap(mutable)]
pub struct Callback<Func>(pub Func);

impl<Func> Default for Callback<Func> {
    fn default() -> Self {
        //        Callback(NOP::nop())
        unimplemented!()
    }
}

impl<Func> fmt::Debug for Callback<Func> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Callback")
    }
}

// ==========================
// === Callback Interface ===
// ==========================

pub trait Callback0: 'static {
    fn call(&mut self);
}

pub trait Callback1<Arg1> {
    fn call(&mut self, arg1: Arg1);
}

pub trait Callback2<Arg1, Arg2> {
    fn call(&mut self, arg1: Arg1, arg2: Arg2);
}

pub trait Callback3<Arg1, Arg2, Arg3> {
    fn call(&mut self, arg1: Arg1, arg2: Arg2, arg3: Arg3);
}

pub trait Callback4<Arg1, Arg2, Arg3, Arg4> {
    fn call(&mut self, arg1: Arg1, arg2: Arg2, arg3: Arg3, arg4: Arg4);
}

pub trait Callback5<Arg1, Arg2, Arg3, Arg4, Arg5> {
    fn call(&mut self, arg1: Arg1, arg2: Arg2, arg3: Arg3, arg4: Arg4, arg5: Arg5);
}

// === Unit Implementations ===

impl Callback0 for () {
    fn call(&mut self) {}
}

impl<Arg1> Callback1<Arg1> for () {
    fn call(&mut self, _arg1: Arg1) {}
}

impl<Arg1, Arg2> Callback2<Arg1, Arg2> for () {
    fn call(&mut self, _arg1: Arg1, _arg2: Arg2) {}
}

impl<Arg1, Arg2, Arg3> Callback3<Arg1, Arg2, Arg3> for () {
    fn call(&mut self, _arg1: Arg1, _arg2: Arg2, _arg3: Arg3) {}
}

impl<Arg1, Arg2, Arg3, Arg4> Callback4<Arg1, Arg2, Arg3, Arg4> for () {
    fn call(&mut self, _arg1: Arg1, _arg2: Arg2, _arg3: Arg3, _arg4: Arg4) {}
}

impl<Arg1, Arg2, Arg3, Arg4, Arg5> Callback5<Arg1, Arg2, Arg3, Arg4, Arg5> for () {
    fn call(&mut self, _arg1: Arg1, _arg2: Arg2, _arg3: Arg3, _arg4: Arg4, _arg5: Arg5) {}
}

// === FnMut Implementations ===

// FIXME: How to make it more generic?
impl<T: 'static, P: 'static> Callback0 for WithPhantomType<Rc<Fn() -> T>, P> {
    fn call(&mut self) {
        (self.t)();
    }
}

// FIXME: How to make it more generic?
impl<Arg1, T: 'static, P: 'static> Callback1<Arg1> for WithPhantomType<Rc<Fn(Arg1) -> T>, P> {
    fn call(&mut self, arg1: Arg1) {
        (self.t)(arg1);
    }
}

impl<F: FnMut() -> T + 'static, T> Callback0 for F {
    fn call(&mut self) {
        self();
    }
}

impl<Arg1, F: FnMut(Arg1) -> T, T> Callback1<Arg1> for F {
    fn call(&mut self, arg1: Arg1) {
        self(arg1);
    }
}

impl<Arg1, Arg2, F: FnMut(Arg1, Arg2) -> T, T> Callback2<Arg1, Arg2> for F {
    fn call(&mut self, arg1: Arg1, arg2: Arg2) {
        self(arg1, arg2);
    }
}

impl<Arg1, Arg2, Arg3, F: FnMut(Arg1, Arg2, Arg3) -> T, T> Callback3<Arg1, Arg2, Arg3> for F {
    fn call(&mut self, arg1: Arg1, arg2: Arg2, arg3: Arg3) {
        self(arg1, arg2, arg3);
    }
}

impl<Arg1, Arg2, Arg3, Arg4, F: FnMut(Arg1, Arg2, Arg3, Arg4) -> T, T>
    Callback4<Arg1, Arg2, Arg3, Arg4> for F
{
    fn call(&mut self, arg1: Arg1, arg2: Arg2, arg3: Arg3, arg4: Arg4) {
        self(arg1, arg2, arg3, arg4);
    }
}

impl<Arg1, Arg2, Arg3, Arg4, Arg5, F: FnMut(Arg1, Arg2, Arg3, Arg4, Arg5) -> T, T>
    Callback5<Arg1, Arg2, Arg3, Arg4, Arg5> for F
{
    fn call(&mut self, arg1: Arg1, arg2: Arg2, arg3: Arg3, arg4: Arg4, arg5: Arg5) {
        self(arg1, arg2, arg3, arg4, arg5);
    }
}
