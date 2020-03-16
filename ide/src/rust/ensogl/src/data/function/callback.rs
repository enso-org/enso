#![allow(missing_docs)]

use crate::prelude::*;
use std::fmt;


// =============
// === Types ===
// =============

pub type NoCallback = ();

#[derive(Shrinkwrap)]
#[shrinkwrap(mutable)]
pub struct Function<Func>(pub Func);

impl<Func> Debug for Function<Func> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Callback")
    }
}



// =================
// === Instances ===
// =================

pub trait Function0 {
    fn call(&mut self);
}

pub trait Function1<Arg1> {
    fn call(&mut self, arg1:Arg1);
}


// === Unit Implementations ===

impl<T:Function0> Function0 for Option<T> {
    fn call(&mut self) {
        self.iter_mut().for_each(|t| {
            t.call()
        })
    }
}

impl Function0 for () {
    fn call(&mut self) {}
}

impl<Arg1> Function1<Arg1> for () {
    fn call(&mut self, _arg1:Arg1) {}
}


// === FnMut Implementations ===

impl<F: FnMut() -> T, T> Function0 for F {
    fn call(&mut self) {
        self();
    }
}

impl<Arg1, F:FnMut(Arg1) -> T, T> Function1<Arg1> for F {
    fn call(&mut self, arg1:Arg1) {
        self(arg1);
    }
}
