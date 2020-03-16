use crate::prelude::*;

use js_sys::Function;
use wasm_bindgen::JsCast;
use wasm_bindgen::convert::FromWasmAbi;



// ======================
// === ClosureStorage ===
// ======================

/// Constraint for JS closure argument types
pub trait ClosureArg = FromWasmAbi + 'static;

/// Function that can be wrapped into a `Closure`.
pub trait ClosureFn<Arg> = FnMut(Arg) + 'static where Arg: ClosureArg;

/// Stores an optional closure.
/// The purpose it reduce boilerplate repeating when setting JS callbacks.
#[derive(Debug,Derivative)]
#[derivative(Default(bound=""))]
pub struct OptionalFmMutClosure<Arg> {
    /// The stored closure.
    pub closure : Option<Closure<dyn FnMut(Arg)>>,
}

impl <Arg> OptionalFmMutClosure<Arg> {
    /// An empty closure storage.
    pub fn new() -> OptionalFmMutClosure<Arg> {
        default()
    }

    /// Stores the given closure.
    pub fn store(&mut self, closure:Closure<dyn FnMut(Arg)>) {
        self.closure = Some(closure);
    }

    /// Obtain JS reference to the closure (that can be passed e.g. as a callback
    /// to an event handler).
    pub fn js_ref(&self) -> Option<&Function> {
        self.closure.as_ref().map(|closure| closure.as_ref().unchecked_ref())
    }

    /// Wraps given function into a Closure.
    pub fn wrap(&mut self, f:impl ClosureFn<Arg>) {
        let boxed = Box::new(f);
        // Note: [mwu] Not sure exactly why, but compiler sometimes require this
        // explicit type below and sometimes does not.
        let wrapped:Closure<dyn FnMut(Arg)> = Closure::wrap(boxed);
        self.store(wrapped);
    }

    /// Clears the current closure.
    /// Note: if reference to it is still used by JS, it will throw an exception
    /// on calling attempt. Be careful of dangling references.
    pub fn clear(&mut self) {
        self.closure = None;
    }
}
