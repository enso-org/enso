use crate::prelude::*;

use js_sys::Function;
use wasm_bindgen::convert::FromWasmAbi;
use wasm_bindgen::JsCast;



// ======================
// === ClosureStorage ===
// ======================

/// Constraint for JS closure argument types
pub trait ClosureArg = FromWasmAbi + 'static;

/// Function that can be wrapped into a `Closure`.
pub trait ClosureFn<Arg> = FnMut(Arg) + 'static where Arg: ClosureArg;

/// Stores an optional closure.
/// The purpose it reduce boilerplate repeating when setting JS callbacks.
#[derive(Debug, Derivative)]
#[derivative(Default(bound = ""))]
pub struct OptionalFmMutClosure<Arg> {
    /// The stored closure.
    pub closure: Option<Closure<dyn FnMut(Arg)>>,
}

impl<Arg> OptionalFmMutClosure<Arg> {
    /// An empty closure storage.
    pub fn new() -> OptionalFmMutClosure<Arg> {
        default()
    }

    /// Stores the given closure.
    pub fn store(&mut self, closure: Closure<dyn FnMut(Arg)>) -> &Function {
        self.closure = Some(closure);
        // TODO [mwu]: `insert` should be used when we bump rustc - and then get rid of unwrap.
        //              Blocked by https://github.com/enso-org/ide/issues/1028
        // The `unwrap` call is safe, because the line above set closure to `Some`.
        self.js_ref().unwrap()
    }

    /// Obtain JS reference to the closure (that can be passed e.g. as a callback
    /// to an event handler).
    pub fn js_ref(&self) -> Option<&Function> {
        self.closure.as_ref().map(|closure| closure.as_ref().unchecked_ref())
    }

    /// Wraps given function into a Closure.
    pub fn wrap(&mut self, f: impl ClosureFn<Arg>) -> &Function {
        let boxed = Box::new(f);
        // Note: [mwu] Not sure exactly why, but compiler sometimes require this
        // explicit type below and sometimes does not.
        let wrapped: Closure<dyn FnMut(Arg)> = Closure::wrap(boxed);
        self.store(wrapped)
    }

    /// Clears the current closure.
    /// Note: if reference to it is still used by JS, it will throw an exception
    /// on calling attempt. Be careful of dangling references.
    pub fn clear(&mut self) {
        self.closure = None;
    }

    /// Register this closure as an event handler.
    /// No action is taken if there is no closure stored.
    pub fn add_listener<EventType: crate::event::Type>(&self, target: &EventType::Target) {
        if let Some(function) = self.js_ref() {
            EventType::add_listener(target, function)
        }
    }

    /// Unregister this closure as an event handler. The closure must be the same as when it was
    /// registered.
    pub fn remove_listener<EventType: crate::event::Type>(&self, target: &EventType::Target) {
        if let Some(function) = self.js_ref() {
            EventType::remove_listener(target, function)
        }
    }
}
