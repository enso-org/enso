//! This module exports scala ast generator.

// ==============
// === Macros ===
// ==============

/// Calls function on each argument.
#[macro_export]
macro_rules! write {
    ($val:expr, $($args:expr),*) => { $($val.write($args));* }
}



// ===============
// === Modules ===
// ===============

pub mod api;
pub mod api;
pub mod ast;
pub mod jni;
pub mod rust;
pub mod scala;
pub mod types;



// =================
// === Generator ===
// =================

/// An ast generator generic over source code builder.
pub trait Generator<Source> : Sized {
    /// Write source code into the buffer.
    fn write(self, source:&mut Source);

    /// Get string representation of self.
    fn source(self) -> Source where Source:Default {
        let mut source = Source::default();
        self.write(&mut source);
        source
    }
}


// == Trait Impls ==

impl<S,T:Generator<S>> Generator<S> for Option<T> {
    fn write(self, source:&mut S) {
        if let Some(t) = self { t.write(source) }
    }
}


// == Tab ==

/// Writes an amount of spaces corresponding to the current indent.
#[derive(Debug,Clone,Copy,Default)]
pub struct Tab();

/// Writes an amount of spaces corresponding to the current indent.
pub const TAB:Tab = Tab();


// == When ==

/// A trait for wrapping a value in Option.
pub trait When : Sized {
    /// Returns Some(self) iff some is true.
    fn when(self, some:bool) -> Option<Self> {
        if some { Some(self) } else { None }
    }
}

impl<T> When for T {}
