//! This module exports scala ast generator.

// ==============
// === Macros ===
// ==============

/// Calls function on each argument.
#[macro_export]
macro_rules! each {
    ($val:ident.$fun:ident, $($args:expr),*) => { $($val.$fun($args));* }
}



// ===============
// === Modules ===
// ===============

pub mod ast;
pub mod scala;
pub mod rust;



// =================
// === Generator ===
// =================

/// An ast generator.
pub trait Generator : Sized {
    /// Source code builder.
    type Source : Default;

    /// Write source code into the buffer.
    fn write(self, source:&mut Self::Source);

    /// Get string representation of self.
    fn source(self) -> Self::Source {
        let mut source = Self::Source::default();
        self.write(&mut source);
        source
    }
}


// == Trait Impls ==

impl<T:Generator> Generator for Option<T> {
    type Source = T::Source;

    fn write(self, source:&mut T::Source) {
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
