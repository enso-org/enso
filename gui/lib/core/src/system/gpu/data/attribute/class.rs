#![allow(missing_docs)]

use crate::prelude::*;

use crate::data::function::callback::Callback0;
use crate::system::gpu::buffer::Buffer;


// =================
// === Attribute ===
// =================

/// View for a particular buffer. Allows reading and writing buffer data
/// via the internal mutability pattern. It is implemented as a view on
/// a selected `Buffer` element under the hood.
#[derive(Clone,Derivative)]
#[derivative(Debug(bound="T:Debug"))]
pub struct Attribute<T,OnMut,OnResize> {
    index  : usize,
    buffer : Buffer<T,OnMut,OnResize>
}

impl<T,OnMut,OnResize> Attribute<T,OnMut,OnResize> {
    /// Creates a new variable as an indexed view over provided buffer.
    pub fn new(index:usize, buffer: Buffer<T,OnMut,OnResize>) -> Self {
        Self {index, buffer}
    }
}

impl<T:Copy,OnMut:Callback0,OnResize> Attribute<T,OnMut,OnResize> {
    /// Gets immutable reference to the underlying data.
    pub fn get(&self) -> T {
        *self.buffer.rc.borrow().index(self.index)
    }

    /// Sets the variable to a new value.
    pub fn set(&self, value:T) {
        *self.buffer.rc.borrow_mut().index_mut(self.index) = value;
    }

    /// Modifies the underlying data by using the provided function.
    pub fn modify<F:FnOnce(&mut T)>(&self, f:F) {
        let mut value = self.get();
        f(&mut value);
        self.set(value);
    }
}
