//! Strongly typed container indexing primitives.
//! Primarily used for text manipulation.

use std::ops::Add;
use std::ops::Sub;
use serde::Serialize;
use serde::Deserialize;



/// Strongly typed index into container.
#[allow(missing_docs)]
#[derive(Clone,Copy,Debug,Default,PartialEq,Eq,PartialOrd,Ord,Serialize,Deserialize)]
pub struct Index { pub value:usize }

/// Strongly typed size of container.
#[allow(missing_docs)]
#[derive(Clone,Copy,Debug,Default,PartialEq,Eq,PartialOrd,Ord,Serialize,Deserialize)]
pub struct Size { pub value:usize }

/// Strongly typed span into container with index and size.
#[allow(missing_docs)]
#[derive(Clone,Copy,Debug,Default,PartialEq,Eq,PartialOrd,Ord,Serialize,Deserialize)]
pub struct Span { pub index:Index, pub size:Size }

impl Index {
    /// Initializes Index with given value.
    pub fn new(value:usize) -> Self {
        Index {value}
    }
}

impl Size {
    /// Initializes Size with given value.
    pub fn new(value:usize) -> Self {
        Size {value}
    }
}

impl Span {
    /// Initializes Span with given values.
    pub fn new(index:Index, size:Size) -> Self {
        Span {index,size}
    }
}

impl Add for Size {
    type Output = Size;
    fn add(self, rhs:Size) -> Size {
        Size {value:self.value + rhs.value}
    }
}

impl Add<Size> for Index {
    type Output = Index;
    fn add(self, rhs:Size) -> Index {
        Index {value:self.value + rhs.value}
    }
}

impl Sub<Size> for Index {
    type Output = Index;
    fn sub(self, rhs:Size) -> Index {
        Index {value:self.value - rhs.value}
    }
}
