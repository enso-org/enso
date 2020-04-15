//! Efficient 256-bit field implementation.

use crate::prelude::*;



// ================
// === BitField ===
// ================

/// Abstraction for any bit field.
pub trait BitField {
    /// Bit length of the bit field.
    const BIT_LENGTH:usize;

    /// Get bit value at the provided index.
    fn get_bit(&self, bit: usize) -> bool;

    /// Set the bit value at the provided index.
    fn set_bit(&mut self, bit:usize, value:bool) -> &mut Self;
}



// ===================
// === BitField256 ===
// ===================

/// Efficient 256 bit field implementation. Encoded as two `u128` under the hood.
/// Warning! Do not use copy if you don't have to. A reference takes 8 bytes. This struct takes
/// 32 bytes (4x the reference size). It is highly probable that passing a reference would be more
/// efficient than passing a value.
#[derive(Clone,Copy,Default,Deref,Eq,Hash,PartialEq)]
pub struct BitField256 {
    chunks : [u128;2]
}

impl BitField256 {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }
}

impl BitField for BitField256 {
    const BIT_LENGTH:usize = 256;

    #[inline]
    fn get_bit(&self, bit: usize) -> bool {
        assert!(bit < Self::BIT_LENGTH);
        if bit > 127 { (self.chunks[1] & (1 << (bit - 128))) != 0 }
        else         { (self.chunks[1] & (1 << bit)) != 0 }
    }

    #[inline]
    #[allow(clippy::collapsible_if)]
    fn set_bit(&mut self, bit:usize, value:bool) -> &mut Self {
        assert!(bit < Self::BIT_LENGTH);
        if value {
            if bit > 127 { self.chunks[1] |= 1 << (bit - 128) }
            else         { self.chunks[0] |= 1 << bit }
        } else {
            if bit > 127 { self.chunks[1] &= !(1 << (bit - 128)) }
            else         { self.chunks[0] &= !(1 << bit) }
        }
        self
    }
}

impl Debug for BitField256 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f,"BitField256({:0128b}{:0128b})",self.chunks[1],self.chunks[0])
    }
}
