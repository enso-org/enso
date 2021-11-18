//! Efficient 256-bit field implementation.

use crate::prelude::*;



// ================
// === BitField ===
// ================

/// Abstraction for any bit field.
pub trait BitField {
    /// Bit length of the bit field.
    const BIT_LENGTH: usize;

    /// Get bit value at the provided index.
    fn get_bit(&self, bit: usize) -> bool;

    /// Set the bit value at the provided index.
    fn set_bit(&mut self, bit: usize, value: bool) -> &mut Self;
}



// ===============================
// === Double Chunk Bit Fields ===
// ===============================

/// Efficient 256 bit field implementation. Encoded as two `u128` under the hood.
///
/// ## Implementation Notes
/// The type does not implement `Copy` on purpose. A single reference takes 8 bytes. This struct
/// takes 32 bytes (4x the reference size). Passing it by reference would be more efficient than
/// by value.
#[derive(Clone, Default, Deref, Eq, Hash, PartialEq)]
#[allow(missing_copy_implementations)]
pub struct BitField256 {
    /// Raw chunks of this bit field.
    pub chunks: [u128; 2],
}

impl BitField256 {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }
}

impl BitField for BitField256 {
    const BIT_LENGTH: usize = 256;

    #[inline]
    fn get_bit(&self, bit: usize) -> bool {
        assert!(bit < Self::BIT_LENGTH);
        if bit > 127 {
            (self.chunks[1] & (1 << (bit - 128))) != 0
        } else {
            (self.chunks[0] & (1 << bit)) != 0
        }
    }

    #[inline]
    #[allow(clippy::collapsible_else_if)]
    fn set_bit(&mut self, bit: usize, value: bool) -> &mut Self {
        assert!(bit < Self::BIT_LENGTH);
        if value {
            if bit > 127 {
                self.chunks[1] |= 1 << (bit - 128)
            } else {
                self.chunks[0] |= 1 << bit
            }
        } else {
            if bit > 127 {
                self.chunks[1] &= !(1 << (bit - 128))
            } else {
                self.chunks[0] &= !(1 << bit)
            }
        }
        self
    }
}

impl Debug for BitField256 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "BitField256({:0128b}{:0128b})", self.chunks[1], self.chunks[0])
    }
}



// ===============================
// === Single Chunk Bit Fields ===
// ===============================

macro_rules! define_single_chunk_bit_field {
    ($name:ident, $raw:ident, $size:tt, $fmt:tt) => {
        /// Efficient $size bit field implementation. Encoded as single $raw under the hood.
        #[derive(Clone, Copy, Default, Deref, Eq, Hash, PartialEq)]
        #[allow(missing_docs)]
        pub struct $name {
            /// Raw implementation of the bit field.
            pub raw: $raw,
        }

        impl $name {
            /// Constructor.
            pub fn new() -> Self {
                default()
            }
        }

        impl BitField for $name {
            const BIT_LENGTH: usize = $size;

            #[inline]
            fn get_bit(&self, bit: usize) -> bool {
                assert!(bit < Self::BIT_LENGTH);
                (self.raw & (1 << bit)) != 0
            }

            #[inline]
            #[allow(clippy::collapsible_if)]
            fn set_bit(&mut self, bit: usize, value: bool) -> &mut Self {
                assert!(bit < Self::BIT_LENGTH);
                if value {
                    self.raw |= 1 << bit
                } else {
                    self.raw &= !(1 << bit)
                }
                self
            }
        }

        impl Debug for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, concat!("{}({:", $fmt, "})"), stringify!($name), self.raw)
            }
        }
    };
}

define_single_chunk_bit_field!(BitField8, u8, 8, "008b");
define_single_chunk_bit_field!(BitField16, u16, 16, "016b");
define_single_chunk_bit_field!(BitField32, u32, 32, "032b");
define_single_chunk_bit_field!(BitField64, u64, 64, "064b");
define_single_chunk_bit_field!(BitField128, u128, 128, "128b");



// =============
// === Tests ===
// =============

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn single_chunk_bitfields() {
        let tested_bits = &[0, 1, 10, 20, 31];

        let mut bit_field = BitField32::default();
        for bit in tested_bits {
            assert_eq!(bit_field.get_bit(*bit), false);
        }

        // Set bits one by one
        for (i, bit) in tested_bits.iter().enumerate() {
            bit_field.set_bit(*bit, true);
            for (j, bit) in tested_bits.iter().enumerate() {
                let should_be_set = j <= i;
                assert_eq!(bit_field.get_bit(*bit), should_be_set);
            }
        }

        // Unsetting bits one by one
        for (i, bit) in tested_bits.iter().enumerate() {
            bit_field.set_bit(*bit, false);
            for (j, bit) in tested_bits.iter().enumerate() {
                let should_be_set = j > i;
                assert_eq!(bit_field.get_bit(*bit), should_be_set);
            }
        }
    }

    #[test]
    fn multi_chunk_bitfields() {
        let tested_bits = &[0, 10, 127, 128, 255];

        let mut bit_field = BitField256::default();
        for bit in tested_bits {
            assert_eq!(bit_field.get_bit(*bit), false);
        }

        // Set bits one by one
        for (i, bit) in tested_bits.iter().enumerate() {
            bit_field.set_bit(*bit, true);
            for (j, bit) in tested_bits.iter().enumerate() {
                let should_be_set = j <= i;
                assert_eq!(bit_field.get_bit(*bit), should_be_set);
            }
        }

        // Unsetting bits one by one
        for (i, bit) in tested_bits.iter().enumerate() {
            bit_field.set_bit(*bit, false);
            for (j, bit) in tested_bits.iter().enumerate() {
                let should_be_set = j > i;
                assert_eq!(bit_field.get_bit(*bit), should_be_set);
            }
        }
    }
}
