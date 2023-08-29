//! Data structure that provides stack-of-boolean operations, implemented based on constant-time
//! bitwise operations supported in hardware on typical modern machines.

#[cfg(test)]
use std::assert_matches::assert_matches;
use std::intrinsics::likely;
use std::mem;
use std::ops::Add;



// =================
// === Bitstring ===
// =================

/// A sequence of bits, supporting stack operations and concatenation. Optimized for sequences of
/// 64 or fewer elements.
#[derive(Debug, Default, Clone)]
pub struct Bitstring {
    /// Current segment (the least-significant/most recently-pushed bits). Contains 0-64 bits.
    head: Word,
    /// Extra segments. Each contains 1-64 bits.
    tail: Vec<Word>,
}

impl Bitstring {
    #[inline]
    pub fn new() -> Self {
        Default::default()
    }

    /// Push the specified number of zeros at the end. This is more efficient than repeated
    /// [`push`].
    #[inline]
    pub fn push_zeros(&mut self, count: core::num::NonZeroU32) {
        let headroom = 64 - self.head.len;
        if likely(count.get() <= headroom) {
            self.head.bits = self.head.bits.checked_shl(count.get()).unwrap_or_default();
            self.head.len += count.get();
        } else {
            self.head.bits = self.head.bits.checked_shl(headroom).unwrap_or_default();
            self.head.len = 64;
            let new_head = Word { len: 0, bits: 0 };
            self.tail.push(core::mem::replace(&mut self.head, new_head));
            self.push_zeros(core::num::NonZeroU32::new(count.get() - headroom).unwrap());
        }
        self.check_invariants();
    }

    /// Push a bit at the end.
    #[inline]
    pub fn push(&mut self, bit: bool) {
        if likely(self.head.len < 64) {
            self.head.len += 1;
            self.head.bits <<= 1;
            self.head.bits |= bit as u64;
        } else {
            let new_head = Word { len: 1, bits: bit as u64 };
            self.tail.push(core::mem::replace(&mut self.head, new_head));
        }
        self.check_invariants();
    }

    /// Remove the bit at the beginning.
    #[inline]
    pub fn drop_front(&mut self) {
        if likely(self.tail.is_empty()) {
            // Branchless equivalent of `self.head.len.saturating_sub(1)`
            self.head.len = self.head.len + (self.head.len == 0) as u32 - 1;
            self.head.bits &= !(1 << self.head.len);
        } else {
            let first = self.tail.first_mut().unwrap();
            let new_len = first.len - 1;
            if new_len == 0 {
                self.tail.remove(0);
            } else {
                first.bits &= !(1 << new_len);
                first.len = new_len;
            }
        }
        self.check_invariants();
    }

    /// Remove the bit at the end and return it.
    #[inline]
    pub fn pop(&mut self) -> Option<bool> {
        let result = if self.head.len > 0 {
            let popped = self.head.bits & 1;
            self.head.bits >>= 1;
            self.head.len -= 1;
            Some(popped != 0)
        } else {
            self.tail.pop().and_then(|word| {
                self.head = word;
                self.pop()
            })
        };
        self.check_invariants();
        result
    }

    /// Remove all consecutive zero bits at the little end. Returns the number of bits removed.
    #[inline]
    pub fn pop_trailing_zeros(&mut self) -> u32 {
        let zeros = self.head.bits.trailing_zeros();
        let result = if zeros < self.head.len {
            self.head.bits >>= zeros;
            self.head.len -= zeros;
            zeros
        } else if let Some(word) = self.tail.pop() {
            let zeros = self.head.len;
            self.head = word;
            zeros + self.pop_trailing_zeros()
        } else {
            self.head.bits = 0;
            mem::replace(&mut self.head.len, 0)
        };
        self.check_invariants();
        result
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.head.len == 0 && self.tail.is_empty()
    }

    #[inline]
    pub fn len(&self) -> u32 {
        self.head.len + self.tail.iter().map(|word| word.len).sum::<u32>()
    }

    #[cfg(test)]
    fn check_invariants(&self) {
        assert_matches!(self.head.len, 0..=64);
        if self.head.len != 64 {
            assert_eq!(self.head.bits & !((1 << self.head.len) - 1), 0, "{:?}", self.head);
        }
        for word in &self.tail {
            assert_matches!(word.len, 1..=64);
            if word.len != 64 {
                assert_eq!(word.bits & !((1 << word.len) - 1), 0, "{word:?}");
            }
        }
    }

    #[cfg(not(test))]
    #[inline]
    fn check_invariants(&self) {}
}


// === Bitstring Trait Implementations ===

impl Add for Bitstring {
    type Output = Self;

    fn add(mut self, rhs: Self) -> Self::Output {
        let result = if likely(rhs.tail.is_empty() && rhs.head.len + self.head.len <= 64) {
            Self {
                head: Word {
                    bits: (self.head.bits << rhs.head.len) | rhs.head.bits,
                    len:  self.head.len + rhs.head.len,
                },
                tail: self.tail,
            }
        } else {
            let old_head = core::mem::replace(&mut self.head, rhs.head);
            if old_head.len != 0 {
                self.tail.push(old_head);
            }
            self.tail.extend(rhs.tail);
            self
        };
        result.check_invariants();
        result
    }
}


// === Word ===

#[derive(Debug, Default, Clone, Copy)]
struct Word {
    len:  u32,
    /// Contains `len` bits at the least-significant end. All other bits are 0.
    bits: u64,
}



// ==================
// === Unit Tests ===
// ==================

#[cfg(test)]
mod test {
    mod reference_impl {
        use std::collections::VecDeque;
        use std::ops::Add;

        /// Naive implementation of `Bitstring` operations for testing.
        #[derive(Debug, Default, Clone)]
        pub(super) struct SlowBitstring {
            bits: VecDeque<bool>,
        }

        impl SlowBitstring {
            pub(super) fn new() -> Self {
                Default::default()
            }

            pub(super) fn push_zeros(&mut self, count: core::num::NonZeroU32) {
                self.bits.extend(core::iter::repeat(false).take(count.get() as usize))
            }

            pub(super) fn push(&mut self, bit: bool) {
                self.bits.push_back(bit)
            }

            pub(super) fn drop_front(&mut self) {
                self.bits.pop_front();
            }

            pub(super) fn pop(&mut self) -> Option<bool> {
                self.bits.pop_back()
            }

            pub(super) fn is_empty(&mut self) -> bool {
                self.bits.is_empty()
            }

            pub(super) fn pop_trailing_zeros(&mut self) -> u32 {
                let mut count = 0;
                while let Some(popped) = self.bits.pop_back() {
                    if popped {
                        self.bits.push_back(popped);
                        break;
                    }
                    count += 1;
                }
                count
            }
        }

        impl Add for SlowBitstring {
            type Output = Self;

            fn add(mut self, rhs: Self) -> Self::Output {
                self.bits.extend(rhs.bits);
                self
            }
        }
    }
    use reference_impl::SlowBitstring;

    /// Test that `Bitstring` behaves the same as the `SlowBitstring` reference when performing a
    /// long arbitrary but deterministically-generated sequence of operations.
    #[test]
    fn test_bitstring() {
        use rand::Rng;
        use rand::SeedableRng;
        use rand_chacha::ChaCha8Rng;
        let mut rng = ChaCha8Rng::seed_from_u64(0);
        let mut strings: Vec<(super::Bitstring, SlowBitstring)> = vec![];
        let mut i = 0;
        let mut shrink_phase = false;
        let mut tested = super::Bitstring::new();
        let mut reference = SlowBitstring::new();
        while !(shrink_phase && tested.is_empty() && reference.is_empty() && strings.is_empty()) {
            assert_eq!(tested.is_empty(), reference.is_empty());
            i += 1;
            if i == 10_000 {
                shrink_phase = true;
            }
            if i == 30_000 {
                break;
            }
            let operation = rng.gen_range(0u8..64);
            match operation {
                // The probabilities here were chosen to be moderately-biased toward growing the
                // string during the initial phase, and moderately-biased toward shrinking the
                // string during the shrink phase. This moderate bias ensures that many cases
                // involving relatively short strings are tested, which is optimal because if
                // operations are correct for bitstrings with a few segments, they will not be wrong
                // for bitstrings with many segments (as there are no differences in the logic for
                // strings with many segments, versus strings with one segment beyond the head).
                //
                // The primary goal is full branch coverage, which has been verified empirically
                // with llvm-cov.
                0..26 =>
                    if !shrink_phase || rng.gen() {
                        let bit = rng.gen();
                        tested.push(bit);
                        reference.push(bit);
                    },
                26..28 =>
                    if !shrink_phase || rng.gen() {
                        let count = core::num::NonZeroU32::new(rng.gen_range(1..80)).unwrap();
                        tested.push_zeros(count);
                        reference.push_zeros(count);
                    },
                28..32 => {
                    tested.drop_front();
                    reference.drop_front();
                }
                32..46 => {
                    assert_eq!(tested.pop(), reference.pop());
                    assert_eq!(tested.is_empty(), reference.is_empty());
                    if tested.is_empty() && rng.gen() && let Some(s) = strings.pop() {
                        tested = s.0;
                        reference = s.1;
                    }
                }
                46..52 => {
                    assert_eq!(tested.pop_trailing_zeros(), reference.pop_trailing_zeros());
                    assert_eq!(tested.is_empty(), reference.is_empty());
                    if tested.is_empty() && rng.gen() && let Some(s) = strings.pop() {
                        tested = s.0;
                        reference = s.1;
                    }
                }
                52..58 =>
                    if let Some((tested_lhs, reference_lhs)) = strings.pop() {
                        let tested_ = core::mem::take(&mut tested);
                        let reference_ = core::mem::take(&mut reference);
                        tested = tested_lhs + tested_;
                        reference = reference_lhs + reference_;
                    },
                58..64 =>
                    if !shrink_phase || rng.gen() {
                        let tested_ = core::mem::take(&mut tested);
                        let reference_ = core::mem::take(&mut reference);
                        strings.push((tested_, reference_));
                    },
                _ => (),
            }
            assert_eq!(tested.is_empty(), reference.is_empty());
        }
    }
}
