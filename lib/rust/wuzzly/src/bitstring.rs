use std::assert_matches::assert_matches;
use std::intrinsics::likely;
use std::mem;
use std::ops::Add;



/// A sequence of bits, supporting stack operations, and concatenation. Optimized for sequences of
/// 64 or fewer elements.
#[derive(Debug, Default, Clone)]
pub struct Bitstring {
    /// Current segments. Contains 0-64 bits.
    head: Word,
    /// Extra segments. Each contains 1-64 bits.
    tail: Vec<Word>,
}

#[derive(Debug, Default, Clone, Copy)]
struct Word {
    len:  u32,
    bits: u64,
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
        } else {
            let first = self.tail.first_mut().unwrap();
            let new_len = first.len - 1;
            if new_len == 0 {
                self.tail.remove(0);
            } else {
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
            mem::replace(&mut self.head.len, 0)
        };
        self.check_invariants();
        result
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.head.len == 0 && self.tail.is_empty()
    }

    #[cfg(test)]
    fn check_invariants(&self) {
        assert_matches!(self.head.len, 0..=64);
        for word in &self.tail {
            assert_matches!(word.len, 1..=64);
        }
    }

    #[cfg(not(test))]
    #[inline]
    fn check_invariants() {}
}

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

    #[test]
    fn test_bitstring() {
        use rand_chacha::ChaCha8Rng;
        use rand::Rng;
        use rand::SeedableRng;
        let mut rng = ChaCha8Rng::seed_from_u64(0);
        let mut strings = vec![];
        let mut i = 0;
        let mut shrink_phase = false;
        let mut string = (super::Bitstring::new(), SlowBitstring::new());
        while !(shrink_phase && string.0.is_empty() && strings.is_empty()) {
            assert_eq!(string.0.is_empty(), string.1.is_empty());
            i += 1;
            if i == 10_000 {
                shrink_phase = true;
            }
            if i == 30_000 {
                break;
            }
            let operation = rng.gen_range(0u8..64);
            match operation {
                0..28 => {
                    if !shrink_phase || rng.gen() {
                        let bit = rng.gen();
                        string.0.push(bit);
                        string.1.push(bit);
                    }
                }
                28..32 => {
                    if !shrink_phase || rng.gen() {
                        let count = core::num::NonZeroU32::new(rng.gen_range(1..80)).unwrap();
                        string.0.push_zeros(count);
                        string.1.push_zeros(count);
                    }
                }
                32..36 => {
                    string.0.drop_front();
                    string.1.drop_front();
                }
                36..50 => {
                    assert_eq!(string.0.pop(), string.1.pop());
                    assert_eq!(string.0.is_empty(), string.1.is_empty());
                    if string.0.is_empty() && rng.gen() && let Some(s) = strings.pop() {
                        string = s;
                    }
                }
                50..56 => {
                    assert_eq!(string.0.pop_trailing_zeros(), string.1.pop_trailing_zeros());
                    assert_eq!(string.0.is_empty(), string.1.is_empty());
                    if string.0.is_empty() && rng.gen() && let Some(s) = strings.pop() {
                        string = s;
                    }
                }
                56..60 => {
                    if let Some(lhs) = strings.pop() {
                        let string_ = core::mem::take(&mut string);
                        string = (lhs.0 + string_.0, lhs.1 + string_.1);
                    }
                }
                60..64 => {
                    if !shrink_phase || rng.gen() {
                        strings.push((super::Bitstring::new(), SlowBitstring::new()));
                    }
                }
                _ => (),
            }
            assert_eq!(string.0.is_empty(), string.1.is_empty());
        }
    }
}
