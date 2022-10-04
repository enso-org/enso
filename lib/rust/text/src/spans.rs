//! Text spans used to store metadata information.

use crate::index::*;
use crate::prelude::*;

use crate::range::Range;
use crate::rope;



// =============
// === Spans ===
// =============

/// Spans (interval tree), useful for rich text annotations. It is parameterized over a data type,
/// so can be used for storing different annotations.
#[derive(Clone, Debug, Default)]
pub struct Spans<T: Clone> {
    raw: rope::Spans<T>,
}

impl<T: Clone + Debug> Spans<T> {
    /// The number of bytes of this span.
    pub fn len(&self) -> Byte {
        self.raw.len().into()
    }

    /// Checks whether the span is empty.
    pub fn is_empty(&self) -> bool {
        self.raw.is_empty()
    }

    /// Replace the provided `range` with the new `value` spanned over `length` bytes.
    ///
    /// Spans are like byte chunk where each byte is associated with a value. This function first
    /// removes the subsequence of bytes described in `range` (shifting later bytes to the left),
    /// and then creates a new byte subsequence of the length `length` and associates it with the
    /// `value`. Use with caution, as it can easily lead to wrong amount of bytes covered by the
    /// span.
    pub fn replace_resize(&mut self, range: Range<Byte>, length: Byte, value: T) {
        let mut builder = rope::spans::Builder::new(length.value);
        builder.add_span(.., value);
        self.raw.edit(range.into_rope_interval(), builder.build())
    }

    /// Modify the parameter value in the given range.
    pub fn modify(&mut self, range: Range<Byte>, f: impl Fn(T) -> T) {
        let subseq = self.raw.subseq(range.into_rope_interval());
        for t in subseq.iter() {
            let sub_start: Byte = t.0.start.into();
            let sub_end: Byte = t.0.end.into();
            let start = range.start + sub_start;
            let end = range.start + sub_end;
            let range = Range::new(start, end);
            let size = Byte::try_from(range.size()).unwrap();
            self.replace_resize(range, size, f(t.1.clone()));
        }
    }

    /// Return all spans contained in the provided range.
    pub fn sub(&self, range: Range<Byte>) -> Self {
        Self { raw: self.raw.subseq(range.into_rope_interval()) }
    }

    // FIXME: convert to iterator
    /// Convert the span tree to vector of non-overlapping ranges and their values.
    pub fn to_vector(&self) -> Vec<RangedValue<Byte, T>> {
        self.raw
            .iter()
            .map(|t| {
                let start: Byte = t.0.start.into();
                let end: Byte = t.0.end.into();
                RangedValue::new(start..end, t.1.clone())
            })
            .collect()
    }
}



// ===================
// === RangedValue ===
// ===================

/// A value associated with a range.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
#[allow(missing_docs)]
pub struct RangedValue<I, A> {
    pub range: Range<I>,
    pub value: A,
}

impl<I, A> RangedValue<I, A> {
    /// Constructor.
    pub fn new(range: impl Into<Range<I>>, value: A) -> Self {
        let range = range.into();
        Self { range, value }
    }

    /// Zip sequence of ranged values. The intervals will be intersected. For example, when given:
    /// `[RangedValue(0..2, 'a'), RangedValue(2..4, 'b')]` and
    /// `[RangedValue(0..1, 'x'), RangedValue(2..3, 'y')]`, it will return:
    /// `[
    ///     RangedValue(0..1, (Some('a'), Some('x')),
    ///     RangedValue(1..2, (Some('a'), None),
    ///     RangedValue(2..3, (Some('b'), Some('y')),
    ///     RangedValue(3..4, (Some('b'), None),
    /// ]`
    ///
    /// See tests for more examples.
    pub fn zip_seq<B, C>(
        a_seq: &[RangedValue<I, A>],
        b_seq: &[RangedValue<I, B>],
        f: impl Fn(Option<A>, Option<B>) -> C,
    ) -> Vec<RangedValue<I, C>>
    where
        I: Copy + Ord,
        A: Copy,
        B: Copy,
    {
        let mut a_iter = a_seq.iter();
        let mut b_iter = b_seq.iter();
        let mut opt_a = a_iter.next();
        let mut opt_b = b_iter.next();
        let mut result = default();

        let mut start = match (&opt_a, &opt_b) {
            (Some(a), Some(b)) => std::cmp::min(a.range.start, b.range.start),
            (Some(a), None) => a.range.start,
            (None, Some(b)) => b.range.start,
            (None, None) => return result,
        };

        loop {
            let val = match (opt_a.as_ref(), opt_b.as_ref()) {
                (None, None) => break,
                (Some(a), Some(b)) => {
                    let sparse_a = start < a.range.start;
                    let sparse_b = start < b.range.start;
                    if sparse_a && sparse_b {
                        let end = std::cmp::min(a.range.start, b.range.start);
                        RangedValue::new(Range::new(start, end), f(None, None))
                    } else if sparse_a {
                        RangedValue::new(Range::new(start, a.range.start), f(None, Some(b.value)))
                    } else if sparse_b {
                        RangedValue::new(Range::new(start, b.range.start), f(Some(a.value), None))
                    } else if a.range.end < b.range.end {
                        let range = Range::new(start, a.range.end);
                        let val = RangedValue::new(range, f(Some(a.value), Some(b.value)));
                        opt_a = a_iter.next();
                        val
                    } else if a.range.end > b.range.end {
                        let range = Range::new(start, b.range.end);
                        let val = RangedValue::new(range, f(Some(a.value), Some(b.value)));
                        opt_b = b_iter.next();
                        val
                    } else {
                        let range = Range::new(start, a.range.end);
                        let val = RangedValue::new(range, f(Some(a.value), Some(b.value)));
                        opt_a = a_iter.next();
                        opt_b = b_iter.next();
                        val
                    }
                }
                (Some(a), None) =>
                    if start < a.range.start {
                        RangedValue::new(Range::new(start, a.range.start), f(None, None))
                    } else {
                        let range = Range::new(start, a.range.end);
                        let val = RangedValue::new(range, f(Some(a.value), None));
                        opt_a = a_iter.next();
                        val
                    },
                (None, Some(b)) =>
                    if start < b.range.start {
                        RangedValue::new(Range::new(start, b.range.start), f(None, None))
                    } else {
                        let range = Range::new(start, b.range.end);
                        let val = RangedValue::new(range, f(None, Some(b.value)));
                        opt_b = b_iter.next();
                        val
                    },
            };
            start = val.range.end;
            result.push(val);
        }

        result
    }

    /// Like `zip_seq`, but for three sequences of ranged values.
    pub fn zip3_seq<B, C, X>(
        a_seq: &[RangedValue<I, A>],
        b_seq: &[RangedValue<I, B>],
        c_seq: &[RangedValue<I, C>],
        f: impl Fn(Option<A>, Option<B>, Option<C>) -> X,
    ) -> Vec<RangedValue<I, X>>
    where
        I: Copy + Ord,
        A: Copy,
        B: Copy,
        C: Copy,
    {
        let ab_seq = RangedValue::zip_seq(a_seq, b_seq, |a, b| (a, b));
        RangedValue::zip_seq(&ab_seq, c_seq, |ab, c| {
            let a = ab.and_then(|t| t.0);
            let b = ab.and_then(|t| t.1);
            f(a, b, c)
        })
    }

    /// Like `zip_seq`, but for four sequences of ranged values.
    pub fn zip4_seq<B, C, D, X>(
        a_seq: &[RangedValue<I, A>],
        b_seq: &[RangedValue<I, B>],
        c_seq: &[RangedValue<I, C>],
        d_seq: &[RangedValue<I, D>],
        f: impl Fn(Option<A>, Option<B>, Option<C>, Option<D>) -> X,
    ) -> Vec<RangedValue<I, X>>
    where
        I: Copy + Ord,
        A: Copy,
        B: Copy,
        C: Copy,
        D: Copy,
    {
        let abc_seq = RangedValue::zip3_seq(a_seq, b_seq, c_seq, |a, b, c| (a, b, c));
        RangedValue::zip_seq(&abc_seq, d_seq, |abc, d| {
            let a = abc.and_then(|t| t.0);
            let b = abc.and_then(|t| t.1);
            let c = abc.and_then(|t| t.2);
            f(a, b, c, d)
        })
    }
}

impl<I, A> RangedValue<I, A> {
    /// Like `zip_seq`, but returns default values instead of `None`.
    pub fn zip_def_seq<B, C>(
        a_seq: &[RangedValue<I, A>],
        b_seq: &[RangedValue<I, B>],
        f: impl Fn(A, B) -> C,
    ) -> Vec<RangedValue<I, C>>
    where
        I: Copy + Ord,
        A: Copy + Default,
        B: Copy + Default,
    {
        Self::zip_seq(a_seq, b_seq, |a, b| f(a.unwrap_or_default(), b.unwrap_or_default()))
    }

    /// Like `zip3_seq`, but returns default values instead of `None`.
    pub fn zip3_def_seq<B, C, X>(
        a_seq: &[RangedValue<I, A>],
        b_seq: &[RangedValue<I, B>],
        c_seq: &[RangedValue<I, C>],
        f: impl Fn(A, B, C) -> X,
    ) -> Vec<RangedValue<I, X>>
    where
        I: Copy + Ord,
        A: Copy + Default,
        B: Copy + Default,
        C: Copy + Default,
    {
        Self::zip3_seq(a_seq, b_seq, c_seq, |a, b, c| {
            f(a.unwrap_or_default(), b.unwrap_or_default(), c.unwrap_or_default())
        })
    }

    /// Like `zip4_seq`, but returns default values instead of `None`.
    pub fn zip4_def_seq<B, C, D, X>(
        a_seq: &[RangedValue<I, A>],
        b_seq: &[RangedValue<I, B>],
        c_seq: &[RangedValue<I, C>],
        d_seq: &[RangedValue<I, D>],
        f: impl Fn(A, B, C, D) -> X,
    ) -> Vec<RangedValue<I, X>>
    where
        I: Copy + Ord,
        A: Copy + Default,
        B: Copy + Default,
        C: Copy + Default,
        D: Copy + Default,
    {
        Self::zip4_seq(a_seq, b_seq, c_seq, d_seq, |a, b, c, d| {
            f(
                a.unwrap_or_default(),
                b.unwrap_or_default(),
                c.unwrap_or_default(),
                d.unwrap_or_default(),
            )
        })
    }
}

impl<I, A> RangedValue<I, A> {
    /// Map the range of this ranged value.
    pub fn map_range<J>(self, f: impl Fn(Range<I>) -> Range<J>) -> RangedValue<J, A> {
        let range = f(self.range);
        let value = self.value;
        RangedValue { range, value }
    }

    /// Map the value of this ranged value.
    pub fn map_value<B>(self, f: impl Fn(A) -> B) -> RangedValue<I, B> {
        let range = self.range;
        let value = f(self.value);
        RangedValue { range, value }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    type V1 = Vec<RangedValue<i32, i32>>;
    type V2 = Vec<RangedValue<i32, char>>;

    #[test]
    fn test_zip_ranged_values() {
        let v1: V1 = vec![RangedValue::new(0..2, 0), RangedValue::new(2..4, 1)];
        let v2: V2 = vec![RangedValue::new(0..1, 'a'), RangedValue::new(1..4, 'b')];
        assert_eq!(RangedValue::zip_seq(&v1, &v2, |a, b| (a, b)), vec![
            RangedValue::new(0..1, (Some(0), Some('a'))),
            RangedValue::new(1..2, (Some(0), Some('b'))),
            RangedValue::new(2..4, (Some(1), Some('b'))),
        ]);

        let v1: V1 = vec![RangedValue::new(0..2, 0), RangedValue::new(2..4, 1)];
        let v2: V2 = vec![RangedValue::new(0..1, 'a'), RangedValue::new(1..5, 'b')];
        assert_eq!(RangedValue::zip_seq(&v1, &v2, |a, b| (a, b)), vec![
            RangedValue::new(0..1, (Some(0), Some('a'))),
            RangedValue::new(1..2, (Some(0), Some('b'))),
            RangedValue::new(2..4, (Some(1), Some('b'))),
            RangedValue::new(4..5, (None, Some('b'))),
        ]);

        let v1: V1 = vec![RangedValue::new(0..1, 0), RangedValue::new(2..4, 1)];
        let v2: V2 = vec![];
        assert_eq!(RangedValue::zip_seq(&v1, &v2, |a, b| (a, b)), vec![
            RangedValue::new(0..1, (Some(0), None)),
            RangedValue::new(1..2, (None, None)),
            RangedValue::new(2..4, (Some(1), None)),
        ]);

        let v1: V1 = vec![RangedValue::new(0..2, 0), RangedValue::new(3..4, 1)];
        let v2: V2 = vec![RangedValue::new(0..1, 'a'), RangedValue::new(1..5, 'b')];
        assert_eq!(RangedValue::zip_seq(&v1, &v2, |a, b| (a, b)), vec![
            RangedValue::new(0..1, (Some(0), Some('a'))),
            RangedValue::new(1..2, (Some(0), Some('b'))),
            RangedValue::new(2..3, (None, Some('b'))),
            RangedValue::new(3..4, (Some(1), Some('b'))),
            RangedValue::new(4..5, (None, Some('b'))),
        ]);
    }
}
