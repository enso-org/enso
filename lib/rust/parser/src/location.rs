use crate::prelude::*;
use crate::Bytes;



// ============
// === Span ===
// ============

/// Location information. In most cases, it is associated with [`Token`] or [`Ast`].
///
/// Please note that the left offset information is stored in two fields, [`left_visible_offset`]
/// and [`left_offset`]. The first one stores the offset visible on the screen in a "spaces" metric.
/// For example, for the tab char, the visible offset will be counted as 4 spaces. The latter can
/// differ depending on which space character is used. See the following link to learn more:
/// https://en.wikipedia.org/wiki/Whitespace_character.
#[derive(Clone, Copy, Default, PartialEq, Eq)]
pub struct Span {
    pub left_visible_offset: usize,
    pub left_offset:         Bytes,
    /// The start position of the span. Does not include the [`left_offset`]. Used mainly to fast
    /// check the repr of the token in the input string.
    pub start:               Bytes,
    pub len:                 Bytes,
}

impl Span {
    /// Constructor.
    pub fn new_no_left_offset(start: Bytes, len: Bytes) -> Self {
        let left_visible_offset = 0;
        let left_offset = Bytes::from(0);
        Self { left_visible_offset, left_offset, start, len }
    }

    /// Constructor.
    pub fn new_no_left_offset_no_len(start: Bytes) -> Self {
        let len = Bytes::from(0);
        Self::new_no_left_offset(start, len)
    }

    /// Constructor.
    pub fn new_no_left_offset_no_start(len: Bytes) -> Self {
        let start = Bytes::from(0);
        Self::new_no_left_offset(start, len)
    }

    /// Constructor of associated span value.
    #[inline(always)]
    pub fn with<S>(self, elem: S) -> With<S> {
        With { span: self, elem }
    }

    /// Extend the span to cover the other span. Please note that the [`other`] span position has to
    /// be bigger than self's one. This condition is not checked.
    pub fn extend_to(&mut self, other: impl Into<Span>) {
        let other = other.into();
        self.len = other.start + other.len - self.start;
    }

    /// Consuming version of [`extend_to`].
    pub fn extended_to(mut self, other: impl Into<Span>) -> Self {
        self.extend_to(other);
        self
    }

    /// Split the span at the provided byte offset. The offset is counted from the span [`start`]
    /// position, which does not include the [`left_offset`]. It means that evaluating
    /// `split_at(Bytes::from(0))` will remove the left offset information.
    #[inline(always)]
    pub fn split_at(self, offset: Bytes) -> (Span, Span) {
        let left_span = {
            let left_visible_offset = self.left_visible_offset;
            let left_offset = self.left_offset;
            let start = self.start;
            let len = self.len - offset;
            Span { left_visible_offset, left_offset, start, len }
        };
        let right_span = {
            let left_visible_offset = 0;
            let left_offset = Bytes::from(0);
            let start = self.start + offset;
            let len = self.len - offset;
            Span { left_visible_offset, left_offset, start, len }
        };
        (left_span, right_span)
    }

    /// Split the span at the start position. Returns two spans, one containing left spacing only,
    /// and the second containing everything else, left-trimmed.
    pub fn split_at_start(self) -> (Span, Span) {
        self.split_at(Bytes::from(0))
    }

    /// Slices the provided slice. The left spacing offset is not used.
    pub fn source_slice<'a>(&self, source: &'a str) -> &'a str {
        source.slice(self.start..self.start + self.len)
    }
}

impl AsRef<Span> for Span {
    fn as_ref(&self) -> &Span {
        self
    }
}



// ============
// === With ===
// ============

/// A location information with an element [`T`]. The struct derefers to the element.
#[derive(Clone, Copy, Deref, DerefMut, PartialEq, Eq)]
#[allow(missing_docs)]
pub struct With<T> {
    #[deref]
    #[deref_mut]
    pub elem: T,
    pub span: Span,
}

impl<T> With<T> {
    /// Constructor.
    #[inline(always)]
    pub fn new_no_left_offset(start: Bytes, len: Bytes, elem: T) -> Self {
        let span = Span::new_no_left_offset(start, len);
        Self { span, elem }
    }

    /// Constructor.
    #[inline(always)]
    pub fn new_no_left_offset_no_len(start: Bytes, elem: T) -> Self {
        let span = Span::new_no_left_offset_no_len(start);
        Self { span, elem }
    }

    /// Constructor.
    #[inline(always)]
    pub fn new_no_left_offset_no_start(len: Bytes, elem: T) -> Self {
        let span = Span::new_no_left_offset_no_start(len);
        Self { span, elem }
    }

    #[inline(always)]
    pub fn with_elem<S>(&self, elem: S) -> With<S> {
        let span = self.span;
        With { span, elem }
    }

    /// Modify the element with the provided function.
    #[inline(always)]
    pub fn mod_elem<S>(self, f: impl FnOnce(T) -> S) -> With<S> {
        let elem = f(self.elem);
        let span = self.span;
        With { span, elem }
    }

    /// Split the span at the start position. Returns span and associated element. The span contains
    /// left spacing information only, and the element contains everything else, left-trimmed.
    pub fn split_at_start(self) -> (Span, With<T>) {
        let (left_span, right_span) = self.span.split_at_start();
        let token_right = right_span.with(self.elem);
        (left_span, token_right)
    }

    /// Removes left offset spacing information.
    pub fn trim_left(&mut self) -> Span {
        let (left_span, right_span) = self.span.split_at_start();
        self.span = right_span;
        left_span
    }

    /// Slices the provided slice. The left spacing offset is not used.
    pub fn source_slice<'a>(&self, source: &'a str) -> &'a str {
        self.span.source_slice(source)
    }

    /// Extend the span to cover the other span. Please note that the [`other`] span position has to
    /// be bigger than self's one. This condition is not checked.
    pub fn extend_to<S>(&mut self, other: &With<S>) {
        self.span.extended_to(other.span);
    }

    /// Consuming version of [`extend_to`].
    pub fn extended_to<S>(mut self, other: &With<S>) -> Self {
        self.extend_to(other);
        self
    }
}

impl<T> From<With<T>> for Span {
    fn from(t: With<T>) -> Span {
        t.span
    }
}

impl<T> From<&With<T>> for Span {
    fn from(t: &With<T>) -> Span {
        t.span
    }
}

impl<T> AsRef<With<T>> for With<T> {
    fn as_ref(&self) -> &With<T> {
        self
    }
}
