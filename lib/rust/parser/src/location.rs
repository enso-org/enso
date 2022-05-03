use crate::prelude::*;
use crate::Bytes;



// ===================
// === Info & With ===
// ===================

#[derive(Clone, Copy, Default, PartialEq, Eq)]
pub struct Span {
    pub left_visible_offset: usize,
    pub left_offset:         Bytes,
    /// Used mainly to fast check the repr of the token in the input string.
    pub start:               Bytes,
    pub len:                 Bytes,
}

impl Span {
    #[inline(always)]
    pub fn with_elem<S>(self, elem: S) -> With<S> {
        With { span: self, elem }
    }

    /// Please note that the [`other`] token's position has to be bigger than self's one. This
    /// condition is not checked.
    pub fn extend_to(&mut self, other: impl Into<Span>) {
        let other = other.into();
        self.len = other.start + other.len - self.start;
    }

    pub fn extended_to(mut self, other: impl Into<Span>) -> Self {
        self.extend_to(other);
        self
    }
}

/// A location information with an element [`T`]. The struct derefers to the element.
///
/// Please note that the left offset information is stored in two fields, [`left_visible_offset`]
/// and [`left_offset`]. The first one stores the offset visible on the screen in a "spaces" metric.
/// For example, for the tab char, the visible offset will be counted as 4 spaces. The latter can
/// differ depending on which space character is used. See the following link to learn more:
/// https://en.wikipedia.org/wiki/Whitespace_character.
#[derive(Clone, Copy, Deref, PartialEq, Eq)]
pub struct With<T> {
    #[deref]
    pub elem: T,
    pub span: Span,
}

impl<T> With<T> {
    #[inline(always)]
    pub fn new_no_offset(start: Bytes, len: Bytes, elem: T) -> Self {
        let left_visible_offset = 0;
        let left_offset = Bytes::from(0);
        let span = Span { left_visible_offset, left_offset, start, len };
        Self { span, elem }
    }

    #[inline(always)]
    pub fn new_no_offset_phantom(start: Bytes, elem: T) -> Self {
        let len = Bytes::from(0);
        Self::new_no_offset(start, len, elem)
    }

    #[inline(always)]
    pub fn new_with_len(len: Bytes, elem: T) -> Self {
        let start = Bytes::from(0);
        Self::new_no_offset(start, len, elem)
    }

    #[inline(always)]
    pub fn test_from_repr(repr: &str, elem: T) -> Self {
        Self::new_with_len(Bytes::from(repr.len()), elem)
    }

    #[inline(always)]
    pub fn replace_elem<S>(self, elem: S) -> (With<S>, T) {
        let span = self.span;
        (With { span, elem }, self.elem)
    }

    #[inline(always)]
    pub fn with_elem<S>(self, elem: S) -> With<S> {
        self.replace_elem(elem).0
    }

    #[inline(always)]
    pub fn clone_with_elem<S>(&self, elem: S) -> With<S> {
        let span = self.span;
        With { span, elem }
    }

    #[inline(always)]
    pub fn mod_elem<S>(self, f: impl FnOnce(T) -> S) -> With<S> {
        let elem = f(self.elem);
        let span = self.span;
        With { span, elem }
    }

    #[inline(always)]
    pub fn split_at(self, offset: Bytes) -> (Span, Span) {
        let (_, token_left, token_right) = self.split_at_internal(offset);
        (token_left, token_right)
    }

    #[inline(always)]
    fn split_at_internal(self, offset: Bytes) -> (T, Span, Span) {
        let left_span = {
            let left_visible_offset = self.span.left_visible_offset;
            let left_offset = self.span.left_offset;
            let start = self.span.start;
            let len = self.span.len - offset;
            Span { left_visible_offset, left_offset, start, len }
        };
        let right_span = {
            let left_visible_offset = 0;
            let left_offset = Bytes::from(0);
            let start = self.span.start + offset;
            let len = self.span.len - offset;
            Span { left_visible_offset, left_offset, start, len }
        };
        (self.elem, left_span, right_span)
    }

    pub fn split_at_start(self) -> (Span, With<T>) {
        let (elem, left_span, right_span) = self.split_at_internal(Bytes::from(0));
        let token_right = right_span.with_elem(elem);
        (left_span, token_right)
    }

    pub fn trim_left(&mut self) -> Span {
        let info = self.clone_with_elem(());
        let (left_span, right_span) = info.split_at(Bytes::from(0));
        self.span = right_span;
        left_span
    }

    pub fn source_slice<'a>(&self, source: &'a str) -> &'a str {
        source.slice(self.span.start..self.span.start + self.span.len)
    }

    /// Please note that the [`other`] token's position has to be bigger than self's one. This
    /// condition is not checked.
    pub fn extend_to<S>(&mut self, other: &With<S>) {
        self.span.len = other.span.start + other.span.len - self.span.start;
    }

    pub fn extended_to<S>(mut self, other: &With<S>) -> Self {
        self.extend_to(other);
        self
    }

    pub fn location(&self) -> Span {
        let left_visible_offset = self.span.left_visible_offset;
        let left_offset = self.span.left_offset;
        let start = self.span.start;
        let len = self.span.len;
        Span { left_visible_offset, left_offset, start, len }
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
