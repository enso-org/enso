use crate::prelude::*;
use crate::Bytes;



// ===================
// === Info & With ===
// ===================

/// A location information. See [`With`] to learn more about the available fields.
pub type Info = With<()>;

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
    pub elem:                T,
    pub left_visible_offset: usize,
    pub left_offset:         Bytes,
    pub start:               Bytes,
    pub len:                 Bytes,
}

impl<T> With<T> {
    #[inline(always)]
    pub fn new_no_offset(start: Bytes, len: Bytes, elem: T) -> Self {
        let left_visible_offset = 0;
        let left_offset = Bytes::from(0);
        Self { left_visible_offset, left_offset, start, len, elem }
    }

    #[inline(always)]
    pub fn new_no_offset_phantom(start: Bytes, elem: T) -> Self {
        let len = Bytes::from(0);
        Self::new_no_offset(start, len, elem)
    }

    #[inline(always)]
    pub fn replace_elem<S>(self, elem: S) -> (With<S>, T) {
        let left_visible_offset = self.left_visible_offset;
        let left_offset = self.left_offset;
        let start = self.start;
        let len = self.len;
        (With { left_visible_offset, left_offset, start, len, elem }, self.elem)
    }

    #[inline(always)]
    pub fn with_elem<S>(self, elem: S) -> With<S> {
        self.replace_elem(elem).0
    }

    #[inline(always)]
    pub fn clone_with_elem<S>(&self, elem: S) -> With<S> {
        let left_visible_offset = self.left_visible_offset;
        let left_offset = self.left_offset;
        let start = self.start;
        let len = self.len;
        With { left_visible_offset, left_offset, start, len, elem }
    }

    #[inline(always)]
    pub fn mod_elem<S>(self, f: impl FnOnce(T) -> S) -> With<S> {
        let left_visible_offset = self.left_visible_offset;
        let left_offset = self.left_offset;
        let start = self.start;
        let len = self.len;
        let elem = f(self.elem);
        With { left_visible_offset, left_offset, start, len, elem }
    }

    #[inline(always)]
    pub fn split_at(self, offset: Bytes) -> (Info, Info) {
        let (_, token_left, token_right) = self.split_at_internal(offset);
        (token_left, token_right)
    }

    #[inline(always)]
    fn split_at_internal(self, offset: Bytes) -> (T, Info, Info) {
        let token_left = {
            let left_visible_offset = self.left_visible_offset;
            let left_offset = self.left_offset;
            let start = self.start;
            let len = self.len - offset;
            let elem = ();
            With { left_visible_offset, left_offset, start, len, elem }
        };
        let token_right = {
            let left_visible_offset = 0;
            let left_offset = Bytes::from(0);
            let start = self.start + offset;
            let len = self.len - offset;
            let elem = ();
            With { left_visible_offset, left_offset, start, len, elem }
        };
        (self.elem, token_left, token_right)
    }

    pub fn split_at_start(self) -> (Info, With<T>) {
        let (elem, token_left, token_right) = self.split_at_internal(Bytes::from(0));
        let token_right = token_right.with_elem(elem);
        (token_left, token_right)
    }

    pub fn trim_left(&mut self) -> Info {
        let info = self.clone_with_elem(());
        let (token_left, token_right) = info.split_at(Bytes::from(0));
        self.left_visible_offset = token_right.left_visible_offset;
        self.left_offset = token_right.left_offset;
        self.start = token_right.start;
        self.len = token_right.len;
        token_left
    }

    pub fn source_slice<'a>(&self, source: &'a str) -> &'a str {
        source.slice(self.start..self.start + self.len)
    }

    /// Please note that the [`other`] token's position has to be bigger than self's one. This
    /// condition is not checked.
    pub fn extend_to<S>(&mut self, other: &With<S>) {
        println!("extend_to: {} {} {}", other.start, other.len, self.start);
        self.len = other.start + other.len - self.start;
    }

    pub fn extended_to<S>(mut self, other: &With<S>) -> Self {
        self.extend_to(other);
        self
    }

    pub fn location(&self) -> Info {
        let left_visible_offset = self.left_visible_offset;
        let left_offset = self.left_offset;
        let start = self.start;
        let len = self.len;
        let elem = ();
        With { left_visible_offset, left_offset, start, len, elem }
    }
}

impl<T> AsRef<With<T>> for With<T> {
    fn as_ref(&self) -> &With<T> {
        self
    }
}
