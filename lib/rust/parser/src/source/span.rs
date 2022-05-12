//! Source code location. Every token and AST node are using [`Offset`] to remember their location
//! in the source code.

use crate::prelude::*;

use crate::Bytes;



// impl<'s> Offset<'s> {
// /// Constructor.
// pub fn new_no_left_offset() -> Self {
//     let left_visible_offset = VisibleOffset::from(0);
//     let left_offset = Bytes::from(0);
//     Self { left_visible_offset, left_offset }
// }

// /// Constructor.
// pub fn new_no_left_offset_no_len() -> Self {
//     let len = Bytes::from(0);
//     Self::new_no_left_offset(len)
// }

// /// Constructor.
// pub fn new_no_left_offset_no_start(len: Bytes) -> Self {
//     let start = Bytes::from(0);
//     Self::new_no_left_offset(start, len)
// }
//
// /// Constructor of associated span value.
// #[inline(always)]
// pub fn with<S>(self, elem: S) -> With<S> {
//     With { span: self, elem }
// }

// /// Extend the span to cover the other span. Please note that the [`other`] span position has
// to /// be bigger than self's one. This condition is not checked.
// pub fn extend_to(&mut self, other: impl Into<Offset>) {
//     panic!()
//     // let other = other.into();
//     // self.len = other.start + other.len - self.start;
// }
//
// /// Consuming version of [`extend_to`].
// pub fn extended_to(mut self, other: impl Into<Offset>) -> Self {
//     self.extend_to(other);
//     self
// }

// /// Split the span at the provided byte offset. The offset is counted from the span [`start`]
// /// position, which does not include the [`left_offset`]. It means that evaluating
// /// `split_at(Bytes::from(0))` will remove the left offset information.
// #[inline(always)]
// pub fn split_at(self, offset: Bytes) -> (Offset, Offset) {
//     let left_span = {
//         let left_visible_offset = self.left_visible_offset;
//         let left_offset = self.left_offset;
//         let len = self.len - offset;
//         Offset { left_visible_offset, left_offset, len }
//     };
//     let right_span = {
//         let left_visible_offset = VisibleOffset::from(0);
//         let left_offset = Bytes::from(0);
//         let len = self.len - offset;
//         Offset { left_visible_offset, left_offset, len }
//     };
//     (left_span, right_span)
// }
//
// /// Split the span at the start position. Returns two spans, one containing left spacing
// only, /// and the second containing everything else, left-trimmed.
// pub fn split_at_start(self) -> (Offset, Offset) {
//     self.split_at(Bytes::from(0))
// }

// /// Slices the provided slice. The left spacing offset is not used.
// pub fn source_slice<'a>(&self, source: &'a str) -> &'a str {
//     source.slice(self.start..self.start + self.len)
// }
// }



// // ============
// // === With ===
// // ============
//
// /// A location information with an element [`T`]. The struct derefers to the element.
// #[derive(Clone, Copy, Deref, DerefMut, PartialEq, Eq)]
// #[allow(missing_docs)]
// pub struct With<T> {
//     #[deref]
//     #[deref_mut]
//     pub elem: T,
//     pub span: Offset,
// }
//
// pub fn With<T>(span: Offset, elem: T) -> With<T> {
//     With { elem, span }
// }
//
// impl<T> With<T> {
//     /// Constructor.
//     #[inline(always)]
//     pub fn new_no_left_offset(len: Bytes, elem: T) -> Self {
//         let span = Offset::new_no_left_offset(len);
//         Self { span, elem }
//     }
//
//     /// Constructor.
//     #[inline(always)]
//     pub fn new_no_left_offset_no_len(elem: T) -> Self {
//         let span = Offset::new_no_left_offset_no_len();
//         Self { span, elem }
//     }
//
//     /// Apply this span to the input element.
//     #[inline(always)]
//     pub fn with_elem<S>(&self, elem: S) -> With<S> {
//         let span = self.span;
//         With { span, elem }
//     }
//
//     /// Split the span at the start position. Returns span and associated element. The span
// contains     /// left spacing information only, and the element contains everything else,
// left-trimmed.     pub fn split_at_start(self) -> (Offset, With<T>) {
//         let (left_span, right_span) = self.span.split_at_start();
//         let token_right = right_span.with(self.elem);
//         (left_span, token_right)
//     }
//
//     /// Remove left offset spacing information.
//     pub fn trim_left(&mut self) -> Offset {
//         let (left_span, right_span) = self.span.split_at_start();
//         self.span = right_span;
//         left_span
//     }
//
//     // /// Extend the span to cover the other span. Please note that the [`other`] span position
// has     // to /// be bigger than self's one. This condition is not checked.
//     // pub fn extend_to<S>(&mut self, other: &With<S>) {
//     //     self.span.extended_to(other.span);
//     // }
//     //
//     // /// Consuming version of [`extend_to`].
//     // pub fn extended_to<S>(mut self, other: &With<S>) -> Self {
//     //     self.extend_to(other);
//     //     self
//     // }
//
//     pub fn split_at(&self, offset: Bytes) -> (With<()>, With<()>) {
//         let (left, right) = self.span.split_at(offset);
//         (With(left, ()), With(right, ()))
//     }
// }
//
// impl<T> From<With<T>> for Offset {
//     fn from(t: With<T>) -> Offset {
//         t.span
//     }
// }
//
// impl<T> From<&With<T>> for Offset {
//     fn from(t: &With<T>) -> Offset {
//         t.span
//     }
// }
//
// impl<T> AsRef<With<T>> for With<T> {
//     fn as_ref(&self) -> &With<T> {
//         self
//     }
// }
//
// impl<T: Debug> Debug for With<T> {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         write!(f, "[off:{}, len:{}] ", self.span.left_visible_offset, self.span.len)?;
//         Debug::fmt(&self.elem, f)
//     }
// }
//
// impl<T: PartialEq> PartialEq<With<T>> for &With<T> {
//     fn eq(&self, other: &With<T>) -> bool {
//         <With<T> as PartialEq<With<T>>>::eq(*self, other)
//     }
// }
