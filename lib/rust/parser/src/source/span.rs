//! Source code location. Every token and AST node are using [`Offset`] to remember their location
//! in the source code.

use crate::prelude::*;

// =====================
// === VisibleOffset ===
// =====================

/// A strongly typed visible offset size. For example, a space character has value of 1, while the
/// tab character has value of 4. For other space-like character sizes, refer to the lexer
/// implementation.
#[derive(
    Clone, Copy, Debug, Default, From, Into, Add, AddAssign, Sub, PartialEq, Eq, Hash, PartialOrd,
    Ord
)]
#[allow(missing_docs)]
pub struct VisibleOffset {
    pub number: usize,
}

/// Constructor.
#[allow(non_snake_case)]
pub fn VisibleOffset(number: usize) -> VisibleOffset {
    VisibleOffset { number }
}

impl Display for VisibleOffset {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.number, f)
    }
}

// ==============
// === Offset ===
// ==============

/// Location information. In most cases, it is associated with [`Token`] or [`Ast`].
///
/// Please note that the left offset information is stored in two fields, [`visible`]
/// and [`left_offset`]. The first one stores the offset visible on the screen in a "spaces" metric.
/// For example, for the tab char, the visible offset will be counted as 4 spaces. The latter can
/// differ depending on which space character is used. See the following link to learn more:
/// https://en.wikipedia.org/wiki/Whitespace_character.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
#[allow(missing_docs)]
pub struct Offset<'s> {
    pub visible: VisibleOffset,
    pub code:    Cow<'s, str>,
}

/// Constructor.
#[allow(non_snake_case)]
pub fn Offset<'s>(visible: VisibleOffset, code: impl Into<Cow<'s, str>>) -> Offset<'s> {
    let code = code.into();
    Offset { visible, code }
}

impl<'s> Offset<'s> {
    pub fn len(&self) -> Bytes {
        Bytes(self.code.len())
    }
}

impl<'s> AsRef<Offset<'s>> for Offset<'s> {
    fn as_ref(&self) -> &Offset<'s> {
        self
    }
}



// ============
// === Span ===
// ============

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct Span<'s> {
    pub left_offset: Offset<'s>,
    pub length:      Bytes,
}

impl<'s> Span<'s> {
    pub fn trim_as_first_child(&mut self) -> Span<'s> {
        let left_offset = mem::take(&mut self.left_offset);
        let length = self.length;
        Span { left_offset, length }
    }

    pub fn extend(&mut self, other: &Span<'s>) {
        self.length += other.left_offset.len() + other.length;
    }

    pub fn extended(mut self, other: &Span<'s>) -> Self {
        self.extend(other);
        self
    }
}

impl<'s> AsRef<Span<'s>> for Span<'s> {
    fn as_ref(&self) -> &Span<'s> {
        self
    }
}

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



// ===============
// === Builder ===
// ===============

use crate::syntax::token;
use crate::syntax::tree::Tree;

#[macro_export]
macro_rules! span_builder {
    ($($arg:ident),* $(,)?) => {
        $crate::source::span::Builder::new() $(.add(&mut $arg))* .span
    };
}


#[derive(Default)]
pub struct Builder<T = ()> {
    pub span: T,
}

pub fn Builder<T>(span: T) -> Builder<T> {
    Builder { span }
}

impl Builder<()> {
    pub fn new() -> Self {
        default()
    }
}

impl<T> Builder<T> {
    pub fn add<S>(self, elem: &mut S) -> Builder<S::Output>
    where S: Build<T> {
        Builder(elem.build(self))
    }
}



pub trait Build<T> {
    type Output;
    fn build(&mut self, builder: Builder<T>) -> Self::Output;
}

impl<'s> Build<()> for Span<'s> {
    type Output = Span<'s>;
    fn build(&mut self, _builder: Builder<()>) -> Self::Output {
        self.trim_as_first_child()
    }
}

impl<'s> Build<Span<'s>> for Span<'s> {
    type Output = Span<'s>;
    fn build(&mut self, builder: Builder<Span<'s>>) -> Self::Output {
        builder.span.extended(&*self)
    }
}

impl<'s> Build<Option<Span<'s>>> for Span<'s> {
    type Output = Span<'s>;
    fn build(&mut self, builder: Builder<Option<Span<'s>>>) -> Self::Output {
        match builder.span {
            Some(span) => span.extended(&*self),
            None => self.trim_as_first_child(),
        }
    }
}

impl<'s> Build<()> for Tree<'s> {
    type Output = Span<'s>;
    fn build(&mut self, builder: Builder<()>) -> Self::Output {
        Build::build(&mut self.span, builder)
    }
}

impl<'s> Build<Span<'s>> for Tree<'s> {
    type Output = Span<'s>;
    fn build(&mut self, builder: Builder<Span<'s>>) -> Self::Output {
        builder.span.extended(&self.span)
    }
}

impl<'s> Build<Option<Span<'s>>> for Tree<'s> {
    type Output = Span<'s>;
    fn build(&mut self, builder: Builder<Option<Span<'s>>>) -> Self::Output {
        Build::build(&mut self.span, builder)
    }
}

impl<'s, T> Build<()> for token::Token<'s, T> {
    type Output = Span<'s>;
    fn build(&mut self, builder: Builder<()>) -> Self::Output {
        self.trim_as_first_child()
    }
}

impl<'s, T> Build<Span<'s>> for token::Token<'s, T> {
    type Output = Span<'s>;
    fn build(&mut self, builder: Builder<Span<'s>>) -> Self::Output {
        builder.span.extended(&self.span())
    }
}

impl<'s, T> Build<Option<Span<'s>>> for token::Token<'s, T> {
    type Output = Span<'s>;
    fn build(&mut self, builder: Builder<Option<Span<'s>>>) -> Self::Output {
        match builder.span {
            Some(span) => span.extended(&self.span()),
            None => self.trim_as_first_child(),
        }
    }
}



impl<T> Build<()> for Option<T>
where T: Build<()>
{
    type Output = Option<<T as Build<()>>::Output>;
    fn build(&mut self, builder: Builder<()>) -> Self::Output {
        self.as_mut().map(|t| Build::build(t, builder))
    }
}

impl<'s, T> Build<Option<Span<'s>>> for Option<T>
where T: Build<Option<Span<'s>>>
{
    type Output = Option<<T as Build<Option<Span<'s>>>>::Output>;
    fn build(&mut self, builder: Builder<Option<Span<'s>>>) -> Self::Output {
        self.as_mut().map(|t| Build::build(t, builder))
    }
}

impl<'s, T> Build<Span<'s>> for Option<T>
where T: Build<Span<'s>, Output = Span<'s>>
{
    type Output = Span<'s>;
    fn build(&mut self, builder: Builder<Span<'s>>) -> Self::Output {
        match self.as_mut() {
            None => builder.span,
            Some(t) => Build::build(t, builder),
        }
    }
}


impl<S, T, E> Build<S> for Result<T, E>
where
    T: Build<S>,
    E: Build<S, Output = <T as Build<S>>::Output>,
{
    type Output = <T as Build<S>>::Output;
    fn build(&mut self, builder: Builder<S>) -> Self::Output {
        match self {
            Ok(t) => Build::build(t, builder),
            Err(t) => Build::build(t, builder),
        }
    }
}




impl<S, T> Build<S> for NonEmptyVec<T>
where
    T: Build<S>,
    [T]: Build<<T as Build<S>>::Output>,
{
    type Output = <[T] as Build<T::Output>>::Output;
    fn build(&mut self, builder: Builder<S>) -> Self::Output {
        let b = Build::build(self.first_mut(), builder);
        Build::build(self.tail_mut(), Builder(b))
    }
}

impl<'s, T> Build<Span<'s>> for Vec<T>
where T: Build<Span<'s>, Output = Span<'s>>
{
    type Output = Span<'s>;
    fn build(&mut self, builder: Builder<Span<'s>>) -> Self::Output {
        let mut out = builder.span;
        for elem in self {
            out = Build::build(elem, Builder(out))
        }
        out
    }
}

impl<'s, T> Build<Option<Span<'s>>> for Vec<T>
where
    T: Build<Option<Span<'s>>>,
    T::Output: Into<Option<Span<'s>>>,
{
    type Output = Option<Span<'s>>;
    fn build(&mut self, builder: Builder<Option<Span<'s>>>) -> Self::Output {
        let mut out = builder.span;
        for elem in self {
            out = Build::build(elem, Builder(out)).into();
        }
        out
    }
}

impl<'s, T> Build<Span<'s>> for [T]
where T: Build<Span<'s>, Output = Span<'s>>
{
    type Output = Span<'s>;
    fn build(&mut self, builder: Builder<Span<'s>>) -> Self::Output {
        let mut out = builder.span;
        for elem in self {
            out = Build::build(elem, Builder(out));
        }
        out
    }
}

impl<'s, T> Build<Option<Span<'s>>> for [T]
where
    T: Build<Option<Span<'s>>>,
    T::Output: Into<Option<Span<'s>>>,
{
    type Output = Option<Span<'s>>;
    fn build(&mut self, builder: Builder<Option<Span<'s>>>) -> Self::Output {
        let mut out = builder.span;
        for elem in self {
            out = Build::build(elem, Builder(out)).into();
        }
        out
    }
}

// fn tst<'s>(
//     mut builder: Builder<Option<Span<'s>>>,
//     mut elem: NonEmptyVec<token::Operator<'s>>,
// ) {
//     let x = builder.extend(&mut elem);
// }

// pub trait PartialParentSpanBuilder2<T> {
//     type Output;
//     fn extend_parent_span2(&mut self, builder: Builder<T>) -> Self::Output;
// }
