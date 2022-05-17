//! Source code location. Every token and AST node are using [`Offset`] to remember their location
//! in the source code.

use crate::prelude::*;
use crate::source::*;
use crate::syntax::*;

use crate::lexer;



/// Common traits.
pub mod traits {
    pub use super::FirstChildTrim;
}



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
    pub width_in_spaces: usize,
}

/// Constructor.
#[allow(non_snake_case)]
pub const fn VisibleOffset(width_in_spaces: usize) -> VisibleOffset {
    VisibleOffset { width_in_spaces }
}

impl Display for VisibleOffset {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.width_in_spaces, f)
    }
}

impl From<&str> for VisibleOffset {
    fn from(code: &str) -> Self {
        code.chars()
            .map(|char| lexer::space_char_visible_size(char).unwrap_or(VisibleOffset(1)))
            .fold(default(), Add::add)
    }
}



// ==============
// === Offset ===
// ==============

/// Offset information. In most cases it is used to express the left-hand-side whitespace offset
/// for tokens and AST nodes.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
#[allow(missing_docs)]
pub struct Offset<'s> {
    pub visible: VisibleOffset,
    pub code:    Code<'s>,
}

/// Constructor.
#[allow(non_snake_case)]
pub fn Offset<'s>(visible: VisibleOffset, code: impl Into<Code<'s>>) -> Offset<'s> {
    let code = code.into();
    Offset { visible, code }
}

impl<'s> Offset<'s> {
    /// Length of the offset.
    pub fn len(&self) -> Bytes {
        self.code.len()
    }
}

impl<'s> AsRef<Offset<'s>> for Offset<'s> {
    fn as_ref(&self) -> &Offset<'s> {
        self
    }
}

impl<'s> From<&'s str> for Offset<'s> {
    #[inline(always)]
    fn from(code: &'s str) -> Self {
        Offset(code.into(), code)
    }
}



// ============
// === Span ===
// ============

/// A span of a given syntactic element (token or AST). It contains the left offset code and the
/// information about the length of the element. It does not contain the code of the element. This
/// is done in order to not duplicate the data. For example, some AST nodes contain a lot of tokens.
/// They need to remember their span, but they do not need to remember their code, because it is
/// already stored in the tokens.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
#[allow(missing_docs)]
pub struct Span<'s> {
    pub left_offset: Offset<'s>,
    /// The length of the code, excluding [`left_offset`].
    pub code_length: Bytes,
}

impl<'s> Span<'s> {
    /// Extend the span with another one. The other span has to be the immediate neighbor of the
    /// current span.
    #[inline(always)]
    pub fn extend<'a, T>(&mut self, other: T)
    where
        T: Into<Ref<'s, 'a>>,
        's: 'a, {
        let other = other.into();
        self.code_length += other.left_offset.len() + other.code_length;
    }

    /// Self consuming version of [`extend`].
    pub fn extended<'a, T>(mut self, other: T) -> Self
    where
        T: Into<Ref<'s, 'a>>,
        's: 'a, {
        self.extend(other);
        self
    }

    /// Get the [`Ref`] of the current span.
    pub fn as_ref(&self) -> Ref<'_, 's> {
        Ref { left_offset: &self.left_offset, code_length: self.code_length }
    }
}

impl<'s> AsRef<Span<'s>> for Span<'s> {
    fn as_ref(&self) -> &Span<'s> {
        self
    }
}



// ===========
// === Ref ===
// ===========

/// A borrowed version of [`Span`]. Used mostly by AST visitors.
///
/// One may wonder why this struct is needed, because it looks like we could use [`&Span<'s>`]
/// instead. The problem is that some structs, such as [`Token`] do not contain [`Span<'s>`], but
/// they contain information the [`Ref`] can be constructed from.
#[derive(Debug, Eq, PartialEq)]
#[allow(missing_docs)]
pub struct Ref<'s, 'a> {
    pub left_offset: &'a Offset<'s>,
    /// The length of the code, excluding [`left_offset`].
    pub code_length: Bytes,
}

impl<'s, 'a> From<&'a Span<'s>> for Ref<'s, 'a> {
    #[inline(always)]
    fn from(span: &'a Span<'s>) -> Self {
        let left_offset = &span.left_offset;
        let code_length = span.code_length;
        Self { left_offset, code_length }
    }
}



// ==============
// === RefMut ===
// ==============

/// A mutably borrowed version of [`Span`]. Used mostly by AST visitors.
///
/// Please note that the [`code_length`] field does not provide the mutable access. Each AST node
/// can contain other AST nodes and tokens. The span of an AST node is computed based on the span of
/// the tokens it contains. Thus, you should never modify the [`code_length`] property, you should
/// modify the AST structure instead and this field should be automatically recomputed.
#[derive(Debug, Eq, PartialEq)]
#[allow(missing_docs)]
pub struct RefMut<'s, 'a> {
    pub left_offset: &'a mut Offset<'s>,
    /// The length of the code, excluding [`left_offset`].
    pub code_length: Bytes,
}



// ======================
// === FirstChildTrim ===
// ======================

/// Trim the left offset and return a new [`Span`] containing the trimmed offset and the length of
/// the code.
///
/// It is used to prepare this element for insertion into parent AST node. Left offsets are kept in
/// a hierarchical way in AST. For example, the expression ` a b` will be represented as two tokens
/// `a` and `b`, each having left offset of 1. However, after constructing the [`App`] AST node, the
/// left span of the `a` token will be removed and will be moved to the AST node instead. This
/// function is responsible exactly for this operation.
#[allow(missing_docs)]
pub trait FirstChildTrim<'s> {
    fn trim_as_first_child(&mut self) -> Span<'s>;
}

impl<'s> FirstChildTrim<'s> for Span<'s> {
    #[inline(always)]
    fn trim_as_first_child(&mut self) -> Span<'s> {
        let left_offset = mem::take(&mut self.left_offset);
        let code_length = self.code_length;
        Span { left_offset, code_length }
    }
}



// ===============
// === Builder ===
// ===============

/// A span builder. You can provide it with any elements that contain spans, and it will compute
/// the total span of the provided elements.
#[macro_export]
macro_rules! span_builder {
    ($($arg:ident),* $(,)?) => {
        $crate::source::span::Builder::new() $(.add(&mut $arg))* .span
    };
}

/// A marker struct for span building. The [`T`] parameter can be one of:
/// - [`()`], which means that the structure was not used yet.
/// - [`Option<Span<'s>>`], which means that the struct was used to build the span, however, we are
///   unsure whether the span is known in all the cases.
/// - [`Span<'s>`], which means that the total span can be always computed for the provided
///   parameters.
#[derive(Default, Debug)]
#[allow(missing_docs)]
pub struct Builder<T = ()> {
    pub span: T,
}

/// Constructor.
#[allow(non_snake_case)]
pub fn Builder<T>(span: T) -> Builder<T> {
    Builder { span }
}

impl Builder<()> {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }
}

impl<T> Builder<T> {
    /// Add a new span to the builder.
    #[inline(always)]
    #[allow(clippy::should_implement_trait)]
    pub fn add<S>(self, elem: &mut S) -> Builder<S::Output>
    where S: Build<T> {
        Builder(elem.build(self))
    }
}

/// A trait defining the behavior of [`Builder`] for different types containing spans.
///
/// The trait definition is a little bit strange, consuming the builder as a parameter instead of
/// consuming it as self. This is done because otherwise Rust type checker goes into infinite
/// loops.
#[allow(missing_docs)]
pub trait Build<T> {
    type Output;
    fn build(&mut self, builder: Builder<T>) -> Self::Output;
}


// === Instances ===

impl<'s> Build<()> for Span<'s> {
    type Output = Span<'s>;
    #[inline(always)]
    fn build(&mut self, _builder: Builder<()>) -> Self::Output {
        self.trim_as_first_child()
    }
}

impl<'s> Build<Span<'s>> for Span<'s> {
    type Output = Span<'s>;
    #[inline(always)]
    fn build(&mut self, builder: Builder<Span<'s>>) -> Self::Output {
        builder.span.extended(&*self)
    }
}

impl<'s> Build<Option<Span<'s>>> for Span<'s> {
    type Output = Span<'s>;
    #[inline(always)]
    fn build(&mut self, builder: Builder<Option<Span<'s>>>) -> Self::Output {
        match builder.span {
            Some(span) => span.extended(&*self),
            None => self.trim_as_first_child(),
        }
    }
}

impl<'s> Build<()> for Tree<'s> {
    type Output = Span<'s>;
    #[inline(always)]
    fn build(&mut self, builder: Builder<()>) -> Self::Output {
        Build::build(&mut self.span, builder)
    }
}

impl<'s> Build<Span<'s>> for Tree<'s> {
    type Output = Span<'s>;
    #[inline(always)]
    fn build(&mut self, builder: Builder<Span<'s>>) -> Self::Output {
        builder.span.extended(&self.span)
    }
}

impl<'s> Build<Option<Span<'s>>> for Tree<'s> {
    type Output = Span<'s>;
    #[inline(always)]
    fn build(&mut self, builder: Builder<Option<Span<'s>>>) -> Self::Output {
        Build::build(&mut self.span, builder)
    }
}

impl<'s, T> Build<()> for Token<'s, T> {
    type Output = Span<'s>;
    #[inline(always)]
    fn build(&mut self, _builder: Builder<()>) -> Self::Output {
        self.trim_as_first_child()
    }
}

impl<'s, T> Build<Span<'s>> for Token<'s, T> {
    type Output = Span<'s>;
    #[inline(always)]
    fn build(&mut self, builder: Builder<Span<'s>>) -> Self::Output {
        builder.span.extended(self.span())
    }
}

impl<'s, T> Build<Option<Span<'s>>> for Token<'s, T> {
    type Output = Span<'s>;
    #[inline(always)]
    fn build(&mut self, builder: Builder<Option<Span<'s>>>) -> Self::Output {
        match builder.span {
            Some(span) => span.extended(self.span()),
            None => self.trim_as_first_child(),
        }
    }
}

impl<T> Build<()> for Option<T>
where T: Build<()>
{
    type Output = Option<<T as Build<()>>::Output>;
    #[inline(always)]
    fn build(&mut self, builder: Builder<()>) -> Self::Output {
        self.as_mut().map(|t| Build::build(t, builder))
    }
}

impl<'s, T> Build<Option<Span<'s>>> for Option<T>
where T: Build<Option<Span<'s>>>
{
    type Output = Option<<T as Build<Option<Span<'s>>>>::Output>;
    #[inline(always)]
    fn build(&mut self, builder: Builder<Option<Span<'s>>>) -> Self::Output {
        self.as_mut().map(|t| Build::build(t, builder))
    }
}

impl<'s, T> Build<Span<'s>> for Option<T>
where T: Build<Span<'s>, Output = Span<'s>>
{
    type Output = Span<'s>;
    #[inline(always)]
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
    #[inline(always)]
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
    #[inline(always)]
    fn build(&mut self, builder: Builder<S>) -> Self::Output {
        let b = Build::build(self.first_mut(), builder);
        Build::build(self.tail_mut(), Builder(b))
    }
}

impl<'s, T> Build<Span<'s>> for Vec<T>
where T: Build<Span<'s>, Output = Span<'s>>
{
    type Output = Span<'s>;
    #[inline(always)]
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
    #[inline(always)]
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
    #[inline(always)]
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
    #[inline(always)]
    fn build(&mut self, builder: Builder<Option<Span<'s>>>) -> Self::Output {
        let mut out = builder.span;
        for elem in self {
            out = Build::build(elem, Builder(out)).into();
        }
        out
    }
}
