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
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[derive(From, Into, Add, AddAssign, Sub, Reflect, Serialize, Deserialize)]
#[allow(missing_docs)]
#[reflect(transparent)]
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
#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Reflect, Deserialize)]
#[allow(missing_docs)]
pub struct Offset<'s> {
    #[reflect(hide)]
    pub visible: VisibleOffset,
    #[reflect(flatten, hide)]
    pub code:    Code<'s>,
}

/// Constructor.
#[allow(non_snake_case)]
pub fn Offset<'s>(visible: VisibleOffset, code: impl Into<Code<'s>>) -> Offset<'s> {
    let code = code.into();
    Offset { visible, code }
}

impl<'s> Offset<'s> {
    /// Check if the offset is 0.
    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.code.is_empty()
    }

    /// Check if the offset is bigger than 0.
    #[inline(always)]
    pub fn exists(&self) -> bool {
        !self.is_empty()
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

impl<'s> std::ops::AddAssign<Offset<'s>> for Offset<'s> {
    fn add_assign(&mut self, other: Offset<'s>) {
        self.visible += other.visible;
        self.code += other.code;
    }
}

impl<'s> std::ops::AddAssign<&Offset<'s>> for Offset<'s> {
    fn add_assign(&mut self, other: &Offset<'s>) {
        self.visible += other.visible;
        self.code += &other.code;
    }
}



// ============
// === Span ===
// ============

/// A span of a given syntactic element (token or AST). It is a monoid that contains the left offset
/// code and the information about the length of the element. It does not contain the code of the
/// element. This is done in order to not duplicate the data. For example, some AST nodes contain a
/// lot of tokens. They need to remember their span, but they do not need to remember their code,
/// because it is already stored in the tokens.
#[derive(Clone, Debug, Default, Eq, PartialEq, Serialize, Reflect, Deserialize)]
#[allow(missing_docs)]
pub struct Span<'s> {
    #[reflect(hide, flatten)]
    pub left_offset: Offset<'s>,
    /// The length of the code, excluding [`left_offset`].
    #[reflect(hide, flatten)]
    pub code_length: code::Length,
}

impl<'s> Span<'s> {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }

    /// Check whether the span is empty.
    pub fn is_empty(&self) -> bool {
        self.left_offset.is_empty() && self.code_length.is_zero()
    }

    /// Check whether the span is only an offset, without the code part.
    pub fn is_only_offset(&self) -> bool {
        self.code_length.is_zero()
    }

    /// Get the [`Ref`] of the current span.
    pub fn as_ref(&self) -> Ref<'_, 's> {
        Ref { left_offset: &self.left_offset, code_length: self.code_length }
    }

    /// Add the item to this span. The item can be anything that implements the span [`Builder`].
    #[allow(clippy::should_implement_trait)]
    pub fn add<T: Builder<'s>>(self, elem: &mut T) -> Self {
        Builder::add_to_span(elem, self)
    }
}

impl<'s> AsRef<Span<'s>> for Span<'s> {
    fn as_ref(&self) -> &Span<'s> {
        self
    }
}

impl<'s, 'a, T> PartialSemigroup<T> for Span<'s>
where
    T: Into<Ref<'s, 'a>>,
    's: 'a,
{
    #[inline(always)]
    fn concat_mut(&mut self, other: T) {
        let other = other.into();
        if self.code_length.is_zero() {
            self.left_offset += other.left_offset;
            self.code_length = other.code_length;
        } else {
            self.code_length += other.left_offset.code.length() + other.code_length;
        }
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
    pub code_length: code::Length,
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
    pub code_length: code::Length,
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
        $crate::source::span::Span::new() $(.add(&mut $arg))*
    };
}


/// Elements implementing this trait can contain a span or multiple spans. If an element is added to
/// an empty span, it means that it is the first element in the span group. In such a case, the left
/// offset of the element will be removed and moved to the resulting span. See the docs of
/// [`FirstChildTrim`] to learn more.
#[allow(missing_docs)]
pub trait Builder<'s> {
    fn add_to_span(&mut self, span: Span<'s>) -> Span<'s>;
}


// === Instances ===

impl<'s> Builder<'s> for Span<'s> {
    #[inline(always)]
    fn add_to_span(&mut self, span: Span<'s>) -> Span<'s> {
        if span.is_only_offset() {
            span.concat(&self.trim_as_first_child())
        } else {
            span.concat(&*self)
        }
    }
}

impl<'s> Builder<'s> for Tree<'s> {
    #[inline(always)]
    fn add_to_span(&mut self, span: Span<'s>) -> Span<'s> {
        Builder::add_to_span(&mut self.span, span)
    }
}

impl<'s, T> Builder<'s> for Token<'s, T> {
    #[inline(always)]
    fn add_to_span(&mut self, span: Span<'s>) -> Span<'s> {
        if span.is_only_offset() {
            span.concat(&self.trim_as_first_child())
        } else {
            span.concat(self.span())
        }
    }
}

impl<'s, T> Builder<'s> for Option<T>
where T: Builder<'s>
{
    #[inline(always)]
    fn add_to_span(&mut self, span: Span<'s>) -> Span<'s> {
        match self {
            Some(t) => Builder::add_to_span(t, span),
            None => span,
        }
    }
}

impl<'s, T, E> Builder<'s> for Result<T, E>
where
    T: Builder<'s>,
    E: Builder<'s>,
{
    #[inline(always)]
    fn add_to_span(&mut self, span: Span<'s>) -> Span<'s> {
        match self {
            Ok(t) => Builder::add_to_span(t, span),
            Err(t) => Builder::add_to_span(t, span),
        }
    }
}

impl<'s, T> Builder<'s> for NonEmptyVec<T>
where T: Builder<'s>
{
    #[inline(always)]
    fn add_to_span(&mut self, span: Span<'s>) -> Span<'s> {
        self.into_iter().fold(span, |sum, new_span| Builder::add_to_span(new_span, sum))
    }
}

impl<'s, T> Builder<'s> for Vec<T>
where T: Builder<'s>
{
    #[inline(always)]
    fn add_to_span(&mut self, span: Span<'s>) -> Span<'s> {
        self.iter_mut().fold(span, |sum, new_span| Builder::add_to_span(new_span, sum))
    }
}

impl<'s, T> Builder<'s> for [T]
where T: Builder<'s>
{
    #[inline(always)]
    fn add_to_span(&mut self, span: Span<'s>) -> Span<'s> {
        self.iter_mut().fold(span, |sum, new_span| Builder::add_to_span(new_span, sum))
    }
}
