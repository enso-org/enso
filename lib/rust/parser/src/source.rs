//! Enso language source code related utilities, including a structure attaching source code to
//! other types or an abstraction allowing for getting the representation of an entity, such as
//! [`Token`] (tokens remember the location only, in order to get their representation, the source
//! code needs to be sampled).

use crate::prelude::*;

pub mod span;

pub use span::Offset;
pub use span::Span;
pub use span::VisibleOffset;


// ==============
// === Token ===
// ==============

/// Structure used to keep an element [`T`] associated with source code.
///
/// # Pretty printing
/// Please note, that neither [`Token`] nor [`Ast`] contain sources, it keeps track of the char
/// offsets only. If you want to pretty print it, you should attach sources to it. The easiest way
/// to do it is by using the [`Token`] data, for example as:
/// ```text
/// println!("{:#?}", source::Token::new(str, &ast));
/// ```
#[derive(Clone, Deref, DerefMut, Eq, PartialEq)]
#[allow(missing_docs)]
pub struct Token<'s, T = crate::syntax::token::Variant> {
    #[deref]
    #[deref_mut]
    pub data:        T,
    pub left_offset: Offset<'s>,
    pub code:        Cow<'s, str>,
}

/// Constructor.
#[allow(non_snake_case)]
pub fn Token<'s, T>(
    left_offset: Offset<'s>,
    code: impl Into<Cow<'s, str>>,
    data: T,
) -> Token<'s, T> {
    let code = code.into();
    Token { data, left_offset, code }
}

impl<'s, T: Debug> Debug for Token<'s, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[off: {}, repr: \"{}\"] ", self.left_offset.visible, self.code)?;
        Debug::fmt(&self.data, f)
    }
}

impl<'s, T: PartialEq> PartialEq<Token<'s, T>> for &Token<'s, T> {
    fn eq(&self, other: &Token<'s, T>) -> bool {
        <Token<'s, T> as PartialEq<Token<'s, T>>>::eq(*self, other)
    }
}

impl<'s, T> Token<'s, T> {
    /// Split the lexeme at the provided byte offset. The offset is counted from the [`code`] start
    /// position, which does not include the [`left_offset`]. It means that evaluating
    /// `split_at(Bytes::from(0))` will split the lexeme into left offset and a left-trimmed lexeme.
    pub fn split_at(self, offset: Bytes) -> (Token<'s, ()>, Token<'s, ()>, T) {
        let left_lexeme_offset = self.left_offset;
        let right_lexeme_offset = Offset::default();
        let left = Token(left_lexeme_offset, self.code.slice(Bytes::from(0)..offset), ());
        let right = Token(right_lexeme_offset, self.code.slice(offset..), ());
        (left, right, self.data)
    }

    /// A version of [`split_at`] that discards the associated data.
    pub fn split_at_(self, offset: Bytes) -> (Token<'s, ()>, Token<'s, ()>) {
        let (left, right, _) = self.split_at(offset);
        (left, right)
    }

    /// Replace the associated data in this lexeme.
    pub fn with<S>(self, elem: S) -> Token<'s, S> {
        Token(self.left_offset, self.code, elem)
    }

    pub fn trim_as_first_child(&mut self) -> Span<'s> {
        let left_offset = mem::take(&mut self.left_offset);
        let length = Bytes(self.code.len());
        Span { left_offset, length }
    }

    pub fn span(&self) -> Span<'s> {
        let left_offset = self.left_offset.clone();
        let length = self.len();
        Span { left_offset, length }
    }

    pub fn len(&self) -> Bytes {
        Bytes(self.code.len())
    }
}
