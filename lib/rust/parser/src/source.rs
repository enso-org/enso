//! Enso language source code related utilities, including a structure attaching source code to
//! other types or an abstraction allowing for getting the representation of an entity, such as
//! [`Token`] (tokens remember the location only, in order to get their representation, the source
//! code needs to be sampled).

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

impl<'s> AsRef<Offset<'s>> for Offset<'s> {
    fn as_ref(&self) -> &Offset<'s> {
        self
    }
}



// ==============
// === Lexeme ===
// ==============

/// Structure used to keep an element [`T`] associated with source code.
///
/// # Pretty printing
/// Please note, that neither [`Token`] nor [`Ast`] contain sources, it keeps track of the char
/// offsets only. If you want to pretty print it, you should attach sources to it. The easiest way
/// to do it is by using the [`Lexeme`] data, for example as:
/// ```text
/// println!("{:#?}", source::Lexeme::new(str, &ast));
/// ```
#[derive(Deref, DerefMut, PartialEq)]
#[allow(missing_docs)]
pub struct Lexeme<'s, T = ()> {
    #[deref]
    #[deref_mut]
    pub data:        T,
    pub left_offset: Offset<'s>,
    pub code:        Cow<'s, str>,
}

/// Constructor.
#[allow(non_snake_case)]
pub fn Lexeme<'s, T>(
    left_offset: Offset<'s>,
    code: impl Into<Cow<'s, str>>,
    data: T,
) -> Lexeme<'s, T> {
    let code = code.into();
    Lexeme { data, left_offset, code }
}

impl<'s, T: Debug> Debug for Lexeme<'s, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[off: {}, repr: \"{}\"] ", self.left_offset.visible, self.code)?;
        Debug::fmt(&self.data, f)
    }
}

impl<'s, T: PartialEq> PartialEq<Lexeme<'s, T>> for &Lexeme<'s, T> {
    fn eq(&self, other: &Lexeme<'s, T>) -> bool {
        <Lexeme<'s, T> as PartialEq<Lexeme<'s, T>>>::eq(*self, other)
    }
}

impl<'s, T> Lexeme<'s, T> {
    /// Split the lexeme at the provided byte offset. The offset is counted from the [`code`] start
    /// position, which does not include the [`left_offset`]. It means that evaluating
    /// `split_at(Bytes::from(0))` will split the lexeme into left offset and a left-trimmed lexeme.
    pub fn split_at(self, offset: Bytes) -> (Lexeme<'s>, Lexeme<'s>, T) {
        let left_lexeme_offset = self.left_offset;
        let right_lexeme_offset = Offset::default();
        let left = Lexeme(left_lexeme_offset, self.code.slice(Bytes::from(0)..offset), ());
        let right = Lexeme(right_lexeme_offset, self.code.slice(offset..), ());
        (left, right, self.data)
    }

    /// A version of [`split_at`] that discards the associated data.
    pub fn split_at_(self, offset: Bytes) -> (Lexeme<'s>, Lexeme<'s>) {
        let (left, right, _) = self.split_at(offset);
        (left, right)
    }

    /// Replace the associated data in this lexeme.
    pub fn with<S>(self, elem: S) -> Lexeme<'s, S> {
        Lexeme(self.left_offset, self.code, elem)
    }
}
