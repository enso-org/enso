//! The text operation utilities.
//!
//! This crate contains several utility structures for operations on text:
//! * The effective [`Text`] structure, optimized for middle-insertions, based on the rope
//!   structure.
//! * A set of units, forcing the developers to think about how the text positions are expressed (in
//!   chars, or in bytes? Or maybe in _grapheme clusters_)?
//! * An alternative [`Range`] with text-related trait implementations + copyable.
//! * Interval tree structure [`Spans`] useful for text rich decorations.
//!
//! To properly understand the implementation and its assumptions, you have to know a lot about
//! text encoding in different formats and text rendering. Especially, these links are very useful:
//! - https://gankra.github.io/blah/text-hates-you
//! - https://lord.io/blog/2019/text-editing-hates-you-too
//! - https://utf8everywhere.org
//! - https://docs.google.com/document/d/1wuzzMOvKOJw93SWZAqoim1VUl9mloUxE0W6Ki_G23tw/edit (copy) https://docs.google.com/document/d/1D7iWPWQHrWY276WPVFZTi8JJqUnTcIVJs4dlG0IdCp8
//!
//! As a very short introduction, there are several common names used in this implementation:
//!
//! - **Code point** Any numerical value in the Unicode codespace. For instance: U+3243F.
//!
//! - **Code unit** The minimal bit combination that can represent a unit of encoded text. For
//!   example, UTF-8, UTF-16 and UTF-32 use 8-bit, 16-bit and 32-bit code units respectively. The
//!   above code point will be encoded as four code units ‘f0 b2 90 bf’ in UTF-8, two code units
//!   ‘d889 dc3f’ in UTF-16 and as a single code unit ‘0003243f’ in UTF-32. Note that these are just
//!   sequences of groups of bits; how they are stored on an octet-oriented media depends on the
//!   endianness of the particular encoding. When storing the above UTF-16 code units, they will be
//!   converted to ‘d8 89 dc 3f’ in UTF-16BE and to ‘89 d8 3f dc’ in UTF-16LE.
//!
//! - **Abstract character** A unit of information used for the organization, control, or
//!   representation of textual data. The standard says:
//!
//!   > For the Unicode Standard, [...] the repertoire is inherently open. Because Unicode is a
//!   > universal encoding, any abstract character that could ever be encoded is a potential
//!   > candidate to be encoded, regardless of whether the character is currently known.
//!
//!   The definition is indeed abstract. Whatever one can think of as a character—is an abstract
//!   character. For example, "tengwar letter ungwe" is an abstract character, although it is not
//!   yet representable in Unicode.
//!
//! - **Encoded character, Coded character** A mapping between a code point and an abstract
//!   character. For example, U+1F428 is a coded character which represents the abstract character
//!   <koala image>.
//!
//!   This mapping is neither total, nor injective, nor surjective:
//!   - Surragates, noncharacters and unassigned code points do not correspond to abstract
//!     characters at all.
//!   - Some abstract characters can be encoded by different code points; U+03A9 greek capital
//!     letter omega and U+2126 ohm sign both correspond to the same abstract character ‘Ω’, and
//!     must be treated identically.
//!   - Some abstract characters cannot be encoded by a single code point. These are represented by
//!     sequences of coded characters. For example, the only way to represent the abstract character
//!     <cyrillic small letter yu with acute> is by the sequence U+044E cyrillic small letter yu
//!     followed by U+0301 combining acute accent.
//!
//!   Moreover, for some abstract characters, there exist representations using multiple code
//!   points, in addition to the single coded character form. The abstract character ǵ can be coded
//!   by the single code point U+01F5 latin small letter g with acute, or by the sequence
//!   <U+0067 latin small letter g, U+0301 combining acute accent>.
//!
//! - **User-perceived character** Whatever the end user thinks of as a character. This notion is
//!   language dependent. For instance, ‘ch’ is two letters in English and Latin, but considered to
//!   be one letter in Czech and Slovak.
//!
//! - **Grapheme cluster** A sequence of coded characters that ‘should be kept together’. Grapheme
//!   clusters approximate the notion of user-perceived characters in a language independent way.
//!   They are used for, e.g., cursor movement and selection.
//!
//! - **Glyph** A particular shape within a font. Fonts are collections of glyphs designed by a type
//!   designer. It’s the text shaping and rendering engine responsibility to convert a sequence of
//!   code points into a sequence of glyphs within the specified font. The rules for this conversion
//!   might be complicated, locale dependent, and are beyond the scope of the Unicode standard.
//!
//! The Rust and our structures uses UTF-8 encoding. The Rust's [`char`] primitive type corresponds
//! roughly with the *Code points* (to be precise, it corresponds with *scalar values* which are
//! "code points except high-surrogate and low-surrogate code points" - but the surrogate code
//! points are not used uin UTF-8 anyway).

#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]

pub mod range;
pub mod rope;
pub mod spans;
pub mod text;
pub mod unit;

pub use range::Range;
pub use range::RangeBounds;
pub use rope::metric;
pub use rope::Cursor;
pub use spans::Spans;
pub use text::Change;
pub use text::Text;
pub use text::TextCell;
pub use unit::traits;
pub use unit::*;

/// Commonly used utilities.
pub mod prelude {
    pub use enso_prelude::*;
    pub use enso_types::*;
}
