//! This file defines the various tokens requried by the Enso lexer.
//!
//! This file makes heavy use of terminology from the Enso design documentation, particularly the
//! [syntax](https://enso.org/docs/developer/docs/enso/syntax) documentation. For the sake of
//! brevity, many terms will _not_ be defined here.

use crate::prelude::*;

use crate::lexeme;



// =============
// === Token ===
// =============

/// A lexer token.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Token {
    /// The shape of the token.
    pub shape:  Shape,
    /// The length (in characters) of this token.
    pub length: usize,
    /// The number of trailing spaces after this token before the next.
    pub offset: usize,
}

impl Token {
    /// Constructor.
    pub fn new(shape: Shape, length: usize, offset: usize) -> Token {
        Token { shape, length, offset }
    }

    /// Get the length that the token takes up in the program source.
    pub fn source_length(&self) -> usize {
        self.length + self.offset
    }
}

/// Constructors for the various forms of token.
impl Token {
    /// Construct a token representing a referent identifier.
    pub fn referent(name: impl Str, offset: usize) -> Token {
        let str = name.into();
        let length = str.chars().count();
        let shape = Shape::Referent(str);
        Token { shape, length, offset }
    }

    /// Construct a token representing a variable identifier.
    pub fn variable(name: impl Str, offset: usize) -> Token {
        let str = name.into();
        let length = str.chars().count();
        let shape = Shape::Variable(str);
        Token { shape, length, offset }
    }

    /// Construct a token representing an external identifier.
    pub fn external(name: impl Str, offset: usize) -> Token {
        let str = name.into();
        let length = str.chars().count();
        let shape = Shape::External(str);
        Token { shape, length, offset }
    }

    /// Construct a token representing a blank identifier.
    pub fn blank(offset: usize) -> Token {
        let shape = Shape::Blank;
        let length = lexeme::len(lexeme::literal::BLANK_IDENT);
        Token { shape, length, offset }
    }

    /// Construct a token representing an operator.
    pub fn operator(name: impl Str, offset: usize) -> Token {
        let name = name.into();
        let length = name.chars().count();
        let shape = Shape::Operator(name);
        Token { shape, length, offset }
    }

    /// Construct a token representing a modifier operator.
    pub fn modifier(name: impl Str, offset: usize) -> Token {
        let name = name.into();
        let modifier_len = lexeme::len(lexeme::literal::EQUALS);
        let length = name.chars().count() + modifier_len;
        let shape = Shape::Modifier(name);
        Token { shape, length, offset }
    }

    /// Construct a token representing
    pub fn annotation(name_str: impl Str, offset: usize) -> Token {
        let name = name_str.into();
        let annotation_len = lexeme::len(lexeme::literal::ANNOTATION_SYMBOL);
        let length = name.chars().count() + annotation_len;
        let shape = Shape::Annotation(name);
        Token { shape, length, offset }
    }

    /// Construct a token representing a number literal.
    pub fn number(base: impl Str, num: impl Into<String>, offset: usize) -> Token {
        let number = num.into();
        let base = base.into();
        let length = if base.is_empty() {
            number.chars().count()
        } else {
            let base_sep_len = lexeme::len(lexeme::literal::NUMBER_BASE_SEPARATOR);
            base.chars().count() + base_sep_len + number.chars().count()
        };
        let shape = Shape::Number { base, number };
        Token { shape, length, offset }
    }

    /// Construct a token representing a dangling number base.
    pub fn dangling_base(base: impl Str, offset: usize) -> Token {
        let base_str = base.into();
        let base_sep_len = lexeme::len(lexeme::literal::NUMBER_BASE_SEPARATOR);
        let length = base_str.chars().count() + base_sep_len;
        let shape = Shape::DanglingBase(base_str);
        Token { shape, length, offset }
    }

    /// Construct a token representing a line of text.
    pub fn text_line(style: TextStyle, segments: Vec<Token>, offset: usize) -> Token {
        let segments_len: usize = segments.iter().map(|s| s.source_length()).sum();
        let length = style.length() + segments_len;
        let shape = Shape::TextLine { style, segments };
        Token { shape, length, offset }
    }

    /// Construct a token representing an inline block text literal.
    pub fn text_inline_block(style: TextStyle, segments: Vec<Token>, offset: usize) -> Token {
        let segments_length: usize = segments.iter().map(|s| s.source_length()).sum();
        let length = style.length() + segments_length;
        let shape = Shape::TextInlineBlock { style, segments };
        Token { shape, length, offset }
    }

    /// Construct a token representing a block of text.
    pub fn text_block(
        start_line_ending: LineEnding,
        style: TextStyle,
        lines: Vec<Token>,
        indent: usize,
        offset: usize,
    ) -> Token {
        let length = style.length()
            + start_line_ending.size()
            + lines.iter().fold(0, |l, r| {
                l + match r.shape {
                    Shape::Line { .. } => indent + r.source_length(),
                    Shape::BlankLine(_) => r.source_length(),
                    _ => unreachable_panic!("Text blocks should only contain lines."),
                }
            });
        let shape = Shape::TextBlock { start_line_ending, style, lines };
        Token { shape, length, offset }
    }

    /// Construct a token representing an invalid quote.
    pub fn invalid_quote(bad_quotes: impl Str, offset: usize) -> Token {
        let bad_string = bad_quotes.into();
        let length = bad_string.chars().count();
        let shape = Shape::InvalidQuote(bad_string);
        Token { shape, length, offset }
    }

    /// Construct a token representing a raw text segment.
    pub fn text_segment_raw(str: impl Str, offset: usize) -> Token {
        let string = str.into();
        let length = string.chars().count();
        let shape = Shape::TextSegmentRaw(string);
        Token { shape, length, offset }
    }

    /// Construct a token representing an escape sequence.
    pub fn text_segment_escape(style: EscapeStyle, repr_str: impl Str, offset: usize) -> Token {
        let repr = repr_str.into();
        let length = style.size() + repr.chars().count();
        let shape = Shape::TextSegmentEscape { style, repr };
        Token { shape, length, offset }
    }

    /// Construct a token representing an escape sequence using a literal `shape`.
    pub fn text_segment_escape_from_shape(shape: Shape, offset: usize) -> Token {
        match &shape {
            Shape::TextSegmentEscape { style, repr } => {
                let length = style.size() + repr.chars().count();
                Token { shape, length, offset }
            }
            _ => unreachable_panic!("Shape must be a TextSegmentEscape."),
        }
    }

    /// Construct a token representing an interpolated text segment.
    pub fn text_segment_interpolate(tokens: Vec<Token>, offset: usize) -> Token {
        let length_of_interpolation_ticks = 2;
        let length =
            length_of_interpolation_ticks + tokens.iter().fold(0, |l, r| l + r.source_length());
        let shape = Shape::TextSegmentInterpolate { tokens };
        Token { shape, length, offset }
    }

    /// Construct a token representing an unclosed interpolated text segment.
    pub fn text_segment_unclosed_interpolate(tokens: Vec<Token>, offset: usize) -> Token {
        let length_of_interpolation_tick = 1;
        let length =
            length_of_interpolation_tick + tokens.iter().fold(0, |l, r| l + r.source_length());
        let shape = Shape::TextSegmentUnclosedInterpolate { tokens };
        Token { shape, length, offset }
    }

    /// Construct a token representing a line of tokens.
    pub fn line(tokens: Vec<Token>, offset: usize, trailing_line_ending: LineEnding) -> Token {
        let line_ending_len = trailing_line_ending.size();
        let length = tokens.iter().fold(line_ending_len, |l, r| l + r.source_length());
        let shape = Shape::Line { tokens, trailing_line_ending };
        Token { shape, length, offset }
    }

    /// Construct a token representing a blank line.
    ///
    /// The `offset` for blank lines is from the leftmost column, not from the parent block's
    /// indentation.
    pub fn blank_line(offset: usize, trailing_line_ending: LineEnding) -> Token {
        let length = trailing_line_ending.size();
        let shape = Shape::BlankLine(trailing_line_ending);
        Token { shape, length, offset }
    }

    /// Construct a token representing a block.
    pub fn block(block_type: BlockType, indent: usize, lines: Vec<Token>, offset: usize) -> Token {
        let length = lines
            .iter()
            .map(|line| match line.shape {
                Shape::Line { .. } => indent + line.source_length(),
                Shape::BlankLine(_) => line.source_length(),
                _ => unreachable_panic!("Tokens in a blocks should always be lines."),
            })
            .sum();
        let shape = Shape::Block { block_type, indent, lines };
        Token { shape, length, offset }
    }

    /// Construct a token representing an invalid suffix.
    pub fn invalid_suffix(text: impl Str, offset: usize) -> Token {
        let text = text.into();
        let length = text.chars().count();
        let shape = Shape::InvalidSuffix(text);
        Token { shape, length, offset }
    }

    /// Construct a token representing an unrecognised lexeme.
    pub fn unrecognized(text: impl Str, offset: usize) -> Token {
        let text = text.into();
        let length = text.chars().count();
        let shape = Shape::Unrecognized(text);
        Token { shape, length, offset }
    }

    /// Construct a token representing a disable comment.
    pub fn disable_comment(text: impl Str, offset: usize) -> Token {
        let text = text.into();
        let comment_len = lexeme::len(lexeme::literal::COMMENT);
        let length = text.chars().count() + comment_len;
        let shape = Shape::DisableComment(text);
        Token { shape, length, offset }
    }

    /// Construct a token representing a documentation comment.
    pub fn doc_comment(lines: Vec<Token>, indent: usize, offset: usize) -> Token {
        let length = lines
            .iter()
            .map(|line| match line.shape {
                Shape::Line { .. } => indent + line.source_length(),
                Shape::BlankLine(_) => line.source_length(),
                _ => unreachable_panic!("Tokens in a doc comment should always be lines."),
            })
            .sum();
        let shape = Shape::DocComment { lines, indent };
        Token { shape, length, offset }
    }
}



// =================
// === BlockType ===
// =================

/// The type for an Enso Block token.
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub enum BlockType {
    /// A block made up of arguments to a function.
    Continuous,
    /// A block made up of separate lines.
    Discontinuous,
}



// ==================
// === LineEnding ===
// ==================

/// The type of newline associated with the line.
#[derive(Copy, Clone, Debug, Display, Eq, Hash, PartialEq)]
pub enum LineEnding {
    /// There is no newline.
    None,
    /// The unix-style line-feed (`'\n'`),
    LF,
    /// The windows-style carriage-return, line-feed (`"\r\n"`).
    CRLF,
}

impl LineEnding {
    const NO_LENGTH: usize = 0;

    /// Get the number of rust `char`s that the newline type takes up.
    pub fn size(self) -> usize {
        match self {
            Self::None => Self::NO_LENGTH,
            Self::LF => lexeme::len(lexeme::literal::LF),
            Self::CRLF => lexeme::len(lexeme::literal::CRLF),
        }
    }
}


// === Trait Impls ===

impl Default for LineEnding {
    fn default() -> Self {
        LineEnding::None
    }
}



// =================
// === TextStyle ===
// =================

/// The style of the text literal.
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub enum TextStyle {
    // === Line ===
    /// A interpolated text line literal.
    FormatLine,
    /// A raw text line literal.
    RawLine,
    /// An unclosed text line literal.
    UnclosedLine,

    // === Inline Block ===
    /// A format inline block text literal.
    FormatInlineBlock,
    /// A raw inline block text literal.
    RawInlineBlock,

    // === Block ===
    /// An interpolated text block literal.
    FormatBlock,
    /// A raw text block literal.
    RawBlock,
}

impl TextStyle {
    /// Calculate the length of the delimiters for a particular style of text literal.
    pub fn length(self) -> usize {
        match self {
            TextStyle::FormatLine => lexeme::len(lexeme::literal::FORMAT_QUOTE) * 2,
            TextStyle::RawLine => lexeme::len(lexeme::literal::RAW_QUOTE) * 2,
            TextStyle::FormatInlineBlock => lexeme::len(lexeme::literal::FORMAT_BLOCK_QUOTE),
            TextStyle::RawInlineBlock => lexeme::len(lexeme::literal::RAW_BLOCK_QUOTE),
            TextStyle::UnclosedLine => lexeme::len(lexeme::literal::FORMAT_QUOTE),
            TextStyle::FormatBlock => lexeme::len(lexeme::literal::FORMAT_BLOCK_QUOTE),
            TextStyle::RawBlock => lexeme::len(lexeme::literal::RAW_BLOCK_QUOTE),
        }
    }

    /// Check if the text literal is a line literal.
    pub fn is_line_literal(self) -> bool {
        matches!(self, TextStyle::RawLine | TextStyle::FormatLine | TextStyle::UnclosedLine)
    }

    /// Check if the text literal is an inline block literal.
    pub fn is_inline_block_literal(self) -> bool {
        matches!(self, TextStyle::FormatInlineBlock | TextStyle::RawInlineBlock)
    }

    /// Check if the text literal is a block literal.
    pub fn is_block_literal(self) -> bool {
        matches!(self, TextStyle::FormatBlock | TextStyle::RawBlock)
    }
}



// ===================
// === EscapeStyle ===
// ===================

/// A description of the style of escape sequence seen.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum EscapeStyle {
    /// A \xNN-style byte escape.
    Byte,
    /// Unicode 16-bit escape sequence.
    U16,
    /// Unicode 21-bit escape sequence.
    U21,
    /// Unicode 32-bit escape sequence.
    U32,
    /// A literal escape character.
    Literal,
    /// An invalid unicode escape.
    InvalidUnicode,
    /// An invalid escape.
    Invalid,
    /// An escape slash without any following escape.
    Unfinished,
}
impl EscapeStyle {
    const NO_ADDITIONAL_LENGTH: usize = 0;

    /// Get the length taken up in source by the delimiters to an escape type.
    pub fn size(self) -> usize {
        match self {
            EscapeStyle::Byte => lexeme::len(lexeme::literal::BYTE_ESCAPE_START),
            EscapeStyle::Literal => lexeme::len(lexeme::literal::SLASH),
            EscapeStyle::U16 => lexeme::len(lexeme::literal::U16_ESCAPE_START),
            EscapeStyle::U32 => lexeme::len(lexeme::literal::U32_ESCAPE_START),
            EscapeStyle::U21 => {
                let start_len = lexeme::len(lexeme::literal::U21_ESCAPE_START);
                let end_len = lexeme::len(lexeme::literal::U21_ESCAPE_END);
                start_len + end_len
            }
            _ => Self::NO_ADDITIONAL_LENGTH,
        }
    }
}



// =============
// === Shape ===
// =============

/// The shapes of tokens needed by the Enso lexer.
///
/// This is a very small set of shapes, because the [`Token`] type only deals with the tokens that
/// the lexer works with, not the full complexity of Enso's syntax.
#[allow(missing_docs)]
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Shape {
    // === Identifiers ===
    /// An identifier in referent form.
    Referent(String),
    /// An identifier in variable form.
    Variable(String),
    /// An identifier not conforming to the Enso identifier rules (e.g. a Java identifier).
    External(String),
    /// A blank identifier (`_`).
    Blank,
    /// An operator identifier.
    Operator(String),
    /// A modifier identifier.
    Modifier(String),
    /// An annotation.
    Annotation(String),

    // === Literals ===
    /// A literal number.
    Number {
        /// The (optional) base for the number to be interpreted in.
        base:   String,
        /// The number itself, possibly with a decimal point.
        number: String,
    },
    /// A dangling base from a number literal.
    DanglingBase(String),
    /// A text line literal.
    TextLine {
        /// The type of literal being encoded.
        style:    TextStyle,
        /// The segments that make up the line of text.
        segments: Vec<Token>,
    },
    /// An inline block text literal.
    TextInlineBlock {
        /// The type of literal being encoded.
        style:    TextStyle,
        /// The segments that make up the line of text.
        segments: Vec<Token>,
    },
    /// A text block literal.
    TextBlock {
        /// The line ending that occurs directly after the opening quote marks.
        start_line_ending: LineEnding,
        /// The type of literal being encoded.
        style:             TextStyle,
        /// The lines in the text block literal.
        lines:             Vec<Token>,
    },
    /// An invalid quote for a text literal.
    InvalidQuote(String),
    /// A segment of a line of text containing only literal text.
    TextSegmentRaw(String),
    /// A segment of a line of text that represents an escape sequence.
    TextSegmentEscape {
        /// The type of escape being represented.
        style: EscapeStyle,
        /// The literal escape sequence.
        repr:  String,
    },
    /// A segment of a line of text that contains an interpolated expression.
    TextSegmentInterpolate {
        /// The tokens making up the interpolated expression.
        tokens: Vec<Token>,
    },
    /// An interpolated expression that hasn't been closed.
    TextSegmentUnclosedInterpolate {
        /// The tokens making up the interpolated expression.
        tokens: Vec<Token>,
    },
    /// An invalid text segment (e.g. unclosed interpolate segment).
    TextSegmentInvalid(String),

    // === Lines ===
    /// A line containing tokens.
    ///
    /// The offset for a line is always zero, as it is contained in a block with a defined
    /// indentation.
    Line {
        /// The tokens on the line.
        tokens:               Vec<Token>,
        /// The line ending that _ends_ the line.
        ///
        /// Please note that the concept of 'ending' the line is a bit strange, as blocks are
        /// treated as tokens in their own right, and hence are included in lines.
        trailing_line_ending: LineEnding,
    },
    /// A blank line.
    ///
    /// The offset for a blank line is from the leftmost column, as it may be negative from the
    /// block's indentation level.
    BlankLine(LineEnding),

    // === Block ===
    /// A block of tokens.
    Block {
        /// The type of the block.
        block_type: BlockType,
        /// The leading indentation of the block.
        indent:     usize,
        /// The lines in the block.
        lines:      Vec<Token>,
    },

    // === Errors ===
    /// An invalid suffix.
    InvalidSuffix(String),
    /// An unrecognized token.
    Unrecognized(String),

    // === Comments ===
    /// A disable comment (`# ...`).
    DisableComment(String),
    /// An Enso documentation comment (`## ...`).
    DocComment {
        /// The lines in the doc comment body. Each line must contain raw text segments only.
        lines:  Vec<Token>,
        /// The indentation of the doc comment's body from the baseline.
        indent: usize,
    },
}

impl Shape {
    /// Construct an identifier in referent form.
    pub fn referent(name: impl Into<String>) -> Shape {
        Shape::Referent(name.into())
    }

    /// Construct an identifier in variable form.
    pub fn variable(name: impl Into<String>) -> Shape {
        Shape::Variable(name.into())
    }

    /// Construct an identifier in external form.
    pub fn external(name: impl Into<String>) -> Shape {
        Shape::External(name.into())
    }

    /// Construct a blank identifier.
    ///
    /// This is provided as a function for completeness.
    pub fn blank() -> Shape {
        Shape::Blank
    }

    /// Construct an operator identifier.
    pub fn operator(opr: impl Into<String>) -> Shape {
        Shape::Operator(opr.into())
    }

    /// Construct a modifier identifier.
    pub fn modifier(opr: impl Into<String>) -> Shape {
        Shape::Modifier(opr.into())
    }

    /// Construct an annotation identifier.
    pub fn annotation(name: impl Into<String>) -> Shape {
        Shape::Annotation(name.into())
    }

    /// Construct a number literal.
    pub fn number(base: impl Into<String>, num: impl Into<String>) -> Shape {
        let base = base.into();
        let number = num.into();
        Shape::Number { base, number }
    }

    /// Construct a dangling base literal.
    pub fn dangling_base(base: impl Into<String>) -> Shape {
        Shape::DanglingBase(base.into())
    }

    /// Construct a text line literal.
    pub fn text_line(style: TextStyle, segments: Vec<Token>) -> Shape {
        Shape::TextLine { style, segments }
    }

    /// Construct an inline block text literal.
    pub fn text_inline_block(style: TextStyle, segments: Vec<Token>) -> Shape {
        Shape::TextInlineBlock { style, segments }
    }

    /// Construct a text block literal.
    pub fn text_block(start_line_ending: LineEnding, style: TextStyle, lines: Vec<Token>) -> Shape {
        Shape::TextBlock { start_line_ending, style, lines }
    }

    /// Construct an invalid quote literal.
    pub fn invalid_quote(bad_quotes: impl Str) -> Shape {
        Shape::InvalidQuote(bad_quotes.into())
    }

    /// Construct a raw text segment.
    pub fn text_segment_raw(text: impl Str) -> Shape {
        Shape::TextSegmentRaw(text.into())
    }

    /// Construct a text segment containing an escape sequence.
    pub fn text_segment_escape(style: EscapeStyle, repr_str: impl Str) -> Shape {
        let repr = repr_str.into();
        Shape::TextSegmentEscape { style, repr }
    }

    /// Construct a text segment containing an interpolated expression.
    pub fn text_segment_interpolate(tokens: Vec<Token>) -> Shape {
        Shape::TextSegmentInterpolate { tokens }
    }

    /// Construct a text segment containing an unclosed interpolated expression.
    pub fn text_segment_unclosed_interpolate(tokens: Vec<Token>) -> Shape {
        Shape::TextSegmentUnclosedInterpolate { tokens }
    }

    /// Construct an invalid text segment.
    pub fn text_segment_invalid(str: impl Str) -> Shape {
        Shape::TextSegmentInvalid(str.into())
    }

    /// Construct a line that contains tokens.
    pub fn line(tokens: Vec<Token>, trailing_line_ending: LineEnding) -> Shape {
        Shape::Line { tokens, trailing_line_ending }
    }

    /// Construct a line that is blank.
    pub fn blank_line(trailing_line_ending: LineEnding) -> Shape {
        Shape::BlankLine(trailing_line_ending)
    }

    /// Construct a block containing lines.
    pub fn block(block_type: BlockType, indent: usize, lines: Vec<Token>) -> Shape {
        Shape::Block { block_type, indent, lines }
    }

    /// Construct an invalid suffix.
    pub fn invalid_suffix(text: impl Into<String>) -> Shape {
        Shape::InvalidSuffix(text.into())
    }

    /// Construct an unrecognised token.
    pub fn unrecognized(text: impl Into<String>) -> Shape {
        Shape::Unrecognized(text.into())
    }

    /// Construct a disable comment shape.
    pub fn disable_comment(text: impl Str) -> Shape {
        Shape::DisableComment(text.into())
    }

    /// Construct a doc comment shape.
    pub fn doc_comment(lines: Vec<Token>, indent: usize) -> Shape {
        Shape::DocComment { lines, indent }
    }
}



// ==============
// === Stream ===
// ==============

/// A representation of the Enso token stream.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct Stream {
    /// The tokens in the token stream.
    tokens: Vec<Token>,
}

impl Stream {
    /// Append the provided `token` to the token stream.
    pub fn append(&mut self, token: Token) {
        self.tokens.push(token)
    }

    /// Get a reference to the tokens in the stream.
    pub fn tokens(&self) -> &Vec<Token> {
        &self.tokens
    }

    /// Get the length of the elements in the token stream.
    pub fn tokens_len(&self) -> usize {
        self.tokens.iter().map(|token| token.length + token.offset).sum()
    }
}

/// Get a consuming iterator over the token stream.
impl std::iter::IntoIterator for Stream {
    type Item = Token;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.tokens.into_iter()
    }
}

impl Deref for Stream {
    type Target = Vec<Token>;

    fn deref(&self) -> &Self::Target {
        &self.tokens
    }
}

impl DerefMut for Stream {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.tokens
    }
}


// === Trait Impls ===

impl From<Vec<Token>> for Stream {
    fn from(tokens: Vec<Token>) -> Self {
        Stream { tokens }
    }
}

impl From<Stream> for Vec<Token> {
    fn from(stream: Stream) -> Self {
        stream.tokens
    }
}
