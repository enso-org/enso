//! Enso documentation parser.

#![recursion_limit = "256"]
// === Features ===
#![feature(assert_matches)]
#![feature(let_chains)]
#![feature(if_let_guard)]
#![feature(local_key_cell_methods)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![allow(clippy::option_map_unit_fn)]
#![allow(clippy::precedence)]
#![allow(dead_code)]
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]

use enso_prelude::*;


// ==============
// === Export ===
// ==============

pub mod doc_sections;

pub use doc_sections::parse;
pub use doc_sections::Argument;
pub use doc_sections::DocSection;



pub(crate) use enso_profiler as profiler;
pub(crate) use enso_profiler::profile;



// ============
// === Tags ===
// ============

/// A [`Tag`], optionally with a following description.
#[derive(Debug)]
pub struct TagWithDescription<'a, L> {
    name:        Tag,
    description: Option<Span<'a, L>>,
}

/// Indicator placed at the beginning of a documentation section, e.g. `PRIVATE`.
#[derive(Hash, Debug, Clone, Copy, PartialEq, Eq, serde::Serialize)]
#[allow(missing_docs)]
pub enum Tag {
    Added,
    Advanced,
    Alias,
    Deprecated,
    Icon,
    Group,
    Modified,
    Private,
    Removed,
    TextOnly,
    Unstable,
    Upcoming,
}


// === Lexing ===

impl<'a, L: Location> TagWithDescription<'a, L> {
    /// Try to lex the given text as a [`TagWithDescription`].
    pub fn new(text: Span<'a, L>) -> Option<Self> {
        let (tag, description) = text.first_word_and_rest();
        Tag::new(tag.text).map(|name| TagWithDescription { name, description })
    }
}

impl Tag {
    /// Try to lex the given text as a [`Tag`].
    pub fn new(text: &str) -> Option<Self> {
        use Tag::*;
        match text {
            "ADDED" => Some(Added),
            "ADVANCED" => Some(Advanced),
            "ALIAS" => Some(Alias),
            "DEPRECATED" => Some(Deprecated),
            "ICON" => Some(Icon),
            "GROUP" => Some(Group),
            "MODIFIED" => Some(Modified),
            "PRIVATE" => Some(Private),
            "REMOVED" => Some(Removed),
            "TEXT_ONLY" => Some(TextOnly),
            "UNSTABLE" => Some(Unstable),
            "UPCOMING" => Some(Upcoming),
            _ => None,
        }
    }

    /// Return a string that lexes as the given tag.
    pub fn to_str(&self) -> &'static str {
        match self {
            Tag::Added => "ADDED",
            Tag::Advanced => "ADVANCED",
            Tag::Alias => "ALIAS",
            Tag::Deprecated => "DEPRECATED",
            Tag::Icon => "ICON",
            Tag::Group => "GROUP",
            Tag::Modified => "MODIFIED",
            Tag::Private => "PRIVATE",
            Tag::Removed => "REMOVED",
            Tag::TextOnly => "TEXT_ONLY",
            Tag::Unstable => "UNSTABLE",
            Tag::Upcoming => "UPCOMING",
        }
    }
}



// =============
// === Marks ===
// =============

/// Header for a section introduced by a marker-character and following text.
#[derive(Debug)]
pub struct Marked<'a, L> {
    mark:   Mark,
    header: Option<Span<'a, L>>,
}

/// Documentation section mark.
#[derive(Hash, Debug, Copy, Clone, PartialEq, Eq, serde::Serialize)]
#[allow(missing_docs)]
pub enum Mark {
    Important,
    Info,
    Example,
}


// === Lexing ===

impl Mark {
    /// Try to lex the given text as a header-introducing marker character.
    pub fn new(text: &str) -> Option<Self> {
        match text {
            "!" => Some(Mark::Important),
            "?" => Some(Mark::Info),
            ">" => Some(Mark::Example),
            _ => None,
        }
    }
}

impl<'a, L: Location> Marked<'a, L> {
    /// Try to lex the given text as a marked-section header.
    pub fn new(text: Span<'a, L>) -> Option<Self> {
        let (first_word, header) = text.first_word_and_rest();
        Mark::new(first_word.text).map(|mark| Marked { mark, header })
    }
}



// =============
// === Lines ===
// =============

/// A line of code, separated into indent and content.
#[derive(Debug)]
pub struct Line<'a, L> {
    indent:  Offset,
    content: Span<'a, L>,
}


// === Offset and Visible Offset ===

/// A length of text, representing a number of spaces.
#[derive(Copy, Clone, Debug, Default)]
pub struct Offset {
    bytes:   usize,
    visible: VisibleOffset,
}

/// A number of spaces.
#[derive(Copy, Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub struct VisibleOffset(usize);

impl Add<Self> for VisibleOffset {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0 + rhs.0)
    }
}

impl From<Offset> for VisibleOffset {
    fn from(value: Offset) -> Self {
        value.visible
    }
}



// =============
// === Spans ===
// =============

/// A slice of text, with its location in the input.
#[derive(Copy, Clone, Debug, Default)]
pub struct Span<'a, L> {
    /// The location of the start of the text.
    pub location: L,
    /// The text.
    pub text:     &'a str,
}

impl<'a, L: Location> Span<'a, L> {
    /// Remove all leading whitespace characters; return the initial offset and following content,
    /// unless the line has no non-whitespace content.
    pub fn trim_start(self) -> Option<Line<'a, L>> {
        let mut indent = Offset::default();
        for b in self.text.bytes() {
            match b {
                b' ' => indent.visible.0 += 1,
                b'\t' => {
                    self.warn("Tab character used for indentation.");
                    indent.visible.0 += 4
                }
                _ => {
                    let (whitespace, text) = self.text.split_at(indent.bytes);
                    let location = self.location.offset_text(whitespace);
                    let content = Span { location, text };
                    return Some(Line { indent, content });
                }
            }
            indent.bytes += 1;
        }
        None
    }

    /// Remove leading whitespace characters corresponding to the specified visible offset; return
    /// the following content.
    pub fn trim_start_exact(self, limit: VisibleOffset) -> Self {
        let mut indent = Offset::default();
        let mut bytes = self.text.bytes();
        while indent.visible < limit {
            match bytes.next() {
                Some(b' ') => {
                    indent.visible.0 += 1;
                }
                Some(b'\t') => {
                    self.warn("Tab character used for indentation.");
                    indent.visible.0 += 4;
                }
                Some(_) => break,
                None => {
                    let unexpected_condition = "Internal error: Expected greater indent level.";
                    self.warn(unexpected_condition);
                    warn!("{unexpected_condition}");
                    break;
                }
            }
            indent.bytes += 1;
        }
        let (whitespace, text) = self.text.split_at(indent.bytes);
        let location = self.location.offset_text(whitespace);
        Self { location, text }
    }

    /// Return the first word (which may be empty), and the rest of the input, if any.
    pub fn first_word_and_rest(self) -> (Self, Option<Self>) {
        match self.text.split_once(' ') {
            Some((first, rest)) => (
                Self { location: self.location, text: first },
                Some(Self { location: self.location.offset_text(first).offset(1), text: rest }),
            ),
            None => (self, None),
        }
    }

    /// Returns a [`Span`] with the given prefix removed, if it was present.
    pub fn strip_prefix(self, prefix: &str) -> Option<Self> {
        self.text
            .strip_prefix(prefix)
            .map(|text| Self { location: self.location.offset_text(prefix), text })
    }

    /// Returns a [`Span`] with the given suffix removed, if it was present.
    pub fn strip_suffix(self, suffix: char) -> Option<Self> {
        self.text.strip_suffix(suffix).map(|text| Self { location: self.location, text })
    }

    /// Return a 0-length span representing the point immediately after this span.
    pub fn after(self) -> Self {
        Self { location: self.location.offset_text(self.text), text: "" }
    }

    /// Emit a warning for this location, if warnings are enabled.
    pub fn warn(self, _warning: impl Warning) {
        // TODO: self.location.warn(warning)
    }

    /// Split at the given index. Panics if the index is not a character boundary.
    pub fn split_at(self, i: usize) -> (Self, Self) {
        let (a, b) = self.text.split_at(i);
        let a_ = Self { location: self.location, text: a };
        let b_ = Self { location: self.location.offset_text(a), text: b };
        (a_, b_)
    }
}

impl<'a, L> AsRef<str> for Span<'a, L> {
    fn as_ref(&self) -> &str {
        self.text
    }
}

impl<'a, L> From<Span<'a, L>> for String {
    fn from(value: Span<'a, L>) -> Self {
        value.text.to_string()
    }
}

impl<'a, L> Display for Span<'a, L> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.text)
    }
}


// === Location ===

/// An offset within source code, expressed as line number and character index within the line.
pub trait Location: Copy + Debug {
    /// The first character of the specified line.
    fn start_of_line(line_number: usize) -> Self;
    /// Return the location, advanced within the line by the given text.
    fn offset_text(self, text: &str) -> Self;
    /// Return the location, advanced within the line by the specified number of characters.
    fn offset(self, chars: usize) -> Self;
}


// === Ignored Location ===

/// [`Location`] type that doesn't track locations, for high-performance parsing when
/// accurately-located warnings are not needed.
#[derive(Copy, Clone, Debug, Default)]
pub struct IgnoredLocation;

impl Location for IgnoredLocation {
    fn start_of_line(_line_number: usize) -> Self {
        Self
    }
    fn offset_text(self, _text: &str) -> Self {
        Self
    }
    fn offset(self, _chars: usize) -> Self {
        Self
    }
}



// ===========
// === Lex ===
// ===========

/// Breaks input state into tokens to be fed to a [`TokenConsumer`].
#[derive(Default, Debug)]
pub struct Lexer {
    scopes: Scopes,
    state:  State,
}

/// Lexer state.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
enum State {
    /// No non-tag line has been read.
    #[default]
    Tags,
    /// Within an example's description.
    ExampleDescription,
    /// Expecting an example's code block to start.
    ExampleExpectingCode { within_indent: VisibleOffset },
    /// Within an example's code block.
    ExampleCode,
    /// Not in any special context.
    Normal,
}


impl Lexer {
    /// Lex a line and feed it to the given [`TokenConsumer`].
    pub fn line<L: Location>(&mut self, raw: Span<'_, L>, docs: &mut impl TokenConsumer<L>) {
        let line = raw.trim_start();
        match (self.state, line) {
            (State::Tags, Some(line)) if let Some(tag) = TagWithDescription::new(line.content) => {
                // TODO: WARN IF: indent != min.
                docs.tag(tag.name, tag.description);
            }
            (State::Tags, Some(line)) => {
                self.state = State::Normal;
                self.normal_line(line, docs)
            }
            (State::Tags, None) =>
                raw.warn("Unneeded empty line before content or between tags."),
            (State::ExampleDescription, None) => {
                self.scopes.end_all().for_each(|scope| docs.end(scope));
                // TODO: within_indent
                self.state = State::ExampleExpectingCode { within_indent: VisibleOffset(0) };
            }
            (State::ExampleDescription, Some(line)) => self.normal_line(line, docs),
            (State::ExampleExpectingCode { .. }, None) =>
                raw.warn("Extra empty line before example code."),
            (State::ExampleExpectingCode { within_indent }, Some(line))
                    if line.indent.visible <= within_indent => {
                line.content.warn("No code found in example section.");
                self.state = State::Normal;
                self.normal_line(line, docs)
            }
            (State::ExampleExpectingCode { .. }, Some(line)) => {
                self.state = State::ExampleCode;
                self.scopes.start_raw(line.indent);
                docs.start_raw();
                docs.raw_line(line.content);
            }
            (State::ExampleCode, None) if let Some(_) = self.scopes.raw() => {
                docs.raw_line(raw.after());
            },
            (State::ExampleCode, None) => (),
            (State::ExampleCode, Some(line)) => {
                self.scopes.end_below(line.indent).for_each(|scope| docs.end(scope));
                if let Some(indent) = self.scopes.raw() {
                    docs.raw_line(raw.trim_start_exact(indent));
                } else {
                    self.state = State::Normal;
                    self.normal_line(line, docs)
                }
            }
            (State::Normal, Some(line)) => self.normal_line(line, docs),
            (State::Normal, None) => {
                self.scopes.end_all().for_each(|scope| docs.end(scope));
            }
        }
    }

    /// Complete the current input, closing any open scopes for the given [`TokenConsumer`] and
    /// resetting to the default state.
    pub fn finish<L>(&mut self, docs: &mut impl TokenConsumer<L>) {
        self.scopes.end_all().for_each(|scope| docs.end(scope));
        let scopes = mem::take(&mut self.scopes);
        *self = Self {
            // Reuse buffers.
            scopes,
            // Reset state.
            state: Default::default(),
        };
    }
}

impl Lexer {
    fn normal_line<L: Location>(&mut self, line: Line<'_, L>, docs: &mut impl TokenConsumer<L>) {
        let Line { indent, content } = line;
        match content {
            _ if let Some(marked) = Marked::new(content) => {
                self.scopes.end_all().for_each(|scope| docs.end(scope));
                docs.enter_marked_section(marked.mark, marked.header);
                if marked.mark == Mark::Example {
                    self.state = State::ExampleDescription;
                }
            },
            t if let Some(t) = t.strip_suffix(':') => {
                self.scopes.end_all().for_each(|scope| docs.end(scope));
                docs.enter_keyed_section(t);
            },
            t if let Some(content) = t.strip_prefix("- ") => {
                self.scopes.end_below(indent).for_each(|scope| docs.end(scope));
                if self.scopes.start_list_if_not_started(indent) {
                    docs.start_list();
                }
                self.scopes.start_list_item(indent);
                docs.start_list_item();
                self.text(content, docs);
            }
            _ => {
                self.scopes.end_below(indent).for_each(|scope| docs.end(scope));
                if self.scopes.is_in_text() {
                    docs.whitespace();
                } else {
                    self.scopes.start_paragraph(indent);
                    docs.start_paragraph();
                }
                self.text(content, docs);
            },
        }
    }

    fn text<L: Location>(&mut self, text: Span<L>, docs: &mut impl TokenConsumer<L>) {
        let mut quote_open = None;
        let mut i = 0;
        let mut remaining = text;
        for c in text.text.bytes() {
            match c {
                b'`' => {
                    let (before_quote, including_quote) = remaining.split_at(i);
                    let (quote, next_remaining) = including_quote.split_at(1);
                    remaining = next_remaining;
                    i = 0;
                    docs.text(before_quote);
                    if quote_open.is_some() {
                        docs.end_quote();
                        quote_open = None;
                    } else {
                        docs.start_quote();
                        quote_open = Some(quote);
                    }
                }
                _ => i += 1,
            }
        }
        docs.text(remaining);
        if let Some(quote) = quote_open {
            quote.warn("Unclosed quote.");
            docs.end_quote();
        }
    }
}


// === Scopes ===

/// A [`Lexer`] scope.
#[derive(Debug)]
struct Scope {
    r#type: ScopeType,
    indent: VisibleOffset,
}

/// A [`Lexer`] scope type.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ScopeType {
    /// An unordered list.
    List,
    /// An unordered-list item.
    ListItem,
    /// A paragraph of text.
    Paragraph,
    /// A block of preformatted text. No further scopes will occur inside.
    Raw,
}

/// A stack of [`Lexer`] scopes.
#[derive(Default, Debug)]
struct Scopes {
    scopes: Vec<Scope>,
}

impl Scopes {
    fn end_all(&mut self) -> impl Iterator<Item = ScopeType> {
        self.end_including(VisibleOffset(0))
    }

    fn end_below(&mut self, indent: impl Into<VisibleOffset>) -> impl Iterator<Item = ScopeType> {
        let indent = indent.into();
        self.end_including(indent + VisibleOffset(1))
    }

    fn end_including(
        &mut self,
        indent: impl Into<VisibleOffset>,
    ) -> impl Iterator<Item = ScopeType> {
        let indent = indent.into();
        // FIXME: Don't allocate.
        let mut scopes = vec![];
        while let Some(scope) = self.scopes.pop_if(|scope| scope.indent >= indent) {
            scopes.push(scope.r#type);
        }
        scopes.into_iter()
    }

    fn start_list_if_not_started(&mut self, indent: impl Into<VisibleOffset>) -> bool {
        let indent = indent.into();
        match self.scopes.last() {
            Some(Scope { r#type: ScopeType::List, .. }) => false,
            _ => {
                self.scopes.push(Scope { r#type: ScopeType::List, indent });
                true
            }
        }
    }

    fn start_list_item(&mut self, indent: impl Into<VisibleOffset>) {
        let indent = indent.into();
        let min_child_indent = indent + VisibleOffset(1);
        self.scopes.push(Scope { r#type: ScopeType::ListItem, indent: min_child_indent });
    }

    fn is_in_text(&self) -> bool {
        let current = self.scopes.last().map(|scope| scope.r#type);
        matches!(current, Some(ScopeType::ListItem | ScopeType::Paragraph))
    }

    fn start_paragraph(&mut self, indent: impl Into<VisibleOffset>) {
        let indent = indent.into();
        self.scopes.push(Scope { r#type: ScopeType::Paragraph, indent });
    }

    fn start_raw(&mut self, indent: impl Into<VisibleOffset>) {
        let indent = indent.into();
        self.scopes.push(Scope { r#type: ScopeType::Raw, indent });
    }

    fn raw(&self) -> Option<VisibleOffset> {
        match self.scopes.last() {
            Some(Scope { r#type: ScopeType::Raw, indent }) => Some(*indent),
            _ => None,
        }
    }
}


// === Warnings ===

/// A message identifying improperly-formatted documentation.
///
/// Note that implementors of this trait should not require any work to be done (e.g. string
/// formatting or other allocations) until `to_string` is called, to prevent overhead when parsing
/// the standard library (in which case warnings are normally not used anyway).
pub trait Warning {
    /// Produce the message.
    fn to_string(&self) -> String;
}

impl<'a> Warning for &'a str {
    fn to_string(&self) -> String {
        str::to_string(self)
    }
}

impl<T> Warning for T
where T: Fn() -> String
{
    fn to_string(&self) -> String {
        (self)()
    }
}



// ======================
// === Token Consumer ===
// ======================

/// Receives a stream of tokens.
pub trait TokenConsumer<L> {
    /// A tag.
    fn tag(&mut self, tag: Tag, description: Option<Span<'_, L>>);
    /// Header for a section introduced by a mark character.
    fn enter_marked_section(&mut self, mark: Mark, header: Option<Span<'_, L>>);
    /// Header for a section introduced by text ending in a colon.
    fn enter_keyed_section(&mut self, header: Span<'_, L>);
    /// Plain text.
    fn text(&mut self, text: Span<'_, L>);
    /// Start an unordered-list.
    fn start_list(&mut self);
    /// Start an unordered-list item.
    fn start_list_item(&mut self);
    /// Start a paragraph.
    fn start_paragraph(&mut self);
    /// Start a preformatted-text section.
    fn start_raw(&mut self);
    /// An opening-quote.
    fn start_quote(&mut self);
    /// A closing-quote.
    fn end_quote(&mut self);
    /// Space between two [`text`] tokens.
    fn whitespace(&mut self);
    /// A line of preformatted text. No newline character is included.
    fn raw_line(&mut self, text: Span<'_, L>);
    /// Close a scope, of specified type. As scopes form a stack, the scope closed will always be
    /// the most recently-opened scope that has not been closed.
    fn end(&mut self, scope: ScopeType);
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_list_parsing() {
        use crate::doc_sections::Argument;
        use crate::DocSection::*;
        use crate::Mark::*;
        use crate::Tag::*;

        let docs = r#"
        ALIAS From Text

        Parses a textual representation of an integer into an integer number, returning
        a `Number_Parse_Error` if the text does not represent a valid integer.

        Arguments:
        - text: The text to parse into a integer.
        - radix: The number base to use for parsing (defaults to 10). `radix`
            must be between 2 and 36 (inclusive)
        - arg argument without colon
        - argument_without_description

        - List item 1
        - List item 2
        - List item 3

        > Example
            Parse the text "20220216" into an integer number.

                Integer.parse "20220216""#;
        let res = parse(docs);
        let expected = [
            Tag { tag: Alias, body: "From Text".into() }, 
            Paragraph { body: "Parses a textual representation of an integer into an integer number, \
                returning a <code>Number_Parse_Error</code> if the text does not represent a valid integer.".into() },
            Keyed { key: "Arguments".into(), body: "".into() },
            Arguments { args: [
                Argument {
                    name: "text".into(),
                    description: "The text to parse into a integer.".into() },
                Argument {
                    name: "radix".into(),
                    description: "The number base to use for parsing (defaults to 10). <code>radix</code> \
                    must be between 2 and 36 (inclusive)".into()
                },
                Argument {
                    name: "arg".into(),
                    description: "argument without colon".into()
                },
                Argument {
                    name: "argument_without_description".into(),
                    description: default(),
                }].to_vec()
            },
            List { items: ["List item 1".into(), "List item 2".into(), "List item 3".into()].to_vec() },
            Marked {
                mark: Example,
                header: Some("Example".into()), 
                body: "<p>Parse the text \"20220216\" into an integer number.<div class=\"example\">\nInteger.parse \"20220216\"</div>".into()
            }].to_vec();
        assert_eq!(res, expected);
    }
}
