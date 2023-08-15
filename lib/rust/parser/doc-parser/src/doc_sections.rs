//! Parses documentation text to the [`DocSection`] representation.

use crate::*;



// ==============================
// === High-level Parsing API ===
// ==============================

/// Parse the given documentation text to a collection of [`DocSection`]s.
pub fn parse(docs: &str) -> Vec<DocSection> {
    // Although this is semantically a pure function, for efficiency we use one persistent parser
    // to reuse its buffers.
    thread_local! {
        static PARSER: RefCell<DocParser> = Default::default();
    }
    PARSER.with_borrow_mut(|parser| parser.parse(docs))
}



// ============================
// === Documentation Parser ===
// ============================

/// Parses documentation text to the [`DocSection`] representation.
///
/// Note that this object is semantically stateless, but reusing it allows better performance by
/// avoiding the need to reallocate its working buffers.
#[derive(Default, Debug)]
pub struct DocParser {
    docs:  DocSectionCollector,
    lexer: Lexer,
}

impl DocParser {
    /// Create a new [`DocParser`].
    pub fn new() -> Self {
        Self::default()
    }

    /// Parse the documentation.
    #[profile(Detail)]
    pub fn parse(&mut self, input: &str) -> Vec<DocSection> {
        for (line_number, line) in input.trim_start().lines().enumerate() {
            let location = Location::start_of_line(line_number);
            let line = Span { location, text: line };
            self.lexer.line::<IgnoredLocation>(line, &mut self.docs);
        }
        self.lexer.finish::<IgnoredLocation>(&mut self.docs);
        self.docs.finish()
    }
}



// ===================
// === Doc Section ===
// ===================

/// Text rendered as HTML (may contain HTML tags).
pub type HtmlString = String;

/// A description of a single argument in the documentation. The name is delimited from the
/// description using a colon.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Argument {
    /// Name of the argument.
    pub name:        String,
    /// Description of the argument.
    pub description: HtmlString,
}

impl Argument {
    /// Convert the given string to the argument description.
    pub fn new(text: &str) -> Self {
        // We split by the first colon or space, whatever comes first.
        // Typically a colon must be used as a separator, but in some documentation snippets we
        // have there is no colon and the name of the argument is simply the first word.
        let split = text.splitn(2, |c| c == ':' || c == ' ');
        let (name, description) = split.collect_tuple().unwrap_or((text, ""));
        let name = name.trim().to_string();
        let description = description.trim().to_string();
        Self { name, description }
    }
}

/// A single section of the documentation.
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
#[allow(missing_docs)]
pub enum DocSection {
    /// The documentation tag.
    Tag {
        /// The tag name.
        tag:  Tag,
        /// The tag text.
        body: HtmlString,
    },
    /// The paragraph of the text.
    Paragraph {
        /// The elements that make up this paragraph.
        body: HtmlString,
    },
    /// A list of items. Each item starts with a dash (`-`).
    List { items: Vec<HtmlString> },
    /// A list of items, but each item is an [`Argument`]. Starts with `Arguments:` keyword.
    Arguments { args: Vec<Argument> },
    /// The section that starts with the key followed by the colon and the body.
    Keyed {
        /// The section key.
        key:  String,
        /// The elements that make up the body of the section.
        body: HtmlString,
    },
    /// The section that starts with the mark followed by the header and the body.
    Marked {
        /// The section mark.
        mark:   Mark,
        /// The section header.
        header: Option<String>,
        /// The elements that make up the body of the section.
        body:   HtmlString,
    },
}



// ============================
// === DocSection Collector ===
// ============================

#[derive(Default, Debug)]
struct DocSectionCollector {
    sections:             Vec<DocSection>,
    in_secondary_section: bool,
    inside_arguments:     bool,
    current_body:         String,
    current_list:         Vec<String>,
}

impl DocSectionCollector {
    fn finish_section(&mut self) {
        let text = self.current_body.clone();
        self.current_body.clear();
        self.in_secondary_section = true;
        match self.sections.last_mut() {
            Some(DocSection::Paragraph { body, .. })
            | Some(DocSection::Keyed { body, .. })
            | Some(DocSection::Marked { body, .. }) => *body = text,
            Some(DocSection::List { .. }) | Some(DocSection::Arguments { .. }) => (),
            Some(DocSection::Tag { .. }) | None =>
                self.sections.push(DocSection::Paragraph { body: text }),
        }
    }

    fn finish(&mut self) -> Vec<DocSection> {
        self.finish_section();
        let result = self.sections.drain(..).collect();
        let current_body = std::mem::take(&mut self.current_body);
        let current_list = std::mem::take(&mut self.current_list);
        let sections = std::mem::take(&mut self.sections);
        *self = Self {
            // Reuse the (empty) buffers.
            current_body,
            current_list,
            sections,
            // Reset the rest of state.
            in_secondary_section: Default::default(),
            inside_arguments: Default::default(),
        };
        result
    }
}

impl<L> TokenConsumer<L> for DocSectionCollector {
    fn tag(&mut self, tag: Tag, description: Option<Span<'_, L>>) {
        let body = description.map(|description| description.to_string()).unwrap_or_default();
        self.sections.push(DocSection::Tag { tag, body });
    }

    fn enter_marked_section(&mut self, mark: Mark, header: Option<Span<'_, L>>) {
        self.finish_section();
        let header = header.map(|header| header.to_string());
        let body = Default::default();
        self.sections.push(DocSection::Marked { mark, header, body });
    }

    fn enter_keyed_section(&mut self, header: Span<'_, L>) {
        self.finish_section();
        let key = header.to_string();
        if key.eq_ignore_ascii_case("Arguments") {
            self.inside_arguments = true;
        }
        let body = Default::default();
        self.sections.push(DocSection::Keyed { key, body });
    }

    fn text(&mut self, text: Span<'_, L>) {
        self.current_body.push_str(text.as_ref());
    }

    fn start_list(&mut self) {
        self.current_list.clear();
    }

    fn start_list_item(&mut self) {}

    fn start_paragraph(&mut self) {
        let first_content = !self.in_secondary_section && self.current_body.is_empty();
        if !first_content {
            self.current_body.push_str("<p>");
        }
    }

    fn start_raw(&mut self) {
        self.current_body.push_str("<pre>");
    }

    fn start_quote(&mut self) {
        self.current_body.push_str("<code>");
    }

    fn end_quote(&mut self) {
        self.current_body.push_str("</code>");
    }

    fn whitespace(&mut self) {
        self.current_body.push(' ');
    }

    fn raw_line(&mut self, text: Span<'_, L>) {
        if !self.current_body.is_empty() {
            self.current_body.push('\n');
        }
        self.current_body.push_str(text.as_ref());
    }

    fn end(&mut self, scope: ScopeType) {
        match scope {
            ScopeType::List => {
                let items = mem::take(&mut self.current_list);
                if self.inside_arguments {
                    let args = items.iter().map(|arg| Argument::new(arg)).collect();
                    self.sections.push(DocSection::Arguments { args });
                    self.inside_arguments = false;
                } else {
                    self.sections.push(DocSection::List { items })
                }
            }
            ScopeType::ListItem => {
                self.current_list.push(self.current_body.drain(..).collect());
            }
            ScopeType::Paragraph => (),
            ScopeType::Raw => self.current_body.push_str("</pre>"),
        }
    }
}
