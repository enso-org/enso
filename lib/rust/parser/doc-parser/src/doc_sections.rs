use crate::*;



// ============================
// === Documentation Parser ===
// ============================

#[derive(Default, Debug)]
pub struct DocParser {
    docs:  DocSectionCollector,
    lexer: Lexer,
}

impl DocParser {
    pub fn new() -> Self {
        Self::default()
    }

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

/// Documentation section mark.
#[derive(Hash, Debug, Copy, Clone, PartialEq, Eq)]
#[allow(missing_docs)]
pub enum Mark {
    Important,
    Info,
    Example,
}

/// A single section of the documentation.
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
#[allow(missing_docs)]
pub enum DocSection {
    /// The documentation tag.
    Tag {
        /// The tag name.
        name: &'static str,
        /// The tag text.
        body: HtmlString,
    },
    /// The paragraph of the text.
    Paragraph {
        /// The elements that make up this paragraph.
        body: HtmlString,
    },
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


// === Mark ===

impl Mark {
    pub fn new(text: &str) -> Option<Self> {
        match text {
            "!" => Some(Mark::Important),
            "?" => Some(Mark::Info),
            ">" => Some(Mark::Example),
            _ => None,
        }
    }
}



// ============================
// === DocSection Collector ===
// ============================

#[derive(Default, Debug)]
struct DocSectionCollector {
    sections:             Vec<DocSection>,
    in_secondary_section: bool,
    current_body:         String,
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
            Some(DocSection::Tag { .. }) | None =>
                self.sections.push(DocSection::Paragraph { body: text }),
        }
    }

    fn finish(&mut self) -> Vec<DocSection> {
        self.finish_section();
        let result = self.sections.drain(..).collect();
        let current_body = std::mem::take(&mut self.current_body);
        let sections = std::mem::take(&mut self.sections);
        *self = Self {
            // Reuse the (empty) buffers.
            current_body,
            sections,
            // Reset the rest of state.
            in_secondary_section: Default::default(),
        };
        result
    }
}

impl<L> TokenConsumer<L> for DocSectionCollector {
    fn flag(&mut self, flag: Flag, description: Option<Span<'_, L>>) {
        let name = flag.to_str();
        let body = description.map(|description| description.to_string()).unwrap_or_default();
        self.sections.push(DocSection::Tag { name, body });
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
        let body = Default::default();
        self.sections.push(DocSection::Keyed { key, body });
    }

    fn text(&mut self, text: Span<'_, L>) {
        self.current_body.push_str(text.as_ref());
    }

    fn start_list(&mut self) {
        self.current_body.push_str("<ul>");
    }

    fn start_list_item(&mut self) {
        self.current_body.push_str("<li>");
    }

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
            ScopeType::List => self.current_body.push_str("</ul>"),
            ScopeType::ListItem => (),
            ScopeType::Paragraph => (),
            ScopeType::Raw => self.current_body.push_str("</pre>"),
        }
    }
}
