use crate::prelude::*;

use pulldown_cmark::Event;
use pulldown_cmark::HeadingLevel;
use pulldown_cmark::Tag::Heading;
use std::ops::Range;


// ==============
// === Export ===
// ==============

pub mod check;



#[derive(Clone, Copy, Debug)]
pub struct Changelog<'a>(pub &'a str);

impl<'a> Changelog<'a> {
    pub fn iterate_headers(&self) -> impl Iterator<Item = Header<'a>> + 'a {
        use pulldown_cmark::Options;
        use pulldown_cmark::Parser;
        let enable_all_exts = Options::all();
        Parser::new_ext(self.0, enable_all_exts)
            .into_offset_iter()
            .filter_map(|(e, pos)| Header::new(self.0, e, pos))
    }

    pub fn last_release(&self) -> Result<Version> {
        self.iterate_headers()
            .find_map(|header| Version::find_in_text(header.text).ok())
            .context("No release header with version number was found.")
    }

    pub fn top_release_notes(&self) -> Result<Entry> {
        let mut headers = self.iterate_headers();
        let first_header = headers.next().context("Failed to find a level one header.")?;
        let file_end_pos = self.0.len() + 1;
        let next_header_start = headers.next().map_or(file_end_pos, |h| h.pos.start);
        let contents = self.0[first_header.pos.end..next_header_start].trim();
        let entry =
            Entry { header: first_header.text.to_string(), contents: contents.to_string() };
        Ok(entry)
    }
}

#[derive(Clone, Debug)]
pub struct Entry {
    pub header:   String,
    pub contents: String,
}

#[derive(Clone, Debug)]
pub struct Header<'a> {
    /// Text of the header.
    pub text: &'a str,
    /// Position in the changelog file text.
    pub pos:  Range<usize>,
}

impl<'a> Header<'a> {
    pub fn new(whole_text: &'a str, event: Event, position: Range<usize>) -> Option<Self> {
        is_release_notes_header(&event)
            .then_some(Self { text: whole_text[position.clone()].trim(), pos: position })
    }
}

pub fn is_release_notes_header(event: &Event) -> bool {
    matches!(event, Event::Start(Heading(HeadingLevel::H1, _, _)))
}
