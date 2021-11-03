//! General-purpose utilities for creating tests for double representation.

use crate::prelude::*;

use regex::Captures;
use regex::Match;

/// Helper type for markdown-defined test cases with `regex` library.
/// When implementing a `Replacer`, on each match the `process_match` should be called.
#[derive(Clone, Copy, Debug, Default)]
pub struct MarkdownProcessor {
    markdown_bytes_consumed: usize,
}

impl MarkdownProcessor {
    /// Convert index from marked to unmarked code.
    fn marked_to_unmarked_index(&self, i: usize) -> usize {
        assert!(self.markdown_bytes_consumed <= i);
        i - self.markdown_bytes_consumed
    }

    /// Convert indices range from marked to unmarked code.
    fn marked_to_unmarked_range(&self, range: Range<usize>) -> Range<usize> {
        Range {
            start: self.marked_to_unmarked_index(range.start),
            end:   self.marked_to_unmarked_index(range.end),
        }
    }

    /// Assumes that given match is the part of capture that should be passed to the dst string.
    /// Appends the `body` match contents to the `dst` and returns its span in unmarked text.
    /// All characters in the capture that do not belong to `body` are considered markdown.
    pub fn process_match(
        &mut self,
        captures: &Captures,
        body: &Match,
        dst: &mut String,
    ) -> Range<usize> {
        let whole_match = captures.get(0).expect("Capture 0 should always be present.");
        let bytes_to_body = body.start() - whole_match.start();
        let bytes_after_body = whole_match.end() - body.end();
        self.markdown_bytes_consumed += bytes_to_body;
        let ret = self.marked_to_unmarked_range(body.range());
        self.markdown_bytes_consumed += bytes_after_body;
        dst.push_str(body.as_str());
        ret
    }
}
