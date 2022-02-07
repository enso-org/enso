//! Implementation of a cursor allowing word-based traversal.
use crate::prelude::*;

use enso_text::rope;
use enso_text::unit::*;



// ==================
// === WordCursor ===
// ==================

/// Cursor allowing word-based traversal.
pub struct WordCursor<'a> {
    cursor: rope::Cursor<'a, rope::Info>,
}

impl<'a> WordCursor<'a> {
    /// Constructor.
    pub fn new(text: &'a rope::Rope, pos: Bytes) -> WordCursor<'a> {
        let cursor = rope::Cursor::new(text, pos.value as usize);
        WordCursor { cursor }
    }

    /// Get previous boundary, and set the cursor at the boundary found.
    pub fn prev_boundary(&mut self) -> Option<Bytes> {
        self.prev_codepoint_class().map(|mut cls| {
            let mut candidate = self.cursor.pos();
            while let Some(prev_cls) = self.prev_codepoint_class() {
                if Boundary::new(prev_cls, cls).is_start() {
                    break;
                }
                cls = prev_cls;
                candidate = self.cursor.pos();
            }
            self.cursor.set(candidate);
            candidate.into()
        })
    }

    /// Get next boundary, and set the cursor at the boundary found.
    pub fn next_boundary(&mut self) -> Option<Bytes> {
        self.next_codepoint_class().map(|mut cls| {
            let mut candidate = self.cursor.pos();
            while let Some(next_cls) = self.next_codepoint_class() {
                if Boundary::new(cls, next_cls).is_end() {
                    break;
                }
                cls = next_cls;
                candidate = self.cursor.pos();
            }
            self.cursor.set(candidate);
            candidate.into()
        })
    }

    /// Return the selection for the word containing the current cursor. The cursor is moved to the
    /// end of that selection.
    pub fn select_word(&mut self) -> (Bytes, Bytes) {
        let initial = self.cursor.pos();
        let init_cls_after = self.next_codepoint_class();
        self.cursor.set(initial); // FIXME ???
        let init_cls_before = self.prev_codepoint_class();
        let mut start = initial;
        let init_boundary_opt = init_cls_before.zip_with(init_cls_after, Boundary::new_initial);
        let init_boundary = init_boundary_opt.unwrap_or(Boundary::EndAndStart);
        let mut cls_after = init_cls_after;
        let mut cls_before = init_cls_before;
        if cls_after.is_none() {
            start = self.cursor.pos();
            cls_after = cls_before;
            cls_before = self.prev_codepoint_class();
        }
        while let (Some(before), Some(after)) = (cls_before, cls_after) {
            if start == initial {
                if init_boundary.is_start() {
                    break;
                }
            } else if !init_boundary.is_interior() {
                if Boundary::new(before, after).is_interior() {
                    break;
                }
            } else if Boundary::new(before, after).is_start() {
                break;
            }
            start = self.cursor.pos();
            cls_after = cls_before;
            cls_before = self.prev_codepoint_class();
        }
        self.cursor.set(initial);
        let mut end = initial;
        cls_after = init_cls_after;
        cls_before = init_cls_before;
        if cls_before.is_none() {
            cls_before = self.next_codepoint_class();
            end = self.cursor.pos();
            cls_after = self.next_codepoint_class();
        }
        while let (Some(before), Some(after)) = (cls_before, cls_after) {
            if end == initial {
                if init_boundary.is_end() {
                    break;
                }
            } else if !init_boundary.is_interior() {
                if Boundary::new(before, after).is_interior() {
                    break;
                }
            } else if Boundary::new(before, after).is_end() {
                break;
            }
            end = self.cursor.pos();
            cls_before = cls_after;
            cls_after = self.next_codepoint_class();
        }
        self.cursor.set(end);
        (start.into(), end.into())
    }

    fn next_codepoint_class(&mut self) -> Option<CharClass> {
        self.cursor.next_codepoint().map(char_class)
    }

    fn prev_codepoint_class(&mut self) -> Option<CharClass> {
        self.cursor.prev_codepoint().map(char_class)
    }
}

impl<'a> Debug for WordCursor<'a> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "WordCursor")
    }
}



// ================
// === Boundary ===
// ================

/// Word boundary classification.
#[derive(PartialEq, Eq)]
enum Boundary {
    Interior,
    Start,
    End,
    EndAndStart,
}

impl Boundary {
    fn new(prev: CharClass, next: CharClass) -> Self {
        use self::Boundary::*;
        use self::CharClass::*;
        match (prev, next) {
            (Lf, _) => EndAndStart,
            (_, Lf) => EndAndStart,
            (Space, Other) => Start,
            (Space, Punctuation) => Start,
            (Punctuation, Other) => Start,
            (Other, Space) => End,
            (Punctuation, Space) => End,
            (Other, Punctuation) => End,
            _ => Interior,
        }
    }

    fn new_initial(prev: CharClass, next: CharClass) -> Boundary {
        use self::Boundary::*;
        use self::CharClass::*;
        match (prev, next) {
            (Lf, Other) => Start,
            (Other, Lf) => End,
            (Lf, Space) => Interior,
            (Lf, Punctuation) => Interior,
            (Space, Lf) => Interior,
            (Punctuation, Lf) => Interior,
            (Space, Punctuation) => Interior,
            (Punctuation, Space) => Interior,
            _ => Boundary::new(prev, next),
        }
    }

    fn is_start(&self) -> bool {
        *self == Boundary::Start || *self == Boundary::EndAndStart
    }

    fn is_end(&self) -> bool {
        *self == Boundary::End || *self == Boundary::EndAndStart
    }

    fn is_interior(&self) -> bool {
        *self != Boundary::Interior
    }
}



// =================
// === CharClass ===
// =================

/// Character classification. Helper used to discover word boundaries.
#[derive(Copy, Clone)]
enum CharClass {
    Lf,
    Space,
    Punctuation,
    Other,
}

fn char_class(codepoint: char) -> CharClass {
    if codepoint <= ' ' {
        // TODO:deal with \r
        if codepoint == '\n' {
            return CharClass::Lf;
        }
        return CharClass::Space;
    } else if codepoint <= '\u{3f}' {
        // Hardcoded:!"#$%&'()*+,-./:;<=>?
        if (0xfc00_fffe_0000_0000u64 >> (codepoint as u32)) & 1 != 0 {
            return CharClass::Punctuation;
        }
    } else if codepoint <= '\u{7f}' {
        // Hardcoded:@[\]^`{|}~
        if (0x7800_0001_7800_0001u64 >> ((codepoint as u32) & 0x3f)) & 1 != 0 {
            return CharClass::Punctuation;
        }
    }
    CharClass::Other
}
