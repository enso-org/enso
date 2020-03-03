//! This module contains definitions for word occurrences in TextField.
//! Words are considered to be composed with alphanumeric and underscores.

use crate::prelude::*;

use std::ops::Range;

use crate::display::shape::text::text_field::content::TextFieldContent;
use crate::display::shape::text::text_field::cursor::Cursor;
use data::text::TextLocation;



// =================
// === WordRange ===
// =================

/// A struct containing indices for word's start and end.
#[derive(Shrinkwrap,Debug,Clone,Derivative)]
#[derivative(PartialEq)]
pub struct WordRange {
    /// A property containing the word start and end.
    #[shrinkwrap(main_field)]
    pub word_range : Range<TextLocation>,
    #[derivative(PartialEq = "ignore")]
    is_selected : bool
}

impl WordRange {
    /// Creates a new `WordRange`.
    pub fn new(word_range : Range<TextLocation>) -> Self {
        let is_selected = false;
        Self {word_range,is_selected}
    }

    /// Returns a word under cursor, if any.
    pub fn word_at_cursor(content:&TextFieldContent, cursor:&Cursor) -> Option<Self> {
        let line  = cursor.position.line;
        let chars = content.lines()[line].chars();
        get_index_range_of_word_at(chars,cursor.position.column).map(|range| {
            let column = range.start;
            let start  = TextLocation { line, column };
            let column = range.end;
            let end    = TextLocation { line, column };
            Self::new(start..end)
        })
    }
}



// =======================
// === WordOccurrences ===
// =======================

/// A struct containing word occurrences in `TextFieldContent`.
#[derive(Shrinkwrap,Clone,Debug)]
pub struct WordOccurrences {
    /// Words occurrences.
    #[shrinkwrap(main_field)]
    pub words     : Vec<WordRange>,
    current_index : usize
}

impl WordOccurrences {
    /// Gets all the occurrences of a word if the cursor is inside a word or if it's selecting a
    /// word. If no occurrence is found, `None` is returned.
    pub fn new(content:&TextFieldContent, cursor:&Cursor) -> Option<Self> {
        let range = Self::get_range(content,cursor);
        let words = Self::get_words(content,range);
        words.map(|words| {
            let cursor_location = cursor.position;
            let current_index   = words.iter().find_position(|current_word| {
                cursor_location.line == current_word.start.line &&
                    cursor_location.column >= current_word.start.column &&
                    cursor_location.column <= current_word.end.column
            }).map(|(index, _)| index).unwrap_or(0);
            let current_index = current_index.wrapping_sub(1);
            Self { words, current_index }.initialize(&cursor)
        })
    }

    fn get_range(content:&TextFieldContent, cursor:&Cursor) -> Option<Range<TextLocation>> {
        if cursor.has_selection() {
            Some(cursor.selection_range())
        } else {
            let word = WordRange::word_at_cursor(content, &cursor);
            word.map(|word| word.word_range)
        }
    }

    fn get_words
    (content:&TextFieldContent, range:Option<Range<TextLocation>>) -> Option<Vec<WordRange>> {
        range.map(|range| {
            let word = content.copy_fragment(range);
            let mut words = Vec::new();
            let word: Vec<char> = word.chars().collect();

            for (index, line) in content.lines().iter().enumerate() {
                let line_chars = line.chars();
                let words_in_line = get_word_occurrences(line_chars, &word);
                for word in words_in_line {
                    let line = index;
                    let column = word.start;
                    let start = TextLocation { line, column };
                    let column = word.end;
                    let end = TextLocation { line, column };
                    words.push(WordRange::new(start..end));
                }
            }
            words
        }).filter(|words| !words.is_empty())
    }

    fn initialize(mut self, cursor:&Cursor) -> Self {
        if cursor.has_selection() {
            self.advance()
        }
        self
    }

    fn current_mut(&mut self) -> &mut WordRange {
        &mut self.words[self.current_index]
    }

    fn advance(&mut self) {
        self.current_index = self.current_index.wrapping_add(1) % self.words.len();
    }

    /// Get next word occurrence if not already selected.
    pub fn select_next(&mut self) -> Option<WordRange> {
        self.advance();
        let mut word = self.current_mut();
        if word.is_selected {
            None
        } else {
            word.is_selected = true;
            Some(word.clone())
        }
    }
}



// =============
// === Utils ===
// =============

fn get_words(content:&[char]) -> Vec<Vec<(usize,char)>> {
    let indexed:Vec<(usize,char)> = content.iter().copied().enumerate().collect();
    let words = indexed.split(|(_,character)| !character.is_alphanumeric() && *character != '_');
    let words = words.filter(|word_in_context| !word_in_context.is_empty());
    words.map(|c| c.to_vec()).collect()
}

fn get_index_range_of_word_at(content:&[char], index:usize) -> Option<Range<usize>> {
    let words       = get_words(content);
    let mut ranges  = words.iter().map(|word| {
        let (start,_) = word.first().unwrap();
        let end       = start + word.len();
        *start..end
    });
    ranges.find(|word_range| index >= word_range.start && index <= word_range.end)
}

fn get_word_occurrences(content:&[char], word:&[char]) -> Vec<Range<usize>> {
    let mut occurrences = Vec::new();

    for word_in_content in get_words(content) {
        let count = word_in_content.iter().zip(word).filter(|&((_, a), b)| a == b).count();
        if count == word_in_content.len() {
            let (start,_) = word_in_content.first().unwrap();
            let end       = start + word_in_content.len();
            occurrences.push(*start..end)
        }
    }
    occurrences
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod test {
    use super::*;
    use crate::display::shape::text::text_field::content::test::mock_properties;

    use basegl_core_msdf_sys as msdf_sys;
    use wasm_bindgen_test::wasm_bindgen_test;

    #[wasm_bindgen_test(async)]
    async fn get_range() {
        msdf_sys::initialized().await;
        let content = "";
        let content = TextFieldContent::new(content,&mock_properties());
        let cursor  = Cursor::new(TextLocation::at_document_begin());
        let range   = WordOccurrences::get_range(&content,&cursor);
        assert!(range.is_none());

        let content = "fn print_n_1(n:i32);\r\n#[test]\r\nfn test() {\r\n  print_n_1(1);\r\n}\n";
        let content = TextFieldContent::new(content,&mock_properties());
        let cursor  = Cursor::new(TextLocation { line:0, column: 4});
        let range   = WordOccurrences::get_range(&content,&cursor);
        assert_eq!(range, Some(TextLocation {line:0, column:3}..TextLocation{line:0, column:12}));

        let cursor = Cursor::new(TextLocation::at_line_begin(1));
        let range  = WordOccurrences::get_range(&content,&cursor);
        assert!(range.is_none());

        let cursor = Cursor::new(TextLocation::at_line_begin(5));
        let range  = WordOccurrences::get_range(&content,&cursor);
        assert!(range.is_none());

        let mut cursor = Cursor::new(TextLocation { line:0, column: 4});
        cursor.select_range(&(TextLocation {line:0, column:3}..TextLocation{line:0, column:12}));
        let range = WordOccurrences::get_range(&content,&cursor);
        assert_eq!(range, Some(TextLocation {line:0, column:3}..TextLocation{line:0, column:12}));
    }

    #[wasm_bindgen_test(async)]
    async fn get_words() {
        msdf_sys::initialized().await;
        let content = "";
        let content = TextFieldContent::new(content,&mock_properties());
        let cursor  = Cursor::new(TextLocation::at_document_begin());
        let range   = WordOccurrences::get_range(&content,&cursor);
        assert!(range.is_none());
        let words   = WordOccurrences::get_words(&content,range);
        assert!(words.is_none());

        let content = "fn print_n_1(n:i32);\r\n#[test]\r\nfn test() {\r\n  print_n_1(1);\r\n}\n";
        let content = TextFieldContent::new(content,&mock_properties());
        let cursor  = Cursor::new(TextLocation { line:0, column: 4});
        let range   = WordOccurrences::get_range(&content,&cursor);
        let words   = WordOccurrences::get_words(&content,range).expect("Couldn't find words");
        assert_eq!(words.len(), 2);

        let words = WordOccurrences::get_words(&content,None);
        assert_eq!(words,None);
    }

    #[wasm_bindgen_test(async)]
    async fn new_word_occurrences() {
        msdf_sys::initialized().await;
        let content = "";
        let content = TextFieldContent::new(content,&mock_properties());
        let cursor  = Cursor::new(TextLocation::at_document_begin());
        let words   = WordOccurrences::new(&content,&cursor);

        assert!(words.is_none());

        let content = "fn print_n_1(n:i32);\r\n#[test]\r\nfn test() {\r\n  print_n_1(1);\r\n}\n";
        let content = TextFieldContent::new(content,&mock_properties());
        let cursor  = Cursor::new(TextLocation::at_line_begin(1));
        let words   = WordOccurrences::new(&content,&cursor);

        assert!(words.is_none());

        let cursor    = Cursor::new(TextLocation {line:0,column:4});
        let mut words = WordOccurrences::new(&content,&cursor).expect("Couldn't find words");

        let word1 = WordRange::new(TextLocation{line:0,column:3}..TextLocation{line:0,column:12});
        let word2 = WordRange::new(TextLocation{line:3,column:2}..TextLocation{line:3,column:11});
        assert_eq!(words.select_next(), Some(word1));
        assert_eq!(words.select_next(), Some(word2));
        assert!(words.select_next().is_none());
    }

    #[test]
    fn index_range_of_word_at() {
        let content           = String::from("   abc    def    ghi  ");
        let content:Vec<char> = content.chars().collect();
        let range             = get_index_range_of_word_at(&content,12);
        assert_eq!(range, Some(10..13));

        let content           = String::from("   abc    def    ghi  ");
        let content:Vec<char> = content.chars().collect();
        let range             = get_index_range_of_word_at(&content,2);
        assert!(range.is_none());

        let content           = String::from("");
        let content:Vec<char> = content.chars().collect();
        let range             = get_index_range_of_word_at(&content,12);
        assert!(range.is_none());
    }

    #[test]
    fn word_occurrences() {
        let content = String::from("_5abc6   _5abc6    def    ghi  _5abc6abc _5abc6");
        let content:Vec<char> = content.chars().collect();
        let word              = String::from("_5abc6");
        let word:Vec<char>    = word.chars().collect();
        let occurrences       = get_word_occurrences(&content,&word);
        assert_eq!(occurrences.len(), 3);
        assert_eq!(occurrences[0],0..6);
        assert_eq!(occurrences[1],9..15);
        assert_eq!(occurrences[2],41..47);
    }
}
