#![feature(test)]
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]

//! This module exports a reader that is able to process large textual inputs in constant memory.

pub mod decoder;

use enso_prelude::*;

use crate::decoder::Char;
use crate::decoder::InvalidChar;
use decoder::Decoder;



// ============
// === Read ===
// ============

/// Trait for reading input data into a buffer.
///
/// Compared to `std::io::Read` this reader supports multiple input encodings.
pub trait Read {
    /// The type of the data in the buffer.
    type Item;

    /// Fills the buffer and returns amount of elements read.
    ///
    /// In case it isn't possible to fill the whole buffer (i.e. if an error like EOF is
    /// encountered), the buffer will be filled with all the data read before encountering such an
    /// error.
    fn read(&mut self, buffer: &mut [Self::Item]) -> usize;
}


// === Trait Impls ===

impl<R: std::io::Read> Read for R {
    type Item = u8;

    fn read(&mut self, mut buffer: &mut [u8]) -> usize {
        let length = buffer.len();
        while !buffer.is_empty() {
            match self.read(buffer) {
                Err(_) => break,
                Ok(0) => break,
                Ok(n) => {
                    buffer = &mut buffer[n..];
                }
            }
        }
        length - buffer.len()
    }
}



// =============
// === Error ===
// =============

/// Set of errors returned by lazy reader.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Error {
    /// End Of Input.
    EOF,
    /// Couldn't decode character.
    InvalidChar,
    /// The lexer has found no matching rule in the current state.
    EndOfGroup,
}

impl Error {
    /// The `u32` value that corresponds to EOF.
    pub const END_OF_FILE: u32 = u32::max_value();
    /// The `u32` value that corresponds to an invalid unicode character.
    pub const INVALID_CHAR: u32 = 0xFFFF;
    /// The `u32` value corresponding to the end of group.
    pub const END_OF_GROUP: u32 = u32::max_value() - 1;
    /// The `u64` value that corresponds to EOF.
    pub const END_OF_FILE_64: u64 = u64::max_value();
    /// The `u64` value that corresponds to an invalid unicode character.
    pub const INVALID_CHAR_64: u64 = 0xFFFF;
    /// The `u32` value corresponding to the end of group.
    pub const END_OF_GROUP_64: u64 = u64::max_value() - 1;
}


// === Trait Impls ===

impl From<decoder::Char<decoder::InvalidChar>> for decoder::Char<Error> {
    fn from(char: Char<InvalidChar>) -> Self {
        let size = char.size;
        let char = match char.char {
            Ok(char) => Ok(char),
            Err(_) => Err(Error::InvalidChar),
        };
        decoder::Char { char, size }
    }
}

impl From<decoder::Char<Error>> for u32 {
    fn from(char: decoder::Char<Error>) -> Self {
        match char.char {
            Ok(char) => char as u32,
            Err(Error::EOF) => Error::END_OF_FILE,
            Err(Error::InvalidChar) => Error::INVALID_CHAR,
            Err(Error::EndOfGroup) => Error::END_OF_GROUP,
        }
    }
}

impl From<decoder::Char<Error>> for u64 {
    fn from(char: decoder::Char<Error>) -> Self {
        match char.char {
            Ok(char) => char as u64,
            Err(Error::EOF) => Error::END_OF_FILE_64,
            Err(Error::InvalidChar) => Error::INVALID_CHAR_64,
            Err(Error::EndOfGroup) => Error::END_OF_GROUP_64,
        }
    }
}



// ==================
// === BookmarkId ===
// ==================

/// Strongly typed identifier of `Bookmark`
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct BookmarkId {
    #[allow(missing_docs)]
    id: usize,
}

impl BookmarkId {
    /// Creates a new bookmark handle using the specified identifier.
    pub fn new(id: usize) -> BookmarkId {
        BookmarkId { id }
    }
}



// =================
// === ReaderOps ===
// =================

/// The behaviour needed by the reader interface.
pub trait ReaderOps {
    /// Read the next character from input.
    fn next_char(&mut self, bookmarks: &mut BookmarkManager) -> Result<char, Error>;
    /// Advance along the input without returning the character.
    fn advance_char(&mut self, bookmarks: &mut BookmarkManager);
    /// Get the current character from the reader.
    fn character(&self) -> decoder::Char<Error>;
    /// Check if the reader has finished reading.
    ///
    /// A reader is finished when it has no further input left to read, and when it does not need to
    /// rewind to any point.
    fn finished(&self, bookmarks: &BookmarkManager) -> bool;
    /// Check if the reader is empty.
    fn empty(&self) -> bool;
    /// Fill the buffer with words from the input.
    fn fill(&mut self, bookmarks: &mut BookmarkManager);
    /// Get the maximum possible rewind for the reader.
    fn max_possible_rewind_len(&self, bookmarks: &BookmarkManager) -> usize;
    /// Append the provided character to the reader's result.
    fn append_result(&mut self, char: char);
    /// Return `self.result` and sets the internal result to empty.
    fn pop_result(&mut self) -> String;
    /// Get the reader's current offset in the buffer.
    fn offset(&self) -> usize;
    /// Get an immutable reference to the reader's result.
    fn result(&self) -> &String;
    /// Get a mutable reference to the reader's result.
    fn result_mut(&mut self) -> &mut String;
    /// Get the current length of the reader's buffer.
    fn buffer_len(&self) -> usize;
    /// Set the buffer offset to the specified value.
    fn set_offset(&mut self, off: usize);
    /// Truncate the current match to the provided length.
    fn truncate_match(&mut self, len: usize);
}

/// The default size of the buffer.
pub const BUFFER_SIZE: usize = 32768;



// ==============
// === Reader ===
// ==============

/// A buffered reader able to efficiently read big inputs in constant memory.
///
/// It supports various encodings via `Decoder` and also bookmarks which allow it to return
/// back to a character at specific offset.
#[derive(Debug, Clone, PartialEq)]
pub struct Reader<D: Decoder, Read> {
    /// The reader that holds the input.
    pub reader:    Read,
    /// The buffer that stores the input data.
    pub buffer:    Vec<D::Word>,
    /// The string representation of data that has been read.
    pub result:    String,
    /// The buffer offset of the current element read.
    pub offset:    usize,
    /// The number of elements stored in buffer.
    pub length:    usize,
    /// The last character read.
    pub character: decoder::Char<Error>,
}

impl<D: Decoder, R: Read<Item = D::Word>> Reader<D, R> {
    /// Creates a new instance of the reader.
    pub fn new(reader: R, _decoder: D) -> Self {
        let mut reader = Reader::<D, R> {
            reader,
            buffer: vec![D::Word::default(); BUFFER_SIZE],
            result: String::from(""),
            offset: 0,
            length: 0,
            character: decoder::Char { char: Err(Error::EOF), size: 0 },
        };
        reader.length = reader.reader.read(&mut reader.buffer[..]);
        reader
    }
}


// === Trait Impls ===

impl<D: Decoder, R: Read<Item = D::Word>> ReaderOps for Reader<D, R> {
    fn next_char(&mut self, bookmarks: &mut BookmarkManager) -> Result<char, Error> {
        if self.empty() {
            self.character.char = Err(Error::EOF);
            return Err(Error::EOF);
        }

        if self.offset >= self.buffer.len() - D::MAX_CODEPOINT_LEN {
            self.fill(bookmarks);
        }

        self.character = D::decode(&self.buffer[self.offset..]).into();
        self.offset += self.character.size;

        self.character.char
    }

    fn advance_char(&mut self, bookmarks: &mut BookmarkManager) {
        let _ = self.next_char(bookmarks);
    }

    fn character(&self) -> Char<Error> {
        self.character
    }

    fn finished(&self, _bookmarks: &BookmarkManager) -> bool {
        let rewinded = self.max_possible_rewind_len(_bookmarks) != 0;
        self.empty() && rewinded
    }

    fn empty(&self) -> bool {
        self.length < self.buffer.len() && self.length <= self.offset
    }

    fn fill(&mut self, bookmarks: &mut BookmarkManager) {
        let len = self.buffer.len();
        let words = len - self.offset;
        self.offset = self.max_possible_rewind_len(bookmarks);
        if self.offset == len {
            panic!("Rewind won't be possible. Buffer is too small.")
        }

        bookmarks.decrease_bookmark_offsets(len - self.offset);
        for i in 1..=self.offset {
            self.buffer[self.offset - i] = self.buffer[len - i];
        }
        self.length = self.offset + self.reader.read(&mut self.buffer[self.offset..]);
        self.offset -= words;
    }

    fn max_possible_rewind_len(&self, bookmarks: &BookmarkManager) -> usize {
        if let Some(offset) = bookmarks.min_offset() {
            return self.buffer_len() - offset;
        }
        D::MAX_CODEPOINT_LEN
    }

    fn append_result(&mut self, char: char) {
        self.result.push(char);
    }

    fn pop_result(&mut self) -> String {
        let str = self.result.clone();
        self.result.truncate(0);
        str
    }

    fn offset(&self) -> usize {
        self.offset
    }

    fn result(&self) -> &String {
        &self.result
    }

    fn result_mut(&mut self) -> &mut String {
        &mut self.result
    }

    fn buffer_len(&self) -> usize {
        self.buffer.len()
    }

    fn set_offset(&mut self, off: usize) {
        self.offset = off;
    }

    fn truncate_match(&mut self, len: usize) {
        self.result.truncate(len);
    }
}



// ================
// === Bookmark ===
// ================

/// Bookmarks a specific character in buffer, so that `LazyReader` can return to it when needed.
#[derive(Debug, Clone, Copy, Default, PartialEq)]
pub struct Bookmark {
    /// The position of the bookmarked character in the `reader.buffer`.
    offset: usize,
    /// The length of `reader.result` up to the bookmarked character.
    length: usize,
    /// Whether or not the bookmark has been set by the user.
    set:    bool,
}



// =======================
// === BookmarkManager ===
// =======================

/// Contains and manages bookmarks for a running lexer.
///
/// Some of its operations operate on a specific [`Reader`]. It is undefined behaviour to not pass
/// the same reader to all calls for a given bookmark manager.
#[allow(missing_docs)]
#[derive(Clone, Debug, PartialEq)]
pub struct BookmarkManager {
    bookmarks:            Vec<Bookmark>,
    /// The bookmark used by the flexer to mark the end of the last matched segment of the input.
    pub matched_bookmark: BookmarkId,
    /// A bookmark used by the flexer to deal with overlapping rules that may fail later.
    pub rule_bookmark:    BookmarkId,
}

#[allow(missing_docs)]
impl BookmarkManager {
    /// Create a new bookmark manager, with no associated bookmarks.
    pub fn new() -> BookmarkManager {
        let mut bookmarks = Vec::new();
        let matched_bookmark = BookmarkManager::make_bookmark(&mut bookmarks);
        let rule_bookmark = BookmarkManager::make_bookmark(&mut bookmarks);
        BookmarkManager { bookmarks, matched_bookmark, rule_bookmark }
    }

    /// Create a new bookmark in the manager, returning a handle to it.
    fn make_bookmark(bookmarks: &mut Vec<Bookmark>) -> BookmarkId {
        bookmarks.push(Bookmark::default());
        BookmarkId::new(bookmarks.len() - 1)
    }

    /// Add a bookmark to the manager, returning a handle to that bookmark.
    pub fn add_bookmark(&mut self) -> BookmarkId {
        BookmarkManager::make_bookmark(&mut self.bookmarks)
    }

    /// Bookmarks the current position in `reader` using `bookmark`.
    pub fn bookmark<R: ReaderOps>(&mut self, bookmark: BookmarkId, reader: &mut R) {
        self.bookmarks[bookmark.id].offset = reader.offset() - reader.character().size;
        self.bookmarks[bookmark.id].length = reader.result().len();
        self.bookmarks[bookmark.id].set = true
    }

    /// Unsets a bookmark.
    pub fn unset<R: ReaderOps>(&mut self, bookmark: BookmarkId) {
        self.bookmarks[bookmark.id].offset = 0;
        self.bookmarks[bookmark.id].length = 0;
        self.bookmarks[bookmark.id].set = false
    }

    /// Decrease the offset for all bookmarks by the specified `amount` in preparation for
    /// truncating the reader's buffer.
    pub fn decrease_bookmark_offsets(&mut self, amount: usize) {
        for bookmark in self.bookmarks.iter_mut() {
            if bookmark.set {
                bookmark.offset -= amount
            }
        }
    }

    /// Rewind the reader to the position marked by `bookmark`.
    pub fn rewind<R: ReaderOps>(&mut self, bookmark: BookmarkId, reader: &mut R) {
        let bookmark = self.bookmarks.get(bookmark.id).expect("Bookmark must exist.");
        reader.set_offset(bookmark.offset);
        reader.truncate_match(bookmark.length);
        reader.advance_char(self);
    }

    /// Obtains the minimum offset from the start of the buffer for any bookmark.
    pub fn min_offset(&self) -> Option<usize> {
        self.bookmarks.iter().filter_map(|b| b.set.and_option(Some(b.offset))).min()
    }
}


// === Trait Impls ===

impl Default for BookmarkManager {
    fn default() -> Self {
        BookmarkManager::new()
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    extern crate test;

    use super::*;
    use decoder::*;

    use test::Bencher;

    // ================
    // === Repeater ===
    // ================

    /// Struct that holds state of `Reader` that repeats an input n times.
    #[derive(Debug, Clone)]
    struct Repeat<T> {
        /// The input to be repeated.
        buffer: Vec<T>,
        /// The current offset of element currently read from buffer.
        offset: usize,
        /// How many more times the input should be repeated.
        repeat: usize,
    }

    /// Creates a reader that repeats an input n times.
    fn repeat<T: Copy>(input: Vec<T>, repeat: usize) -> impl Read<Item = T> {
        Repeat { buffer: input, repeat, offset: 0 }
    }


    // === Trait Impls ===

    impl<T: Copy> Read for Repeat<T> {
        type Item = T;

        fn read(&mut self, mut buffer: &mut [Self::Item]) -> usize {
            if self.repeat == 0 {
                return 0;
            }

            let len = self.buffer.len();
            let read = buffer.len();

            if read < len - self.offset {
                buffer.copy_from_slice(&self.buffer[self.offset..self.offset + read]);
                self.offset += read;
                return read;
            }

            buffer[..len - self.offset].copy_from_slice(&self.buffer[self.offset..]);
            buffer = &mut buffer[len - self.offset..];

            let repeat = std::cmp::min(buffer.len() / len, self.repeat - 1);
            self.repeat = self.repeat - repeat - 1;
            for _ in 0..repeat {
                buffer[..len].copy_from_slice(&self.buffer[..]);
                buffer = &mut buffer[len..];
            }

            if self.repeat == 0 {
                return len - self.offset + repeat * len;
            }
            buffer.copy_from_slice(&self.buffer[..buffer.len()]);
            self.offset = buffer.len();
            read
        }
    }



    // =============
    // === Utils ===
    // =============

    /// Constructs an _empty_ bookmark manager for testing purposes.
    pub fn bookmark_manager() -> BookmarkManager {
        BookmarkManager::new()
    }



    // =============
    // === Tests ===
    // =============

    #[test]
    fn test_repeater_with_small_buffer() {
        let mut repeater = repeat(vec![1, 2, 3], 1);
        let mut buffer = [0; 2];
        assert_eq!(repeater.read(&mut buffer), 2);
        assert_eq!(&buffer, &[1, 2]);
        assert_eq!(repeater.read(&mut buffer), 1);
        assert_eq!(&buffer, &[3, 2])
    }

    #[test]
    fn test_repeater_with_big_buffer() {
        let mut repeater = repeat(vec![1, 2], 3);
        let mut buffer = [0; 5];
        assert_eq!(repeater.read(&mut buffer), 5);
        assert_eq!(&buffer, &[1, 2, 1, 2, 1]);
        assert_eq!(repeater.read(&mut buffer), 1);
        assert_eq!(&buffer, &[2, 2, 1, 2, 1])
    }

    #[test]
    fn test_reader_small_input() {
        let mut mgr = bookmark_manager();
        let str = "a.b^c! #𤭢界んにち𤭢#𤭢";
        let mut reader = Reader::new(str.as_bytes(), DecoderUTF8());
        let mut result = String::from("");
        while let Ok(char) = reader.next_char(&mut mgr) {
            result.push(char);
        }
        assert_eq!(&result, str);
    }

    #[test]
    fn test_reader_big_input() {
        let mut mgr = bookmark_manager();
        let str = "a.b^c! #𤭢界んにち𤭢#𤭢".repeat(10_000);
        let mut reader = Reader::new(str.as_bytes(), DecoderUTF8());
        let mut result = String::from("");
        while let Ok(char) = reader.next_char(&mut mgr) {
            mgr.bookmark(mgr.matched_bookmark, &mut reader);
            result.push(char);
        }
        assert_eq!(&result, &str);
        assert_eq!(reader.buffer.len(), BUFFER_SIZE);
    }

    #[bench]
    fn bench_reader(bencher: &mut Bencher) {
        let run = || {
            let mut mgr = bookmark_manager();
            let str = repeat("Hello, World!".as_bytes().to_vec(), 10_000_000);
            let mut reader = Reader::new(str, DecoderUTF8());
            let mut count = 0;
            while reader.next_char(&mut mgr) != Err(Error::EOF) {
                count += 1;
            }
            count
        };
        bencher.iter(run);
    }
}
