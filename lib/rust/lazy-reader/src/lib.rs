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

use decoder::Decoder;
use crate::decoder::{Char, InvalidChar};
use crate::Error::EOF;


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
    fn read(&mut self,buffer:&mut [Self::Item]) -> usize;
}


// === Trait Impls ===

impl<R:std::io::Read> Read for R {
    type Item = u8;

    fn read(&mut self,mut buffer:&mut [u8]) -> usize {
        let length = buffer.len();
        while !buffer.is_empty() {
            match self.read(buffer) {
                Err(_) => break,
                Ok (0) => break,
                Ok (n) => {
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
#[derive(Debug,Clone,Copy,PartialEq,Eq)]
pub enum Error {
    /// End Of Input.
    EOF,
    /// Couldn't decode character.
    InvalidChar,
}



// ==================
// === BookmarkId ===
// ==================

/// Strongly typed identifier of `Bookmark`
#[derive(Debug,Clone,Copy,PartialEq)]
pub struct BookmarkId {
    #[allow(missing_docs)]
    id: usize
}

impl BookmarkId {
    /// Creates a new bookmark handle using the specified identifier.
    pub fn new(id:usize) -> BookmarkId {
        BookmarkId{id}
    }
}



// ================
// === Bookmark ===
// ================

/// Bookmarks a specific character in buffer, so that `LazyReader` can return to it when needed.
#[derive(Debug,Clone,Copy,Default,PartialEq)]
pub struct Bookmark {
    /// The position of bookmarked character in `reader.buffer`.
    offset: usize,
    /// The length of `reader.result` up to the bookmarked character.
    length: usize,
}



// ==================
// === LazyReader ===
// ==================

/// The behaviour needed by the lazy reader interface.
pub trait LazyReader {
    /// Creates a new bookmark, providing a handle so it can be used later.
    fn add_bookmark(&mut self) -> BookmarkId;
    /// Bookmarks the current character using the provided `bookmark`, so that the reader can later
    /// return to it using `rewind()`.
    ///
    /// Panics if `bookmark` refers to a nonexistent bookmark.
    fn bookmark(&mut self, bookmark:BookmarkId);
    /// Returns the reader to the character bookmarked using `bookmark`.
    fn rewind(&mut self, bookmark:BookmarkId);
    /// The maximum number of words that may be rewound in the buffer.
    fn max_possible_rewind_len(&self) -> usize;
    /// Decrease the offset for all bookmarks.
    fn decrease_offset(&mut self, off:usize);
    /// Fill the buffer with words from the input.
    fn fill(&mut self);
    /// Checks if the reader is empty.
    fn empty(&self) -> bool;
    /// Checks if the reader has finished reading.
    fn finished(&self) -> bool;
    /// Reads the next character from input.
    fn next_char(&mut self) -> Result<char,Error>;
    /// Gets the current character from the reader.
    fn character(&self) -> decoder::Char<Error>;
    /// Advances along the input without returning the character.
    fn advance_char(&mut self);
    /// Appends the provided character to the reader's result.
    fn append_result(&mut self, char:char);
    /// Returns `self.result` and sets the internal result to empty.
    fn pop_result(&mut self) -> String;
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
#[derive(Debug,Clone,PartialEq)]
pub struct Reader<D:Decoder,Read> {
    /// The reader that holds the input.
    pub reader: Read,
    /// The buffer that stores the input data.
    pub buffer: Vec<D::Word>,
    /// The string representation of data that has been read.
    pub result: String,
    /// The buffer offset of the current element read.
    pub offset: usize,
    /// The number of elements stored in buffer.
    pub length: usize,
    /// Flag that is true iff the reader was just rewinded and no new chars were read.
    pub bookmark: Vec<Bookmark>,
    /// The last character read.
    pub character: decoder::Char<Error>,
}

impl<D:Decoder,R:Read<Item=D::Word>> Reader<D,R> {
    /// Creates a new instance of the reader.
    pub fn new(reader:R, _decoder:D) -> Self {
        let mut reader = Reader::<D,R> {
            reader,
            buffer    : vec![D::Word::default(); BUFFER_SIZE],
            result    : String::from(""),
            offset    : 0,
            length    : 0,
            bookmark  : Vec::new(),
            character : decoder::Char{char:Err(Error::EOF), size:0},
        };
        reader.length = reader.reader.read(&mut reader.buffer[..]);
        reader
    }
}


// === Trait Impls ===

impl<D:Decoder, R:Read<Item=D::Word>> LazyReader for Reader<D,R> {
    fn add_bookmark(&mut self) -> BookmarkId {
        self.bookmark.push(Bookmark::default());
        BookmarkId::new(self.bookmark.len() - 1)
    }

    fn bookmark(&mut self, bookmark:BookmarkId) {
        self.bookmark[bookmark.id].offset = self.offset - self.character.size;
        self.bookmark[bookmark.id].length = self.result.len();
    }

    fn rewind(&mut self, bookmark:BookmarkId) {
        self.offset = self.bookmark[bookmark.id].offset;
        self.result.truncate(self.bookmark[bookmark.id].length);
        let _ = self.next_char();
    }

    fn max_possible_rewind_len(&self) -> usize {
        if let Some(offset) = self.bookmark.iter().map(|b| b.offset).min() {
            return self.buffer.len() - offset
        }
        D::MAX_CODEPOINT_LEN
    }

    fn decrease_offset(&mut self, off:usize) {
        for bookmark in self.bookmark.iter_mut() {
            bookmark.offset -= off
        }
    }

    fn fill(&mut self) {
        let len     = self.buffer.len();
        let words   = len - self.offset;
        self.offset = self.max_possible_rewind_len();
        if self.offset == len {
            panic!("Rewind won't be possible. Buffer is too small.")
        }

        self.decrease_offset(len - self.offset);
        for i in 1..=self.offset {
            self.buffer[self.offset - i] = self.buffer[len - i];
        }
        self.length  = self.offset + self.reader.read(&mut self.buffer[self.offset..]);
        self.offset -= words;
    }

    fn empty(&self) -> bool {
        self.length < self.buffer.len() && self.length <= self.offset
    }

    fn finished(&self) -> bool {
        self.empty() && self.character.char == Err(EOF)
    }

    fn next_char(&mut self) -> Result<char,Error> {
        if self.empty() { self.character.char = Err(Error::EOF); return Err(Error::EOF) }

        if self.offset >= self.buffer.len() - D::MAX_CODEPOINT_LEN {
            self.fill();
        }

        self.character = D::decode(&self.buffer[self.offset..]).into();
        self.offset   += self.character.size;

        self.character.char
    }

    fn character(&self) -> Char<Error> {
        self.character
    }

    fn advance_char(&mut self) {
        let _ = self.next_char();
    }

    fn append_result(&mut self,char:char) {
        self.result.push(char);
    }

    fn pop_result(&mut self) -> String {
        let str = self.result.clone();
        self.result.truncate(0);
        str
    }
}

impl From<decoder::Char<decoder::InvalidChar>> for decoder::Char<Error> {
    fn from(char:Char<InvalidChar>) -> Self {
        let size = char.size;
        let char = match char.char {
            Ok(char) => Ok(char),
            Err(_)   => Err(Error::InvalidChar),
        };
        decoder::Char{char,size}
    }
}

impl From<decoder::Char<Error>> for u32 {
    fn from(char:decoder::Char<Error>) -> Self {
        match char.char {
            Ok (char)               => char as u32,
            Err(Error::EOF)         => u32::max_value(),
            Err(Error::InvalidChar) => u32::max_value() - 1,
        }
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
    fn repeat<T:Copy>(input:Vec<T>, repeat:usize) -> impl Read<Item=T> {
        Repeat { buffer:input, repeat, offset: 0 }
    }


    // === Trait Impls ===

    impl<T:Copy> Read for Repeat<T> {
        type Item = T;

        fn read(&mut self, mut buffer:&mut [Self::Item]) -> usize {
            if self.repeat == 0 { return 0 }

            let len  = self.buffer.len();
            let read = buffer.len();

            if read < len - self.offset {
                buffer.copy_from_slice(&self.buffer[self.offset..self.offset + read]);
                self.offset += read;
                return read
            }

            buffer[..len - self.offset].copy_from_slice(&self.buffer[self.offset..]);
            buffer = &mut buffer[len - self.offset..];

            let repeat  = std::cmp::min(buffer.len() / len, self.repeat - 1);
            self.repeat = self.repeat - repeat - 1;
            for _ in 0..repeat {
                buffer[..len].copy_from_slice(&self.buffer[..]);
                buffer = &mut buffer[len..];
            }

            if self.repeat == 0 {
                return len - self.offset + repeat * len
            }
            buffer.copy_from_slice(&self.buffer[..buffer.len()]);
            self.offset = buffer.len();
            read
        }
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
        let     str    = "a.b^c! #𤭢界んにち𤭢#𤭢";
        let mut reader = Reader::new(str.as_bytes(), DecoderUTF8());
        let mut result = String::from("");
        while let Ok(char) = reader.next_char() {
            result.push(char);
        }
        assert_eq!(&result, str);
    }

    #[test]
    fn test_reader_big_input() {
        let     str    = "a.b^c! #𤭢界んにち𤭢#𤭢".repeat(10_000);
        let mut reader = Reader::new(str.as_bytes(), DecoderUTF8());
        let mut result = String::from("");
        while let Ok(char) = reader.next_char() {
            result.push(char);
        }
        assert_eq!(&result, &str);
        assert_eq!(reader.bookmark.len(), 0);
        assert_eq!(reader.buffer.len(), BUFFER_SIZE);
    }

    #[bench]
    fn bench_reader(bencher:&mut Bencher) {
        let run = || {
            let     str    = repeat("Hello, World!".as_bytes().to_vec(), 10_000_000);
            let mut reader = Reader::new(str, DecoderUTF8());
            let mut count  = 0;
            while reader.next_char() != Err(Error::EOF) {
                count += 1;
            }
            count
        };
        bencher.iter(run);
    }
}
