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
    /// In case it isn't possible to fill the whole buffer (i.e. if an error like EOF is encountered),
    /// the buffer will be filled with all the data read before encountering such error.
    fn read(&mut self, buffer:&mut [Self::Item]) -> usize;
}


// === Trait Impls ===

impl<R:std::io::Read> Read for R {
    type Item = u8;

    fn read(&mut self, mut buffer:&mut [u8]) -> usize {
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



/// ==================
/// === LazyReader ===
/// ==================

/// Set of errors returned by lazy reader.
#[derive(Debug,Clone,Copy,PartialEq,Eq)]
pub enum Error {
    /// End Of Input.
    EOF,
    /// Couldn't decode character.
    InvalidChar,
}

/// Strongly typed identifier of `Bookmark`
#[derive(Debug,Clone,Copy)]
pub struct BookmarkId {
    #[allow(missing_docs)]
    id: usize
}

/// Bookmarks a specific character in buffer, so that `LazyReader` can return to it when needed.
#[derive(Debug,Clone,Copy)]
pub struct Bookmark {
    /// The position of bookmarked character in `reader.buffer`.
    offset: usize,
}

/// The default size of buffer.
pub const BUFFER_SIZE: usize = 32768;

/// A buffered reader able to efficiently read big inputs in constant memory.
///
/// It supports various encodings via `Decoder` and also bookmarks which allow it to return
/// back to a character at specific offset.
#[derive(Debug,Clone)]
pub struct Reader<D:Decoder,Read> {
    /// The reader that holds the input.
    pub reader: Read,
    /// The buffer that stores the input data.
    pub buffer: Vec<D::Word>,
    /// The buffer offset of the current element read.
    pub offset: usize,
    /// The number of elements stored in buffer.
    pub length: usize,
    /// Flag that is true iff the reader was just rewinded and no new chars were read.
    pub rewinded: bool,
    /// Bookmarks allow reader to return to a character at specific offset.
    pub bookmark: Vec<Bookmark>,
    /// The last character read.
    pub character: decoder::Char,
}

impl<D:Decoder,R: Read<Item=D::Word>> Reader<D,R> {
    /// Returns new instance of `LazyReader`.
    pub fn new(reader:R, _decoder:D, bookmarks:usize) -> Self {
        let mut reader = Reader::<D,R> {
            reader,
            buffer    : vec![D::Word::default(); BUFFER_SIZE],
            offset    : 0,
            length    : 0,
            rewinded  : false,
            bookmark  : vec![Bookmark{offset:0};bookmarks],
            character : decoder::Char{char:None, size:0},
        };
        reader.length = reader.reader.read(&mut reader.buffer[..]);
        reader
    }

    /// Bookmarks the current character, so that the reader can return to it later with `rewind()`.
    pub fn bookmark(&mut self, bookmark:BookmarkId) {
        self.bookmark[bookmark.id].offset = self.offset - self.character.size;
    }

    /// Returns to the bookmarked character.
    pub fn rewind(&mut self, bookmark:BookmarkId) {
        self.offset = self.bookmark[bookmark.id].offset;
        let _ = self.next_char();
        self.rewinded = true;
    }

    /// How many words could be rewinded
    fn max_possible_rewind_len(&self) -> usize {
        if let Some(offset) = self.bookmark.iter().map(|b| b.offset).min() {
            return self.buffer.len() - offset
        }
        D::MAX_CODEPOINT_LEN
    }

    /// Decrease the offset all bookmarks.
    pub fn decrease_offset(&mut self, off:usize) {
        for bookmark in self.bookmark.iter_mut() {
            bookmark.offset -= off
        }
    }

    /// Fill the buffer with words from input.
    pub fn fill(&mut self) {
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

    /// Is the reader empty.
    pub fn empty(&self) -> bool {
        self.length < self.buffer.len() && self.length <= self.offset
    }

    /// Reads the next char from input.
    pub fn next_char(&mut self) -> Result<char,Error> {
        if self.empty() { return Err(Error::EOF) }

        if self.offset >= self.buffer.len() - D::MAX_CODEPOINT_LEN {
            self.fill();
        }

        self.character = D::decode(&self.buffer[self.offset..]);
        self.rewinded  = false;
        self.offset   += self.character.size;

        match self.character.char {
            Some(char) => Ok(char),
            None       => Err(Error::InvalidChar)
        }
    }
}


// === Trait Impls ===

impl From<Error> for u32 {
    fn from(error: Error) -> Self {
        match error {
            Error::EOF         => u32::max_value(),
            Error::InvalidChar => u32::max_value() - 1,
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
        let mut reader = Reader::new(str.as_bytes(), DecoderUTF8(), 0);
        let mut result = String::from("");
        while let Ok(char) = reader.next_char() {
            result.push(char);
        }
        assert_eq!(&result, str);
    }

    #[test]
    fn test_reader_big_input() {
        let     str    = "a.b^c! #𤭢界んにち𤭢#𤭢".repeat(10_000);
        let mut reader = Reader::new(str.as_bytes(), DecoderUTF8(), 0);
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
            let mut reader = Reader::new(str, DecoderUTF8(), 0);
            let mut count  = 0;
            while reader.next_char() != Err(Error::EOF) {
                count += 1;
            }
            count
        };
        bencher.iter(run);
    }
}
