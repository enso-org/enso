use crate::reader::decoder::Decoder;
use std::io::Read;



// ==============
// === Reader ===
// ==============

/// Trait for reading input data into a buffer.
pub trait Reader {
    /// The type of the data in the buffer.
    type Item;

    /// Fills the buffer and returns amount of elements read.
    ///
    /// In case it isn't possible to fill the whole buffer (happens on EOF or any kind of error),
    /// the buffer will be filled with all the data read before encountering the problem.
    fn read(&mut self, buffer:&mut [Self::Item]) -> usize;
}

impl<R:Read> Reader for R {
    type Item = u8;

    fn read(&mut self, mut buffer: &mut [Self::Item]) -> usize {
        let length = buffer.len();
        while !buffer.is_empty() {
            match self.read(buffer) {
                Err(_) => break,
                Ok(0)  => break,
                Ok(n)  => {
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

/// Strongly typed identifier of `Rewinder`
#[derive(Debug,Clone,Copy)]
pub struct RewinderId {
    #[allow(missing_docs)]
    id: usize
}

/// Struct that allows `LazyReader` to return to a character at specific offset.
#[derive(Debug,Clone,Copy)]
pub struct Rewinder {
    /// The value of `lazy_reader.offset`
    offset: usize,
    /// The value of `lazy_reader.result.len()`
    length: usize,
}

/// A buffered reader able to read big inputs in constant memory.
///
/// It can read any input which can be decoded with a `Decoder`.
#[derive(Debug,Clone)]
pub struct LazyReader<D:Decoder,Reader> {
    /// The reader that holds the input.
    reader: Reader,
    /// The buffer that stores the input data.
    buffer: Vec<D::Word>,
    /// The buffer offset of the current element read.
    offset: usize,
    /// The number of elements stored in buffer.
    length: usize,
    /// Flag that is true iff the reader was just rewinded and no new symbols were read.
    rewinded: bool,
    /// Rewinders allow reader to return to a character at specific offset.
    rewinder: [Rewinder; 2],
    /// The length of the last symbol read.
    symbol_len: usize,
}

impl<D:Decoder,R:Reader<Item=D::Word>> LazyReader<D,R> {
    /// Symbol for end of input.
    const END_OF_INPUT: u32  = 666;
    /// The default size of buffer.
    const BUFFER_SIZE: usize = 15;
    /// The identifier of rewinder called `match`.
    const MATCH: RewinderId = RewinderId{id:0};
    /// The identifier of rewinder called `rule`.
    const RULE: RewinderId = RewinderId{id:1};

    /// Returns new instance of `LazyReader`.
    fn new(reader:R, _decoder:D) -> Self {
        let     rewinder = Rewinder{length:0,offset:0};
        let mut reader   = LazyReader::<D,R> {
            reader,
            buffer     : vec![D::Word::default(); Self::BUFFER_SIZE],
            offset     : 0,
            length     : 0,
            rewinded   : false,
            rewinder   : [rewinder,rewinder],
            symbol_len : 0,
        };
        reader.length = reader.reader.read(&mut reader.buffer[..]);
        reader
    }

    /// Saves the current state of `LazyReader` in the specified `Rewinder`.
    fn rewind_here(&mut self, rewinder:usize) {
        self.rewinder[rewinder].offset = self.offset - self.symbol_len;
        // self.rewinder[rewinder].length = self.result.length;
    }

    /// Overwrites current state with the state saved in the specified `Rewinder`.
    fn rewind(&mut self, rewinder:RewinderId) {
        // self.result.set_length(self.rewinder[rewinder.id].offset);
        self.offset = self.rewinder[rewinder.id].length; // ???
        self.next_symbol();
        self.rewinded = true
    }

    /// How many words could be rewinded when `self.rewind` is called.
    fn max_possible_rewind_len(&self) -> usize {
        let rewind_off1 = self.rewinder[Self::MATCH.id].offset;
        let rewind_off2 = self.rewinder[Self::RULE.id].offset;

        self.buffer.len() - std::cmp::min(rewind_off1, rewind_off2)
    }

    /// Decrease the offset all rewinders.
    fn decrease_offset(&mut self, off:usize) {
        for rewinder in self.rewinder.iter_mut() {
            rewinder.offset -= off
        }
    }

    /// Fill the buffer with words from input.
    fn fill(&mut self) {
        let len     = self.buffer.len();
        self.offset = self.max_possible_rewind_len();
        if self.offset == len {
            panic!("Rewind won't be possible. Buffer is too small.")
        }
        println!("{:?}", self.buffer);
        self.decrease_offset(len - self.offset);
        for i in 1..=self.offset {
            self.buffer[self.offset - i] = self.buffer[len - i];
        }
        println!("{:?}", self.buffer);
        self.length = self.offset + self.reader.read(&mut self.buffer[self.offset..]);
        println!("{}, {}", self.offset, self.length);
        self.offset = self.offset - D::MAX_SYMBOL_LEN;
    }

    /// Is the reader empty.
    fn empty(&self) -> bool {
        self.length < Self::BUFFER_SIZE && self.length <= self.offset
    }

    /// Reads the next symbol from input.
    pub fn next_symbol(&mut self) -> u32 {
        if self.empty() { return Self::END_OF_INPUT }

        if self.offset >= self.buffer.len() - D::MAX_SYMBOL_LEN {
            self.fill();
        }

        let (symbol, length) = D::decode(&self.buffer[self.offset..]);

        self.rewinded   = false;
        self.symbol_len = length;
        self.offset     = self.offset + length;

        symbol
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use crate::reader::reader::*;
    use crate::reader::decoder::DecoderUTF8;


    #[derive(Debug,Clone)]
    struct Repeat<T> {
        buffer: Vec<T>,
        offset: usize,
        repeat: usize,
    }

    impl<T> Repeat<T> {
        fn new(buffer:Vec<T>, repeat:usize) -> Self {
            Repeat {buffer,repeat,offset:0}
        }
    }

    impl<T:Copy> Reader for Repeat<T> {
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
            for i in 0..repeat {
                buffer[..len].copy_from_slice(&self.buffer[..]);
                buffer = &mut buffer[len..];
            }

            if self.repeat == 0 {
                return len - self.offset + repeat*len
            }
            buffer.copy_from_slice(&self.buffer[..buffer.len()]);
            self.offset = buffer.len();
            read
        }
    }

    #[test]
    fn test_repeater_with_small_buffer() {
        let mut repeater = Repeat::new(vec![1,2,3], 1);

        let mut buffer = [0;2];
        assert_eq!(repeater.read(&mut buffer), 2);
        assert_eq!(&buffer, &[1,2]);
        assert_eq!(repeater.read(&mut buffer), 1);
        assert_eq!(&buffer, &[3,2])
    }

    #[test]
    fn test_repeater_with_big_buffer() {
        let mut repeater = Repeat::new(vec![1,2], 3);

        let mut buffer = [0;5];
        assert_eq!(repeater.read(&mut buffer), 5);
        assert_eq!(&buffer, &[1,2,1,2,1]);
        assert_eq!(repeater.read(&mut buffer), 1);
        assert_eq!(&buffer, &[2,2,1,2,1])
    }

    #[test]
    fn test_reader() {
        let     str      = "Hello, World!";
        let mut reader   = LazyReader::new(str.as_bytes(), DecoderUTF8());

        let mut result = String::from("");
        for i in 0..str.len() {
            let s = reader.next_symbol();
            result.push(std::char::from_u32(s).unwrap());
        }
        assert_eq!(&result, str);
    }

    #[test]
    fn test_readr_big_input() {
        let     str      = "ě".repeat(10);
        let mut reader   = LazyReader::new(str.as_bytes(), DecoderUTF8());

        let mut result = String::from("");
        for i in 0..str.len() {
            let s = reader.next_symbol();
            reader.rewind_here(0);
            reader.rewind_here(1);
            result.push(std::char::from_u32(s).unwrap());
        }
        assert_eq!(&result, &str);
    }
}