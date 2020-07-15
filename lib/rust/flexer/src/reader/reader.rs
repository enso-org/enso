use crate::reader::decoder::Decoder;



// ==============
// === Reader ===
// ==============

/// Trait for reading input data into a buffer.
pub trait Reader {
    /// The type of the data in the buffer.
    type Item;

    /// Fills the buffer and returns amount of elements read.
    fn read(&mut self, buffer:&mut [Self::Item]) -> usize;
}



// ================
// === Rewinder ===
// ================

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



/// ==================
/// === LazyReader ===
/// ==================

/// A buffered reader able to read big inputs in constant memory.
///
/// It can read any input which can be decoded with a `Decoder`.
#[derive(Debug,Clone)]
pub struct LazyReader<D:Decoder,Reader> {
    /// The reader that holds the input.
    reader: Reader,
    /// The buffer that stores the input data.
    buffer: Vec<D::Word>,
    /// Flag that is true iff the reader was just rewinded and no new symbols were read.
    rewinded: bool,
    /// Rewinders allow reader to return to a character at specific offset.
    rewinder: [Rewinder; 2],
}

impl<D:Decoder,R:Reader<Item=D::Word>> LazyReader<D,R> {
    /// Symbol for end of input.
    const END_OF_INPUT: u32  = 0;
    /// The default size of buffer.
    const BUFFER_SIZE: usize = 32768;
    /// The identifier of rewinder called `match`.
    const MATCH: RewinderId = RewinderId{id:0};
    /// The identifier of rewinder called `rule`.
    const RULE: RewinderId = RewinderId{id:1};

    /// Returns new instance of `LazyReader`.
    fn new(reader:R) -> Self {
        let rewinder = Rewinder{length:0,offset:Self::BUFFER_SIZE};
        LazyReader {
            reader,
            buffer   : Vec::with_capacity(Self::BUFFER_SIZE),
            rewinded : false,
            rewinder : [rewinder,rewinder],
        }
    }

    /// Saves the current state of `LazyReader` in the specified `Rewinder`.
    fn rewind_here(&mut self, rewinder:RewinderId) {
        self.rewinder[rewinder.id].offset = self.offset - self.symbol_len;
        self.rewinder[rewinder.id].length = self.result.length;
    }

    /// Overwrites current state with the state saved in the specified `Rewinder`.
    fn rewind(&mut self, rewinder:RewinderId) {
        self.result.set_length(self.rewinder[rewinder.id].offset);
        self.offset = self.rewinder[rewinder.id].length; // ???
        self.next_symbol();
        self.rewinded = true
    }

    /// How many words could be rewinded when `self.rewind` is called.
    fn max_possible_rewind_len(&self) -> usize {
        self.length - self.rewinder[Self::MATCH.id].offset
    }

    /// Decrease the offset all rewinders.
    fn decrease_offset(&mut self, off:usize) {
        for rewinder in self.rewinder.iter_mut() {
            rewinder.offset -= off
        }
    }

    /// Fill the buffer with words from input.
    fn fill(&mut self, off:usize) {
        self.offset = self.max_possible_rewind_len();
        if self.offset == self.buffer.len() {
            panic!("Rewind won't be possible. Buffer is too small.")
        }
        self.decrease_offset(self.length - self.offset);
        for i in 1..=self.offset {
            self.buffer[self.offset - i] = self.buffer[self.length - i];
        }
        self.length = self.offset + self.reader.read(&mut self.buffer[self.offset..]);
    }

    /// Is the reader empty.
    fn empty(&self) -> bool {
        self.length < Self::BUFFER_SIZE && self.length <= self.offset
    }

    /// Reads the next symbol from input.
    pub fn next_symbol(&mut self) -> u32 {
        if self.empty() { return Self::END_OF_INPUT }

        if self.offset >= self.buffer.len() - D::MAX_SYMBOL_LEN {
            self.fill(0);
        }

        let (symbol, length) = D::decode(&self.buffer[self.offset..]);

        self.rewinded   = false;
        self.symbol_len = length;

        symbol
    }
}
