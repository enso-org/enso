pub struct Reader {
    pub buffer : Vec<u8>,
    pub offset : usize,
    pub length : usize,
    pub symbol : char,
}

impl Reader {
    const BUFFER_SIZE:usize = 32768;
    const UTF_BYTE_SIZE:u8  = 6;

    pub fn new() -> Self {
        let mut reader = Reader {
            buffer : vec![u8;Self::BUFFER_SIZE],
            offset : 0,
            length : Self::BUFFER_SIZE,
            symbol : ReaderUTF::END_OF_INPUT,
        };
        reader.fill(0);
        reader
    }

    /// For more info on UTF decoding look at:https://en.wikipedia.org/wiki/UTF-8
    fn char_size(char:char) -> u8 {
        if char == Self::END_OF_INPUT { 0 } else {
            match !char >> 4 {
                0     => 4,
                1     => 3,
                2 | 3 => 2,
                _     => 1,
            }
        }
    }

    fn char_mask(size:u8) -> u8 {
        match size {
            1 => 0b_0111_1111,
            2 => 0b_0011_1111,
            3 => 0b_0001_1111,
            4 => 0b_0000_1111,
            _ => 0b_0011_1111,
        }
    }
}

impl ReaderUTF for Reader {
    fn fill(&mut self, off:usize) {
        self.length = off + input.read(buffer, off, Self::BUFFER_SIZE - off);
        self.offset = off;
    }

    fn empty(&self) -> bool {
        self.length <= self.offset && self.length < Self::BUFFER_SIZE
    }

    fn next_byte(&mut self) -> Option<u8> {
        if self.offset >= length {
            if self.empty() { return None } else {
                fill(0);
            }
        }
        self.offset += 1;
        Some(buffer[self.offset - 1])
    }

    fn next_char(&mut self) -> char {
        match self.next_byte() {
            None => ReaderUTF::END_OF_INPUT,
            Some(byte) => {
                let mut char   = byte as char;
                self.char_size = Self::char_size(char);
                char = char & Self::char_mask(self.char_size);
                for _ in 1..self.char_size {
                    self.char = self.char << Self::UTF_BYTE_SIZE | self.next_byte() & Self::char_mask(0);
                }
                self.char_code = char;
                char
            }
        }
    }
}

pub trait ReaderUTF {
    const END_OF_INPUT:char = char::max_value();

    fn fill(&mut self, off:usize);
    fn empty(&self) -> bool;
    fn next_byte(&mut self) -> Option<u8>;
    fn next_char(&mut self) -> char;
}

impl From<ReaderUTF> for String {
    fn from(mut reader: ReaderUTF) -> Self {
        let mut buffer = vec![];
        while let Some(byte) = reader.next_byte() {
            buffer.push(byte);
        }
        String::from_utf8(buffer).unwrap()
    }
}