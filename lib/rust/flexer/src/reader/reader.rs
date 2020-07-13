use crate::reader::reader_utf8::Reader;
use crate::reader::reader_utf8::ReaderUTF;

#[derive(Debug,Clone,Copy)]
pub struct RewinderId {
    id : usize
}

#[derive(Debug,Clone,Copy)]
pub struct Rewinder {
    length : usize,
    offset : usize,
}

#[derive(Debug,Clone,Copy,Default)]
pub struct Repeater {
    byte   : u8,
    repeat : usize,
}

#[derive(Debug,Clone)]
pub struct FlexReader {
    reader   : Reader,
    rewinded : bool,
    rewinder : [Rewinder; 2],
    repeater : Repeater,
}

impl FlexReader {
    const TAB_SIZE : usize      = 4;
    const MATCH    : RewinderId = RewinderId{id:0};
    const RULE     : RewinderId = RewinderId{id:0};

    fn new() -> FlexReader {
        let reader   = Reader::new();
        let rewinder = Rewinder{length:0,offset:reader.length};
        FlexReader {
            reader,
            rewinded : false,
            rewinder : [rewinder,rewinder],
            repeater : Repeater::default(),
        }
    }


    fn set(&mut self, rewinder:RewinderId) {
        self.rewinder[rewinder.id].offset = self.char_offset();
        self.rewinder[rewinder.id].length = self.result.length;
    }

    fn run(&mut self, rewinder:RewinderId) {
        self.result.set_length(self.rewinder[rewinder.id].offset);
        self.offset = self.rewinder[rewinder.id].length;
        self.next_char();
        self.rewinded = true
    }

    fn next_byte_helper(&mut self) -> Option<u8> {
        if self.repeater.repeat == 0 {
            let repeated_byte = match (self.repeater.byte, self.reader.next_byte()?) {
                (b'\r', b'\n') => (self.next_byte_helper()?, 1             ),
                (_    , b'\t') => (b'\t'                   , Self::TAB_SIZE),
                (_    , byte ) => (byte                    , 1             ),
            };
            (self.repeater.byte, self.repeater.repeat) = repeated_byte;
        }
        self.repeater.repeat -= 1;
        Some(self.repeater.byte)
    }

    fn char_offset(&self) -> usize {
        self.offset - self.char_size
    }

    fn max_rewind_offset(&self) -> usize {
        self.rewinder[Self::MATCH].offset
    }

    fn decrease_offset(&mut self, off:usize) {
        for rewinder in self.rewinder.iter_mut() {
            rewinder.offset -= off
        }
    }
}

impl ReaderUTF for FlexReader {
    fn fill(&mut self, off:usize) {
        if self.max_rewind_offset() == 0 {
            panic!("Rewind is impossible. Buffer is too small.")
        }
        let keepchars = self.reader.length - self.max_rewind_offset();
        self.decrease_offset(length - keepchars);
        for i in 1..keepchars {
            buffer(keepchars - i) = buffer(length - i);
        }
        self.reader.fill(keepchars)
    }

    fn empty(&self) -> bool {
        self.copyByte == 0 && self.reader.empty
    }

    fn next_byte(&mut self) -> Option<u8> {
        let byte = match self.next_byte_helper()? {
            b'\r' => b'\n',
            b'\t' => b' ',
            byte => byte,
        };
        Some(byte)
    }

    fn next_char(&mut self) -> char {
        self.rewind.rewinded = false;
        self.reader.next_char()
    }
}
