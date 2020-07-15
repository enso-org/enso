// use crate::reader::reader::LazyReader;
// use crate::reader::reader::Reader;
// use crate::reader::decoder::Decoder;
//
//
//
// // ====================
// // === Preprocessor ===
// // ====================
//
// /// Utility function to allow using characters in pattern matching on u32 variables.
// const fn u32(char:char) -> u32 {
//     char as u32
// }
//
// /// Struct that
// #[derive(Debug,Clone,Default)]
// pub struct Preprocessor<D:Decoder,R> {
//     symbol : u32,
//     repeat : usize,
//     reader : LazyReader<D,R>,
// }
//
// impl<D:Decoder,R:Reader<Item=D::Word>> Preprocessor<D,R> {
//     pub fn new(reader:LazyReader<D,R>) -> Self {
//         Self {symbol:0, repeat:0, reader}
//     }
//
//     pub fn read(&mut self) -> u32 {
//         self.symbol = match (self.symbol, self.reader.next_symbol()) {
//             (_        , u32('\t')) => {self.repeat = Self::TAB_SIZE; u32('\t')}
//             (u32('\r'), u32('\n')) => self.read(),
//             (_        , symbol   ) => symbol,
//         };
//         self.symbol
//     }
//
//     pub fn next_symbol(&mut self) -> Option<u32> {
//         let symbol = match self.read() {
//             u32('\r') => u32('\n'),
//             u32('\t') => u32( ' '),
//             symbol    => symbol,
//         };
//         Some(symbol)
//     }
// }