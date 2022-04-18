#![feature(allocator_api)]
#![feature(slice_index_methods)]
#![feature(test)]
#![feature(generic_associated_types)]
#![recursion_limit = "256"]

use crate::prelude::*;

pub mod lexer;

pub mod prelude {
    pub use enso_prelude::*;
}


use lexer::Lexer;

fn main() {
    let str = "\n  foo";
    let mut lexer = Lexer::new(str);
    println!("{:#?}", lexer.run());
    println!("{:#?}", lexer.output);
}
