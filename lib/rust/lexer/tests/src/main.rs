#![feature(test)]
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]

//! This file tests the generated source code of the enso lexer.



// =============
// === Tests ===
// =============

fn main() {
    println!("This needs to be here because the crate isn't a library.")
}

#[cfg(test)]
mod tests {
    use lexer_generation::lexer::Lexer;



    #[test]
    fn test_lexer_generation() {
        assert_eq!(format!("{:?}", Lexer{}), "Lexer");
    }
}
