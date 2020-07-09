/// Definition of enso parser.
pub struct Parser {}

impl Parser {
    /// Creates a new parser.
    pub fn new() -> Self {
        Parser{}
    }

    /// Returns specialized version of this parser in string format.
    pub fn specialize(&mut self) -> String {
        String::from("#[derive(Debug)]\npub struct Parser {}")
    }
}