//! Defines a Symbol that is operated on by the finite automata.



// ==============
// === Symbol ===
// ==============

// TODO [AA] Should this be `char` instead? Char holds _valid_ unicode literal values, which has
//  some practical issues:
//  - `std::char::MAX` is defined in a private use area, which _may_ conflict, but due to it being
//    private use this could be fine.
//  - Not all valid `u32` values are valid `char` values, so we end up with some marshalling
//    boilerplate in some places.
/// An input symbol to a finite automaton.
#[derive(Clone,Copy,Debug,PartialEq,Eq,PartialOrd,Ord,Hash)]
pub struct Symbol {
    #[allow(missing_docs)]
    pub val: u32
}

impl Symbol {

    /// Converts a character to a symbol.
    fn from(character:char) -> Symbol {
        Symbol{val:character as u32}
    }

    /// A representation of the end of the file.
    pub const EOF_CODE:Symbol = Symbol{val:u32::max_value()};

    /// A representation of the null symbol.
    pub const NULL:Symbol = Symbol{val:0};
}


// === Trait Impls ===

impl Default for Symbol {
    fn default() -> Self {
        Symbol::NULL
    }
}
