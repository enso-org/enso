//! Defines a Symbol that is operated on by the finite automata.



// ==============
// === Symbol ===
// ==============

// TODO [AA] Should this be `char` instead? Char holds _valid_ unicode literal values
//  If we don't make this change should we redefine the API to only be in terms of characters
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

    // TODO [AA] This has issues in that `char.MAX` is defined in a PUA
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
