//! Defines a Symbol that is operated on by the finite automata.



// ==============
// === Symbol ===
// ==============

/// An input symbol to a finite automaton.
#[derive(Clone,Copy,Debug,PartialEq,Eq,PartialOrd,Ord,Hash)]
pub struct Symbol {
    /// The 4-byte representation of the symbol.
    pub value:u32
}

impl Symbol {
    /// A representation of the end of the file.
    pub const EOF_CODE:Symbol = Symbol{value:u32::max_value()};

    /// A representation of the null symbol.
    pub const NULL:Symbol = Symbol{value:0};
}


// === Trait Impls ===

impl Default for Symbol {
    fn default() -> Self {
        Symbol::NULL
    }
}

impl From<u32> for Symbol {
    fn from(value:u32) -> Symbol {
        Symbol{value}
    }
}

impl From<char> for Symbol {
    fn from(value:char) -> Symbol {
        Symbol{value:value as u32}
    }
}

impl From<&Symbol> for Symbol {
    fn from(symbol:&Symbol) -> Self {
        let value = symbol.value;
        Symbol{value}
    }
}
