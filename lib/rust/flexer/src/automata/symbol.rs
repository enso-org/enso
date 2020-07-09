//! Defines a Symbol that is operated on by the finite automata.



// ==============
// === Symbol ===
// ==============

/// An input symbol to a finite automaton.
#[derive(Clone,Copy,Debug,PartialEq,Eq,PartialOrd,Ord,Hash)]
pub struct Symbol {
    #[allow(missing_docs)]
    pub val: u32
}

impl Symbol {
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

impl From<u32> for Symbol {
    fn from(val:u32) -> Symbol {
        Symbol{val}
    }
}

impl From<char> for Symbol {
    fn from(val:char) -> Symbol {
        Symbol{val:val as u32}
    }
}

impl From<&Symbol> for Symbol {
    fn from(symb: &Symbol) -> Self {
        Symbol{val:symb.val}
    }
}
