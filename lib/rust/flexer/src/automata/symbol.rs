//! Defines a Symbol that is operated on by the finite automata.

use std::default::Default;



// ==============
// === Symbol ===
// ==============

// TODO [AA] Extract this. Turn it into using `char`.
/// Newtype wrapper for finite automata input symbol.
#[derive(Clone,Copy,Debug,PartialEq,Eq,PartialOrd,Ord,Hash)]
pub struct Symbol {
    #[allow(missing_docs)]
    pub val: u32
}

impl Symbol {

    // === Constants ===

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
