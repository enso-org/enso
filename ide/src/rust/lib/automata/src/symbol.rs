//! Defines a Symbol that is operated on by the finite automata.

use crate::prelude::*;



// ==============
// === Symbol ===
// ==============

/// An input symbol to a finite automaton.
#[derive(Clone,Debug,PartialEq,Eq,PartialOrd,Ord,Hash)]
#[allow(missing_docs)]
pub struct Symbol {
    pub index : u64,
    pub name  : String
}

impl Symbol {
    /// End of line symbol.
    pub fn eof() -> Self {
        Self::new(u64::max_value())
    }

    /// Invalid symbol.
    pub fn invalid() -> Self {
        Self::new(u64::min_value() + 1)
    }

    /// Null symbol.
    pub fn null() -> Self {
        Self::new(0)
    }

    /// Constructor.
    pub fn new(index:u64) -> Self {
        let name = "unnamed".into();
        Self {index,name}
    }

    /// Named constructor.
    pub fn new_named(index:u64, name:impl Into<String>) -> Self {
        let name = name.into();
        Self {index,name}
    }

    /// Next symbol, if any.
    pub fn next(&self) -> Option<Self> {
        self.index.checked_add(1).map(Self::new)
    }
}


// === Impls ===

impl Display for Symbol {
    fn fmt(&self, f:&mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f,"{}",self.name)
    }
}

impl Default for Symbol {
    fn default() -> Self {
        Symbol::null()
    }
}

impl From<u64> for Symbol {
    fn from(index:u64) -> Symbol {
        Symbol::new(index)
    }
}

impl From<char> for Symbol {
    fn from(ch:char) -> Symbol {
        Symbol::new_named(ch as u64,format!("{}",ch))
    }
}

impl From<&Symbol> for Symbol {
    fn from(symbol:&Symbol) -> Self {
        symbol.clone()
    }
}
