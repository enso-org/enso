//! Defines a Symbol that is operated on by the finite automata.

use crate::prelude::*;

use std::cmp::Ordering;



// =============
// === Types ===
// =============

/// The index type for a symbol.
pub type SymbolIndex = u32;



// ===================
// === SymbolRange ===
// ===================


#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Range {
    pub start: Symbol,
    pub end:   Symbol,
}

impl From<RangeInclusive<Symbol>> for Range {
    fn from(t: RangeInclusive<Symbol>) -> Self {
        let start = *t.start();
        let end = *t.end();
        Self { start, end }
    }
}

impl From<RangeInclusive<char>> for Range {
    fn from(t: RangeInclusive<char>) -> Self {
        let start = Symbol::from(t.start());
        let end = Symbol::from(t.end());
        Self { start, end }
    }
}

impl From<char> for Range {
    fn from(t: char) -> Self {
        Self::from(t..=t)
    }
}

impl From<&char> for Range {
    fn from(t: &char) -> Self {
        Self::from(*t)
    }
}


// ==============
// === Symbol ===
// ==============

/// An input symbol to a finite automaton.
#[derive(Copy, Clone, Debug, PartialOrd, Ord, PartialEq, Eq)]
#[allow(missing_docs)]
pub struct Symbol {
    pub index: SymbolIndex,
}



impl Symbol {
    /// End of line symbol.
    pub fn eof() -> Self {
        Self::new(SymbolIndex::MAX)
    }

    /// Invalid symbol.
    pub fn invalid() -> Self {
        Self::new(SymbolIndex::MAX - 1)
    }

    /// Null symbol.
    pub fn null() -> Self {
        Self::new(0)
    }

    /// The minimum symbol value.
    pub fn min() -> Self {
        Self::new(SymbolIndex::MIN)
    }

    /// The maximum symbol value.
    pub fn max() -> Self {
        Self::new(SymbolIndex::MAX)
    }
    //
    /// Constructor.
    pub fn new(index: SymbolIndex) -> Self {
        Self { index }
    }

    // /// Named constructor.
    // pub fn new_named(index: SymbolIndex, name: impl Into<String>) -> Self {
    //     let name = name.into();
    //     Self { index, name }
    // }

    /// Next symbol, if any.
    pub fn next(&self) -> Option<Self> {
        self.index.checked_add(1).map(Self::new)
    }
}


// === Impls ===

// impl PartialEq for Symbol {
//     fn eq(&self, other: &Self) -> bool {
//         self.index.eq(&other.index)
//     }
// }
// impl Eq for Symbol {}
//
// impl PartialOrd for Symbol {
//     fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
//         Some(self.cmp(other))
//     }
// }
// impl Ord for Symbol {
//     fn cmp(&self, other: &Self) -> Ordering {
//         self.index.cmp(&other.index)
//     }
// }
//
// impl Hash for Symbol {
//     fn hash<H: Hasher>(&self, state: &mut H) {
//         self.index.hash(state);
//     }
// }
//
impl Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            char::from_u32(self.index).map(|t| t.to_string()).unwrap_or("Invalid char".into())
        )
    }
}
//
impl Default for Symbol {
    fn default() -> Self {
        Symbol::null()
    }
}
//
impl From<u32> for Symbol {
    fn from(index: u32) -> Symbol {
        Symbol::new(index)
    }
}
//
// impl From<u32> for Symbol {
//     fn from(index: u32) -> Symbol {
//         Symbol::new(index as u64)
//     }
// }
//
impl From<char> for Symbol {
    fn from(ch: char) -> Symbol {
        Symbol::new(ch as u32)
    }
}

impl From<&char> for Symbol {
    fn from(ch: &char) -> Symbol {
        Symbol::from(*ch)
    }
}

impl From<&Symbol> for Symbol {
    fn from(symbol: &Symbol) -> Self {
        *symbol
    }
}



// =============
// === Tests ===
// =============
//
// #[cfg(test)]
// mod tests {
//     use super::*;
//
//     #[test]
//     fn default() {
//         let sym = Symbol::default();
//         assert_eq!(sym, Symbol::null());
//     }
//
//     #[test]
//     fn from_natural() {
//         let sym = Symbol::from(12143u64);
//         assert_eq!(sym.index, 12143u64);
//     }
//
//     #[test]
//     fn from_char() {
//         let sym = Symbol::from('a');
//         assert_eq!(sym.index, 97);
//     }
// }
