//! Simple API for constructing regex patterns that are used in parser implementation.

use crate::automata::symbol::Symbol;

use core::iter;
use itertools::Itertools;
use std::ops::BitAnd;
use std::ops::BitOr;
use std::ops::RangeInclusive;

use Pattern::*;



// =============
// == Pattern ==
// =============

/// A representation of a simple regular pattern.
#[derive(Clone,Debug)]
pub enum Pattern {
    /// The pattern that triggers on any symbol from the given range.
    Range(RangeInclusive<Symbol>),
    /// The pattern that triggers on any given pattern from a sequence.
    Or(Vec<Pattern>),
    /// The pattern that triggers when a sequence of patterns is encountered.
    And(Vec<Pattern>),
    /// The pattern that triggers on 0..N repetitions of given pattern.
    Many(Box<Pattern>)
}

impl Pattern {

    /// A pattern that never triggers.
    pub fn never() -> Self {
        Pattern::symbols(1..=0)
    }

    /// A pattern that always triggers
    pub fn always() -> Self {
        Pattern::symbols(u32::min_value()..=u32::max_value())
    }

    /// A pattern that triggers on any character.
    pub fn any_char() -> Self {
        Pattern::symbols(0..=u32::max_value())
    }

    /// A pattern that triggers on 0..N repetitions of the pattern described by `self`.
    pub fn many(self) -> Self {
        Many(Box::new(self))
    }

    /// A pattern that triggers on 1..N repetitions of the pattern described by `self`.
    pub fn many1(self) -> Self {
        self.clone() & self.many()
    }

    /// A pattern that triggers on 0..=1 repetitions of the pattern described by `self`.
    pub fn opt(self) -> Self {
        self | Self::always()
    }

    /// A pattern that triggers on the given symbol.
    // TODO [AA] This should be `char`
    pub fn symbol(symbol:u32) -> Self {
        Pattern::symbols(symbol..=symbol)
    }

    /// A pattern that triggers on any of the provided `symbols`.
    pub fn symbols(symbols:RangeInclusive<u32>) -> Self {
        Pattern::Range(Symbol{val:*symbols.start()}..=Symbol{val:*symbols.end()})
    }

    /// A pattern that triggers at the end of the file.
    pub fn eof() -> Self {
        Self::symbol(Symbol::EOF_CODE.val)
    }

    /// A pattern that triggers on a given character
    pub fn char(char:char) -> Self {
        Self::symbol(char as u32)
    }

    /// A pattern that triggers on any character in the provided `range`.
    pub fn range(range:RangeInclusive<char>) -> Self {
        Pattern::symbols((*range.start() as u32)..=(*range.end() as u32))
    }

    // TODO [AA] This may be unnecessary once things are moved to `char
    /// Pattern that triggers when sequence of characters given by `chars` is encountered.
    pub fn all(chars:&str) -> Self {
        chars.chars().fold(Self::never(), |pat,char| pat & Self::char(char))
    }

    /// The pattern that triggers on any characters contained in `chars`.
    pub fn any(chars:&str) -> Self {
        chars.chars().fold(Self::never(), |pat,char| pat | Self::char(char))
    }

    /// The pattern that doesn't trigger on any character contained in `chars`.
    pub fn none(chars:&str) -> Self {
        let max        = u32::max_value();
        let char_iter  = chars.chars().map(|char| char as u32);
        let char_iter2 = iter::once(0).chain(char_iter).chain(iter::once(max));
        let mut codes  = char_iter2.collect_vec();

        codes.sort();
        codes.iter().tuple_windows().fold(Self::never(), |pat,(start,end)| {
            if end < start {pat} else {
                pat | Pattern::symbols(*start..=*end)
            }
        })
    }

    /// The pattern that triggers on any character but `char`.
    pub fn not(char:char) -> Self {
        Self::none(&char.to_string())
    }

    /// The pattern that triggers on `num` repetitions of `pat`.
    pub fn repeat(pat:Pattern, num:usize) -> Self {
        (0..num).fold(Self::always(), |p,_| p & pat.clone())
    }

    /// Pattern that triggers on `min`..`max` repetitions of `pat`.
    pub fn repeat_between(pat:Pattern, min:usize, max:usize) -> Self {
        (min..max).fold(Self::never(), |p,n| p | Self::repeat(pat.clone(),n))
    }
}


// === Trait Impls ====

impl BitOr<Pattern> for Pattern {
    type Output = Pattern;
    fn bitor(self, rhs: Pattern) -> Self::Output {
        match (self, rhs) {
            (Or(mut lhs), Or(    rhs)) => {lhs.extend(rhs) ; Or(lhs)},
            (Or(mut lhs), rhs        ) => {lhs.push(rhs)   ; Or(lhs)},
            (lhs        , Or(mut rhs)) => {rhs.push(lhs)   ; Or(rhs)},
            (lhs        , rhs        ) => Or(vec![lhs,rhs]),
        }
    }
}

impl BitAnd<Pattern> for Pattern {
    type Output = Pattern;
    fn bitand(self, rhs: Pattern) -> Self::Output {
        match (self, rhs) {
            (And(mut lhs), And(    rhs)) => {lhs.extend(rhs) ; And(lhs)},
            (And(mut lhs), rhs         ) => {lhs.push(rhs)   ; And(lhs)},
            (lhs         , And(mut rhs)) => {rhs.push(lhs)   ; And(rhs)},
            (lhs         , rhs         ) => And(vec![lhs,rhs]),
        }
    }
}
