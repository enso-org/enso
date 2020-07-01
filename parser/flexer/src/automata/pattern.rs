//! Simple API for constructing regex patterns that are used in parser implementation.

use crate::parser;
use crate::automata::state::Symbol;

use core::iter;
use itertools::Itertools;
use std::ops::BitAnd;
use std::ops::BitOr;
use std::ops::RangeInclusive;


// =============
// == Pattern ==
// =============

/// Simple regex pattern.
#[derive(Clone,Debug)]
pub enum Pattern {
    /// Pattern that triggers on any symbol from given range.
    Range(RangeInclusive<Symbol>),
    /// Pattern that triggers on any given pattern from sequence.
    Or(Vec<Pattern>),
    /// Pattern that triggers when a sequence of patterns is encountered.
    And(Vec<Pattern>),
    /// Pattern that triggers on 0..N repetitions of given pattern.
    Many(Box<Pattern>)
}

use Pattern::*;

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

impl Pattern {

    /// Pattern that never triggers.
    pub fn never() -> Self {
        Pattern::symbols(1..=0)
    }

    /// Pattern that always triggers.
    pub fn always() -> Self {
        Pattern::symbols(u32::min_value()..=u32::max_value())
    }

    /// Pattern that triggers on any char.
    pub fn any_char() -> Self {
        Pattern::symbols(0..=u32::max_value())
    }

    /// Pattern that triggers on 0..N repetitions of given pattern.
    pub fn many(self) -> Self {
        Many(Box::new(self))
    }

    /// Pattern that triggers on 1..N repetitions of given pattern.
    pub fn many1(self) -> Self {
        self.clone() & self.many()
    }

    /// Pattern that triggers on 0..=1 repetitions of given pattern.
    pub fn opt(self) -> Self {
        self | Self::always()
    }

    /// Pattern that triggers on given symbol
    pub fn symbol(symbol:u32) -> Self {
        Pattern::symbols(symbol..=symbol)
    }

    /// Pattern that triggers on any of the given symbols.
    pub fn symbols(symbols:RangeInclusive<u32>) -> Self {
        Pattern::Range(Symbol{val:*symbols.start()}..=Symbol{val:*symbols.end()})
    }

    /// Pattern that triggers on end of file.
    pub fn eof() -> Self {
        Self::symbol(parser::EOF_CODE.val)
    }

    /// Pattern that triggers on given character.
    pub fn char(char:char) -> Self {
        Self::symbol(char as u32)
    }

    /// Pattern that triggers on any of the given characters.
    pub fn range(chars:RangeInclusive<char>) -> Self {
        Pattern::symbols((*chars.start() as u32)..=(*chars.end() as u32))
    }

    /// Pattern that triggers when sequence of characters is encountered.
    pub fn all(chars:&str) -> Self {
        chars.chars().fold(Self::never(), |pat,char| pat & Self::char(char))
    }

    /// Pattern that triggers on any characters from given sequence.
    pub fn any(chars:&str) -> Self {
        chars.chars().fold(Self::never(), |pat,char| pat | Self::char(char))
    }

    /// Pattern that doesn't trigger on any given character from given sequence.
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

    /// Pattern that triggers on any character but the one given.
    pub fn not(char:char) -> Self {
        Self::none(&char.to_string())
    }

    /// Pattern that triggers on N repetitions of given pattern.
    pub fn repeat(pat:Pattern, num:usize) -> Self {
        (0..num).fold(Self::always(), |p,_| p & pat.clone())
    }

    /// Pattern that triggers on MIN..MAX repetitions of given pattern.
    pub fn repeat_between(pat:Pattern, min:usize, max:usize) -> Self {
        (min..max).fold(Self::never(), |p,n| p | Self::repeat(pat.clone(),n))
    }
}
