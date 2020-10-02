//! Simple API for constructing regex patterns that are used in parser implementation.

use crate::prelude::*;

use crate::symbol::Symbol;

use core::iter;
use std::ops::BitOr;
use std::ops::RangeInclusive;
use std::ops::Shr;



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
    Seq(Vec<Pattern>),
    /// The pattern that triggers on 0..N repetitions of given pattern.
    Many(Box<Pattern>),
    /// The pattern that always triggers without consuming any input.
    Always,
    /// The pattern that never triggers and does not consume any input.
    Never,
}

impl Pattern {

    /// A pattern that never triggers.
    pub fn never() -> Self {
        Pattern::Never
    }

    /// A pattern that always triggers
    pub fn always() -> Self {
        Pattern::Always
    }

    /// A pattern that triggers on any character.
    pub fn any() -> Self {
        Pattern::symbols(Symbol::new_named(0,"MIN")..=Symbol::new_named(u64::max_value(),"MAX"))
    }

    /// A pattern that triggers on the minimum value.
    pub fn min() -> Self {
        Pattern::symbol(&Symbol::new_named(0,"MIN"))
    }

    /// A pattern that triggers on the minimum value.
    pub fn max() -> Self {
        Pattern::symbol(&Symbol::new_named(u64::max_value(),"MAX"))
    }

    /// A pattern that triggers on 0..N repetitions of the pattern described by `self`.
    pub fn many(&self) -> Self {
        Pattern::Many(Box::new(self.clone()))
    }

    /// A pattern that triggers on 1..N repetitions of the pattern described by `self`.
    pub fn many1(&self) -> Self {
        self.clone() >> self.many()
    }

    /// A pattern that triggers on 0..=1 repetitions of the pattern described by `self`.
    pub fn opt(&self) -> Self {
        self | Self::always()
    }

    /// A pattern that triggers on the given character.
    pub fn char(character:char) -> Self {
        Self::symbol(&Symbol::from(character))
    }

    /// A pattern that triggers on the given symbol.
    pub fn symbol(symbol:&Symbol) -> Self {
        Pattern::symbols(symbol.clone()..=symbol.clone())
    }

    /// A pattern that triggers on any of the provided `symbols`.
    pub fn symbols(symbols:RangeInclusive<Symbol>) -> Self {
        Pattern::Range(symbols)
    }

    /// A pattern that triggers at the end of the file.
    pub fn eof() -> Self {
        Self::symbol(&Symbol::eof())
    }

    /// A pattern that triggers on any character in the provided `range`.
    pub fn range(range:RangeInclusive<char>) -> Self {
        Pattern::symbols(Symbol::from(*range.start())..=Symbol::from(*range.end()))
    }

    /// Pattern that triggers when sequence of characters given by `chars` is encountered.
    pub fn all_of(chars:&str) -> Self {
        chars.chars().fold(Self::always(),|pat,char| pat >> Self::char(char))
    }

    /// The pattern that triggers on any characters contained in `chars`.
    pub fn any_of(chars:&str) -> Self {
        chars.chars().fold(Self::never(),|pat,char| pat | Self::char(char))
    }

    /// The pattern that doesn't trigger on any character contained in `chars`.
    pub fn none_of(chars:&str) -> Self {
        let max        = u64::max_value();
        let char_iter  = chars.chars().map(|char| char as u64);
        let char_iter2 = iter::once(0).chain(char_iter).chain(iter::once(max));
        let mut codes  = char_iter2.collect_vec();

        codes.sort();
        codes.iter().tuple_windows().fold(Self::never(),|pat,(prev_code,next_code)| {
            let start = prev_code + 1;
            let end   = next_code - 1;
            if end < start {pat} else {
                pat | Pattern::symbols(Symbol::from(start)..=Symbol::from(end))
            }
        })
    }

    /// The pattern that triggers on any character but `char`.
    pub fn not(char:char) -> Self {
        Self::none_of(&char.to_string())
    }

    /// The pattern that triggers on `num` repetitions of `pat`.
    pub fn repeat(pat:Pattern, num:usize) -> Self {
        (0..num).fold(Self::always(),|p,_| p >> pat.clone())
    }

    /// Pattern that triggers on `min`..`max` repetitions of `pat`.
    pub fn repeat_between(pat:Pattern, min:usize, max:usize) -> Self {
        (min..max).fold(Self::never(),|p,n| p | Self::repeat(pat.clone(),n))
    }
}


// === Trait Impls ====

impl AsRef<Pattern> for Pattern {
    fn as_ref(&self) -> &Pattern {
        self
    }
}

impl BitOr<&Pattern> for Pattern {
    type Output = Pattern;
    fn bitor(self, rhs:&Pattern) -> Self::Output {
        self.bitor(rhs.clone())
    }
}

impl BitOr<Pattern> for &Pattern {
    type Output = Pattern;
    fn bitor(self, rhs:Pattern) -> Self::Output {
        self.clone().bitor(rhs)
    }
}

impl BitOr<&Pattern> for &Pattern {
    type Output = Pattern;
    fn bitor(self, rhs:&Pattern) -> Self::Output {
        self.clone().bitor(rhs.clone())
    }
}

impl BitOr<Pattern> for Pattern {
    type Output = Pattern;
    fn bitor(self, rhs:Pattern) -> Self::Output {
        use Pattern::*;
        match (self, rhs) {
            (Or(mut lhs), Or(    rhs)) => {lhs.extend(rhs) ; Or(lhs)},
            (Or(mut lhs), rhs        ) => {lhs.push(rhs)   ; Or(lhs)},
            (lhs        , Or(mut rhs)) => {rhs.push(lhs)   ; Or(rhs)},
            (lhs        , rhs        ) => Or(vec![lhs,rhs]),
        }
    }
}

impl Shr<&Pattern> for Pattern {
    type Output = Pattern;
    fn shr(self, rhs:&Pattern) -> Self::Output {
        self.shr(rhs.clone())
    }
}

impl Shr<Pattern> for &Pattern {
    type Output = Pattern;
    fn shr(self, rhs:Pattern) -> Self::Output {
        self.clone().shr(rhs)
    }
}

impl Shr<&Pattern> for &Pattern {
    type Output = Pattern;
    fn shr(self, rhs:&Pattern) -> Self::Output {
        self.clone().shr(rhs.clone())
    }
}

impl Shr<Pattern> for Pattern {
    type Output = Pattern;
    fn shr(self, rhs:Pattern) -> Self::Output {
        use Pattern::*;
        match (self, rhs) {
            (Seq(mut lhs), Seq(rhs))     => {lhs.extend(rhs) ; Seq(lhs)},
            (Seq(mut lhs), rhs         ) => {lhs.push(rhs)   ; Seq(lhs)},
            (lhs         , Seq(mut rhs)) => {rhs.push(lhs)   ; Seq(rhs)},
            (lhs         , rhs         ) => Seq(vec![lhs,rhs]),
        }
    }
}
