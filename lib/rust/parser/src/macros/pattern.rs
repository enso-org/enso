//! This module defines patterns Pattern used to validate incoming token stream against expected
//! macro input.

use crate::prelude::*;

use crate::syntax;

use std::collections::VecDeque;



// ===============
// === Pattern ===
// ===============

/// Patterns are used to validate incoming token stream against expected macro input.
///
/// The idea is similar to patterns used in macro rules in Rust with a few differences:
/// 1. These patterns allow for other constructs than macro rules.
/// 2. The macro resolution never reifies tokens as given types, which means that every defined
///    pattern behaves like a TT-muncher in Rust.
#[derive(Clone, Debug, Deref)]
#[allow(missing_docs)]
pub struct Pattern {
    #[deref]
    pub data:                Rc<PatternData>,
    pub matches_empty_input: bool,
}

impl Pattern {
    /// Constructor.
    pub fn new(data: PatternData, matches_empty_input: bool) -> Self {
        Self { data: Rc::new(data), matches_empty_input }
    }
}

/// Variants of [`Pattern`].
#[derive(Clone, Debug)]
#[allow(missing_docs)]
pub enum PatternData {
    /// Consume all items, till the end of the token stream.
    Everything,
    /// Consume nothing.
    Nothing,
    /// Consume items matching the first pattern. If the match was unsuccessful, the second match
    /// will be tried.
    Or(Pattern, Pattern),
    Seq(Pattern, Pattern),
    /// Consume many times (zero or more) the given pattern. If the given pattern succeeds on empty
    /// input, it will be repeated as long as it consumes any input.
    Many(Pattern),
    /// Consume an identifier.
    Identifier,
    /// Consume a block and run the provided pattern in its body.
    Block(Pattern),
    /// Indicator of an error. The provided pattern is used to consume input when an error occurs.
    /// For example, if you want to consume an identifier, but the identifier is not found, you can
    /// use this pattern to consume any token instead and mark it as invalid.
    Expected(String, Pattern),
    /// Named pattern. Mainly used for splicing the code in the macro definition body.
    Named(String, Pattern),
    /// Anything that is not a block.
    NotBlock,
}

/// Constructor.
pub fn everything() -> Pattern {
    Pattern::new(PatternData::Everything, true)
}

/// Constructor.
pub fn identifier() -> Pattern {
    Pattern::new(PatternData::Identifier, false)
}

/// Constructor.
pub fn not_block() -> Pattern {
    Pattern::new(PatternData::NotBlock, false)
}

/// Constructor.
pub fn nothing() -> Pattern {
    Pattern::new(PatternData::Nothing, true)
}

/// Constructor.
pub fn or(fst: Pattern, snd: Pattern) -> Pattern {
    let matches_empty_input = fst.matches_empty_input || snd.matches_empty_input;
    Pattern::new(PatternData::Or(fst, snd), matches_empty_input)
}

/// Constructor.
pub fn seq(fst: Pattern, snd: Pattern) -> Pattern {
    let matches_empty_input = fst.matches_empty_input && snd.matches_empty_input;
    Pattern::new(PatternData::Seq(fst, snd), matches_empty_input)
}

/// Constructor.
pub fn many(item: Pattern) -> Pattern {
    Pattern::new(PatternData::Many(item), true)
}

/// Constructor.
pub fn block(body: Pattern) -> Pattern {
    Pattern::new(PatternData::Block(body), false)
}

/// Constructor.
pub fn expected(message: impl Into<String>, item: Pattern) -> Pattern {
    let matches_empty_input = item.matches_empty_input;
    Pattern::new(PatternData::Expected(message.into(), item), matches_empty_input)
}

/// Constructor.
pub fn named(message: impl Into<String>, item: Pattern) -> Pattern {
    let matches_empty_input = item.matches_empty_input;
    Pattern::new(PatternData::Named(message.into(), item), matches_empty_input)
}

impl Pattern {
    /// Repeat the current pattern multiple times.
    pub fn many(self) -> Self {
        many(self)
    }

    /// Match self or consume any token that is not a block and mark it as invalid.
    pub fn expect(self, message: impl Into<String>) -> Self {
        self | expected(message, not_block() | nothing())
    }

    /// Match self or consume any token that is not a block and mark it as invalid.
    pub fn named(self, label: impl Into<String>) -> Self {
        named(label, self)
    }
}

/// The syntax `pattern1 >> pattern2` is a shortcut for `seq(pattern1, pattern2)`.
impl std::ops::Shr for Pattern {
    type Output = Pattern;
    fn shr(self, rhs: Pattern) -> Self::Output {
        seq(self, rhs)
    }
}

/// The syntax `pattern1 | pattern2` is a shortcut for `or(pattern1, pattern2)`.
impl std::ops::BitOr for Pattern {
    type Output = Pattern;
    fn bitor(self, rhs: Pattern) -> Self::Output {
        or(self, rhs)
    }
}

/// The syntax `pattern % "message"` is a shortcut for `pattern.expect("message")`.
impl<T: Into<String>> std::ops::Rem<T> for Pattern {
    type Output = Pattern;
    fn rem(self, message: T) -> Self::Output {
        self.expect(message)
    }
}

/// The syntax `pattern / "label"` is a shortcut for `pattern.named("label")`.
impl<T: Into<String>> Div<T> for Pattern {
    type Output = Pattern;
    fn div(self, message: T) -> Self::Output {
        named(message, self)
    }
}



// =============
// === Match ===
// =============

/// The result of applying [`Pattern`] to a token stream. After a successful match, a variant of the
/// [`Pattern`] is transformed to variant of [`Match`] of the same name.
#[derive(Clone, Debug)]
#[allow(missing_docs)]
pub enum Match<'s> {
    Everything(VecDeque<syntax::Item<'s>>),
    Nothing,
    Or(Box<OrMatch<'s>>),
    Seq(Box<Match<'s>>, Box<Match<'s>>),
    Many(Vec<Match<'s>>),
    Identifier(syntax::Item<'s>),
    Expected(String, Box<Match<'s>>),
    Named(String, Box<Match<'s>>),
    NotBlock(syntax::Item<'s>),
}

/// The result of the [`Pattern::Or`] resolution.
#[derive(Clone, Debug)]
#[allow(missing_docs)]
pub enum OrMatch<'s> {
    First(Match<'s>),
    Second(Match<'s>),
}

impl<'s> Match<'s> {
    /// Constructor.
    pub fn or(m: OrMatch<'s>) -> Self {
        Self::Or(Box::new(m))
    }

    /// Constructor.
    pub fn seq(first: Match<'s>, second: Match<'s>) -> Self {
        Self::Seq(Box::new(first), Box::new(second))
    }

    /// Constructor.
    pub fn expected(expected: impl Into<String>, second: Match<'s>) -> Self {
        Self::Expected(expected.into(), Box::new(second))
    }

    /// Constructor.
    pub fn named(label: impl Into<String>, second: Match<'s>) -> Self {
        Self::Named(label.into(), Box::new(second))
    }

    /// Get all tokens of the match.
    pub fn tokens(self) -> Vec<syntax::Item<'s>> {
        match self {
            Self::Everything(tokens) => tokens.into(),
            Self::Nothing => default(),
            Self::Seq(fst, snd) => fst.tokens().extended(snd.tokens()),
            Self::Many(t) => t.into_iter().flat_map(|s| s.tokens()).collect(),
            Self::Identifier(ident) => vec![ident],
            Self::Expected(_, item) => item.tokens(),
            Self::Named(_, item) => item.tokens(),
            Self::NotBlock(item) => vec![item],
            Self::Or(t) => match *t {
                OrMatch::First(fst) => fst.tokens(),
                OrMatch::Second(snd) => snd.tokens(),
            },
        }
    }
}



// ===================
// === MatchResult ===
// ===================

/// Result of a successful pattern resolution. It contains a match and the remaining token stream.
#[derive(Debug)]
#[allow(missing_docs)]
pub struct MatchResult<'s> {
    pub matched: Match<'s>,
    pub rest:    VecDeque<syntax::Item<'s>>,
}

impl<'s> MatchResult<'s> {
    /// Constructor.
    pub fn new(matched: Match<'s>, rest: VecDeque<syntax::Item<'s>>) -> Self {
        Self { matched, rest }
    }

    /// Map the match with the provided function.
    pub fn map(mut self, f: impl FnOnce(Match<'s>) -> Match<'s>) -> Self {
        self.matched = f(self.matched);
        self
    }
}



// ======================
// === MatchedSegment ===
// ======================

/// List of matched segments.
pub type MatchedSegments<'s> = NonEmptyVec<MatchedSegment<'s>>;

/// A matched segment. See the [`macros::resolver::Segment`] to learn more.
#[derive(Clone, Debug)]
#[allow(missing_docs)]
pub struct MatchedSegment<'s> {
    pub header: syntax::Token<'s>,
    pub result: Match<'s>,
}

impl<'s> MatchedSegment<'s> {
    /// Constructor.
    pub fn new(header: syntax::Token<'s>, result: Match<'s>) -> Self {
        Self { header, result }
    }
}



// ==========================
// === Pattern Resolution ===
// ==========================

impl Pattern {
    /// Resolve the pattern. Return [`MatchResult`] if the pattern is matched, otherwise all the
    /// input tokens.
    pub fn resolve<'s>(
        &self,
        mut input: VecDeque<syntax::Item<'s>>,
    ) -> Result<MatchResult<'s>, VecDeque<syntax::Item<'s>>> {
        match &*self.data {
            PatternData::Expected(msg, item) =>
                item.resolve(input).map(|t| t.map(|s| Match::expected(msg, s))),
            PatternData::Named(msg, item) =>
                item.resolve(input).map(|t| t.map(|s| Match::named(msg, s))),
            PatternData::Everything => Ok(MatchResult::new(Match::Everything(input), default())),
            PatternData::Nothing => Ok(MatchResult::new(Match::Nothing, input)),
            PatternData::Or(fst, snd) => fst
                .resolve(input)
                .map(|t| t.map(|s| Match::or(OrMatch::First(s))))
                .or_else(|t| snd.resolve(t).map(|t| t.map(|s| Match::or(OrMatch::Second(s))))),
            PatternData::Seq(fst, snd) => fst
                .resolve(input)
                .and_then(|t| snd.resolve(t.rest).map(|s| s.map(|x| Match::seq(t.matched, x)))),
            PatternData::Many(pat) => {
                let mut out = vec![];
                let mut input_len = input.len();
                loop {
                    match pat.resolve(input) {
                        Err(rest) => {
                            input = rest;
                            break;
                        }
                        Ok(t) => {
                            input = t.rest;
                            if pat.matches_empty_input {
                                let no_input_consumed = input_len == input.len();
                                if no_input_consumed {
                                    break;
                                }
                                input_len = input.len();
                            }
                            out.push(t.matched);
                        }
                    }
                }
                Ok(MatchResult::new(Match::Many(out), input))
            }
            PatternData::Identifier => match input.pop_front() {
                None => Err(default()),
                Some(t) =>
                    if t.is_variant(syntax::token::variant::VariantMarker::Ident) {
                        Ok(MatchResult::new(Match::Identifier(t), input))
                    } else {
                        input.push_front(t);
                        Err(input)
                    },
            },
            PatternData::Block(body) => match input.pop_front() {
                Some(syntax::Item::Block(tokens)) =>
                    body.resolve(tokens.into_iter().rev().map_into().collect()),
                Some(t) => {
                    input.push_front(t);
                    Err(input)
                }
                None => Err(default()),
            },
            PatternData::NotBlock => match input.pop_front() {
                Some(t @ syntax::Item::Block(_)) => {
                    input.push_front(t);
                    Err(input)
                }
                None => Err(default()),
                Some(t) => Ok(MatchResult::new(Match::NotBlock(t), input)),
            },
        }
    }
}
