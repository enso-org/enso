//! This module defines patterns Pattern used to validate incoming token stream against expected
//! macro input.

use crate::prelude::*;

use crate::syntax;



// ===============
// === Pattern ===
// ===============

/// Pattern used to validate incoming token stream against expected macro input.
///
/// The idea is similar to patterns used in `macro_rules` definitions in Rust. There are a few
/// differences though:
/// 1. This pattern implementation exposes different matchers and operations.
/// 2. This macro implementation never attaches types to tokens, which means that every defined
///    pattern behaves like a TT-muncher in Rust.
#[derive(Clone, Debug)]
#[allow(missing_docs)]
pub enum Pattern {
    /// Consume all items, till the end of the token stream.
    Everything,
    /// Consume nothing.
    Nothing,
    /// Consume items matching the first pattern. If the match was unsuccessful, the second match
    /// will be tried.
    Or(Box<Pattern>, Box<Pattern>),
    Seq(Box<Pattern>, Box<Pattern>),
    Many(Box<Pattern>),
    /// Consume a single item if it matches the configuration.
    Item(Item),
    Identifier,
}

impl Pattern {
    pub fn many(self) -> Self {
        Pattern::Many(Box::new(self))
    }
}

impl std::ops::Shr for Pattern {
    type Output = Pattern;
    fn shr(self, rhs: Pattern) -> Self::Output {
        Pattern::Seq(Box::new(self), Box::new(rhs))
    }
}

/// Item pattern configuration.
#[derive(Clone, Copy, Debug)]
#[allow(missing_docs)]
pub struct Item {
    /// Check whether the token has spaces on right-hand-side. The [`None`] value means that the
    /// condition would not be checked.
    pub has_rhs_spacing: Option<bool>,
}



// =======================
// === ResolutionError ===
// =======================

/// Pattern resolution error.
#[derive(Clone, Debug)]
#[allow(missing_docs)]
pub struct ResolutionError {
    pub message: String,
}

impl ResolutionError {
    /// Constructor.
    pub fn new(message: impl Into<String>) -> Self {
        let message = message.into();
        Self { message }
    }
}



/// ==================
/// === Resolution ===
/// ==================

/// Successful pattern match result.
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct Match<T> {
    /// All the matched tokens.
    pub matched: Vec<T>,
    /// The rest of the token stream that was not needed for the successful pattern match.
    pub rest:    Vec<T>,
    pub error:   Option<ResolutionError>,
}

impl<T> Match<T> {
    /// Constructor.
    pub fn new(matched: Vec<T>, rest: Vec<T>, error: Option<ResolutionError>) -> Self {
        Self { matched, rest, error }
    }
}

impl Pattern {
    /// Match the token stream with this pattern.
    pub fn resolve<'s, T: TryAsRef<syntax::Item<'s>>>(
        &self,
        mut input: Vec<T>,
        has_spacing_at_end: bool,
        right_to_left_mode: bool,
    ) -> Match<T> {
        let reject = |input: Vec<T>, message: &str| {
            Match::new(default(), input, Some(ResolutionError::new(message)))
        };

        match self {
            Self::Everything => Match::new(input, default(), None),
            Self::Nothing => Match::new(default(), input, None),
            Self::Or(fst, snd) => {
                let fst_result = fst.resolve(input, has_spacing_at_end, right_to_left_mode);
                if fst_result.error.is_none() {
                    fst_result
                } else {
                    let input =
                        fst_result.matched.into_iter().chain(fst_result.rest.into_iter()).collect();
                    snd.resolve(input, has_spacing_at_end, right_to_left_mode)
                }
            }
            Self::Seq(fst, snd) => {
                println!(">> Seq");

                let fst_result = fst.resolve(input, has_spacing_at_end, right_to_left_mode);
                if fst_result.error.is_none() {
                    let snd_result =
                        snd.resolve(fst_result.rest, has_spacing_at_end, right_to_left_mode);
                    Match::new(
                        fst_result
                            .matched
                            .into_iter()
                            .chain(snd_result.matched.into_iter())
                            .collect(),
                        snd_result.rest,
                        snd_result.error,
                    )
                } else {
                    fst_result
                }
            }
            Self::Many(pat) => {
                let mut matched = vec![];
                loop {
                    let result =
                        pat.resolve(mem::take(&mut input), has_spacing_at_end, right_to_left_mode);
                    if result.error.is_none() {
                        matched.extend(result.matched);
                        input = result.rest;
                    } else {
                        input = result.matched.into_iter().chain(result.rest.into_iter()).collect();
                        break;
                    }
                }
                Match::new(matched, input, None)
            }
            Self::Item(item) => match input.first() {
                None => reject(input, "Expected item"),
                Some(first) => match first.try_as_ref() {
                    None => reject(input, "Expected item"),
                    Some(_) => match item.has_rhs_spacing {
                        Some(spacing) =>
                            if right_to_left_mode {
                                if spacing == has_spacing_at_end {
                                    Match::new(vec![input.pop_front().unwrap()], input, None)
                                } else {
                                    reject(input, "Expected item")
                                }
                            } else {
                                todo!()
                            },
                        None => Match::new(vec![input.pop_front().unwrap()], input, None),
                    },
                },
            },
            Self::Identifier => match input.first() {
                None => reject(input, "Expected identifier, got nothing."),
                Some(first) => match first.try_as_ref() {
                    None => reject(input, "Expected identifier, got ..."),
                    Some(item) =>
                        if item.is_variant(syntax::token::variant::VariantMarker::Ident) {
                            Match::new(vec![input.pop_front().unwrap()], input, None)
                        } else {
                            reject(input, "Expected identifier, got ...")
                        },
                },
            },
        }
    }
}
