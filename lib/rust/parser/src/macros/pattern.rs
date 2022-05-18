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
    /// Consume a single item if it matches the configuration.
    Item(Item),
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
#[derive(Debug)]
#[allow(missing_docs)]
pub struct ResolutionError<T> {
    /// All the incoming tokens. The resolver consumes vector of tokens and returns it back in case
    /// an error happened.
    pub tokens:  Vec<T>,
    pub message: String,
}

impl<T> ResolutionError<T> {
    /// Constructor.
    pub fn new(tokens: Vec<T>, message: impl Into<String>) -> Self {
        let message = message.into();
        Self { tokens, message }
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
}

impl<T> Match<T> {
    /// Constructor.
    pub fn new(matched: Vec<T>, rest: Vec<T>) -> Self {
        Self { matched, rest }
    }
}

impl Pattern {
    /// Match the token stream with this pattern.
    pub fn resolve<'s, T: TryAsRef<syntax::Item<'s>>>(
        &self,
        mut input: Vec<T>,
        has_spacing_at_end: bool,
        right_to_left_mode: bool,
    ) -> Result<Match<T>, ResolutionError<T>> {
        match self {
            Self::Everything => Ok(Match::new(input, default())),
            Self::Nothing => Ok(Match::new(default(), input)),
            Self::Or(fst, snd) => fst
                .resolve(input, has_spacing_at_end, right_to_left_mode)
                .or_else(|err| snd.resolve(err.tokens, has_spacing_at_end, right_to_left_mode)),
            Self::Item(item) => match input.first() {
                None => Err(ResolutionError::new(input, "Expected an item.")),
                Some(first) => match first.try_as_ref() {
                    None => Err(ResolutionError::new(input, "Expected an item.")),
                    Some(_) => match item.has_rhs_spacing {
                        Some(spacing) =>
                            if right_to_left_mode {
                                if spacing == has_spacing_at_end {
                                    Ok(Match::new(vec![input.pop_front().unwrap()], input))
                                } else {
                                    Err(ResolutionError::new(input, "Expected an item."))
                                }
                            } else {
                                todo!()
                            },
                        None => Ok(Match::new(vec![input.pop_front().unwrap()], input)),
                    },
                },
            },
        }
    }
}
