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
    Everything,
    Nothing,
    Or(Box<Pattern>, Box<Pattern>),
    Item(Item),
}

/// Any item. Can specify whether it requires the rhs spacing.
#[derive(Clone, Copy, Debug)]
#[allow(missing_docs)]
pub struct Item {
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
    pub matched:     Vec<T>,
    pub not_matched: Vec<T>,
}

impl<T> Match<T> {
    /// Constructor.
    pub fn new(matched: Vec<T>, not_matched: Vec<T>) -> Self {
        Self { matched, not_matched }
    }
}

impl Pattern {
    /// Match the token stream with this pattern.
    pub fn resolve<T: TryAsRef<syntax::Item>>(
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
