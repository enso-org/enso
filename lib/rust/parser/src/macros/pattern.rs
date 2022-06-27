//! This module defines patterns Pattern used to validate incoming token stream against expected
//! macro input.

use crate::prelude::*;
use std::collections::VecDeque;

use crate::syntax;

use crate::macros::resolver::SyntaxItemOrMacroResolver;


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
    // Item(Item),
    Identifier,
    Block(Box<Pattern>),
    Expected(String, Box<Pattern>),
    Named(String, Box<Pattern>),
}


#[derive(Clone, Debug)]
pub enum Match<'s> {
    Everything(VecDeque<syntax::Item<'s>>),
    Nothing,
    Or(Box<OrMatch<'s>>),
    Seq(Box<Match<'s>>, Box<Match<'s>>),
    Many(Vec<Match<'s>>),
    Identifier(syntax::Item<'s>),
    Expected(String, Box<Match<'s>>),
    Named(String, Box<Match<'s>>),
}

#[derive(Clone, Debug)]
pub enum OrMatch<'s> {
    First(Match<'s>),
    Second(Match<'s>),
}

impl<'s> Match<'s> {
    pub fn or(m: OrMatch<'s>) -> Self {
        Self::Or(Box::new(m))
    }

    pub fn seq(first: Match<'s>, second: Match<'s>) -> Self {
        Self::Seq(Box::new(first), Box::new(second))
    }

    pub fn expected(expected: String, second: Match<'s>) -> Self {
        Self::Expected(expected, Box::new(second))
    }

    pub fn named(label: String, second: Match<'s>) -> Self {
        Self::Named(label, Box::new(second))
    }
}

#[derive(Clone, Debug)]
pub struct MatchedSegment2<'s> {
    pub header: syntax::Token<'s>,
    pub result: Match<'s>,
}

impl<'s> MatchedSegment2<'s> {
    pub fn new(header: syntax::Token<'s>, result: Match<'s>) -> Self {
        Self { header, result }
    }
}

//
// $(
//     foo {
//         $ (
//             $t: ident
//         ),*
//     }
//     $(
//         bar {
//             $ (
//                 $s: ident
//             ),*
//         }
//     )
//
// )*
//
// foo {a}
// bar {1,2}
//
// foo {a,b,c}
// foo {}
// bar {3,4}
//
//
// [
//   [
//     $t = a
//     [
//       $s = 1
//     ]
//   ]
// ]
//
//
//


// #[derive(Clone, Debug)]
// pub enum Match<'s> {
//     Everything(VecDeque<syntax::Item<'s>>),
//     Nothing,
//     Or(Box<OrMatch<'s>>),
//     Seq(Box<Match<'s>>, Box<Match<'s>>),
//     Many(Vec<Match<'s>>),
//     Identifier(syntax::Item<'s>),
//     Expected(String, Box<Match<'s>>),
// }


#[derive(Clone, Debug, Default)]
pub struct MatchTree<'s> {
    nested: Option<Box<MatchTree<'s>>>,
    map:    HashMap<String, Vec<Vec<syntax::Item<'s>>>>,
}

impl<'s> Match<'s> {
    pub fn into_match_tree(self) -> MatchTree<'s> {
        let mut tree = MatchTree::default();
        self.make_match_tree(&mut tree);
        tree
    }

    pub fn make_match_tree(self, tree: &mut MatchTree<'s>) {
        match self {
            Self::Everything(_) => {}
            Self::Nothing => {}
            Self::Or(t) => match *t {
                OrMatch::First(first) => first.make_match_tree(tree),
                OrMatch::Second(second) => second.make_match_tree(tree),
            },
            Self::Seq(first, second) => {
                first.make_match_tree(tree);
                second.make_match_tree(tree);
            }

            Self::Many(matches) => {
                if tree.nested.is_none() {
                    tree.nested = Some(default());
                }
                let nested = tree.nested.as_mut().unwrap();
                for m in matches {
                    m.make_match_tree(nested);
                }
            }
            Self::Identifier(_) => {}
            Self::Expected(_, _) => {}
            Self::Named(name, t) => {
                tree.map.entry(name).or_default().push(t.tokens());
            }
        }
    }
}


// #[derive(Clone, Debug, Default)]
// pub struct MatchTree<'s> {
//     nested: Vec<MatchTree<'s>>,
//     map:    HashMap<String, Vec<syntax::Item<'s>>>,
// }
//
// impl<'s> Match<'s> {
//     pub fn into_match_tree(self) -> MatchTree<'s> {
//         let mut tree = MatchTree::default();
//         self.make_match_tree(&mut tree);
//         tree
//     }
//
//     pub fn make_match_tree(self, tree: &mut MatchTree<'s>) {
//         match self {
//             Self::Everything(_) => {}
//             Self::Nothing => {}
//             Self::Or(t) => match *t {
//                 OrMatch::First(first) => first.make_match_tree(tree),
//                 OrMatch::Second(second) => second.make_match_tree(tree),
//             },
//             Self::Seq(first, second) => {
//                 first.make_match_tree(tree);
//                 second.make_match_tree(tree);
//             }
//
//             Self::Many(matches) =>
//                 for m in matches {
//                     let mut nested = default();
//                     m.make_match_tree(&mut nested);
//                     tree.nested.push(nested);
//                 },
//             Self::Identifier(_) => {}
//             Self::Expected(_, _) => {}
//             Self::Named(name, t) => {
//                 tree.map.insert(name, t.tokens());
//             }
//         }
//     }
// }

pub use Pattern::Everything;
pub use Pattern::Identifier;
pub use Pattern::Nothing;

pub fn Or(fst: Pattern, snd: Pattern) -> Pattern {
    Pattern::Or(Box::new(fst), Box::new(snd))
}

pub fn Seq(fst: Pattern, snd: Pattern) -> Pattern {
    Pattern::Seq(Box::new(fst), Box::new(snd))
}

pub fn Many(item: Pattern) -> Pattern {
    Pattern::Many(Box::new(item))
}

pub fn Block(body: Pattern) -> Pattern {
    Pattern::Block(Box::new(body))
}

pub fn Expected(message: impl Into<String>, item: Pattern) -> Pattern {
    Pattern::Expected(message.into(), Box::new(item))
}

pub fn Named(message: impl Into<String>, item: Pattern) -> Pattern {
    Pattern::Named(message.into(), Box::new(item))
}

impl Pattern {
    pub fn many(self) -> Self {
        Many(self)
    }

    pub fn many1(self) -> Self {
        Seq(self.clone(), Many(self))
    }
}

impl std::ops::Shr for Pattern {
    type Output = Pattern;
    fn shr(self, rhs: Pattern) -> Self::Output {
        Seq(self, rhs)
    }
}

impl std::ops::BitOr for Pattern {
    type Output = Pattern;
    fn bitor(self, rhs: Pattern) -> Self::Output {
        Or(self, rhs)
    }
}

impl<T: Into<String>> std::ops::Rem<T> for Pattern {
    type Output = Pattern;
    fn rem(self, message: T) -> Self::Output {
        self | Expected(message, Nothing)
    }
}

impl<T: Into<String>> std::ops::Div<T> for Pattern {
    type Output = Pattern;
    fn div(self, message: T) -> Self::Output {
        Named(message, self)
    }
}

// /// Item pattern configuration.
// #[derive(Clone, Copy, Debug)]
// #[allow(missing_docs)]
// pub struct Item {
//     /// Check whether the token has spaces on right-hand-side. The [`None`] value means that the
//     /// condition would not be checked.
//     pub has_rhs_spacing: Option<bool>,
// }


#[derive(Debug)]
pub struct MatchResult<'s> {
    /// All the matched tokens.
    pub matched: Match<'s>,
    /// The rest of the token stream that was not consumed.
    pub rest:    VecDeque<syntax::Item<'s>>,
}

impl<'s> MatchResult<'s> {
    /// Constructor.
    pub fn new(matched: Match<'s>, rest: VecDeque<syntax::Item<'s>>) -> Self {
        Self { matched, rest }
    }

    pub fn map(mut self, f: impl FnOnce(Match<'s>) -> Match<'s>) -> Self {
        self.matched = f(self.matched);
        self
    }
}



impl<'s> Match<'s> {
    pub fn tokens(self) -> Vec<syntax::Item<'s>> {
        match self {
            Self::Everything(tokens) => tokens.into(),
            Self::Nothing => default(),
            Self::Or(t) => t.tokens(),
            Self::Seq(fst, snd) => fst.tokens().extended(snd.tokens()),
            Self::Many(t) => t.into_iter().map(|s| s.tokens()).flatten().collect(),
            Self::Identifier(ident) => vec![ident],
            Self::Expected(_, item) => item.tokens(),
            Self::Named(_, item) => item.tokens(),
        }
    }
}

impl<'s> OrMatch<'s> {
    pub fn tokens(self) -> Vec<syntax::Item<'s>> {
        match self {
            Self::First(t) => t.tokens(),
            Self::Second(t) => t.tokens(),
        }
    }
}

impl Pattern {
    #[allow(missing_docs)]
    pub fn resolve<'s>(
        &self,
        mut input: VecDeque<syntax::Item<'s>>,
    ) -> Result<MatchResult<'s>, VecDeque<syntax::Item<'s>>> {
        match self {
            Self::Expected(msg, item) =>
                item.resolve(input).map(|t| t.map(|s| Match::expected(msg.clone(), s))),
            Self::Named(msg, item) =>
                item.resolve(input).map(|t| t.map(|s| Match::named(msg.clone(), s))),
            Self::Everything => Ok(MatchResult::new(Match::Everything(input), default())),
            Self::Nothing => Ok(MatchResult::new(Match::Nothing, input)),
            Self::Or(fst, snd) => fst
                .resolve(input)
                .map(|t| t.map(|s| Match::or(OrMatch::First(s))))
                .or_else(|t| snd.resolve(t).map(|t| t.map(|s| Match::or(OrMatch::Second(s))))),
            Self::Seq(fst, snd) => fst
                .resolve(input)
                .and_then(|t| snd.resolve(t.rest).map(|s| s.map(|x| Match::seq(t.matched, x)))),
            Self::Many(pat) => {
                let mut out = vec![];
                loop {
                    match pat.resolve(input) {
                        Err(rest) => {
                            input = rest;
                            break;
                        }
                        Ok(t) => {
                            input = t.rest;
                            out.push(t.matched);
                        }
                    }
                }
                Ok(MatchResult::new(Match::Many(out), input))
            }
            Self::Identifier => match input.pop_front() {
                None => Err(default()),
                Some(t) =>
                    if t.is_variant(syntax::token::variant::VariantMarker::Ident) {
                        Ok(MatchResult::new(Match::Identifier(t), input))
                    } else {
                        input.push_front(t);
                        Err(input)
                    },
            },
            Self::Block(body) => match input.pop_front() {
                Some(syntax::Item::Block(tokens)) =>
                    body.resolve(tokens.into_iter().rev().map_into().collect()),
                Some(t) => {
                    input.push_front(t);
                    Err(input)
                }
                None => Err(default()),
            },
        }
    }
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



// /// Successful pattern match result.
// #[derive(Debug, Clone)]
// #[allow(missing_docs)]
// pub struct Match<T> {
//     /// All the matched tokens.
//     pub matched: Vec<T>,
//     /// The rest of the token stream that was not needed for the successful pattern match.
//     pub rest:    Vec<T>,
//     pub error:   Option<ResolutionError>,
// }
//
// impl<T> Match<T> {
//     /// Constructor.
//     pub fn new(matched: Vec<T>, rest: Vec<T>, error: Option<ResolutionError>) -> Self {
//         Self { matched, rest, error }
//     }
// }


//
// impl Pattern {
//     /// Match the token stream with this pattern.
//     pub fn resolve_old<'s, T: TryAsRef<syntax::Item<'s>>>(
//         &self,
//         mut input: Vec<T>,
//         has_spacing_at_end: bool,
//         right_to_left_mode: bool,
//     ) -> Match<T> {
//         let reject = |input: Vec<T>, message: &str| {
//             Match::new(default(), input, Some(ResolutionError::new(message)))
//         };
//
//         match self {
//             Self::Everything => Match::new(input, default(), None),
//             Self::Nothing => Match::new(default(), input, None),
//             Self::Or(fst, snd) => {
//                 let fst_result = fst.resolve_old(input, has_spacing_at_end, right_to_left_mode);
//                 if fst_result.error.is_none() {
//                     fst_result
//                 } else {
//                     let input =
//
// fst_result.matched.into_iter().chain(fst_result.rest.into_iter()).collect();
// snd.resolve_old(input, has_spacing_at_end, right_to_left_mode)                 }
//             }
//             Self::Seq(fst, snd) => {
//                 let fst_result = fst.resolve_old(input, has_spacing_at_end, right_to_left_mode);
//                 if fst_result.error.is_none() {
//                     let snd_result =
//                         snd.resolve_old(fst_result.rest, has_spacing_at_end, right_to_left_mode);
//                     Match::new(
//                         fst_result
//                             .matched
//                             .into_iter()
//                             .chain(snd_result.matched.into_iter())
//                             .collect(),
//                         snd_result.rest,
//                         snd_result.error,
//                     )
//                 } else {
//                     fst_result
//                 }
//             }
//             Self::Many(pat) => {
//                 let mut matched = vec![];
//                 loop {
//                     let result =
//                         pat.resolve_old(mem::take(&mut input), has_spacing_at_end,
// right_to_left_mode);                     if result.error.is_none() {
//                         matched.extend(result.matched);
//                         input = result.rest;
//                     } else {
//                         input =
// result.matched.into_iter().chain(result.rest.into_iter()).collect();
// break;                     }
//                 }
//                 Match::new(matched, input, None)
//             }
//             Self::Item(item) => match input.first() {
//                 None => reject(input, "Expected item"),
//                 Some(first) => match first.try_as_ref() {
//                     None => reject(input, "Expected item"),
//                     Some(_) => match item.has_rhs_spacing {
//                         Some(spacing) =>
//                             if right_to_left_mode {
//                                 if spacing == has_spacing_at_end {
//                                     Match::new(vec![input.pop_front().unwrap()], input, None)
//                                 } else {
//                                     reject(input, "Expected item")
//                                 }
//                             } else {
//                                 todo!()
//                             },
//                         None => Match::new(vec![input.pop_front().unwrap()], input, None),
//                     },
//                 },
//             },
//             Self::Identifier => match input.first() {
//                 None => reject(input, "Expected identifier, got nothing."),
//                 Some(first) => match first.try_as_ref() {
//                     None => reject(input, "Expected identifier, got ..."),
//                     Some(item) =>
//                         if item.is_variant(syntax::token::variant::VariantMarker::Ident) {
//                             Match::new(vec![input.pop_front().unwrap()], input, None)
//                         } else {
//                             reject(input, "Expected identifier, got ...")
//                         },
//                 },
//             },
//             Self::Block(body) => todo!()
//             // match input.first() {
//             //     None => reject(input, "Expected block, got nothing."),
//             //     Some(first) => match first.try_as_ref() {
//             //         None => reject(input, "Expected block, got ..."),
//             //         Some(item) => match item {
//             //             syntax::Item::Block(tokens) => {
//             //                 let tokens = tokens.clone(); // FIXME: perf
//             //                 let front = input.pop_front().unwrap();
//             //                 let m = body.resolve_old(tokens, has_spacing_at_end,
// right_to_left_mode);             //                 if m.error.is_none() {
//             //                     Match::new(vec![front], input, None)
//             //                 } else {
//             //                     Match::new(vec![front], input, None)
//             //                 }
//             //                 // pub fn resolve_old<'s, T: TryAsRef<syntax::Item<'s>>>(
//             //                 //     &self,
//             //                 //     mut input: Vec<T>,
//             //                 //     has_spacing_at_end: bool,
//             //                 //     right_to_left_mode: bool,
//             //             }
//             //             _ => reject(input, "Expected identifier, got ...")
//             //         }
//             //     },
//             // },
//         }
//     }
// }


// pub struct Match<T> {
//     /// All the matched tokens.
//     pub matched: Vec<T>,
//     /// The rest of the token stream that was not needed for the successful pattern match.
//     pub rest:    Vec<T>,
//     pub er
