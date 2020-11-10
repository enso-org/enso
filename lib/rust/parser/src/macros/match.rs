//! The macro builder.

use crate::prelude::*;

use crate::macros::literal::Literal;
use crate::macros::definition::Definition;
use crate::prelude::lexer::library::token::Token;



/// TODO
#[derive(Clone,Debug,Eq,PartialEq)]
pub enum Ast {
    Token(Token),
    Macro(Match)
}



// =============
// === Match ===
// =============

/// A builder for a macro match.
#[derive(Clone,Debug,Default,Eq,PartialEq)]
pub struct Match {
    /// The type of match.
    match_type : MatchType,
    /// Tokens occurring before the first segment.
    pre_tokens : Vec<Ast>,
    /// The segments making up the macro match.
    segments : Vec<SegmentBuilder>,
    /// The optional macro definition for the builder.
    definition : Option<Definition>,
    /// Whether this is the top-level match of the resolver.
    top_level_match : bool,
}

impl Match {
    /// Create a top-level match.
    pub fn top_level() -> Self {
        let mut new_match = Match::default();
        new_match.top_level_match = true;
        new_match
    }

    /// Add a segment with `offset` and `segment_head` to the macro match.
    pub fn add_segment(&mut self, offset:usize, segment_head:Literal) -> &SegmentBuilder {
        let segment = SegmentBuilder::new(offset,segment_head);
        self.segments.push_and_get(segment)
    }

    /// Get a reference to the current segment.
    pub fn current_segment(&self) -> Option<&SegmentBuilder> {
        self.segments.last()
    }

    /// Get a mutable reference to the current segment.
    pub fn current_segment_mut(&mut self) -> Option<&mut SegmentBuilder> {
        self.segments.last_mut()
    }

    /// Add a token to the match.
    ///
    /// If a segment exists, this token will be added to it, otherwise, the token will be added to
    /// the pre-tokens.
    pub fn add_token(&mut self, token:Ast) {
        match self.current_segment_mut() {
            Some(seg) => seg.append_token(token),
            None      => self.add_pre_token(token),
        }
    }

    /// Add a token that occurs before the first match.
    pub fn add_pre_token(&mut self, token:Ast) {
        self.pre_tokens.push(token);
    }

    /// Set the match's definition to `def`.
    pub fn set_definition(&mut self, def:Definition) {
        self.definition = Some(def);
    }

    /// Get the definition for this match, if set.
    pub fn definition(&self) -> &Option<Definition> {
        &self.definition
    }
}



// =================
// === MatchType ===
// =================

/// Type type of a macro match.
#[derive(Clone,Debug,Eq,PartialEq)]
pub enum MatchType {
    Partial,
    Complete,
}


// === Trait Impls ===

impl Default for MatchType {
    fn default() -> Self {
        MatchType::Partial
    }
}



// ======================
// === SegmentBuilder ===
// ======================

/// A builder for segments in a macro match.
#[derive(Clone,Debug,Eq,PartialEq)]
pub struct SegmentBuilder {
    /// The offset of this macro segment.
    offset : usize,
    /// The segment head.
    segment_head : Literal,
    /// The tokens in this segment.
    accumulator : Vec<Ast>,
}

impl SegmentBuilder {
    /// Constructor
    pub fn new(offset:usize, segment_head:Literal) -> Self {
        let accumulator = default();
        Self{offset,segment_head,accumulator}
    }

    /// Append `token` to this segment.
    pub fn append_token(&mut self, token:Ast) {
        self.accumulator.push(token)
    }

    /// Get the offset of this segment.
    pub fn offset(&self) -> usize {
        self.offset
    }
}

