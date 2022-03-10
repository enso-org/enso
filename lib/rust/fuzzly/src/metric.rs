//! The Metric trait definition and standard implementations.

use crate::prelude::*;

use crate::subsequence_graph;



// =============
// === Trait ===
// =============

/// Provides functions for measure pattern match score for specific text from various aspect.
///
/// The pattern match is represented as a path in `SubsequenceGraph` (see it's docs for details).
/// Its score is counted as a sum of measures "how good is the vertex/edge" for each vertex and
/// edge on the path.
pub trait Metric {
    /// How good is vertex on the path on the Subsequence Graph.
    fn measure_vertex(&self, vertex: subsequence_graph::Vertex, text: &str, pattern: &str) -> f32;

    /// How good is the edge on the path on the Subsequence Graph.
    fn measure_edge(&self, edge: subsequence_graph::Edge, text: &str, pattern: &str) -> f32;

    /// Return a new metric being a sum of this and `rhs`.
    fn sum<Rhs: Metric>(self, rhs: Rhs) -> Sum<Self, Rhs>
    where Self: Sized {
        Sum(self, rhs)
    }
}



// ==========================
// === The Default Metric ===
// ==========================

/// The default metric, recommended by this library
pub fn default() -> impl Metric {
    SubsequentLettersBonus::default().sum(CaseMatchBonus::default())
}



// =======================
// === Implementations ===
// =======================

// === Sum ===

/// The structure representing the sum of two metrics
#[derive(Copy, Clone, Debug, Default)]
pub struct Sum<Metrics1, Metrics2>(Metrics1, Metrics2);

impl<M1: Metric, M2: Metric> Metric for Sum<M1, M2> {
    fn measure_vertex(&self, vertex: subsequence_graph::Vertex, text: &str, pattern: &str) -> f32 {
        let Self(left, right) = self;
        let left = left.measure_vertex(vertex, text, pattern);
        let right = right.measure_vertex(vertex, text, pattern);
        left + right
    }

    fn measure_edge(&self, edge: subsequence_graph::Edge, text: &str, pattern: &str) -> f32 {
        let Self(left, right) = self;
        let left = left.measure_edge(edge, text, pattern);
        let right = right.measure_edge(edge, text, pattern);
        left + right
    }
}


// === SubsequentLettersBonus ===

/// A metric which measure how far are matched letters from each other and how far is first matched
/// char from text beginning and last character from text ending.
#[derive(Copy, Clone, Debug)]
pub struct SubsequentLettersBonus {
    /// The base weight of this metric.
    pub base_weight:      f32,
    /// How important is the distance of first matched char from the text beginning.
    pub beginning_weight: f32,
    /// How important is the distance of last matched char from the text ending.
    pub ending_weight:    f32,
}

impl Default for SubsequentLettersBonus {
    fn default() -> Self {
        SubsequentLettersBonus {
            base_weight:      1.0,
            beginning_weight: 0.5,
            ending_weight:    0.01,
        }
    }
}

impl Metric for SubsequentLettersBonus {
    fn measure_vertex(&self, vertex: subsequence_graph::Vertex, text: &str, _pattern: &str) -> f32 {
        let is_first_pattern_char = vertex.layer == 0;
        let is_last_pattern_char = text.len().checked_sub(1).contains(&vertex.layer);
        let first_char_bonus = if is_first_pattern_char {
            self.base_weight / (vertex.position_in_text as f32 + 1.0) * self.beginning_weight
        } else {
            0.0
        };
        let last_char_bonus = if is_last_pattern_char {
            self.base_weight / (text.len() - vertex.position_in_text) as f32 * self.ending_weight
        } else {
            0.0
        };
        first_char_bonus + last_char_bonus
    }

    fn measure_edge(&self, edge: subsequence_graph::Edge, _text: &str, _pattern: &str) -> f32 {
        self.base_weight / (edge.to.position_in_text - edge.from.position_in_text) as f32
    }
}


// === CaseMatchBonus ===

/// A metrics which scores the matches where case matches.
#[derive(Copy, Clone, Debug)]
pub struct CaseMatchBonus {
    /// A score added for each char matching.
    pub bonus_per_char: f32,
}

impl Default for CaseMatchBonus {
    fn default() -> Self {
        CaseMatchBonus { bonus_per_char: 0.01 }
    }
}

impl Metric for CaseMatchBonus {
    fn measure_vertex(&self, vertex: subsequence_graph::Vertex, text: &str, pattern: &str) -> f32 {
        let text_ch = text.chars().nth(vertex.position_in_text);
        let pattern_ch = pattern.chars().nth(vertex.layer);
        match (text_ch, pattern_ch) {
            (Some(w), Some(q)) if w.is_uppercase() == q.is_uppercase() => self.bonus_per_char,
            _ => 0.0,
        }
    }

    fn measure_edge(&self, _: subsequence_graph::Edge, _: &str, _: &str) -> f32 {
        0.0
    }
}
