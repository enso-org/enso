//! Scoring how given text matches the given pattern.

use crate::prelude::*;

use crate::metric::Metric;
use crate::subsequence_graph;
use crate::SubsequenceGraph;

use std::collections::hash_map::Entry;



// =====================
// === VerticesScore ===
// =====================

/// The description of path which finishes at some specific vertex.
#[derive(Clone, Copy, Debug)]
struct InputPath {
    value: f32,
    from:  subsequence_graph::Vertex,
}

/// The score of single vertex in graph.
///
/// The score is a sum of measure of the vertex alone, and the best score of input path.
/// The `best_input_path` is updated during the scoring algorithm run. See the `score_match`
/// function.
#[derive(Copy, Clone, Debug, Default)]
struct VertexScore {
    my_measure:      f32,
    best_input_path: Option<InputPath>,
}

impl VertexScore {
    fn new(my_measure: f32) -> Self {
        let best_input_path = default();
        VertexScore { my_measure, best_input_path }
    }

    fn update_input_path(&mut self, candidate: InputPath) {
        let new_score = match self.best_input_path.take() {
            Some(score) if score.value < candidate.value => candidate,
            Some(score) => score,
            None => candidate,
        };
        self.best_input_path = Some(new_score)
    }

    fn score(&self) -> f32 {
        self.my_measure + self.best_input_path.map_or(0.0, |s| s.value)
    }
}

/// All graph's vertices' scores.
///
/// Used in the `score_match` function.
#[derive(Debug, Default)]
struct VerticesScores(HashMap<subsequence_graph::Vertex, VertexScore>);

impl VerticesScores {
    fn init_vertex(&mut self, vertex: subsequence_graph::Vertex, measure: f32) {
        let Self(scores) = self;
        scores.insert(vertex, VertexScore::new(measure));
    }

    fn update_input_path(&mut self, edge: subsequence_graph::Edge, value: f32) {
        let Self(scores) = self;
        let subsequence_graph::Edge { from, to } = edge;
        let candidate = InputPath { value, from };
        match scores.entry(to) {
            Entry::Occupied(mut entry) => entry.get_mut().update_input_path(candidate),
            Entry::Vacant(entry) => {
                let mut vertex = VertexScore::default();
                vertex.update_input_path(candidate);
                entry.insert(vertex);
            }
        }
    }

    fn get_score(&self, vertex: subsequence_graph::Vertex) -> f32 {
        let Self(scores) = self;
        scores.get(&vertex).map(|v| v.score()).unwrap_or(0.0)
    }

    fn best_vertex(
        &self,
        vertices: impl Iterator<Item = subsequence_graph::Vertex>,
    ) -> Option<subsequence_graph::Vertex> {
        let pairs = vertices.map(|v| (v, self.get_score(v)));
        let best_pair = pairs.fold(None, |prev, (vertex, score)| match prev {
            Some((_, prev_score)) if score > prev_score => Some((vertex, score)),
            Some(prev) => Some(prev),
            None => Some((vertex, score)),
        });
        best_pair.map(|(vertex, _)| vertex)
    }

    fn best_path_rev(&self, end: subsequence_graph::Vertex) -> BestPathRevIter {
        BestPathRevIter { scores: self, next_vertex: Some(end) }
    }
}

struct BestPathRevIter<'a> {
    scores:      &'a VerticesScores,
    next_vertex: Option<subsequence_graph::Vertex>,
}

impl<'a> Iterator for BestPathRevIter<'a> {
    type Item = subsequence_graph::Vertex;

    fn next(&mut self) -> Option<Self::Item> {
        let next = std::mem::take(&mut self.next_vertex);
        self.next_vertex = (|| {
            let VerticesScores(scores) = self.scores;
            Some(scores.get(&next?)?.best_input_path?.from)
        })();
        next
    }
}



// ===================
// === Score Match ===
// ===================

/// Fast-check if the pattern matches text.
///
/// This is faster way than calling `score_match(text,pattern,metric).is_some()`, therefore it's
/// recommended to call this function before scoring when we are not sure if the pattern actually
/// matches the text.
pub fn matches(text: impl Str, pattern: impl Str) -> bool {
    let mut pattern_chars = pattern.as_ref().chars();
    let mut next_pattern_char = pattern_chars.next();
    for text_char in text.as_ref().chars() {
        match next_pattern_char {
            Some(ch) if ch.eq_ignore_ascii_case(&text_char) =>
                next_pattern_char = pattern_chars.next(),
            Some(_) => {}
            None => {
                break;
            }
        }
    }
    next_pattern_char.is_none()
}

/// The result of `find_best_subsequence` function.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct Subsequence {
    /// The score of found subsequence.
    pub score:   f32,
    /// Indices of `text`'s chars which belong to the subsequence.
    pub indices: Vec<usize>,
}

impl Subsequence {
    /// Compare scores of subsequences.
    ///
    /// The `f32` does not implement total ordering, however that does not help when we want to
    /// sort items by their matching score. Therefore this function assumes that all NaNs are the
    /// lowest values.
    pub fn compare_scores(&self, rhs: &Subsequence) -> std::cmp::Ordering {
        if self.score.is_nan() && rhs.score.is_nan() {
            std::cmp::Ordering::Equal
        } else if self.score.is_nan() {
            std::cmp::Ordering::Less
        } else if rhs.score.is_nan() {
            std::cmp::Ordering::Greater
        } else if self.score < rhs.score {
            std::cmp::Ordering::Less
        } else if self.score > rhs.score {
            std::cmp::Ordering::Greater
        } else {
            std::cmp::Ordering::Equal
        }
    }
}

/// Find best subsequence in `text` which case-insensitively equals to `pattern` in terms of given
/// `metric`.
///
/// Returns `None` if `text` does not match `pattern`. Empty `pattern` gives 0.0 score.
///
/// ## Algorithm specification
///
/// In essence, it looks through all possible subsequences of `text` being the `pattern` and pick
/// the one with the best score. Not directly (because there may be a lot of such subsequences), but
/// by building the `SubsequenceGraph` and computing best score for each vertex. See
/// `SubsequenceGraph` docs for detailed description of the graph.
pub fn find_best_subsequence(
    text: impl Str,
    pattern: impl Str,
    metric: impl Metric,
) -> Option<Subsequence> {
    let text = text.as_ref();
    let pattern = pattern.as_ref();
    if pattern.is_empty() {
        Some(default())
    } else {
        let last_layer = pattern.chars().count() - 1;
        let mut scores = VerticesScores::default();
        let graph = SubsequenceGraph::new(text, pattern);
        for vertex in &graph.vertices {
            let measure = metric.measure_vertex(*vertex, text, pattern);
            scores.init_vertex(*vertex, measure);
        }
        for edge in &graph.edges {
            let from_score = scores.get_score(edge.from);
            let input_score = from_score + metric.measure_edge(*edge, text, pattern);
            scores.update_input_path(*edge, input_score);
        }
        let end_vertices = graph.vertices_in_layer(last_layer).cloned();
        let best_vertex = scores.best_vertex(end_vertices)?;
        let score = scores.get_score(best_vertex);
        let best_path_rev = scores.best_path_rev(best_vertex);
        let mut indices = best_path_rev.map(|v| v.position_in_text).collect_vec();
        indices.reverse();
        Some(Subsequence { score, indices })
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod test {
    use super::*;

    mod mock_metric {
        use super::*;

        use crate::metric;

        #[derive(Debug, Default)]
        pub struct WordIndex;

        impl Metric for WordIndex {
            fn measure_vertex(
                &self,
                vertex: subsequence_graph::Vertex,
                _text: &str,
                _pattern: &str,
            ) -> f32 {
                vertex.position_in_text as f32
            }

            fn measure_edge(&self, _: subsequence_graph::Edge, _: &str, _: &str) -> f32 {
                0.0
            }
        }

        #[derive(Debug, Default)]
        pub struct SquareEdgeLength;

        impl Metric for SquareEdgeLength {
            fn measure_vertex(&self, _: subsequence_graph::Vertex, _: &str, _: &str) -> f32 {
                0.0
            }

            fn measure_edge(
                &self,
                edge: subsequence_graph::Edge,
                _text: &str,
                _pattern: &str,
            ) -> f32 {
                (edge.to.position_in_text - edge.from.position_in_text).pow(2) as f32
            }
        }

        pub type Sum = metric::Sum<WordIndex, SquareEdgeLength>;
    }

    #[test]
    fn matches_test() {
        assert!(matches("abba", "aba"));
        assert!(matches("abba", "ba"));
        assert!(matches("abba", ""));
        assert!(!matches("abba", "abc"));
        assert!(!matches("abba", "baa"));
        assert!(!matches("", "ba"));
    }

    #[test]
    fn finding_best_subsequence() {
        let pattern = "abc";
        let text = "aabxbacc";

        let expected = Subsequence {
            score:   12.0,
            indices: vec![1, 4, 7], // Always pick the latest character possible
        };
        assert_eq!(find_best_subsequence(text, pattern, mock_metric::WordIndex), Some(expected));

        let expected = Subsequence {
            score:   29.0,
            indices: vec![0, 2, 7], // Prefer the long edges
        };
        assert_eq!(
            find_best_subsequence(text, pattern, mock_metric::SquareEdgeLength),
            Some(expected)
        );

        let expected = Subsequence {
            score:   38.0,
            indices: vec![0, 2, 7], // The edges metric should have more impact
        };
        assert_eq!(
            find_best_subsequence(text, pattern, mock_metric::Sum::default()),
            Some(expected)
        );
    }

    #[test]
    fn finding_best_subsequence_when_does_not_match() {
        let pattern = "abc";
        let text = "aabxbyy";
        assert_eq!(find_best_subsequence(text, pattern, mock_metric::Sum::default()), None);
    }

    #[test]
    fn finding_best_subsequence_corner_cases() {
        let pattern = "";
        let text = "any";
        let expected = Subsequence { score: 0.0, indices: vec![] };
        assert_eq!(
            find_best_subsequence(text, pattern, mock_metric::Sum::default()),
            Some(expected)
        );
        let pattern = "any";
        let text = "";
        assert_eq!(find_best_subsequence(text, pattern, mock_metric::Sum::default()), None);
    }
}
