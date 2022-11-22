//! The Subsequence Graph.

use crate::prelude::*;

use std::collections::BTreeSet;



// =============
// === Graph ===
// =============

/// A graph vertex.
///
/// The vertices are identified by two indexes: a layer index and text's char index. See
/// `Graph` docs for details.
///
/// The field order is significant, because it affects how they are ordered in the `Graph`'s
/// `vertices`.
#[derive(Copy, Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Vertex {
    /// The layer this vertex belongs to. It is equal to position in `pattern`.
    pub layer:            usize,
    /// The position in `text` this vertex represents.
    pub position_in_text: usize,
}

/// A graph edge.
///
/// The field order is significant, because it affects how they are ordered in the `Graph`'s
/// `edges`.
#[allow(missing_docs)]
#[derive(Copy, Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Edge {
    pub from: Vertex,
    pub to:   Vertex,
}

/// The Subsequence Graph.
///
/// This structure helps analyzing all subsequences in given `text` which are case insensitively
/// equal to given `pattern`. The graph is directional.
///
/// The vertices are arranged in `pattern.len()` layers: each vertex in i-th layer represents
/// a possible position of the i-th subsequence element in `text`.
///
/// Each edge _v → w_ is spanned between vertices from consecutive layers _i_ and _i_+1, and
/// indicates that having i-th subsequence element at position represented by _v_ we can pick
/// (i+1)-th subsequence element at position represented by _w_.
///
/// In such graph all paths spanned between first and last layer represents the possible subsequence
/// of `text`.
///
/// We keep vertices and edges ordered, because the scoring algorithm requires this ordering to be
/// effective.
#[allow(missing_docs)]
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct Graph {
    pub vertices: BTreeSet<Vertex>,
    pub edges:    BTreeSet<Edge>,
}

impl Graph {
    /// Generate graph based on `text` and `pattern`.
    pub fn new(text: impl Str, pattern: impl Str) -> Self {
        let vertices = Self::create_vertices(text.as_ref(), pattern.as_ref());
        let edges = Self::create_edges(&vertices);
        Graph { vertices, edges }
    }

    fn create_vertices(text: &str, pattern: &str) -> BTreeSet<Vertex> {
        let mut result = BTreeSet::default();
        let mut first_reachable_text_char = 0;
        for (layer, pattern_ch) in pattern.chars().enumerate() {
            // For each layer we skip positions which won't be reachable.
            let to_skip = first_reachable_text_char;
            first_reachable_text_char = text.len();
            for (position_in_text, text_ch) in text.chars().enumerate().skip(to_skip) {
                if pattern_ch.eq_ignore_ascii_case(&text_ch) {
                    result.insert(Vertex { layer, position_in_text });
                    first_reachable_text_char = first_reachable_text_char.min(position_in_text + 1);
                }
            }
        }
        result
    }

    fn create_edges(vertices: &BTreeSet<Vertex>) -> BTreeSet<Edge> {
        let mut result = BTreeSet::default();
        for from in vertices {
            let first_possible_to = Vertex {
                layer:            from.layer + 1,
                position_in_text: from.position_in_text + 1,
            };
            let first_impossible_to =
                Vertex { layer: from.layer + 2, position_in_text: 0 };
            for to in vertices.range(first_possible_to..first_impossible_to) {
                result.insert(Edge { from: *from, to: *to });
            }
        }
        result
    }

    /// Returns an iterator over all vertices in given layer.
    pub fn vertices_in_layer(&self, index: usize) -> impl Iterator<Item = &Vertex> {
        let start = Vertex { layer: index, position_in_text: 0 };
        let end = Vertex { layer: index + 1, position_in_text: 0 };
        self.vertices.range(start..end)
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn generating_graph() {
        struct Case {
            text:     &'static str,
            pattern:  &'static str,
            vertices: Vec<(usize, usize)>,
            edges:    Vec<((usize, usize), (usize, usize))>,
        }

        impl Case {
            fn run(self) {
                let graph = Graph::new(self.text, self.pattern);
                let expected_vertices = self.vertices.into_iter().map(Self::convert_vertex);
                let expected_edges = self.edges.into_iter().map(|(from, to)| Edge {
                    from: Self::convert_vertex(from),
                    to:   Self::convert_vertex(to),
                });
                let expected_graph = Graph {
                    vertices: expected_vertices.collect(),
                    edges:    expected_edges.collect(),
                };
                assert_eq!(graph, expected_graph);
            }

            fn convert_vertex((layer, position_in_text): (usize, usize)) -> Vertex {
                Vertex { layer, position_in_text }
            }
        }

        let classic = Case {
            text:     "lalala",
            pattern:  "alA",
            vertices: vec![(0, 1), (0, 3), (0, 5), (1, 2), (1, 4), (2, 3), (2, 5)],
            edges:    vec![
                ((0, 1), (1, 2)),
                ((0, 1), (1, 4)),
                ((0, 3), (1, 4)),
                ((1, 2), (2, 3)),
                ((1, 2), (2, 5)),
                ((1, 4), (2, 5)),
            ],
        };
        let missing_layer = Case {
            text:     "laall",
            pattern:  "ala",
            vertices: vec![(0, 1), (0, 2), (1, 3), (1, 4)],
            edges:    vec![((0, 1), (1, 3)), ((0, 1), (1, 4)), ((0, 2), (1, 3)), ((0, 2), (1, 4))],
        };
        let empty_text = Case { text: "", pattern: "ala", vertices: vec![], edges: vec![] };
        let empty_pattern =
            Case { text: "lalala", pattern: "", vertices: vec![], edges: vec![] };
        let longer_pattern =
            Case { text: "la", pattern: "ala", vertices: vec![(0, 1)], edges: vec![] };
        let non_ascii = Case {
            text:     "test wiadomości push: ęśąćż",
            pattern:  "tęś",
            vertices: vec![(0, 0), (0, 3), (1, 22), (2, 23)],
            edges:    vec![((0, 0), (1, 22)), ((0, 3), (1, 22)), ((1, 22), (2, 23))],
        };

        for case in
            vec![classic, missing_layer, empty_pattern, empty_text, longer_pattern, non_ascii]
        {
            case.run()
        }
    }
}
