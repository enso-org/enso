use crate::prelude::*;

use span_tree::traits::*;
use span_tree::SpanTree;



// ==================
// === Expression ===
// ==================

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct Expression {
    pub pattern:             Option<String>,
    pub code:                String,
    pub whole_expression_id: Option<ast::Id>,
    pub input_span_tree:     SpanTree,
    pub output_span_tree:    SpanTree,
}

impl Expression {
    /// Constructor without output SpanTree and with single node as an input SpanTree.
    pub fn new_plain(code: impl Into<String>) -> Self {
        let pattern = default();
        let code = code.into();
        let input_span_tree =
            code.generate_tree(&span_tree::generate::context::Empty).unwrap_or_default();
        let output_span_tree = default();
        let whole_expression_id = default();
        Self { pattern, code, whole_expression_id, input_span_tree, output_span_tree }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.code, f)
    }
}

impl From<&Expression> for Expression {
    fn from(t: &Expression) -> Self {
        t.clone()
    }
}
