//! FIXME[everyone] Modules should be documented.

use crate::prelude::*;
use span_tree::traits::*;

use span_tree::SpanTree;



// ==================
// === Expression ===
// ==================

#[derive(Clone, Debug, Default, Eq, PartialEq)]
#[allow(missing_docs)] // FIXME[everyone] Public-facing API should be documented.
pub struct Expression {
    pub pattern:             Option<String>,
    pub code:                ImString,
    pub whole_expression_id: Option<ast::Id>,
    pub input_span_tree:     SpanTree,
    pub output_span_tree:    SpanTree,
}

impl Expression {
    /// Constructor without output SpanTree and with single node as an input SpanTree.
    pub fn new_plain(code: impl Into<String>) -> Self {
        let pattern = default();
        let code = ImString::new(code.into());
        let input_span_tree =
            code.as_str().generate_tree(&span_tree::generate::context::Empty).unwrap_or_default();
        let output_span_tree = default();
        let whole_expression_id = default();
        Self { pattern, code, whole_expression_id, input_span_tree, output_span_tree }
    }

    /// Get the expression code with given span replaced with provided string. Does not modify the
    /// existing expression.
    pub fn code_with_replaced_span(
        &self,
        crumbs: &span_tree::Crumbs,
        replacement: &str,
    ) -> ImString {
        if let Ok(span_ref) = self.input_span_tree.get_node(crumbs) {
            let span = span_ref.span();
            let byte_range = span.start.value..span.end.value;
            let mut code = self.code.to_string();
            code.replace_range(byte_range, replacement);
            code.into()
        } else {
            self.code.clone()
        }
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
