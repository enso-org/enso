use crate::prelude::*;

use span_tree::SpanTree;
use span_tree::traits::*;



// ==================
// === Expression ===
// ==================

#[derive(Clone,Default,Eq,PartialEq)]
pub struct Expression {
    pub pattern          : Option<String>,
    pub code             : String,
    pub input_span_tree  : SpanTree,
    pub output_span_tree : SpanTree,
}

impl Expression {
    /// Constructor without output SpanTree and with single node as an input SpanTree.
    pub fn new_plain(code:impl Into<String>) -> Self {
        let pattern          = default();
        let code             = code.into();
        let input_span_tree  = code.generate_tree(&span_tree::generate::context::Empty).unwrap_or_default();
        let output_span_tree = default();
        Self {pattern,code,input_span_tree,output_span_tree}
    }
}

impl Debug for Expression {
    fn fmt(&self, f:&mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f,"Expression({})",self.code)
    }
}

impl From<&Expression> for Expression {
    fn from(t:&Expression) -> Self {
        t.clone()
    }
}
