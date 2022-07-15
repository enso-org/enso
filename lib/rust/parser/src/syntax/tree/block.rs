//! Code blocks.



use crate::syntax::tree::*;



/// A line of code.
#[derive(Debug, Clone, PartialEq, Eq, Visitor, Reflect, Serialize, Deserialize)]
pub struct Line<'s> {
    /// The content of the line, if any.
    pub expression: Option<Tree<'s>>,
    /// The end-of-line token.
    pub newline:    token::Newline<'s>,
}

impl<'s> Line<'s> {
    /// Transform the content of the line with the provided function, if any is present; return the
    /// result.
    pub fn map_expression(self, f: impl FnOnce(Tree<'s>) -> Tree<'s>) -> Self {
        let Self { newline, expression } = self;
        let expression = expression.map(f);
        Self { newline, expression }
    }
}

impl<'s> From<token::Newline<'s>> for Line<'s> {
    fn from(newline: token::Newline<'s>) -> Self {
        Self { expression: None, newline }
    }
}

impl<'s> span::Builder<'s> for Line<'s> {
    fn add_to_span(&mut self, span: Span<'s>) -> Span<'s> {
        span.add(&mut self.expression).add(&mut self.newline)
    }
}


// === BodyBlock ===

/// Build a body block from a sequence of lines; this involves reinterpreting the input expressions
/// in statement context (i.e. expressions at the top-level of the block that involve the `=`
/// operator will be reinterpreted as function/variable bindings).
pub fn body_from_lines<'s>(
    block_start_newline: token::Newline<'s>,
    expressions: impl IntoIterator<Item = Line<'s>>,
) -> Tree<'s> {
    use crate::expression_to_statement;
    let expressions = expressions.into_iter();
    let statements = expressions.map(|line| line.map_expression(expression_to_statement));
    let statements = statements.collect();
    Tree::body_block(block_start_newline, statements)
}


// === OperatorBlock ===

/// The content of a line in an operator block.
#[derive(Debug, Clone, PartialEq, Eq, Visitor, Reflect, Serialize, Deserialize)]
pub struct OperatorBlockExpression<'s> {
    /// The operator at the beginning of the line.
    pub operator:   OperatorOrError<'s>,
    /// The rest of the expression.
    pub expression: Tree<'s>,
}

/// Interpret the given expression as an `OperatorBlockExpression`, if it fits the correct pattern.
fn to_operator_block_expression(
    expression_: Tree<'_>,
) -> Result<OperatorBlockExpression<'_>, Tree<'_>> {
    let tree_ = match &*expression_.variant {
        Variant::OprSectionBoundary(OprSectionBoundary { ast }) => ast,
        _ => return Err(expression_),
    };
    if let Variant::OprApp(OprApp { lhs: None, opr, rhs: Some(expression) }) = &*tree_.variant {
        if expression.span.left_offset.visible.width_in_spaces < 1 {
            return Err(expression_);
        }
        let mut operator = opr.clone();
        operator.first_operator_mut().left_offset = expression_.span.left_offset;
        let expression = expression.clone();
        Ok(OperatorBlockExpression { operator, expression })
    } else {
        Err(expression_)
    }
}

impl<'s> span::Builder<'s> for OperatorBlockExpression<'s> {
    fn add_to_span(&mut self, span: Span<'s>) -> Span<'s> {
        span.add(&mut self.operator).add(&mut self.expression)
    }
}

/// A line in an operator block.
#[derive(Debug, Clone, PartialEq, Eq, Visitor, Reflect, Serialize, Deserialize)]
pub struct OperatorLine<'s> {
    /// The operator-expression, if any.
    pub expression: Option<OperatorBlockExpression<'s>>,
    /// The end-of-line token.
    pub newline:    token::Newline<'s>,
}

impl<'s> From<token::Newline<'s>> for OperatorLine<'s> {
    fn from(newline: token::Newline<'s>) -> Self {
        Self { expression: None, newline }
    }
}

impl<'s> span::Builder<'s> for OperatorLine<'s> {
    fn add_to_span(&mut self, span: Span<'s>) -> Span<'s> {
        span.add(&mut self.expression).add(&mut self.newline)
    }
}



// =====================
// === Block Builder ===
// =====================

/// Builds an AST block type from a sequence of lines.
///
/// Note that the block type is not fully determined at this stage: We apply context information
/// later (see `apply_operator`) to distinguish the two non-operator block types, `BodyBlock` and
/// `ArgumentBlockApplication`. Here we treat every non-operator block as an argument block,
/// because creating a body block involves re-interpreting the expressions in statement context.
///
/// The implementation is a state machine. The only top-level transitions are:
/// - `Indeterminate` -> `Operator`
/// - `Indeterminate` -> `NonOperator`
///
/// The `Operator` state has two substates, and one possible transition:
/// - `body_lines is empty` -> `body_lines is not empty`
#[derive(Debug)]
pub enum Builder<'s> {
    /// The builder is in an indeterminate state until a non-empty line has been encountered, which
    /// would distinguish an operator-block from a non-operator block.
    Indeterminate {
        /// The `Newline` token introducing the block, and `Newline` tokens for any empty lines
        /// that have been encountered.
        empty_lines: Vec<token::Newline<'s>>,
    },
    /// Building an operator block. If any line doesn't fit the operator-block syntax, that line
    /// and all following will be placed in `body_lines`.
    Operator {
        /// The `Newline` token introducing the block.
        block_start_newline: token::Newline<'s>,
        /// Valid operator-block expressions.
        operator_lines:      Vec<OperatorLine<'s>>,
        /// Any lines violating the expected operator-block syntax.
        body_lines:          Vec<Line<'s>>,
    },
    /// Building a non-operator block (either a body block or an argument block).
    NonOperator {
        /// The `Newline` token introducing the block.
        block_start_newline: token::Newline<'s>,
        /// The block content.
        body_lines:          Vec<Line<'s>>,
    },
}

impl<'s> Builder<'s> {
    /// Create a new instance, in initial state.
    pub fn new() -> Self {
        Self::Indeterminate { empty_lines: default() }
    }

    /// Create a new instance, in a state appropriate for the given expression.
    fn new_with_expression(
        empty_lines: impl IntoIterator<Item = token::Newline<'s>>,
        expression: Tree<'s>,
        newline: token::Newline<'s>,
    ) -> Self {
        let mut empty_lines = empty_lines.into_iter();
        let block_start_newline = empty_lines.next().unwrap_or_else(|| token::newline("", ""));
        let new_lines = 1;
        match to_operator_block_expression(expression) {
            Ok(expression) => {
                let expression = Some(expression);
                let mut operator_lines = Vec::with_capacity(empty_lines.size_hint().0 + new_lines);
                operator_lines.extend(empty_lines.map(block::OperatorLine::from));
                operator_lines.push(OperatorLine { expression, newline });
                Self::Operator { operator_lines, block_start_newline, body_lines: default() }
            }
            Err(expression) => {
                let expression = Some(expression);
                let mut body_lines = Vec::with_capacity(empty_lines.size_hint().0 + new_lines);
                body_lines.extend(empty_lines.map(block::Line::from));
                body_lines.push(Line { expression, newline });
                Self::NonOperator { body_lines, block_start_newline }
            }
        }
    }

    /// Apply a new line to the state.
    pub fn push(&mut self, expression: Option<Tree<'s>>, newline: token::Newline<'s>) {
        match self {
            Builder::Indeterminate { empty_lines } => match expression {
                Some(expression) =>
                    *self = Self::new_with_expression(empty_lines.drain(..), expression, newline),
                None => empty_lines.push(newline),
            },
            Builder::NonOperator { body_lines, .. } =>
                body_lines.push(Line { expression, newline }),
            Builder::Operator { body_lines, .. } if !body_lines.is_empty() => {
                body_lines.push(Line { expression, newline });
            }
            Builder::Operator { operator_lines, body_lines, .. }
            if let Some(expression) = expression => {
                match to_operator_block_expression(expression) {
                    Ok(expression) => {
                        let expression = Some(expression);
                        operator_lines.push(OperatorLine { expression, newline });
                    }
                    Err(expression) => {
                        let expression = Some(expression);
                        body_lines.push(Line { expression, newline })
                    },
                }
            }
            Builder::Operator { operator_lines, .. } => operator_lines.push(newline.into()),
        }
    }

    /// Produce an AST node from the state.
    pub fn build(self) -> Tree<'s> {
        match self {
            Builder::Indeterminate { empty_lines } => {
                let mut empty_lines = empty_lines.into_iter();
                let block_start_newline =
                    empty_lines.next().unwrap_or_else(|| token::newline("", ""));
                let lines = empty_lines.map(Line::from).collect();
                Tree::argument_block_application(None, block_start_newline, lines)
            }
            Builder::Operator { operator_lines, body_lines, block_start_newline } =>
                Tree::operator_block_application(
                    None,
                    block_start_newline,
                    operator_lines,
                    body_lines,
                ),
            Builder::NonOperator { body_lines, block_start_newline } =>
                Tree::argument_block_application(None, block_start_newline, body_lines),
        }
    }
}

impl<'s> Default for Builder<'s> {
    fn default() -> Self {
        Self::new()
    }
}
