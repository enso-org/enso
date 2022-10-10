//! Code blocks.

use crate::syntax::tree::*;

use crate::syntax::token;



// =============
// === Lines ===
// =============

/// A line of code.
#[derive(Debug, Clone, PartialEq, Eq, Visitor, Reflect, Serialize, Deserialize)]
pub struct Line<'s> {
    /// Token ending the previous line, if any.
    pub newline:    token::Newline<'s>,
    /// The content of the line, if any.
    pub expression: Option<Tree<'s>>,
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
        Self { newline, expression: None }
    }
}

impl<'s> span::Builder<'s> for Line<'s> {
    fn add_to_span(&mut self, span: Span<'s>) -> Span<'s> {
        span.add(&mut self.newline).add(&mut self.expression)
    }
}



// ==================
// === Body Block ===
// ==================

/// Build a body block from a sequence of lines; this involves reinterpreting the input expressions
/// in statement context (i.e. expressions at the top-level of the block that involve the `=`
/// operator will be reinterpreted as function/variable bindings).
pub fn body_from_lines<'s>(lines: impl IntoIterator<Item = Line<'s>>) -> Tree<'s> {
    use crate::expression_to_statement;
    let mut lines = lines.into_iter();
    let mut statements = Vec::with_capacity(lines.size_hint().0);
    while let Some(line) = lines.next() {
        let mut statement = line.map_expression(expression_to_statement);
        if let Some(Tree {
            variant: box Variant::Annotated(Annotated { newlines, expression, .. }),
            ..
        }) = &mut statement.expression
        {
            while expression.is_none() && let Some(line) = lines.next() {
                let statement = line.map_expression(expression_to_statement);
                newlines.push(statement.newline);
                *expression = statement.expression;
            }
        }
        statements.push(statement);
    }
    Tree::body_block(statements)
}



// ======================
// === Operator Block ===
// ======================

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
    mut tree: Tree<'_>,
) -> Result<OperatorBlockExpression<'_>, Tree<'_>> {
    let mut left_operator = None;
    recurse_left_mut_while(&mut tree, |tree| {
        if let Variant::OprApp(OprApp { lhs: None, opr, rhs }) = &mut *tree.variant
            && let Some(rhs_) = rhs
            && rhs_.span.left_offset.visible.width_in_spaces >= 1
        {
            left_operator = Some(opr.clone());
            *tree = mem::take(rhs).unwrap();
        }
        true
    });
    let Some(mut operator) = left_operator else {
        return Err(tree);
    };
    operator.first_operator_mut().left_offset += mem::take(&mut tree.span.left_offset);
    if let Variant::OprSectionBoundary(OprSectionBoundary { arguments, .. }) = &mut *tree.variant {
        *arguments -= 1;
    }
    let expression = match *tree.variant {
        Variant::OprSectionBoundary(OprSectionBoundary { ast, arguments: 0 }) => {
            operator.first_operator_mut().left_offset += tree.span.left_offset;
            ast
        }
        _ => tree,
    };
    Ok(OperatorBlockExpression { operator, expression })
}

impl<'s> span::Builder<'s> for OperatorBlockExpression<'s> {
    fn add_to_span(&mut self, span: Span<'s>) -> Span<'s> {
        span.add(&mut self.operator).add(&mut self.expression)
    }
}


// === Operator block lines ====

/// A line in an operator block.
#[derive(Debug, Clone, PartialEq, Eq, Visitor, Reflect, Serialize, Deserialize)]
pub struct OperatorLine<'s> {
    /// Token ending the previous line, if any.
    pub newline:    token::Newline<'s>,
    /// The operator-expression, if any.
    pub expression: Option<OperatorBlockExpression<'s>>,
}

impl<'s> From<token::Newline<'s>> for OperatorLine<'s> {
    fn from(newline: token::Newline<'s>) -> Self {
        Self { newline, expression: None }
    }
}

impl<'s> span::Builder<'s> for OperatorLine<'s> {
    fn add_to_span(&mut self, span: Span<'s>) -> Span<'s> {
        span.add(&mut self.newline).add(&mut self.expression)
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
        /// Valid operator-block expressions.
        operator_lines: Vec<OperatorLine<'s>>,
        /// Any lines violating the expected operator-block syntax.
        body_lines:     Vec<Line<'s>>,
    },
    /// Building a non-operator block (either a body block or an argument block).
    NonOperator {
        /// The block content.
        body_lines: Vec<Line<'s>>,
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
        newline: token::Newline<'s>,
        expression: Tree<'s>,
    ) -> Self {
        let empty_lines = empty_lines.into_iter();
        let new_lines = 1;
        match to_operator_block_expression(expression) {
            Ok(expression) => {
                let expression = Some(expression);
                let mut operator_lines = Vec::with_capacity(empty_lines.size_hint().0 + new_lines);
                operator_lines.extend(empty_lines.map(block::OperatorLine::from));
                operator_lines.push(OperatorLine { newline, expression });
                Self::Operator { operator_lines, body_lines: default() }
            }
            Err(expression) => {
                let expression = Some(expression);
                let mut body_lines = Vec::with_capacity(empty_lines.size_hint().0 + new_lines);
                body_lines.extend(empty_lines.map(block::Line::from));
                body_lines.push(Line { newline, expression });
                Self::NonOperator { body_lines }
            }
        }
    }

    /// Apply a new line to the state.
    pub fn push(&mut self, newline: token::Newline<'s>, expression: Option<Tree<'s>>) {
        match self {
            Builder::Indeterminate { empty_lines } => match expression {
                Some(expression) =>
                    *self = Self::new_with_expression(empty_lines.drain(..), newline, expression),
                None => empty_lines.push(newline),
            },
            Builder::NonOperator { body_lines, .. } =>
                body_lines.push(Line { newline, expression }),
            Builder::Operator { body_lines, .. } if !body_lines.is_empty() => {
                body_lines.push(Line { newline, expression });
            }
            Builder::Operator { operator_lines, body_lines, .. }
            if let Some(expression) = expression => {
                match to_operator_block_expression(expression) {
                    Ok(expression) => {
                        let expression = Some(expression);
                        operator_lines.push(OperatorLine { newline, expression });
                    }
                    Err(expression) => {
                        let expression = Some(expression);
                        body_lines.push(Line { newline, expression })
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
                let empty_lines = empty_lines.into_iter();
                let lines = empty_lines.map(Line::from).collect();
                Tree::argument_block_application(None, lines)
            }
            Builder::Operator { operator_lines, body_lines } =>
                Tree::operator_block_application(None, operator_lines, body_lines),
            Builder::NonOperator { body_lines } =>
                Tree::argument_block_application(None, body_lines),
        }
    }
}

impl<'s> Default for Builder<'s> {
    fn default() -> Self {
        Self::new()
    }
}



// =============
// === Lines ===
// =============

/// Given an iterable of [`Item`]s, return an iterator of the [`Line`]s produced by dividing the
/// input at newline tokens, and parsing the expressions with
/// [`operator::resolve_operator_precedence`].
pub fn lines<'s, I, J>(items: I) -> Lines<'s, J>
where
    I: IntoIterator<IntoIter = J>,
    J: Iterator<Item = Item<'s>>, {
    let items = items.into_iter();
    let newline = default();
    let line = default();
    let finished = default();
    let precedence = default();
    Lines { items, newline, line, finished, precedence }
}

/// An iterator of [`Line`]s.
#[derive(Debug)]
pub struct Lines<'s, I> {
    items:      I,
    newline:    token::Newline<'s>,
    line:       Vec<Item<'s>>,
    finished:   bool,
    precedence: operator::Precedence<'s>,
}

impl<'s, I> Lines<'s, I> {
    fn parse_current_line(&mut self, newline: token::Newline<'s>) -> Line<'s> {
        let expression = self.precedence.resolve(self.line.drain(..));
        Line { newline, expression }
    }
}

impl<'s, I> Iterator for Lines<'s, I>
where I: Iterator<Item = Item<'s>>
{
    type Item = Line<'s>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.finished {
            return None;
        }
        while let Some(item) = self.items.next() {
            match item {
                Item::Token(Token { variant: token::Variant::Newline(_), left_offset, code }) => {
                    let token = token::newline(left_offset, code);
                    let newline = mem::replace(&mut self.newline, token);
                    // If the block started with a real newline, ignore the implicit newline.
                    if !(newline.code.is_empty()
                        && newline.left_offset.code.is_empty()
                        && self.line.is_empty())
                    {
                        return self.parse_current_line(newline).into();
                    }
                }
                _ => {
                    self.line.push(item);
                }
            }
        }
        self.finished = true;
        let newline = mem::take(&mut self.newline);
        self.parse_current_line(newline).into()
    }
}
