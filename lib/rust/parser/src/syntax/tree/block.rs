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

/// Build a body block from a sequence of lines; this includes:
/// - Reinterpret the input expressions in statement context (i.e. expressions at the top-level of
///   the block that involve the `=` operator will be reinterpreted as function/variable bindings).
/// - Combine sibling lines in case of multi-line statements, such as annotated statements and
///   documented statements.
pub fn body_from_lines<'s>(lines: impl IntoIterator<Item = Line<'s>>) -> Tree<'s> {
    use crate::expression_to_statement;
    let lines = lines.into_iter();
    let mut statements = Vec::with_capacity(lines.size_hint().0);
    let mut prefixes: Vec<Prefix> = Vec::new();
    let mut newline = None;
    for line in lines {
        match prefixes.last_mut() {
            Some(prefix) => prefix.newlines().push(line.newline),
            None =>
                if let Some(empty_line) = newline.replace(line.newline) {
                    statements.push(empty_line.into());
                },
        };
        if let Some(expression) = line.expression {
            match Prefix::try_from(expression_to_statement(expression)) {
                Ok(prefix) => prefixes.push(prefix),
                Err(mut statement) => {
                    for prefix in prefixes.drain(..).rev() {
                        statement = prefix.apply_to(statement);
                    }
                    let newline = newline.take().unwrap();
                    statements.push(Line { newline, expression: Some(statement) });
                }
            }
        }
    }
    if let Some(prefix) = prefixes.pop() {
        let mut statement = prefix.into();
        for prefix in prefixes.drain(..).rev() {
            statement = prefix.apply_to(statement);
        }
        let newline = newline.take().unwrap();
        statements.push(Line { newline, expression: Some(statement) });
    }
    if let Some(line) = newline {
        match prefixes.last_mut() {
            Some(prefix) => prefix.newlines().push(line),
            None => statements.push(line.into()),
        }
    }
    Tree::body_block(statements)
}


// === Prefix-list representation ===

/// Representation used to build multi-line statements.
enum Prefix<'s> {
    Annotation { node: Annotated<'s>, span: Span<'s> },
    BuiltinAnnotation { node: AnnotatedBuiltin<'s>, span: Span<'s> },
    Documentation { node: Documented<'s>, span: Span<'s> },
}

impl<'s> TryFrom<Tree<'s>> for Prefix<'s> {
    type Error = Tree<'s>;
    fn try_from(tree: Tree<'s>) -> Result<Self, Self::Error> {
        match tree.variant {
            box Variant::Annotated(node) => Ok(Prefix::Annotation { node, span: tree.span }),
            box Variant::AnnotatedBuiltin(node @ AnnotatedBuiltin { expression: None, .. }) =>
                Ok(Prefix::BuiltinAnnotation { node, span: tree.span }),
            box Variant::Documented(node) => Ok(Prefix::Documentation { node, span: tree.span }),
            _ => Err(tree),
        }
    }
}

impl<'s> Prefix<'s> {
    fn newlines(&mut self) -> &mut Vec<token::Newline<'s>> {
        match self {
            Prefix::Annotation { node: Annotated { newlines, .. }, .. }
            | Prefix::BuiltinAnnotation { node: AnnotatedBuiltin { newlines, .. }, .. }
            | Prefix::Documentation {
                node: Documented { documentation: DocComment { newlines, .. }, .. },
                ..
            } => newlines,
        }
    }

    fn apply_to(mut self, expression: Tree<'s>) -> Tree<'s> {
        *(match &mut self {
            Prefix::Annotation { node, .. } => &mut node.expression,
            Prefix::BuiltinAnnotation { node, .. } => &mut node.expression,
            Prefix::Documentation { node, .. } => &mut node.expression,
        }) = Some(expression);
        self.into()
    }
}

impl<'s> From<Prefix<'s>> for Tree<'s> {
    fn from(prefix: Prefix<'s>) -> Self {
        match prefix {
            Prefix::Annotation { node, span } =>
                Tree { variant: Box::new(Variant::Annotated(node)), span },
            Prefix::BuiltinAnnotation { node, span } =>
                Tree { variant: Box::new(Variant::AnnotatedBuiltin(node)), span },
            Prefix::Documentation { node, span } =>
                Tree { variant: Box::new(Variant::Documented(node)), span },
        }
    }
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
fn to_operator_block_expression<'s>(
    items: Vec<Item<'s>>,
    precedence: &mut operator::Precedence<'s>,
) -> Result<OperatorBlockExpression<'s>, Tree<'s>> {
    if let Some(b) = items.get(1) && b.left_visible_offset().width_in_spaces != 0
        && let Some(Item::Token(a)) = items.first()
        && let token::Variant::Operator(op) = &a.variant {
            let operator = Ok(Token(a.left_offset.clone(), a.code.clone(), *op));
            let mut items = items.into_iter();
            items.next();
            let expression = precedence.resolve(items).unwrap();
            Ok(OperatorBlockExpression { operator, expression })
        } else {
            Err(precedence.resolve(items).unwrap())
        }
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
        items: Vec<Item<'s>>,
        precedence: &mut operator::Precedence<'s>,
    ) -> Self {
        let empty_lines = empty_lines.into_iter();
        let new_lines = 1;
        match to_operator_block_expression(items, precedence) {
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
    pub fn push(
        &mut self,
        newline: token::Newline<'s>,
        items: Vec<Item<'s>>,
        precedence: &mut operator::Precedence<'s>,
    ) {
        match self {
            Builder::Indeterminate { empty_lines } if items.is_empty() => empty_lines.push(newline),
            Builder::Indeterminate { empty_lines } =>
                *self = Self::new_with_expression(empty_lines.drain(..), newline, items, precedence),
            Builder::NonOperator { body_lines, .. } =>
                body_lines.push(Line { newline, expression: precedence.resolve(items) }),
            Builder::Operator { body_lines, .. } if !body_lines.is_empty() => {
                body_lines.push(Line { newline, expression: precedence.resolve(items) });
            }
            Builder::Operator { operator_lines, body_lines, .. } if !items.is_empty() =>
                match to_operator_block_expression(items, precedence) {
                    Ok(expression) => {
                        let expression = Some(expression);
                        operator_lines.push(OperatorLine { newline, expression });
                    }
                    Err(expression) => {
                        let expression = Some(expression);
                        body_lines.push(Line { newline, expression })
                    }
                },
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
