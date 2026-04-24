//! Code blocks.

use crate::syntax::tree::*;

use crate::syntax::statement::BodyBlockParser;

// =============
// === Lines ===
// =============

/// A line of code.
#[cfg_attr(feature = "debug", derive(Visitor))]
#[derive(Debug, Clone, PartialEq, Eq, Reflect, Serialize, Deserialize)]
pub struct Line<'s> {
    /// Token ending the previous line, if any.
    pub newline: token::Newline<'s>,
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

/// Parse the top-level of a module.
pub fn parse_module<'s>(
    lines: &mut Vec<item::Line<'s>>,
    expression_parser: &mut expression::ExpressionParser<'s>,
) -> Tree<'s> {
    BodyBlockParser::default().parse_module(lines, expression_parser)
}

/// Parse a body block.
pub fn parse_block<'s>(
    lines: &mut Vec<item::Line<'s>>,
    expression_parser: &mut expression::ExpressionParser<'s>,
) -> Tree<'s> {
    BodyBlockParser::default().parse_body_block(lines, expression_parser)
}

// === Multi-line expression construction ===

/// Adapts a sequence of lines by combining sibling lines in case of multi-line statements, such as
/// annotated statements and documented statements.
pub fn compound_lines<'s, I: IntoIterator<Item = Line<'s>>>(
    lines: I,
) -> CompoundLines<'s, I::IntoIter> {
    CompoundLines { lines: lines.into_iter(), prefixes: default(), newline: default() }
}

/// [`Iterator`] that adapts a sequence of lines by merging multi-line statements.
#[derive(Debug)]
pub struct CompoundLines<'s, I> {
    lines: I,
    prefixes: Vec<Prefix<'s>>,
    newline: Option<token::Newline<'s>>,
}

impl<'s, I> Iterator for CompoundLines<'s, I>
where
    I: Iterator<Item = Line<'s>>,
{
    type Item = Line<'s>;
    fn next(&mut self) -> Option<Self::Item> {
        for line in &mut self.lines {
            match line.expression.map(Prefix::try_from) {
                Some(Ok(prefix)) => {
                    match self.prefixes.last_mut() {
                        Some(prefix) => prefix.push_newline(line.newline),
                        None => self.newline = Some(line.newline),
                    };
                    self.prefixes.push(prefix);
                }
                Some(Err(mut statement)) => {
                    return Some(match self.prefixes.last_mut() {
                        Some(prefix) => {
                            prefix.push_newline(line.newline);
                            for prefix in self.prefixes.drain(..).rev() {
                                statement = prefix.apply_to(statement);
                            }
                            let newline = self.newline.take().unwrap();
                            Line { newline, expression: Some(statement) }
                        }
                        None => Line { newline: line.newline, expression: Some(statement) },
                    });
                }
                None => {
                    match self.prefixes.last_mut() {
                        Some(prefix) => prefix.push_newline(line.newline),
                        None => return Some(line.newline.into()),
                    };
                }
            }
        }
        if let Some(prefix) = self.prefixes.pop() {
            let mut statement = prefix.into();
            for prefix in self.prefixes.drain(..).rev() {
                statement = prefix.apply_to(statement);
            }
            let newline = self.newline.take().unwrap();
            return Some(Line { newline, expression: Some(statement) });
        }
        if let Some(line) = self.newline.take() {
            return Some(line.into());
        }
        None
    }
}

// === Prefix-list representation ===

/// Representation used to build multi-line statements.
#[derive(Debug)]
enum Prefix<'s> {
    BuiltinAnnotation { node: Box<AnnotatedBuiltin<'s>>, span: Span<'s> },
}

impl<'s> TryFrom<Tree<'s>> for Prefix<'s> {
    type Error = Tree<'s>;
    fn try_from(tree: Tree<'s>) -> Result<Self, Self::Error> {
        match tree.variant {
            Variant::AnnotatedBuiltin(node) if node.expression.is_none() => {
                Ok(Prefix::BuiltinAnnotation { node, span: tree.span })
            }
            _ => Err(tree),
        }
    }
}

impl<'s> Prefix<'s> {
    fn push_newline(&mut self, newline: token::Newline<'s>) {
        let (newlines, span) = match self {
            Prefix::BuiltinAnnotation { node, span } => (&mut node.newlines, span),
        };
        span.code_length += newline.left_offset.code.length() + newline.code.length();
        newlines.push(newline);
    }

    fn apply_to(mut self, expression: Tree<'s>) -> Tree<'s> {
        let (expr, span) = match &mut self {
            Prefix::BuiltinAnnotation { node, span } => (&mut node.expression, span),
        };
        span.code_length += expression.span.left_offset.code.length() + expression.span.code_length;
        *expr = Some(expression);
        self.into()
    }
}

impl<'s> From<Prefix<'s>> for Tree<'s> {
    fn from(prefix: Prefix<'s>) -> Self {
        match prefix {
            Prefix::BuiltinAnnotation { node, span } => {
                Tree { variant: Variant::AnnotatedBuiltin(node), span, warnings: default() }
            }
        }
    }
}

// ======================
// === Operator Block ===
// ======================

/// The content of a line in an operator block.
#[cfg_attr(feature = "debug", derive(Visitor))]
#[derive(Debug, Clone, PartialEq, Eq, Reflect, Serialize, Deserialize)]
pub struct OperatorBlockExpression<'s> {
    /// The operator at the beginning of the line.
    pub operator: OperatorOrError<'s>,
    /// The rest of the expression.
    pub expression: Tree<'s>,
}

impl<'s> span::Builder<'s> for OperatorBlockExpression<'s> {
    fn add_to_span(&mut self, span: Span<'s>) -> Span<'s> {
        span.add(&mut self.operator).add(&mut self.expression)
    }
}

// === Operator block lines ====

/// A line in an operator block.
#[cfg_attr(feature = "debug", derive(Visitor))]
#[derive(Debug, Clone, PartialEq, Eq, Reflect, Serialize, Deserialize)]
pub struct OperatorLine<'s> {
    /// Token ending the previous line, if any.
    pub newline: token::Newline<'s>,
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
