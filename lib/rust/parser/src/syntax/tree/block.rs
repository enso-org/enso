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

/// Parse the top-level of a module.
pub fn parse_module<'s>(
    lines: &mut Vec<item::Line<'s>>,
    precedence: &mut operator::Precedence<'s>,
) -> Tree<'s> {
    BodyBlockParser::default().parse_module(lines, precedence)
}

/// Parse a body block.
pub fn parse_block<'s>(
    lines: &mut Vec<item::Line<'s>>,
    precedence: &mut operator::Precedence<'s>,
) -> Tree<'s> {
    BodyBlockParser::default().parse_body_block(lines, precedence)
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
    lines:    I,
    prefixes: Vec<Prefix<'s>>,
    newline:  Option<token::Newline<'s>>,
}

impl<'s, I> Iterator for CompoundLines<'s, I>
where I: Iterator<Item = Line<'s>>
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
            Variant::AnnotatedBuiltin(node) if node.expression.is_none() =>
                Ok(Prefix::BuiltinAnnotation { node, span: tree.span }),
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
            Prefix::BuiltinAnnotation { node, span } =>
                Tree { variant: Variant::AnnotatedBuiltin(node), span, warnings: default() },
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
    pub operator:   OperatorOrError<'s>,
    /// The rest of the expression.
    pub expression: Tree<'s>,
}

/// Interpret the given expression as an `OperatorBlockExpression`, if it fits the correct pattern.
fn to_operator_block_expression<'s>(
    mut items: Vec<Item<'s>>,
    precedence: &mut operator::Precedence<'s>,
) -> Result<OperatorBlockExpression<'s>, Tree<'s>> {
    match &items[..] {
        [Item::Token(a), b, ..]
            if b.left_visible_offset().width_in_spaces != 0
                && a.operator_properties().is_some_and(|p| p.can_form_section()) =>
        {
            let expression = precedence.resolve_offset(1, &mut items).unwrap();
            let operator = Ok(items
                .pop()
                .unwrap()
                .into_token()
                .unwrap()
                .with_variant(token::variant::Operator()));
            Ok(OperatorBlockExpression { operator, expression })
        }
        _ => Err(precedence.resolve(&mut items).unwrap()),
    }
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

/// Builds an argument block or operator block from a sequence of lines.
///
/// The implementation is a state machine. The only top-level transitions are:
/// - `Indeterminate` -> `Operator`
/// - `Indeterminate` -> `NonOperator`
///
/// The `Operator` state has two substates, and one possible transition:
/// - `body_lines is empty` -> `body_lines is not empty`
#[derive(Debug, Default)]
pub struct Builder<'s> {
    state:          State,
    empty_lines:    Vec<token::Newline<'s>>,
    operator_lines: Vec<OperatorLine<'s>>,
    body_lines:     Vec<Line<'s>>,
}

#[derive(Debug, Default)]
enum State {
    /// The builder is in an indeterminate state until a non-empty line has been encountered, which
    /// would distinguish an operator-block from a non-operator block.
    // `empty_lines` contains the `Newline` token introducing the block, and `Newline` tokens for
    // any empty lines that have been encountered.
    #[default]
    Indeterminate,
    /// Building an operator block. If any line doesn't fit the operator-block syntax, that line
    /// and all following will be placed in `body_lines`.
    // `operator_lines` contains valid operator-block expressions.
    // `body_lines` contains any lines violating the expected operator-block syntax.
    Operator,
    /// Building an argument block.
    // `body_lines` contains the block content.
    Argument,
}

impl<'s> Builder<'s> {
    /// Create a new instance, in initial state.
    pub fn new() -> Self {
        Self::default()
    }

    /// Apply a new line to the state.
    pub fn push(
        &mut self,
        newline: token::Newline<'s>,
        mut items: Vec<Item<'s>>,
        precedence: &mut operator::Precedence<'s>,
    ) {
        match &mut self.state {
            State::Indeterminate if items.is_empty() => self.empty_lines.push(newline),
            State::Indeterminate => {
                self.state = match to_operator_block_expression(items, precedence) {
                    Ok(expression) => {
                        self.operator_lines
                            .push(OperatorLine { newline, expression: Some(expression) });
                        State::Operator
                    }
                    Err(expression) => {
                        self.body_lines.push(Line { newline, expression: Some(expression) });
                        State::Argument
                    }
                };
            }
            State::Argument =>
                self.body_lines.push(Line { newline, expression: precedence.resolve(&mut items) }),
            State::Operator if !self.body_lines.is_empty() =>
                self.body_lines.push(Line { newline, expression: precedence.resolve(&mut items) }),
            State::Operator if items.is_empty() => self.operator_lines.push(newline.into()),
            State::Operator => match to_operator_block_expression(items, precedence) {
                Ok(expression) =>
                    self.operator_lines.push(OperatorLine { newline, expression: Some(expression) }),
                Err(expression) =>
                    self.body_lines.push(Line { newline, expression: Some(expression) }),
            },
        }
    }

    /// Produce an AST node from the state.
    pub fn build(&mut self) -> Tree<'s> {
        match self.state {
            State::Operator => {
                let mut operator_lines =
                    Vec::with_capacity(self.empty_lines.len() + self.operator_lines.len());
                operator_lines.extend(self.empty_lines.drain(..).map(OperatorLine::from));
                operator_lines.append(&mut self.operator_lines);
                Tree::operator_block_application(None, operator_lines, self.body_lines.split_off(0))
            }
            State::Argument | State::Indeterminate => {
                let mut body_lines =
                    Vec::with_capacity(self.empty_lines.len() + self.body_lines.len());
                body_lines.extend(self.empty_lines.drain(..).map(Line::from));
                body_lines.append(&mut self.body_lines);
                Tree::argument_block_application(None, body_lines)
            }
        }
    }
}
