use crate::prelude::*;

use crate::syntax::Finish;
use crate::syntax::Item;
use crate::syntax::ItemConsumer;
use crate::syntax::Tree;
use crate::syntax::expression::ExpressionParser;
use crate::syntax::expression::Spacing;
use crate::syntax::expression::operand::Operand;
use crate::syntax::expression::types::Arity;
use crate::syntax::expression::types::ModifiedPrecedence;
use crate::syntax::expression::types::Operator;
use crate::syntax::expression::types::OperatorConsumer;
use crate::syntax::item;
use crate::syntax::statement::BodyBlockParser;
use crate::syntax::token;
use crate::syntax::token::Associativity;
use crate::syntax::token::Precedence;
use crate::syntax::token::TokenOperatorProperties;
use crate::syntax::tree::block::Line;
use crate::syntax::tree::block::OperatorBlockExpression;
use crate::syntax::tree::block::OperatorLine;
use crate::unwrap_call;

/// Consumes `Item`s and passes their content to a token/tree consumer, using an
/// [`ExpressionParser`] to flatten blocks.
#[derive(Debug, Default)]
pub struct FlattenBlockTrees<'s, Inner> {
    /// Consumes child blocks. Stores no semantic state, but is reused for performance.
    child: Option<Box<ExpressionParser<'s>>>,
    block_builder: ApplicableBlockBuilder<'s>,
    block_parser: BodyBlockParser<'s>,
    inner: Inner,
}

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
enum BlockContext {
    #[default]
    Body,
    ArgumentOrOperator,
}

impl<'s> From<ApplicableBlock<'s>> for Operator<'s> {
    fn from(value: ApplicableBlock<'s>) -> Self {
        let precedence = ModifiedPrecedence::new(Spacing::Spaced, Precedence::Arrow, false);
        Operator {
            left_precedence: Some(precedence),
            right_precedence: None,
            associativity: Associativity::Left,
            arity: Arity::UnappliedBlock(value),
        }
    }
}

impl<'s, Inner> FlattenBlockTrees<'s, Inner>
where
    Inner: ItemConsumer<'s> + OperatorConsumer<'s> + Finish<Result = Option<Operand<'s>>>,
{
    pub fn run(&mut self, start: usize, items: &mut Vec<Item<'s>>) -> Option<Operand<'s>> {
        if let Some(Item::Block(_)) = items.last() {
            let Some(Item::Block(lines)) = items.pop() else { unreachable!() };
            let block_context = match items.last() {
                Some(Item::Token(token)) => match token.operator_properties() {
                    Some(properties) if properties.rhs_is_expression() => BlockContext::Body,
                    _ => BlockContext::ArgumentOrOperator,
                },
                None => BlockContext::Body,
                _ => BlockContext::ArgumentOrOperator,
            };
            for item in items.drain(start..) {
                self.inner.push_item(item);
            }
            let mut child = self.child.take().unwrap_or_default();
            match block_context {
                BlockContext::Body => {
                    // A body block completes the expression (or it may be the entire expression).
                    // Parse the block and append it to the expression to be treated as an operand.
                    let child_block =
                        self.block_parser.parse_body_block(&mut lines.into_vec(), &mut child);
                    self.inner.push_item(child_block.into());
                }
                BlockContext::ArgumentOrOperator => {
                    // A special block acts as an operator applied to the preceding expression.
                    let special_block =
                        self.block_builder.build_block(lines.into_vec(), &mut child);
                    self.inner.push_operator(special_block.into());
                }
            }
            self.child = Some(child);
        } else {
            for item in items.drain(start..) {
                self.inner.push_item(item);
            }
        }
        self.inner.finish()
    }
}

// === Applicable Block Builder ===

/// Builds block that act as postfix operators applied to the preceding expression (argument blocks,
/// operator blocks).
///
/// The implementation is a state machine. The only top-level transitions are:
/// - `Indeterminate` -> `Operator`
/// - `Indeterminate` -> `NonOperator`
///
/// The `Operator` state has two substates, and one possible transition:
/// - `body_lines is empty` -> `body_lines is not empty`
#[derive(Debug, Default)]
struct ApplicableBlockBuilder<'s> {
    state: State,
    empty_lines: Vec<token::Newline<'s>>,
    operator_lines: Vec<OperatorLine<'s>>,
    body_lines: Vec<Line<'s>>,
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

#[derive(Debug, ApplyToOperand)]
pub enum ApplicableBlock<'s> {
    OperatorBlock { operator_lines: Vec<OperatorLine<'s>>, excess: Vec<Line<'s>> },
    ArgumentBlock { body_lines: Vec<Line<'s>> },
}

impl<'s> ApplicableBlock<'s> {
    fn apply(self, expression: Option<Tree<'s>>) -> Tree<'s> {
        match self {
            Self::OperatorBlock { operator_lines, excess } => {
                Tree::operator_block_application(expression, operator_lines, excess)
            }
            Self::ArgumentBlock { body_lines } => {
                let expression = expression.map(unwrap_call);
                Tree::argument_block_application(expression, body_lines)
            }
        }
    }
}

impl<'s> ApplicableBlockBuilder<'s> {
    /// Produce an AST node from the given lines.
    pub fn build_block(
        &mut self,
        lines: impl IntoIterator<Item = item::Line<'s>>,
        expression_parser: &mut ExpressionParser<'s>,
    ) -> ApplicableBlock<'s> {
        for item::Line { newline, items } in lines {
            self.push(newline, items, expression_parser);
        }
        self.build()
    }

    fn push(
        &mut self,
        newline: token::Newline<'s>,
        mut items: Vec<Item<'s>>,
        expression_parser: &mut ExpressionParser<'s>,
    ) {
        match &mut self.state {
            State::Indeterminate if items.is_empty() => self.empty_lines.push(newline),
            State::Indeterminate => {
                self.state = match to_operator_block_expression(items, expression_parser) {
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
            State::Argument => self
                .body_lines
                .push(Line { newline, expression: expression_parser.parse(&mut items) }),
            State::Operator if !self.body_lines.is_empty() => self
                .body_lines
                .push(Line { newline, expression: expression_parser.parse(&mut items) }),
            State::Operator if items.is_empty() => self.operator_lines.push(newline.into()),
            State::Operator => match to_operator_block_expression(items, expression_parser) {
                Ok(expression) => {
                    self.operator_lines.push(OperatorLine { newline, expression: Some(expression) })
                }
                Err(expression) => {
                    self.body_lines.push(Line { newline, expression: Some(expression) })
                }
            },
        }
    }

    fn build(&mut self) -> ApplicableBlock<'s> {
        match self.state {
            State::Operator => {
                let mut operator_lines =
                    Vec::with_capacity(self.empty_lines.len() + self.operator_lines.len());
                operator_lines.extend(self.empty_lines.drain(..).map(OperatorLine::from));
                operator_lines.append(&mut self.operator_lines);
                let excess = self.body_lines.split_off(0);
                ApplicableBlock::OperatorBlock { operator_lines, excess }
            }
            State::Argument | State::Indeterminate => {
                let mut body_lines =
                    Vec::with_capacity(self.empty_lines.len() + self.body_lines.len());
                body_lines.extend(self.empty_lines.drain(..).map(Line::from));
                body_lines.append(&mut self.body_lines);
                ApplicableBlock::ArgumentBlock { body_lines }
            }
        }
    }
}

/// Interpret the given expression as an `OperatorBlockExpression`, if it fits the correct pattern.
fn to_operator_block_expression<'s>(
    mut items: Vec<Item<'s>>,
    expression_parser: &mut ExpressionParser<'s>,
) -> Result<OperatorBlockExpression<'s>, Tree<'s>> {
    match &items[..] {
        [Item::Token(a), b, ..]
            if b.left_visible_offset().width_in_spaces != 0
                && a.operator_properties().is_some_and(|p| p.can_form_section()) =>
        {
            let expression = expression_parser.parse_offset(1, &mut items).unwrap();
            let operator = Ok(items
                .pop()
                .unwrap()
                .try_into_token()
                .unwrap()
                .with_variant(token::variant::Operator()));
            Ok(OperatorBlockExpression { operator, expression })
        }
        _ => Err(expression_parser.parse(&mut items).unwrap()),
    }
}
