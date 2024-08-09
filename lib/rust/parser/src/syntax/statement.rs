//! Parses statements in module, body blocks, and type blocks.



mod function_def;
mod type_def;

use crate::empty_tree;
use crate::expression_to_pattern;
use crate::is_qualified_name;
use crate::prelude::*;
use crate::syntax::item;
use crate::syntax::maybe_with_error;
use crate::syntax::operator::Precedence;
use crate::syntax::statement::function_def::parse_function_decl;
use crate::syntax::statement::function_def::try_parse_foreign_function;
use crate::syntax::statement::type_def::try_parse_type_def;
use crate::syntax::token;
use crate::syntax::tree;
use crate::syntax::tree::block;
use crate::syntax::tree::ArgumentDefinition;
use crate::syntax::tree::SyntaxError;
use crate::syntax::treebuilding::Spacing;
use crate::syntax::Item;
use crate::syntax::Token;
use crate::syntax::Tree;

pub use function_def::parse_args;

/// Parses normal statements.
#[derive(Debug, Default)]
pub struct BodyBlockParser<'s> {
    statement_parser: StatementParser<'s>,
}

impl<'s> BodyBlockParser<'s> {
    /// Parse the statements in a block.
    pub fn parse_body_block(
        &mut self,
        lines: impl IntoIterator<Item = item::Line<'s>>,
        precedence: &mut Precedence<'s>,
    ) -> Tree<'s> {
        let lines = lines.into_iter().map(|item::Line { newline, mut items }| block::Line {
            newline,
            expression: self.statement_parser.parse_body_block_statement(&mut items, 0, precedence),
        });
        Tree::body_block(block::compound_lines(lines).collect())
    }

    /// Parse the declarations and statements at the top level of a module.
    pub fn parse_module(
        &mut self,
        lines: impl IntoIterator<Item = item::Line<'s>>,
        precedence: &mut Precedence<'s>,
    ) -> Tree<'s> {
        let lines = lines.into_iter().map(|item::Line { newline, mut items }| block::Line {
            newline,
            expression: self.statement_parser.parse_module_statement(&mut items, 0, precedence),
        });
        Tree::body_block(block::compound_lines(lines).collect())
    }
}

#[derive(Debug, Default)]
struct StatementParser<'s> {
    args_buffer: Vec<ArgumentDefinition<'s>>,
}

impl<'s> StatementParser<'s> {
    fn parse_body_block_statement(
        &mut self,
        items: &mut Vec<Item<'s>>,
        start: usize,
        precedence: &mut Precedence<'s>,
    ) -> Option<Tree<'s>> {
        let private_keywords = scan_private_keywords(&*items);
        let mut statement = parse_body_block_statement(
            items,
            start + private_keywords,
            precedence,
            &mut self.args_buffer,
        );
        for _ in 0..private_keywords {
            let Item::Token(keyword) = items.pop().unwrap() else { unreachable!() };
            let token::Variant::Private(variant) = keyword.variant else { unreachable!() };
            let keyword = keyword.with_variant(variant);
            let error = match statement.as_ref().map(|tree| &tree.variant) {
                Some(tree::Variant::Invalid(_) | tree::Variant::Function(_)) => None,
                _ => SyntaxError::StmtUnexpectedPrivateUsage.into(),
            };
            let private_stmt = Tree::private(keyword, statement.take());
            statement = maybe_with_error(private_stmt, error).into();
        }
        statement
    }

    fn parse_module_statement(
        &mut self,
        items: &mut Vec<Item<'s>>,
        start: usize,
        precedence: &mut Precedence<'s>,
    ) -> Option<Tree<'s>> {
        let private_keywords = scan_private_keywords(&*items);
        let mut statement = parse_body_block_statement(
            items,
            start + private_keywords,
            precedence,
            &mut self.args_buffer,
        );
        for _ in 0..private_keywords {
            let Item::Token(keyword) = items.pop().unwrap() else { unreachable!() };
            let token::Variant::Private(variant) = keyword.variant else { unreachable!() };
            let keyword = keyword.with_variant(variant);
            let error = match statement.as_ref().map(|tree| &tree.variant) {
                Some(tree::Variant::Invalid(_) | tree::Variant::Function(_)) | None => None,
                _ => SyntaxError::StmtUnexpectedPrivateUsage.into(),
            };
            let private_stmt = Tree::private(keyword, statement.take());
            statement = maybe_with_error(private_stmt, error).into();
        }
        statement
    }
}

fn scan_private_keywords<'s>(items: impl IntoIterator<Item = impl AsRef<Item<'s>>>) -> usize {
    items
        .into_iter()
        .take_while(|item| {
            matches!(item.as_ref(), Item::Token(Token { variant: token::Variant::Private(_), .. }))
        })
        .count()
}

fn parse_body_block_statement<'s>(
    items: &mut Vec<Item<'s>>,
    start: usize,
    precedence: &mut Precedence<'s>,
    args_buffer: &mut Vec<ArgumentDefinition<'s>>,
) -> Option<Tree<'s>> {
    use token::Variant;
    if let Some(type_def) = try_parse_type_def(items, start, precedence, args_buffer) {
        return Some(type_def);
    }
    let top_level_operator = match find_top_level_operator(&items[start..]) {
        Ok(top_level_operator) => top_level_operator.map(|(i, t)| (i + start, t)),
        Err(e) =>
            return precedence
                .resolve_non_section_offset(start, items)
                .unwrap()
                .with_error(e)
                .into(),
    };
    let statement = match top_level_operator {
        Some((i, Token { variant: Variant::AssignmentOperator(_), .. })) =>
            parse_assignment_like_statement(items, start, i, precedence, args_buffer).into(),
        Some((i, Token { variant: Variant::TypeAnnotationOperator(_), .. })) => {
            let type_ = precedence.resolve_non_section_offset(i + 1, items);
            let Some(Item::Token(operator)) = items.pop() else { unreachable!() };
            let Variant::TypeAnnotationOperator(variant) = operator.variant else { unreachable!() };
            let operator = operator.with_variant(variant);
            let lhs = precedence.resolve_non_section_offset(start, items);
            let type_ = type_.unwrap_or_else(|| {
                empty_tree(operator.code.position_after()).with_error(SyntaxError::ExpectedType)
            });
            if lhs.as_ref().is_some_and(is_qualified_name) {
                Tree::type_signature(lhs.unwrap(), operator, type_).into()
            } else {
                let lhs = lhs.unwrap_or_else(|| {
                    empty_tree(operator.left_offset.code.position_before())
                        .with_error(SyntaxError::ExpectedExpression)
                });
                Tree::type_annotated(lhs, operator, type_).into()
            }
        }
        Some(_) => unreachable!(),
        None => precedence.resolve_offset(start, items),
    };
    debug_assert_eq!(items.len(), start);
    statement
}

fn parse_assignment_like_statement<'s>(
    items: &mut Vec<Item<'s>>,
    start: usize,
    operator: usize,
    precedence: &mut Precedence<'s>,
    args_buffer: &mut Vec<ArgumentDefinition<'s>>,
) -> Tree<'s> {
    if operator == start {
        return precedence
            .resolve_non_section_offset(start, items)
            .unwrap()
            .with_error(SyntaxError::StmtInvalidAssignmentOrMethod);
    }

    let mut expression = precedence.resolve_offset(operator + 1, items);

    let Some(Item::Token(operator)) = items.pop() else { unreachable!() };
    let token::Variant::AssignmentOperator(variant) = operator.variant else { unreachable!() };
    let operator = operator.with_variant(variant);

    let qn_len = scan_qn(&items[start..]);

    let mut operator = Some(operator);
    if let Some(function) = try_parse_foreign_function(
        items,
        start,
        &mut operator,
        &mut expression,
        precedence,
        args_buffer,
    ) {
        return function;
    }
    let operator = operator.unwrap();

    match (expression, qn_len) {
        (Some(e), Some(qn_len)) if matches!(e.variant, tree::Variant::BodyBlock(_)) => {
            let (qn, args, return_) =
                parse_function_decl(items, start, qn_len, precedence, args_buffer);
            Tree::function(qn, args, return_, operator, Some(e))
        }
        (Some(expression), None) =>
            parse_assignment(start, items, operator, expression, precedence),
        (Some(expression), Some(1)) if items.len() == start + 1 =>
            parse_assignment(start, items, operator, expression, precedence),
        (e, Some(qn_len)) => {
            let (qn, args, return_) =
                parse_function_decl(items, start, qn_len, precedence, args_buffer);
            Tree::function(qn, args, return_, operator, e)
        }
        (None, None) => Tree::opr_app(
            precedence.resolve_non_section_offset(start, items),
            Ok(operator.with_variant(token::variant::Operator())),
            None,
        )
        .with_error(SyntaxError::StmtInvalidAssignmentOrMethod),
    }
}

fn parse_assignment<'s>(
    start: usize,
    items: &mut Vec<Item<'s>>,
    operator: token::AssignmentOperator<'s>,
    expression: Tree<'s>,
    precedence: &mut Precedence<'s>,
) -> Tree<'s> {
    let pattern =
        expression_to_pattern(precedence.resolve_non_section_offset(start, items).unwrap());
    Tree::assignment(pattern, operator, expression)
}

fn parse_pattern<'s>(
    items: &mut Vec<Item<'s>>,
    arg_start: usize,
    precedence: &mut Precedence<'s>,
) -> (Option<token::SuspensionOperator<'s>>, Option<Tree<'s>>) {
    let have_suspension = matches!(
        items.get(arg_start),
        Some(Item::Token(Token { variant: token::Variant::SuspensionOperator(_), .. }))
    );
    let pattern_start = arg_start + have_suspension as usize;
    let pattern = if items.len() - pattern_start == 1 {
        Some(match items.last().unwrap() {
            Item::Token(_) => {
                let Some(Item::Token(token)) = items.pop() else { unreachable!() };
                match token.variant {
                    token::Variant::Ident(variant) => Tree::ident(token.with_variant(variant)),
                    token::Variant::Wildcard(variant) =>
                        Tree::wildcard(token.with_variant(variant), None),
                    _ => tree::to_ast(token).with_error(SyntaxError::ArgDefExpectedPattern),
                }
            }
            _ => precedence
                .resolve_non_section_offset(items.len() - 1, items)
                .map(|tree| tree.with_error(SyntaxError::ArgDefExpectedPattern))
                .unwrap(),
        })
    } else {
        precedence
            .resolve_non_section_offset(pattern_start, items)
            .map(|tree| tree.with_error(SyntaxError::ArgDefExpectedPattern))
    };
    let suspension = have_suspension.then(|| {
        let Item::Token(token) = items.pop().unwrap() else { unreachable!() };
        let token::Variant::SuspensionOperator(variant) = token.variant else { unreachable!() };
        token.with_variant(variant)
    });
    (suspension, pattern)
}

fn find_top_level_operator<'a, 's>(
    items: &'a [Item<'s>],
) -> Result<Option<(usize, &'a Token<'s>)>, SyntaxError> {
    use token::Variant;
    let mut candidate: Option<(usize, &'a Token<'s>, bool)> = None;
    let mut after_first_space = false;
    for (i, item) in items.iter().enumerate() {
        let next_is_after_space =
            i != 0 && (after_first_space || matches!(Spacing::of_item(item), Spacing::Spaced));
        if let Item::Token(token) = item {
            let is_spaced = token.is_spaced();
            if !after_first_space || is_spaced {
                match &token.variant {
                    Variant::AssignmentOperator(_) => {
                        if is_spaced
                            && items
                                .get(i + 1)
                                .is_some_and(|item| Spacing::of_item(item) == Spacing::Unspaced)
                        {
                            return Err(SyntaxError::StmtLhsInvalidOperatorSpacing);
                        }
                        if is_spaced {
                            return Ok(Some((i, token)));
                        }
                        if candidate.is_none()
                            || (is_spaced && !candidate.unwrap().2)
                            || !matches!(
                                candidate.unwrap().1.variant,
                                Variant::AssignmentOperator(_)
                            )
                        {
                            candidate = Some((i, token, is_spaced));
                        }
                    }
                    Variant::TypeAnnotationOperator(_) => {
                        if is_spaced
                            && items
                                .get(i + 1)
                                .is_some_and(|item| Spacing::of_item(item) == Spacing::Unspaced)
                        {
                            return Err(SyntaxError::StmtLhsInvalidOperatorSpacing);
                        }
                        if candidate.is_none() || (is_spaced && !candidate.unwrap().2) {
                            candidate = Some((i, token, is_spaced));
                        }
                    }
                    Variant::Operator(_)
                    | Variant::DotOperator(_)
                    | Variant::ArrowOperator(_)
                    | Variant::CommaOperator(_) =>
                        if is_spaced && candidate.is_some_and(|(_, _, is_spaced)| !is_spaced) {
                            candidate = None;
                        },
                    _ => {}
                }
            }
        }
        after_first_space = next_is_after_space;
    }
    Ok(candidate.map(|(i, t, _)| (i, t)))
}

fn next_spaced(items: &[Item]) -> Option<usize> {
    for (i, item) in items.iter().enumerate().skip(1) {
        if matches!(Spacing::of_item(item), Spacing::Spaced) {
            return Some(i);
        }
    }
    None
}

/// Returns length of the QN.
fn scan_qn<'s>(items: impl IntoIterator<Item = impl AsRef<Item<'s>>>) -> Option<usize> {
    enum State {
        ExpectingDot,
        ExpectingIdent,
    }
    use token::Variant::*;
    use Item::*;
    use State::*;
    let mut state = ExpectingIdent;
    for (i, item) in items.into_iter().enumerate() {
        match item.as_ref() {
            Token(token) if i != 0 && token.is_spaced() => break,
            Token(token) => match (state, &token.variant) {
                (ExpectingDot, DotOperator(_)) => state = ExpectingIdent,
                (ExpectingIdent, Ident(ident)) if ident.is_type => state = ExpectingDot,
                (
                    ExpectingIdent,
                    Ident(_) | Operator(_) | NegationOperator(_) | UnaryOperator(_),
                ) => return Some(i + 1),
                _ => break,
            },
            Group(_) | Tree(_) => break,
            Block(_) => unreachable!(),
        }
    }
    None
}
