use crate::prelude::*;

use crate::syntax::Item;
use crate::syntax::Token;
use crate::syntax::Tree;
use crate::syntax::expression::ExpressionParser;
use crate::syntax::expression::Spacing;
use crate::syntax::item;
use crate::syntax::maybe_with_error;
use crate::syntax::statement::Line;
use crate::syntax::statement::StatementPrefix;
use crate::syntax::statement::StatementPrefixLine;
use crate::syntax::statement::StatementPrefixes;
use crate::syntax::statement::TopLevelOperator;
use crate::syntax::statement::VisibilityContext;
use crate::syntax::statement::apply_excess_private_keywords;
use crate::syntax::statement::apply_private_keywords;
use crate::syntax::statement::find_top_level_operator;
use crate::syntax::statement::parse_pattern;
use crate::syntax::token;
use crate::syntax::tree;
use crate::syntax::tree::AnnotationLine;
use crate::syntax::tree::ArgumentDefault;
use crate::syntax::tree::ArgumentDefinition;
use crate::syntax::tree::ArgumentDefinitionLine;
use crate::syntax::tree::ArgumentType;
use crate::syntax::tree::DocLine;
use crate::syntax::tree::ReturnSpecification;
use crate::syntax::tree::SyntaxError;
use crate::syntax::tree::TypeSignatureLine;

use crate::{empty_tree, expression_to_type, qn_deep_unwrap_calls};

pub struct FunctionBuilder<'s> {
    name: Tree<'s>,
    return_: Option<ReturnSpecification<'s>>,
    args: Vec<ArgumentDefinition<'s>>,
    line: item::Line<'s>,
    start: usize,
}

impl<'s> FunctionBuilder<'s> {
    pub fn new(
        mut line: item::Line<'s>,
        start: usize,
        qn_len: usize,
        expression_parser: &mut ExpressionParser<'s>,
        args_buffer: &mut Vec<ArgumentDefinition<'s>>,
    ) -> Self {
        let mut arg_starts = vec![];
        let mut arrow = None;
        let items = &mut line.items;
        for (i, item) in items.iter().enumerate().skip(start + qn_len) {
            if let Item::Token(Token { variant: token::Variant::ArrowOperator(_), .. }) = item {
                arrow = Some(i);
                break;
            }
            if i == start + qn_len || Spacing::of_item(item) == Spacing::Spaced {
                arg_starts.push(i);
            }
        }
        let return_ = arrow.map(|arrow| parse_return_spec(items, arrow, expression_parser));

        args_buffer.extend(
            arg_starts
                .drain(..)
                .rev()
                .map(|arg_start| parse_arg_def(items, arg_start, expression_parser)),
        );
        let args = args_buffer.drain(..).rev().collect();

        let mut name = expression_parser.parse_non_section_offset(start, items).unwrap();
        qn_deep_unwrap_calls(&mut name);

        Self { name, return_, args, line, start }
    }

    pub fn build(
        mut self,
        prefixes: &mut StatementPrefixes<'s>,
        operator: token::AssignmentOperator<'s>,
        expression: Option<Tree<'s>>,
        visibility_context: VisibilityContext,
    ) -> Line<'s, Tree<'s>> {
        let items = &mut self.line.items;
        let private_keywords_start = 0;

        let private = (visibility_context != VisibilityContext::Private
            && self.start > private_keywords_start)
            .then(|| items.pop().unwrap().try_into_token().unwrap().try_into().unwrap());

        let mut first_newline = self.line.newline;

        #[derive(Default)]
        struct PrefixesAccumulator<'s> {
            docs: Option<DocLine<'s>>,
            annotations: Option<Vec<AnnotationLine<'s>>>,
            signature: Option<TypeSignatureLine<'s>>,
        }

        let mut acc = PrefixesAccumulator::default();

        while let Some(prefix) = prefixes.last() {
            match (&acc, &prefix) {
                (
                    PrefixesAccumulator { docs: None, annotations: None, signature: None },
                    StatementPrefix::TypeSignature(signature),
                ) if qn_equivalent(&self.name, &signature.name) => {
                    let StatementPrefixLine::TypeSignature(signature_line) =
                        prefixes.pop(&mut first_newline)
                    else {
                        unreachable!()
                    };
                    acc.signature = Some(signature_line);
                }
                (PrefixesAccumulator { docs: None, .. }, StatementPrefix::Annotation(_)) => {
                    let StatementPrefixLine::Annotation(annotation_line) =
                        prefixes.pop(&mut first_newline)
                    else {
                        unreachable!()
                    };
                    let mut annotations = acc.annotations.take().unwrap_or_default();
                    annotations.push(annotation_line);
                    acc.annotations = Some(annotations);
                }
                (PrefixesAccumulator { docs: None, .. }, StatementPrefix::Documentation(_)) => {
                    let StatementPrefixLine::Documentation(doc_line) =
                        prefixes.pop(&mut first_newline)
                    else {
                        unreachable!()
                    };
                    acc.docs = Some(doc_line);
                }
                _ => break,
            }
        }
        let signature = acc.signature;
        let annotations = {
            let mut annotations = acc.annotations.take().unwrap_or_default();
            annotations.reverse();
            annotations
        };
        let docs = acc.docs;

        Line {
            newline: first_newline,
            content: apply_private_keywords(
                Some(Tree::function(
                    docs,
                    annotations,
                    signature,
                    private,
                    self.name,
                    self.args,
                    self.return_,
                    operator,
                    expression,
                )),
                items.drain(..),
                visibility_context,
            ),
        }
    }
}

fn qn_equivalent(a: &Tree, b: &Tree) -> bool {
    use tree::Variant::*;
    match (&a.variant, &b.variant) {
        (Call(a), Call(b)) => qn_equivalent(&a.value, &b.value),
        (Ident(a), Ident(b)) => a.token.code.repr == b.token.code.repr,
        (PropertyAccess(a), PropertyAccess(b)) => {
            a.rhs.code.repr == b.rhs.code.repr && opt_qn_equivalent(&a.lhs, &b.lhs)
        }
        _ => false,
    }
}

fn opt_qn_equivalent(a: &Option<Tree>, b: &Option<Tree>) -> bool {
    match (a, b) {
        (Some(a), Some(b)) => qn_equivalent(a, b),
        (None, None) => true,
        _ => false,
    }
}

/// Parse a sequence of argument definitions.
pub fn parse_args<'s>(
    items: &mut Vec<Item<'s>>,
    start: usize,
    expression_parser: &mut ExpressionParser<'s>,
    args_buffer: &mut Vec<ArgumentDefinition<'s>>,
) -> Vec<ArgumentDefinition<'s>> {
    let mut arg_starts = vec![];
    for (i, item) in items.iter().enumerate().skip(start) {
        if i == start || Spacing::of_item(item) == Spacing::Spaced {
            arg_starts.push(i);
        }
    }
    args_buffer.extend(
        arg_starts
            .drain(..)
            .rev()
            .map(|arg_start| parse_arg_def(items, arg_start, expression_parser)),
    );
    debug_assert_eq!(items.len(), start);
    args_buffer.drain(..).rev().collect()
}

pub fn parse_constructor_definition<'s>(
    prefixes: &mut StatementPrefixes<'s>,
    mut line: item::Line<'s>,
    private_keywords_start: usize,
    start: usize,
    expression_parser: &mut ExpressionParser<'s>,
    args_buffer: &mut Vec<ArgumentDefinition<'s>>,
) -> Line<'s, Tree<'s>> {
    let newline = line.newline;
    let items = &mut line.items;
    let mut block_args = vec![];
    if matches!(items.last().unwrap(), Item::Block(_)) {
        let Item::Block(block) = items.pop().unwrap() else { unreachable!() };
        block_args.extend(block.into_vec().into_iter().map(|item::Line { newline, mut items }| {
            let argument =
                (!items.is_empty()).then(|| parse_arg_def(&mut items, 0, expression_parser));
            ArgumentDefinitionLine { newline, argument }
        }))
    }
    let (name, inline_args) = parse_constructor_decl(items, start, expression_parser, args_buffer);
    let private = (private_keywords_start < start)
        .then(|| items.pop().unwrap().try_into_token().unwrap().try_into().unwrap());

    let mut first_newline = newline;
    let mut annotations_reversed = vec![];
    let mut doc_line = None;
    while let Some(prefix) = prefixes.last() {
        match &prefix {
            StatementPrefix::Annotation(_) => {
                let StatementPrefixLine::Annotation(annotation_line) =
                    prefixes.pop(&mut first_newline)
                else {
                    unreachable!()
                };
                annotations_reversed.push(annotation_line);
            }
            StatementPrefix::Documentation(_) => {
                let StatementPrefixLine::Documentation(line) = prefixes.pop(&mut first_newline)
                else {
                    unreachable!()
                };
                doc_line = Some(line);
                break;
            }
            _ => {
                break;
            }
        }
    }
    let annotations = {
        annotations_reversed.reverse();
        annotations_reversed
    };

    let def =
        Tree::constructor_definition(doc_line, annotations, private, name, inline_args, block_args);

    Line {
        newline: first_newline,
        content: apply_excess_private_keywords(
            Some(def),
            line.items.drain(..),
            SyntaxError::TypeBodyUnexpectedPrivateUsage,
        ),
    }
}

fn parse_constructor_decl<'s>(
    items: &mut Vec<Item<'s>>,
    start: usize,
    expression_parser: &mut ExpressionParser<'s>,
    args_buffer: &mut Vec<ArgumentDefinition<'s>>,
) -> (token::Ident<'s>, Vec<ArgumentDefinition<'s>>) {
    let args = parse_args(items, start + 1, expression_parser, args_buffer);
    let name = items.pop().unwrap().try_into_token().unwrap().try_into().unwrap();
    debug_assert_eq!(items.len(), start);
    (name, args)
}

fn item_is_non_type_ident(item: &Item) -> bool {
    matches!(
        item,
        Item::Token(Token {
            variant: token::Variant::Ident(token::variant::Ident { is_type: false, .. }),
            ..
        })
    )
}

pub fn try_parse_foreign_function<'s>(
    items: &mut Vec<Item<'s>>,
    start: usize,
    operator: &mut Option<token::AssignmentOperator<'s>>,
    expression: &mut Option<Tree<'s>>,
    expression_parser: &mut ExpressionParser<'s>,
    args_buffer: &mut Vec<ArgumentDefinition<'s>>,
) -> Option<Tree<'s>> {
    match items.get(start) {
        Some(Item::Token(token)) if token.code == "foreign" => {}
        _ => return None,
    }
    let operator = operator.take().unwrap();

    let error_missing_language = (!items.get(start + 1).is_some_and(item_is_non_type_ident))
        .then_some(SyntaxError::ForeignFnExpectedLanguage);
    let error_missing_name = (!items.get(start + 2).is_some_and(item_is_non_type_ident))
        .then_some(SyntaxError::ForeignFnExpectedName);
    if let Some(error) = error_missing_language.or(error_missing_name) {
        items.push(Item::from(Token::from(operator)));
        items.extend(expression.take().map(Item::from));
        return expression_parser
            .parse_non_section_offset(start, items)
            .unwrap()
            .with_error(error)
            .into();
    }

    let body = expression
        .take()
        .map(|body| {
            let error = match &body.variant {
                tree::Variant::TextLiteral(_) => None,
                _ => Some(SyntaxError::ForeignFnExpectedStringBody),
            };
            maybe_with_error(body, error)
        })
        .unwrap_or_else(|| {
            empty_tree(operator.code.position_after())
                .with_error(SyntaxError::ForeignFnExpectedStringBody)
        });

    let mut arg_starts = vec![];
    for (i, item) in items.iter().enumerate().skip(start + 3) {
        if i == start + 3 || Spacing::of_item(item) == Spacing::Spaced {
            arg_starts.push(i);
        }
    }
    args_buffer.extend(
        arg_starts
            .drain(..)
            .rev()
            .map(|arg_start| parse_arg_def(items, arg_start, expression_parser)),
    );
    let args = args_buffer.drain(..).rev().collect();

    let name = items.pop().unwrap().try_into_token().unwrap().try_into().unwrap();
    let language = items.pop().unwrap().try_into_token().unwrap().try_into().unwrap();
    let keyword = items
        .pop()
        .unwrap()
        .try_into_token()
        .unwrap()
        .with_variant(token::variant::ForeignKeyword());
    Tree::foreign_function(keyword, language, name, args, operator, body).into()
}

#[derive(Debug, PartialEq, Eq)]
enum IsParenthesized {
    Parenthesized,
    Unparenthesized,
}
use IsParenthesized::*;

struct ArgDefInfo {
    type_: Option<(IsParenthesized, usize)>,
    default: Option<usize>,
}

fn parse_return_spec<'s>(
    items: &mut Vec<Item<'s>>,
    arrow: usize,
    expression_parser: &mut ExpressionParser<'s>,
) -> ReturnSpecification<'s> {
    let r#type = expression_parser.parse_non_section_offset(arrow + 1, items);
    let arrow: token::ArrowOperator =
        items.pop().unwrap().try_into_token().unwrap().try_into().unwrap();
    let r#type = r#type.unwrap_or_else(|| {
        empty_tree(arrow.code.position_after()).with_error(SyntaxError::ExpectedExpression)
    });
    ReturnSpecification { arrow, r#type }
}

fn parse_arg_def<'s>(
    items: &mut Vec<Item<'s>>,
    mut start: usize,
    expression_parser: &mut ExpressionParser<'s>,
) -> ArgumentDefinition<'s> {
    let mut items = items;
    let mut parenthesized_body: Option<Vec<Item>>;
    let mut inner_parenthesized_body: Option<Vec<Item>>;

    // If the entire definition is parenthesized, enter it.
    let mut open1 = None;
    let mut close1 = None;
    if matches!(items[start..], [Item::Group(_)]) {
        let Some(Item::Group(item::Group { open, body, close })) = items.pop() else {
            unreachable!()
        };
        open1 = open.into();
        close1 = close;
        parenthesized_body = body.into_vec().into();
        debug_assert_eq!(items.len(), start);
        items = parenthesized_body.as_mut().unwrap();
        start = 0;
    }

    let ArgDefInfo { type_: type_op, default } = match analyze_arg_def(&items[start..]) {
        Err(e) => {
            let pattern =
                expression_parser.parse_non_section_offset(start, items).unwrap().with_error(e);
            return ArgumentDefinition {
                open: open1,
                open2: None,
                suspension: None,
                pattern,
                type_: None,
                close2: None,
                default: None,
                close: close1,
            };
        }
        Ok(arg_def) => arg_def,
    };
    let default = default.map(|default| {
        let tree = expression_parser.parse_offset(start + default + 1, items);
        let equals = items.pop().unwrap().try_into_token().unwrap();
        let expression = tree.unwrap_or_else(|| {
            empty_tree(equals.code.position_after()).with_error(SyntaxError::ExpectedExpression)
        });
        let Token { variant: token::Variant::AssignmentOperator(variant), .. } = equals else {
            unreachable!()
        };
        let equals = equals.with_variant(variant);
        ArgumentDefault { equals, expression }
    });
    let mut open2 = None;
    let mut close2 = None;
    let mut type_ = None;
    if let Some((parenthesized, type_op)) = type_op {
        if parenthesized == Parenthesized {
            debug_assert_eq!(items.len(), start + 1);
            let Some(Item::Group(item::Group { open, body, close })) = items.pop() else {
                unreachable!()
            };
            open2 = open.into();
            close2 = close;
            inner_parenthesized_body = body.into_vec().into();
            items = inner_parenthesized_body.as_mut().unwrap();
            start = 0;
        }
        let tree = expression_parser.parse_non_section_offset(start + type_op + 1, items);
        let operator = items.pop().unwrap().try_into_token().unwrap();
        let tree = tree.map(expression_to_type).unwrap_or_else(|| {
            empty_tree(operator.code.position_after()).with_error(SyntaxError::ExpectedType)
        });
        let token::Variant::TypeAnnotationOperator(variant) = operator.variant else {
            unreachable!()
        };
        let operator = operator.with_variant(variant);
        type_ = ArgumentType { operator, type_: tree }.into();
    }
    let (suspension, pattern) = parse_pattern(items, start, expression_parser);
    let pattern = pattern.unwrap_or_else(|| {
        empty_tree(
            suspension
                .as_ref()
                .map(|t| t.code.position_after())
                .or_else(|| open2.as_ref().map(|t| t.code.position_after()))
                .or_else(|| open1.as_ref().map(|t| t.code.position_after()))
                .or_else(|| type_.as_ref().map(|t| t.operator.left_offset.code.position_before()))
                .or_else(|| close2.as_ref().map(|t| t.left_offset.code.position_before()))
                .or_else(|| default.as_ref().map(|t| t.equals.left_offset.code.position_before()))
                .or_else(|| close1.as_ref().map(|t| t.left_offset.code.position_before()))
                .unwrap(),
        )
        .with_error(SyntaxError::ArgDefExpectedPattern)
    });
    ArgumentDefinition {
        open: open1,
        open2,
        suspension,
        pattern,
        type_,
        close2,
        default,
        close: close1,
    }
}

fn analyze_arg_def(outer: &[Item]) -> Result<ArgDefInfo, SyntaxError> {
    Ok(match find_top_level_operator(outer)? {
        None => ArgDefInfo { type_: None, default: None },
        Some(TopLevelOperator::TypeAnnotationOperator(annotation_op_pos)) => {
            ArgDefInfo { type_: (Unparenthesized, annotation_op_pos).into(), default: None }
        }
        Some(TopLevelOperator::AssignmentOperator(assignment_op_pos)) => ArgDefInfo {
            type_: match &outer[..assignment_op_pos] {
                [Item::Group(item::Group { body: inner, .. })] => {
                    let inner_type = match find_top_level_operator(inner)? {
                        None => return Err(SyntaxError::ArgDefSpuriousParens),
                        Some(TopLevelOperator::TypeAnnotationOperator(inner_op_pos)) => {
                            inner_op_pos
                        }
                        Some(_) => return Err(SyntaxError::ArgDefUnexpectedOpInParenClause),
                    };
                    (Parenthesized, inner_type).into()
                }
                items => match find_top_level_operator(items)? {
                    None => None,
                    Some(TopLevelOperator::TypeAnnotationOperator(annotation_op_pos)) => {
                        (Unparenthesized, annotation_op_pos).into()
                    }
                    Some(_) => return Err(SyntaxError::ArgDefUnexpectedOp),
                },
            },
            default: assignment_op_pos.into(),
        },
    })
}
