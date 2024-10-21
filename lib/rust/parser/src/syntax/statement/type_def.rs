use crate::prelude::*;

use crate::syntax::item;
use crate::syntax::maybe_with_error;
use crate::syntax::operator::Precedence;
use crate::syntax::statement::function_def::parse_constructor_definition;
use crate::syntax::statement::function_def::parse_type_args;
use crate::syntax::statement::parse_statement;
use crate::syntax::statement::scan_private_keywords;
use crate::syntax::statement::EvaluationContext;
use crate::syntax::token;
use crate::syntax::tree;
use crate::syntax::tree::block;
use crate::syntax::tree::ArgumentDefinition;
use crate::syntax::tree::SyntaxError;
use crate::syntax::treebuilding::Spacing;
use crate::syntax::Item;
use crate::syntax::Token;
use crate::syntax::Tree;



pub fn try_parse_type_def<'s>(
    items: &mut Vec<Item<'s>>,
    start: usize,
    precedence: &mut Precedence<'s>,
    args_buffer: &mut Vec<ArgumentDefinition<'s>>,
) -> Option<Tree<'s>> {
    match items.get(start) {
        Some(Item::Token(token)) if token.code == "type" => {}
        _ => return None,
    }
    match items.get(start + 1) {
        Some(Item::Token(Token { variant: token::Variant::Ident(ident), .. })) if ident.is_type => {
        }
        _ =>
            return precedence
                .resolve_non_section_offset(start, items)
                .unwrap()
                .with_error(SyntaxError::TypeDefExpectedTypeName)
                .into(),
    }

    let body = if let Some(Item::Block(lines)) = items.last_mut() {
        let block = mem::take(lines).into_vec();
        items.pop();
        let lines = block.into_iter().map(|item::Line { newline, mut items }| block::Line {
            newline,
            expression: {
                if let Some(Item::Token(token)) = items.first_mut() {
                    if matches!(token.variant, token::Variant::Operator(_)) {
                        let opr_ident =
                            token::variant::Ident { is_operator_lexically: true, ..default() };
                        token.variant = token::Variant::Ident(opr_ident);
                    }
                }
                parse_type_body_statement(items, precedence, args_buffer)
            },
        });
        block::compound_lines(lines).collect()
    } else {
        default()
    };

    let params = parse_type_args(items, start + 2, precedence, args_buffer);

    let name = {
        let Item::Token(name) = items.pop().unwrap() else { unreachable!() };
        let token::Variant::Ident(variant) = name.variant else { unreachable!() };
        name.with_variant(variant)
    };

    let Item::Token(keyword) = items.pop().unwrap() else { unreachable!() };
    let keyword = keyword.with_variant(token::variant::TypeKeyword());

    debug_assert_eq!(items.len(), start);

    Tree::type_def(keyword, name, params, body).into()
}

fn parse_type_body_statement<'s>(
    mut items: Vec<Item<'s>>,
    precedence: &mut Precedence<'s>,
    args_buffer: &mut Vec<ArgumentDefinition<'s>>,
) -> Option<Tree<'s>> {
    let private_keywords = scan_private_keywords(&items);
    let mut statement = match items.get(private_keywords) {
        Some(Item::Token(Token { variant: token::Variant::Ident(ident), .. }))
            if ident.is_type
                && !items
                    .get(private_keywords + 1)
                    .is_some_and(|item| Spacing::of_item(item) == Spacing::Unspaced) =>
            Some(parse_constructor_definition(
                &mut items,
                private_keywords,
                precedence,
                args_buffer,
            )),
        None => None,
        _ => {
            let tree = parse_statement(
                &mut items,
                private_keywords,
                precedence,
                args_buffer,
                EvaluationContext::Lazy,
            )
            .unwrap();
            let error = match &tree.variant {
                tree::Variant::Function(_)
                | tree::Variant::ForeignFunction(_)
                | tree::Variant::Assignment(_)
                | tree::Variant::Documented(_)
                | tree::Variant::Annotated(_)
                | tree::Variant::AnnotatedBuiltin(_) => None,
                tree::Variant::TypeSignature(_) => None,
                tree::Variant::TypeDef(_) => None,
                _ => Some(SyntaxError::UnexpectedExpressionInTypeBody),
            };
            maybe_with_error(tree, error).into()
        }
    };
    for _ in 0..private_keywords {
        let Item::Token(keyword) = items.pop().unwrap() else { unreachable!() };
        let token::Variant::Private(variant) = keyword.variant else { unreachable!() };
        let keyword = keyword.with_variant(variant);
        let error = match statement.as_ref().map(|tree| &tree.variant) {
            Some(
                tree::Variant::Invalid(_)
                | tree::Variant::ConstructorDefinition(_)
                | tree::Variant::Function(_),
            ) => None,
            _ => SyntaxError::TypeBodyUnexpectedPrivateUsage.into(),
        };
        let private_stmt = Tree::private(keyword, statement.take());
        statement = maybe_with_error(private_stmt, error).into();
    }
    statement
}
