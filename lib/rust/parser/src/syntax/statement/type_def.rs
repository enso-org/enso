use crate::prelude::*;

use crate::syntax::item;
use crate::syntax::maybe_with_error;
use crate::syntax::operator::Precedence;
use crate::syntax::statement::apply_excess_private_keywords;
use crate::syntax::statement::compound_lines;
use crate::syntax::statement::function_def::parse_constructor_definition;
use crate::syntax::statement::function_def::parse_type_args;
use crate::syntax::statement::parse_statement;
use crate::syntax::statement::scan_private_keywords;
use crate::syntax::statement::EvaluationContext;
use crate::syntax::statement::Line;
use crate::syntax::statement::StatementContext;
use crate::syntax::statement::StatementOrPrefix;
use crate::syntax::statement::StatementPrefix;
use crate::syntax::statement::VisibilityContext;
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
        let mut block = mem::take(lines).into_vec();
        items.pop();
        let lines = compound_lines(&mut block, |prefixes, mut line| {
            if let Some(Item::Token(token)) = line.items.first_mut() {
                if matches!(token.variant, token::Variant::Operator(_)) {
                    let opr_ident =
                        token::variant::Ident { is_operator_lexically: true, ..default() };
                    token.variant = token::Variant::Ident(opr_ident);
                }
            }
            parse_type_body_statement(prefixes, line, precedence, args_buffer)
        });
        block::compound_lines(lines).collect()
    } else {
        default()
    };

    let params = parse_type_args(items, start + 2, precedence, args_buffer);

    let name = items.pop().unwrap().into_token().unwrap().try_into().unwrap();

    let keyword = items.pop().unwrap().into_token().unwrap();
    let keyword = keyword.with_variant(token::variant::TypeKeyword());

    debug_assert_eq!(items.len(), start);

    Tree::type_def(keyword, name, params, body).into()
}

fn parse_type_body_statement<'s>(
    prefixes: &mut Vec<Line<'s, StatementPrefix<'s>>>,
    mut line: item::Line<'s>,
    precedence: &mut Precedence<'s>,
    args_buffer: &mut Vec<ArgumentDefinition<'s>>,
) -> Line<'s, StatementOrPrefix<'s>> {
    let private_keywords = scan_private_keywords(&line.items);
    match line.items.get(private_keywords) {
        Some(Item::Token(Token { variant: token::Variant::Ident(ident), .. }))
            if ident.is_type
                && !line
                    .items
                    .get(private_keywords + 1)
                    .is_some_and(|item| Spacing::of_item(item) == Spacing::Unspaced) =>
            parse_constructor_definition(
                prefixes,
                line,
                0,
                private_keywords,
                precedence,
                args_buffer,
            )
            .map_content(StatementOrPrefix::from),
        None => Line {
            newline: line.newline,
            content: apply_excess_private_keywords(
                None,
                line.items.drain(..),
                SyntaxError::TypeBodyUnexpectedPrivateUsage,
            )
            .map(StatementOrPrefix::from),
        },
        _ => parse_statement(prefixes, line, precedence, args_buffer, StatementContext {
            evaluation_context: EvaluationContext::Lazy,
            visibility_context: VisibilityContext::Public,
            tail_expression:    false,
        })
        .map_content(|statement_or_prefix| {
            statement_or_prefix.map_statement(|tree| {
                let error = match &tree.variant {
                    tree::Variant::Function(_)
                    | tree::Variant::ForeignFunction(_)
                    | tree::Variant::Assignment(_)
                    | tree::Variant::Documented(_)
                    | tree::Variant::Annotation(_)
                    | tree::Variant::AnnotatedBuiltin(_) => None,
                    tree::Variant::TypeSignatureDeclaration(_) => None,
                    tree::Variant::TypeDef(_) => None,
                    _ => Some(SyntaxError::UnexpectedExpressionInTypeBody),
                };
                maybe_with_error(tree, error)
            })
        }),
    }
}
