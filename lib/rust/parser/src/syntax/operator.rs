//! Operator related functionalities.

use crate::prelude::*;

use crate::syntax;
use crate::syntax::token;
use crate::syntax::token::Token;



// ==================
// === Precedence ===
// ==================

/// Annotate expressions that should use spacing, because otherwise they are misleading. For
/// example, `if cond then.x else.y` is parsed as `if cond then .x else .y`, which after expansion
/// translates to `if cond then (\t -> t.x) else (\t -> t.y)`. However, for some macros spacing is
/// not needed. For example, `(.x)` is parsed as `(\t -> t.x)`, which is understandable.
fn annotate_tokens_that_need_spacing(item: syntax::Item) -> syntax::Item {
    use syntax::tree::Variant::*;
    item.map_tree(|ast| match &*ast.variant {
        MultiSegmentApp(data) if !data.segments.first().header.is_symbol() =>
            ast.with_error("This expression cannot be used in a non-spaced equation."),
        _ => ast,
    })
}

/// Take [`Item`] stream, resolve operator precedence and return the final AST.
///
/// The precedence resolution algorithm is based on the Shunting yard algorithm[1], extended to
/// handle operator sections.
/// [1]: https://en.wikipedia.org/wiki/Shunting_yard_algorithm
pub fn resolve_operator_precedence(items: NonEmptyVec<syntax::Item<'_>>) -> syntax::Tree<'_> {
    resolve_operator_precedence_if_non_empty(items).unwrap()
}

/// If the input sequence is non-empty, return the result of applying
/// [`resolve_operator_precedence`] to it.
pub fn resolve_operator_precedence_if_non_empty<'s>(
    items: impl IntoIterator<Item = syntax::Item<'s>>,
) -> Option<syntax::Tree<'s>> {
    type Tokens<'s> = Vec<syntax::Item<'s>>;
    let mut flattened: Tokens<'s> = default();
    let mut no_space_group: Tokens<'s> = default();
    let process_no_space_group = |flattened: &mut Tokens<'s>, no_space_group: &mut Tokens<'s>| {
        let tokens = no_space_group.drain(..);
        if tokens.len() < 2 {
            flattened.extend(tokens);
        } else {
            let tokens = tokens.map(annotate_tokens_that_need_spacing);
            let ast = resolve_operator_precedence_internal(tokens).unwrap();
            flattened.push(ast.into());
        }
    };
    // Returns `true` for an item if that item should not follow any other item in a no-space group
    // (i.e. the item has "space" before it).
    let starts_new_no_space_group = |item: &syntax::item::Item| {
        if item.left_visible_offset().width_in_spaces != 0 {
            return true;
        }
        if let syntax::item::Item::Block(_) = item {
            return true;
        }
        false
    };
    for item in items {
        if starts_new_no_space_group(&item) {
            process_no_space_group(&mut flattened, &mut no_space_group);
        }
        no_space_group.push(item);
    }
    process_no_space_group(&mut flattened, &mut no_space_group);
    resolve_operator_precedence_internal(flattened)
}

fn resolve_operator_precedence_internal<'s>(
    items: impl IntoIterator<Item = syntax::Item<'s>>,
) -> Option<syntax::Tree<'s>> {
    // Reverse-polish notation encoding.
    /// Classify an item as an operator-token, or other data; we track this state information
    /// because whenever consecutive operators or consecutive non-operators occur, we merge them
    /// into one node.
    #[derive(PartialEq, Eq)]
    enum ItemType {
        Ast,
        Opr,
    }
    use ItemType::*;
    let mut was_section_used = false;
    let mut output: Vec<syntax::Item> = default();
    let mut operator_stack: Vec<Vec<token::Operator>> = default();
    let mut unary_operator: Option<token::Operator> = default();
    let mut prev_type = None;
    let mut precedence_error = None;
    for item in items {
        if let syntax::Item::Token(
                Token { variant: token::Variant::Operator(opr), left_offset, code }) = item {
            // Item is an operator.
            if let Some(unsatisified_opr) = unary_operator.take() {
                output.push(syntax::Tree::unary_opr_app(unsatisified_opr, None).into());
                prev_type = Some(Ast);
            }
            let prev_type = mem::replace(&mut prev_type, Some(Opr));
            if opr.can_be_binary_infix {
            } else if opr.can_be_unary_prefix {
                if prev_type == Some(Ast) {
                    operator_stack.push(default());
                }
                let opr = Token(left_offset, code, opr);
                unary_operator = Some(opr);
                continue;
            } else {
                precedence_error.get_or_insert_with(|| format!("Precedence of: {:?}", code));
            };
            let prec = opr.precedence;
            let opr = Token(left_offset, code, opr);
            if prev_type == Some(Opr) && let Some(prev_opr) = operator_stack.last_mut() {
                // Error. Multiple operators next to each other.
                prev_opr.push(opr);
            } else {
                // Application has the highest precedence.
                const APP_PREC: usize = std::usize::MAX;
                while let Some(prev_opr) = operator_stack.last()
                    && prev_opr.first().map(|opr| opr.precedence).unwrap_or(APP_PREC) >= prec
                    && let Some(prev_opr) = operator_stack.pop()
                    && let Some(rhs) = output.pop()
                {
                    // Prev operator in the [`operator_stack`] has a higher precedence.
                    let lhs = output.pop().map(|t| t.to_ast());
                    if lhs.is_none() { was_section_used = true; }
                    let ast = syntax::tree::apply_operator(lhs, prev_opr, Some(rhs.to_ast()));
                    output.push(ast.into());
                }
                operator_stack.push(vec![opr]);
            }
        } else if let Some(opr) = unary_operator.take() {
            let rhs = Some(item.to_ast());
            output.push(syntax::Tree::unary_opr_app(opr, rhs).into());
            prev_type = Some(Ast);
        } else if prev_type == Some(Ast) && let Some(lhs) = output.pop() {
            // Multiple non-operators next to each other.
            let lhs = lhs.to_ast();
            let rhs = item.to_ast();
            let ast = syntax::tree::apply(lhs, rhs);
            output.push(ast.into());
        } else {
            // Non-operator that follows previously consumed operator.
            prev_type = Some(Ast);
            output.push(item);
        }
    }
    if let Some(unsatisified_opr) = unary_operator.take() {
        output.push(syntax::Tree::unary_opr_app(unsatisified_opr, None).into());
        prev_type = Some(Ast);
    }
    let mut opt_rhs = (prev_type == Some(Ast)).and_option_from(|| output.pop().map(|t| t.to_ast()));
    while let Some(opr) = operator_stack.pop() {
        let opt_lhs = output.pop().map(|t| t.to_ast());
        if opt_lhs.is_none() || opt_rhs.is_none() {
            was_section_used = true;
        }
        opt_rhs = Some(syntax::tree::apply_operator(opt_lhs, opr, opt_rhs));
    }
    if !output.is_empty() {
        panic!("Internal error. Not all tokens were consumed while constructing the expression.");
    }
    let out = if was_section_used {
        // This can't fail: `was_section_used` won't be true unless we had at least one input,
        // and if we have at least one input, we have output.
        let out = opt_rhs.unwrap();
        Some(syntax::Tree::opr_section_boundary(out))
    } else {
        opt_rhs
    };
    if let Some(error) = precedence_error {
        return Some(syntax::Tree::with_unsupported(out.unwrap(), error));
    }
    out
}
