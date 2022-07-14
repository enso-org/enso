//! Operator related functionalities.

use crate::prelude::*;

use crate::syntax;
use crate::syntax::token;
use crate::syntax::token::Token;



// ==================
// === Precedence ===
// ==================

// FIXME: The current implementation hard-codes precedence values and does not support precedence
// computations for any operator (according to the spec)
fn precedence_of(operator: &str) -> usize {
    match operator {
        "=" => 1,
        "+" => 3,
        "-" => 3,
        "*" => 7,
        _ => panic!("Operator not supported: {}", operator),
    }
}

/// An item with an assigned precedence.
#[derive(Clone, Copy, Debug, Deref, DerefMut)]
struct WithPrecedence<T> {
    #[deref]
    #[deref_mut]
    elem:       T,
    precedence: usize,
}

impl<T> WithPrecedence<T> {
    /// Constructor.
    pub fn new(precedence: usize, elem: T) -> Self {
        Self { elem, precedence }
    }
}


/// Annotate expressions that should use spacing, because otherwise they are misleading. For
/// example, `if cond then.x else.y` is parsed as `if cond then .x else .y`, which after expansion
/// translates to `if cond then (\t -> t.x) else (\t -> t.y)`. However, for some macros spacing is
/// not needed. For example, `(.x)` is parsed as `(\t -> t.x)`, which is understandable.
fn annotate_tokens_that_need_spacing(items: Vec<syntax::Item>) -> Vec<syntax::Item> {
    // TODO: It should be possible to make it faster by iterating over mut vec. To be checked.
    items
        .into_iter()
        .map(|item| match item {
            syntax::Item::Block(_) => item,
            syntax::Item::Token(_) => item,
            syntax::Item::Tree(ast) => syntax::Item::Tree(match &*ast.variant {
                syntax::tree::Variant::MultiSegmentApp(data)
                    if !data.segments.first().header.is_symbol() =>
                    ast.with_error("This expression cannot be used in a non-spaced equation."),
                _ => ast,
            }),
        })
        .collect()
}

/// Take [`Item`] stream, resolve operators precedence and return the final AST. The precedence
/// resolution algorithm bases on the [Shunting yard algorithm](https://en.wikipedia.org/wiki/Shunting_yard_algorithm).
/// It is extended to handle operator sections.
#[inline(always)]
pub fn resolve_operator_precedence<'s>(items: Vec<syntax::Item<'s>>) -> syntax::Tree<'s> {
    type Tokens<'s> = Vec<syntax::Item<'s>>;
    let mut flattened: Tokens<'s> = default();
    let mut no_space_group: Tokens<'s> = default();
    let processs_no_space_group = |flattened: &mut Tokens<'s>, no_space_group: &mut Tokens<'s>| {
        let tokens = mem::take(no_space_group);
        if tokens.len() == 1 {
            flattened.extend(tokens);
        } else {
            let tokens = annotate_tokens_that_need_spacing(tokens);
            let ast = resolve_operator_precedence_internal(tokens);
            flattened.push(ast.into());
        }
    };
    for item in items {
        if item.left_visible_offset().width_in_spaces == 0 || no_space_group.is_empty() {
            no_space_group.push(item)
        } else if !no_space_group.is_empty() {
            processs_no_space_group(&mut flattened, &mut no_space_group);
            no_space_group.push(item);
        } else {
            // FIXME: this is unreachable.
            flattened.push(item);
        }
    }
    if !no_space_group.is_empty() {
        processs_no_space_group(&mut flattened, &mut no_space_group);
    }
    resolve_operator_precedence_internal(flattened)
}

fn resolve_operator_precedence_internal(items: Vec<syntax::Item<'_>>) -> syntax::Tree<'_> {
    // Reverse-polish notation encoding.
    let mut was_section_used = false;
    let mut output: Vec<syntax::Item> = default();
    let mut operator_stack: Vec<WithPrecedence<syntax::tree::OperatorOrError>> = default();
    let mut last_token_was_ast = false;
    let mut last_token_was_opr = false;
    for item in items {
        if let syntax::Item::Token(token) = item.clone()
        && let token::Variant::Operator(opr) = token.variant {
            // Item is an operator.
            let last_token_was_opr_copy = last_token_was_opr;
            last_token_was_ast = false;
            last_token_was_opr = true;

            let prec = precedence_of(&token.code);
            let opr = Token(token.left_offset, token.code, opr);

            if last_token_was_opr_copy && let Some(prev_opr) = operator_stack.last_mut() {
                // Error. Multiple operators next to each other.
                match &mut prev_opr.elem {
                    Err(err) => err.operators.push(opr),
                    Ok(prev) => {
                        let operators = NonEmptyVec::new(prev.clone(),vec![opr]);
                        prev_opr.elem = Err(syntax::tree::MultipleOperatorError{operators});
                    }
                }
            } else {
                while let Some(prev_opr) = operator_stack.last()
                    && prev_opr.precedence >= prec
                    && let Some(prev_opr) = operator_stack.pop()
                    && let Some(rhs) = output.pop()
                {
                    // Prev operator in the [`operator_stack`] has a higher precedence.
                    let lhs = output.pop().map(|t| t.to_ast());
                    if lhs.is_none() { was_section_used = true; }
                    let ast = syntax::Tree::opr_app(lhs, prev_opr.elem, Some(rhs.to_ast()));
                    output.push(ast.into());
                }
                operator_stack.push(WithPrecedence::new(prec, Ok(opr)));
            }
        } else if last_token_was_ast && let Some(lhs) = output.pop() {
            // Multiple non-operators next to each other.
            let lhs = lhs.to_ast();
            let rhs = item.to_ast();
            let ast = syntax::Tree::app(lhs, rhs);
            output.push(ast.into());
        } else {
            // Non-operator that follows previously consumed operator.
            last_token_was_ast = true;
            last_token_was_opr = false;
            output.push(item);
        }
    }
    let mut opt_rhs = last_token_was_ast.and_option_from(|| output.pop().map(|t| t.to_ast()));
    while let Some(opr) = operator_stack.pop() {
        let opt_lhs = output.pop().map(|t| t.to_ast());
        if opt_lhs.is_none() || opt_rhs.is_none() {
            was_section_used = true;
        }
        opt_rhs = Some(syntax::Tree::opr_app(opt_lhs, opr.elem, opt_rhs));
    }
    if !output.is_empty() {
        panic!("Internal error. Not all tokens were consumed while constructing the expression.");
    }

    // FIXME
    let out = opt_rhs.unwrap();
    if was_section_used {
        syntax::Tree::opr_section_boundary(out)
    } else {
        out
    }
}
