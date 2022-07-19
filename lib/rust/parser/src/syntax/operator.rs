//! Operator related functionalities.

use crate::prelude::*;

use crate::syntax;
use crate::syntax::token;
use crate::syntax::token::Token;



// ==================
// === Precedence ===
// ==================

// FIXME: The current implementation hard-codes precedence values and does not support precedence
//  computations for any operator (according to the spec)
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
fn annotate_tokens_that_need_spacing(item: syntax::Item) -> syntax::Item {
    use syntax::tree::Variant::*;
    item.map_tree(|ast| match &*ast.variant {
        MultiSegmentApp(data) if !data.segments.first().header.is_symbol() =>
            ast.with_error("This expression cannot be used in a non-spaced equation."),
        _ => ast,
    })
}

/// If the input sequence is non-empty, return the result of applying
/// [`resolve_operator_precedence`] to it.
pub fn resolve_operator_precedence_if_non_empty(
    items: Vec<syntax::Item<'_>>,
) -> Option<syntax::Tree<'_>> {
    match NonEmptyVec::try_from(items) {
        Ok(items) => Some(resolve_operator_precedence(items)),
        _ => None,
    }
}

/// Take [`Item`] stream, resolve operator precedence and return the final AST.
///
/// The precedence resolution algorithm is based on the Shunting yard algorithm[1], extended to
/// handle operator sections.
/// [1]: https://en.wikipedia.org/wiki/Shunting_yard_algorithm
pub fn resolve_operator_precedence<'s>(items: NonEmptyVec<syntax::Item<'s>>) -> syntax::Tree<'s> {
    type Tokens<'s> = Vec<syntax::Item<'s>>;
    let mut flattened: Tokens<'s> = default();
    let mut no_space_group: Tokens<'s> = default();
    let process_no_space_group = |flattened: &mut Tokens<'s>, no_space_group: &mut Tokens<'s>| {
        let tokens = no_space_group.drain(..);
        if tokens.len() < 2 {
            flattened.extend(tokens);
        } else {
            let tokens = tokens.map(annotate_tokens_that_need_spacing);
            let ast = resolve_operator_precedence_internal(tokens);
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
) -> syntax::Tree<'s> {
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
    let mut operator_stack: Vec<WithPrecedence<syntax::tree::OperatorOrError>> = default();
    let mut prev_type = None;
    for item in items {
        if let syntax::Item::Token(
                Token { variant: token::Variant::Operator(opr), left_offset, code }) = item {
            // Item is an operator.
            let prev_type = mem::replace(&mut prev_type, Some(Opr));

            let prec = precedence_of(&code);
            let opr = Token(left_offset, code, opr);

            if prev_type == Some(Opr) && let Some(prev_opr) = operator_stack.last_mut() {
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
                    let ast = syntax::tree::apply_operator(lhs, prev_opr.elem, Some(rhs.to_ast()));
                    output.push(ast.into());
                }
                operator_stack.push(WithPrecedence::new(prec, Ok(opr)));
            }
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
    let mut opt_rhs = (prev_type == Some(Ast)).and_option_from(|| output.pop().map(|t| t.to_ast()));
    while let Some(opr) = operator_stack.pop() {
        let opt_lhs = output.pop().map(|t| t.to_ast());
        if opt_lhs.is_none() || opt_rhs.is_none() {
            was_section_used = true;
        }
        opt_rhs = Some(syntax::tree::apply_operator(opt_lhs, opr.elem, opt_rhs));
    }
    if !output.is_empty() {
        panic!("Internal error. Not all tokens were consumed while constructing the expression.");
    }

    // This unwrap is safe because:
    // - resolve_operator_precedence only calls this function with non-empty sequences as inputs.
    // - Given a non-empty input, we will always have at least one output.
    let out = opt_rhs.unwrap();
    if was_section_used {
        syntax::Tree::opr_section_boundary(out)
    } else {
        out
    }
}
