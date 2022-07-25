//! Operator related functionalities.

use crate::prelude::*;

use crate::syntax;
use crate::syntax::token;
use crate::syntax::token::Token;



// ==================
// === Precedence ===
// ==================

// FIXME: Compute precedences according to spec. Issue: #182497344
fn precedence_of(operator: &str) -> WipResult<usize> {
    Ok(match operator {
        // "There are a few operators with the lowest precedence possible."
        "=" => 1,
        ":" => 2,
        "->" => 3,
        "|" | "\\\\" | "&" => 4,
        ">>" | "<<" => 5,
        "|>" | "|>>" | "<|" | "<<|" => 6,
        // "The precedence of all other operators is determined by the operator's Precedence
        // Character:"
        "!" => 10,
        "||" => 11,
        "^" => 12,
        "&&" => 13,
        "+" | "++" | "-" => 14,
        "*" | "/" | "%" => 15,
        // FIXME: Not sure about these:
        "==" => 1,
        "," => 1,
        "@" => 20,
        "." => 21,
        _ => return Err(format!("precedence_of({:?})", operator)),
    })
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
    let mut operator_stack: Vec<WithPrecedence<syntax::tree::OperatorOrError>> = default();
    let mut prev_type = None;
    let mut precedence_error = None;
    // Used until precedence computation is implemented, to attempt to parse inputs with operators
    // that aren't in the precedence table. If we make use of this value the result will be
    // placed in an `Unsupported` node, so the exact number isn't important because we're
    // explicitly doing a best-effort parse of unknown syntax.
    const ARBITRARY_FALLBACK_PRECEDENCE: usize = 10;
    for item in items {
        if let syntax::Item::Token(
                Token { variant: token::Variant::Operator(opr), left_offset, code }) = item {
            // Item is an operator.
            let prev_type = mem::replace(&mut prev_type, Some(Opr));

            let prec = match precedence_of(&code) {
                Ok(prec) => prec,
                Err(e) => {
                    precedence_error.get_or_insert(e);
                    ARBITRARY_FALLBACK_PRECEDENCE
                }
            };
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
