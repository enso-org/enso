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
            let ast = resolve_operator_precedence_internal(tokens, true).unwrap();
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
    resolve_operator_precedence_internal(flattened, false)
}

fn resolve_operator_precedence_internal<'s>(
    items: impl IntoIterator<Item = syntax::Item<'s>>,
    nospace: bool,
) -> Option<syntax::Tree<'s>> {
    let mut expression = ExpressionBuilder { nospace, ..default() };
    for item in items {
        if let syntax::Item::Token(Token {
            variant: token::Variant::Operator(opr),
            left_offset,
            code,
        }) = item
        {
            expression.operator(Token(left_offset, code, opr));
        } else {
            expression.operand(item);
        }
    }
    expression.finish()
}


// === Expression builder ===

/// Stack machine that builds an expression from syntax nodes.
///
/// The operator-precedence algorithm[1] used is based on the shunting yard algorithm[2], extended
/// to support *operator sections*, function application, and unary operators, and correctly report
/// errors relating to consecutive operators.
///
/// [^1](https://en.wikipedia.org/wiki/Operator-precedence_parser)
/// [^2](https://en.wikipedia.org/wiki/Shunting_yard_algorithm)
#[derive(Default)]
struct ExpressionBuilder<'s> {
    was_section_used: bool,
    output:           Vec<syntax::Item<'s>>,
    operator_stack:   Vec<Operator<'s>>,
    prev_type:        Option<ItemType>,
    precedence_error: Option<String>,
    nospace:          bool,
}

impl<'s> ExpressionBuilder<'s> {
    /// Extend the expression with an operand.
    pub fn operand(&mut self, item: syntax::Item<'s>) {
        let item = if self.prev_type == Some(ItemType::Ast) {
            // Multiple non-operators next to each other.
            let lhs = self.output.pop().unwrap();
            let lhs = lhs.to_ast();
            let rhs = item.to_ast();
            syntax::tree::apply(lhs, rhs).into()
        } else {
            // Non-operator that follows previously consumed operator.
            item
        };
        self.prev_type = Some(ItemType::Ast);
        self.output.push(item);
    }

    /// Extend the expression with an operator.
    pub fn operator(&mut self, opr: token::Operator<'s>) {
        use ItemType::*;
        let assoc = opr.properties.associativity();
        match (
            self.nospace,
            opr.properties.binary_infix_precedence(),
            opr.properties.unary_prefix_precedence(),
        ) {
            // If an operator has a binary role, and a LHS is available, it's acting as binary.
            (_, Some(prec), _) if self.prev_type == Some(Ast) =>
                self.binary_operator(prec, assoc, opr),
            // Otherwise, if the operator is inside a nospace group, and it has a unary role,
            // it's acting as unary.
            (true, _, Some(prec)) => self.push_operator(prec, assoc, Arity::Unary(opr)),
            // Outside of a nospace group, a unary-only operator is missing an operand.
            (false, None, Some(_)) => self.operand(syntax::Tree::unary_opr_app(opr, None).into()),
            // Binary operator section (no LHS).
            (_, Some(prec), _) => self.binary_operator(prec, assoc, opr),
            // Failed to compute a role for the operator; this should not be possible.
            (_, None, None) => {
                // We don't know the correct precedence, so we can't structure the tree correctly.
                // Pick some arbitrary value so we can at least produce *some* tree containing all
                // the right subexpressions; we'll wrap the expression in an `Invalid` node.
                const ARBITRARY_PRECEDENCE: token::Precedence = token::Precedence { value: 20 };
                let error = || format!("Precedence of: {:?}", opr.code);
                self.precedence_error.get_or_insert_with(error);
                self.binary_operator(ARBITRARY_PRECEDENCE, assoc, opr);
            }
        }
    }

    /// Extend the expression with a binary operator, by pushing it to the `operator_stack` or
    /// emitting a multiple-operator error.
    fn binary_operator(
        &mut self,
        prec: token::Precedence,
        assoc: token::Associativity,
        opr: token::Operator<'s>,
    ) {
        if self.prev_type == Some(ItemType::Opr)
                && let Some(prev_opr) = self.operator_stack.last_mut()
                && let Arity::Binary(oprs) = &mut prev_opr.opr {
            oprs.push(opr);
            return;
        }
        self.push_operator(prec, assoc, Arity::Binary(vec![opr]));
    }

    /// Add an operator to the stack; [`reduce`] the stack first, as appropriate for the specified
    /// precedence.
    fn push_operator(
        &mut self,
        precedence: token::Precedence,
        associativity: token::Associativity,
        opr: Arity<'s>,
    ) {
        let opr = Operator { precedence, associativity, opr };
        if self.prev_type != Some(ItemType::Opr) {
            // If the previous item was also an operator, this must be a unary operator following a
            // binary operator; we cannot reduce the stack because the unary operator must be
            // evaluated before the binary operator, regardless of precedence.
            let mut rhs = self.output.pop().map(|rhs| rhs.to_ast());
            self.reduce(precedence, &mut rhs);
            if let Some(rhs) = rhs {
                self.output.push(rhs.into());
            }
        }
        self.operator_stack.push(opr);
        self.prev_type = Some(ItemType::Opr);
    }

    /// Given a starting value, replace it with the result of successively applying to it all
    /// operators in the `operator_stack` that have precedence greater than or equal to the
    /// specified value, consuming LHS values from the `output` stack as needed.
    fn reduce(&mut self, prec: token::Precedence, rhs: &mut Option<syntax::Tree<'s>>) {
        while let Some(opr) = self.operator_stack.pop_if(|opr| {
            opr.precedence > prec
                || (opr.precedence == prec && opr.associativity == token::Associativity::Left)
        }) {
            let rhs_ = rhs.take();
            let ast = match opr.opr {
                Arity::Unary(opr) => syntax::Tree::unary_opr_app(opr, rhs_),
                Arity::Binary(opr) => {
                    let lhs = self.output.pop().map(|t| t.to_ast());
                    let can_form_section = opr.len() != 1 || opr[0].properties.can_form_section();
                    self.was_section_used = self.was_section_used
                        || (can_form_section && (lhs.is_none() || rhs_.is_none()));
                    syntax::tree::apply_operator(lhs, opr, rhs_)
                }
            };
            *rhs = Some(ast);
        }
    }

    /// Return an expression constructed from the accumulated state. Will return `None` only if no
    /// inputs were provided.
    pub fn finish(mut self) -> Option<syntax::Tree<'s>> {
        use ItemType::*;
        let mut item =
            (self.prev_type == Some(Ast)).and_option_from(|| self.output.pop().map(|t| t.to_ast()));
        self.reduce(token::Precedence::min(), &mut item);
        if !self.output.is_empty() {
            panic!(
                "Internal error. Not all tokens were consumed while constructing the expression."
            );
        }
        let out = if self.was_section_used {
            // This can't fail: `was_section_used` won't be true unless we had at least one input,
            // and if we have at least one input, we have output.
            let out = item.unwrap();
            Some(syntax::Tree::opr_section_boundary(out))
        } else {
            item
        };
        if let Some(error) = self.precedence_error {
            return Some(syntax::Tree::with_unsupported(out.unwrap(), error));
        }
        out
    }
}

/// Classify an item as an operator, or operand; this is used in [`resolve_operator_precedence`] to
/// merge consecutive nodes of the same type.
#[derive(PartialEq, Eq)]
enum ItemType {
    Ast,
    Opr,
}

/// An operator, whose arity and precedence have been determined.
#[derive(Debug)]
struct Operator<'s> {
    precedence:    token::Precedence,
    associativity: token::Associativity,
    opr:           Arity<'s>,
}

/// Classifies the role of an operator.
#[derive(Debug)]
enum Arity<'s> {
    Unary(token::Operator<'s>),
    Binary(Vec<token::Operator<'s>>),
}
