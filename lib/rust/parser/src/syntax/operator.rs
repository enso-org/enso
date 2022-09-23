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
        MultiSegmentApp(data)
            if !matches!(data.segments.first().header.variant, token::Variant::OpenSymbol(_)) =>
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
        if let syntax::item::Item::Token(Token { variant: token::Variant::Operator(opr), .. })
                = item && opr.properties.is_sequence() {
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
    output:         Vec<Operand<syntax::Item<'s>>>,
    operator_stack: Vec<Operator<'s>>,
    prev_type:      Option<ItemType>,
    nospace:        bool,
}

impl<'s> ExpressionBuilder<'s> {
    /// Extend the expression with an operand.
    pub fn operand(&mut self, item: syntax::Item<'s>) {
        let mut operand = Operand::from(item);
        if self.prev_type.replace(ItemType::Ast) == Some(ItemType::Ast) {
            operand = map2(self.output.pop().unwrap(), operand, |lhs, rhs| {
                syntax::tree::apply(lhs.to_ast(), rhs.to_ast()).into()
            });
        }
        self.output.push(operand);
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
            (_, None, None) => unreachable!(),
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
            if oprs.len() == 1 && opr.properties.is_type_annotation() {
                let prev = match self.operator_stack.pop().unwrap().opr {
                    Arity::Binary(oprs) => oprs.into_iter().next().unwrap(),
                    _ => unreachable!(),
                };
                let tp = token::Variant::ident(false, 0, false, false);
                let prev = Token(prev.left_offset, prev.code, tp);
                self.output.push(syntax::item::Item::Token(prev).into());
            } else {
                oprs.push(opr);
                return;
            }
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
            let mut rhs = self.output.pop();
            self.reduce(precedence, &mut rhs);
            if let Some(rhs) = rhs {
                self.output.push(rhs);
            }
        }
        self.operator_stack.push(opr);
        self.prev_type = Some(ItemType::Opr);
    }

    /// Given a starting value, replace it with the result of successively applying to it all
    /// operators in the `operator_stack` that have precedence greater than or equal to the
    /// specified value, consuming LHS values from the `output` stack as needed.
    fn reduce(&mut self, prec: token::Precedence, rhs: &mut Option<Operand<syntax::Item<'s>>>) {
        while let Some(opr) = self.operator_stack.pop_if(|opr| {
            opr.precedence > prec
                || (opr.precedence == prec && opr.associativity == token::Associativity::Left)
        }) {
            let rhs_ = rhs.take();
            let map_ast = |item: Option<syntax::Item<'s>>| item.map(|item| item.to_ast().into());
            let ast = match opr.opr {
                Arity::Unary(opr) => Operand::from(rhs_)
                    .map(|item| syntax::Tree::unary_opr_app(opr, map_ast(item)).into()),
                Arity::Binary(opr) => {
                    let lhs = self.output.pop();
                    if opr.iter().any(|op| op.properties.is_operator_section_barrier()) {
                        let lhs = lhs.map(syntax::Tree::from);
                        let rhs = rhs_.map(syntax::Tree::from);
                        let ast = syntax::tree::apply_operator(lhs, opr, rhs, self.nospace);
                        syntax::Item::from(ast).into()
                    } else {
                        let lhs = Operand::from(lhs);
                        let rhs = Operand::from(rhs_);
                        let mut elided = 0;
                        if opr.len() != 1 || opr[0].properties.can_form_section() {
                            elided += lhs.value.is_none() as u32 + rhs.value.is_none() as u32;
                        }
                        let mut operand = map2(lhs, rhs, |lhs, rhs| {
                            syntax::tree::apply_operator(
                                map_ast(lhs),
                                opr,
                                map_ast(rhs),
                                self.nospace,
                            )
                            .into()
                        });
                        operand.elided += elided;
                        operand
                    }
                }
            };
            *rhs = Some(ast);
        }
    }

    /// Return an expression constructed from the accumulated state. Will return `None` only if no
    /// inputs were provided.
    pub fn finish(mut self) -> Option<syntax::Tree<'s>> {
        use ItemType::*;
        let mut out = (self.prev_type == Some(Ast)).and_option_from(|| self.output.pop());
        self.reduce(token::Precedence::min(), &mut out);
        if !self.output.is_empty() {
            panic!(
                "Internal error. Not all tokens were consumed while constructing the expression."
            );
        }
        out.map(syntax::Tree::from)
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


// === Operand ===

#[derive(Default, Debug)]
struct Operand<T> {
    value:     T,
    elided:    u32,
    wildcards: u32,
}

impl<T> From<Option<Operand<T>>> for Operand<Option<T>> {
    fn from(operand: Option<Operand<T>>) -> Self {
        match operand {
            Some(Operand { value, elided, wildcards }) =>
                Self { value: Some(value), elided, wildcards },
            None => default(),
        }
    }
}

impl<'s> From<syntax::Item<'s>> for Operand<syntax::Item<'s>> {
    fn from(value: syntax::Item<'s>) -> Self {
        let is_wildcard = matches!(
            value,
            syntax::Item::Token(Token { variant: token::Variant::Wildcard(_), .. })
        );
        let wildcards = is_wildcard as _;
        Self { value, wildcards, elided: default() }
    }
}

impl<'s> From<Operand<syntax::Item<'s>>> for syntax::Tree<'s> {
    fn from(operand: Operand<syntax::Item<'s>>) -> Self {
        let Operand { value, elided, wildcards } = operand;
        let value = value.to_ast();
        if elided != 0 || wildcards != 0 {
            syntax::tree::operator_section(elided, wildcards, value)
        } else {
            value
        }
    }
}

impl<T> Operand<T> {
    fn map<U>(self, f: impl FnOnce(T) -> U) -> Operand<U> {
        let Self { value, elided, wildcards } = self;
        let value = f(value);
        Operand { value, elided, wildcards }
    }
}

impl<T> Operand<Operand<T>> {
    fn join(self) -> Operand<T> {
        let Self { mut value, elided, wildcards } = self;
        value.elided += elided;
        value.wildcards += wildcards;
        value
    }
}

fn map2<T, U, V>(t: Operand<T>, u: Operand<U>, f: impl FnOnce(T, U) -> V) -> Operand<V> {
    t.map(|t| u.map(|u| f(t, u))).join()
}
