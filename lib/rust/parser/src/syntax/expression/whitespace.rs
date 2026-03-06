use crate::prelude::*;

use crate::syntax::Flush;
use crate::syntax::GroupHierarchyConsumer;
use crate::syntax::Item;
use crate::syntax::Token;
use crate::syntax::Tree;
use crate::syntax::expression::consumer::TokenConsumer;
use crate::syntax::expression::consumer::TreeConsumer;
use crate::syntax::expression::types::Operator;
use crate::syntax::expression::types::OperatorConsumer;
use crate::syntax::token;
use crate::syntax::tree;

// ===============
// === Spacing ===
// ===============

/// Whether a term is logically separated from the previous term by whitespace.
#[derive(Debug, Default, PartialEq, Eq, Copy, Clone)]
pub enum Spacing {
    /// The term is preceded by whitespace, or should be treated as if it were.
    #[default]
    Spaced,
    /// The term is not preceded by whitespace.
    Unspaced,
}

impl Spacing {
    /// Get the whitespace information for a `Tree`.
    pub fn of_tree(tree: &Tree) -> Self {
        match tree_starts_new_no_space_group(tree) {
            false => Spacing::Unspaced,
            true => Spacing::Spaced,
        }
    }

    /// Get the whitespace information for a `Token`.
    pub fn of_token<'a: 'b, 'b, T: Into<token::Ref<'a, 'b>>>(token: T) -> Self {
        match token_starts_new_no_space_group(token) {
            false => Spacing::Unspaced,
            true => Spacing::Spaced,
        }
    }

    /// Get the whitespace information for an `Item`.
    pub fn of_item(item: &Item) -> Self {
        match item {
            Item::Token(token) => Spacing::of_token(token),
            Item::Tree(tree) => Spacing::of_tree(tree),
            Item::Group(group) => Spacing::of_token(&group.open),
            Item::Block(_) => Spacing::Spaced,
        }
    }
}

// Returns `true` for an item if that item should not follow any other item in a no-space group
// (i.e. the item has "space" before it).
fn token_starts_new_no_space_group<'a: 'b, 'b, T: Into<token::Ref<'a, 'b>>>(token: T) -> bool {
    let token = token.into();
    token.left_offset.visible.width_in_spaces != 0
        || matches!(token.data, token::Variant::CommaOperator(_))
}

fn tree_starts_new_no_space_group(tree: &Tree) -> bool {
    use tree::Variant::*;
    tree.span.left_offset.visible.width_in_spaces != 0
        || match &tree.variant {
            BodyBlock(_)
            | ArgumentBlockApplication(_)
            | OperatorBlockApplication(_)
            | SuspendedDefaultArguments(_) => true,
            Invalid(_)
            | Ident(_)
            | Private(_)
            | Number(_)
            | Wildcard(_)
            | TextLiteral(_)
            | App(_)
            | NamedApp(_)
            | OprApp(_)
            | PropertyAccess(_)
            | UnaryOprApp(_)
            | AutoscopedIdentifier(_)
            | TemplateFunction(_)
            | MultiSegmentApp(_)
            | TypeDef(_)
            | Assignment(_)
            | Function(_)
            | ForeignFunction(_)
            | Import(_)
            | Export(_)
            | Group(_)
            | TypeSignatureDeclaration(_)
            | TypeAnnotated(_)
            | CaseOf(_)
            | Lambda(_)
            | Array(_)
            | Tuple(_)
            | Annotation(_)
            | AnnotatedBuiltin(_)
            | Documentation(_)
            | ExpressionStatement(_)
            | ConstructorDefinition(_)
            | Call(_) => false,
        }
}

// ============================
// === Whitespace Lookahead ===
// ============================

pub trait SpacingLookaheadTreeConsumer<'s> {
    fn push_tree(&mut self, tree: Tree<'s>, following_spacing: Option<Spacing>);
}

pub trait SpacingLookaheadTokenConsumer<'s> {
    fn push_token(&mut self, token: Token<'s>, following_spacing: Option<Spacing>);
}

/// Maintains 1-token whitespace lookahead.
#[derive(Debug, Default, Finish)]
pub struct PeekSpacing<'s, Inner> {
    current: Option<Term<'s>>,
    inner: Inner,
}

#[derive(Debug, From)]
enum Term<'s> {
    Token(Token<'s>),
    Tree(Tree<'s>),
    Operator(Operator<'s>),
}

impl<'s, Inner> PeekSpacing<'s, Inner>
where
    Inner:
        SpacingLookaheadTreeConsumer<'s> + SpacingLookaheadTokenConsumer<'s> + OperatorConsumer<'s>,
{
    fn emit(&mut self, tt: Option<Term<'s>>, rhs: Option<Spacing>) {
        match tt {
            Some(Term::Token(token)) => self.inner.push_token(token, rhs),
            Some(Term::Tree(tree)) => self.inner.push_tree(tree, rhs),
            Some(Term::Operator(operator)) => self.inner.push_operator(operator),
            None => {}
        }
    }
}

impl<'s, Inner> Flush for PeekSpacing<'s, Inner>
where
    Inner:
        SpacingLookaheadTreeConsumer<'s> + SpacingLookaheadTokenConsumer<'s> + OperatorConsumer<'s>,
{
    fn flush(&mut self) {
        let last = self.current.take();
        self.emit(last, None);
    }
}

impl<'s, Inner> TokenConsumer<'s> for PeekSpacing<'s, Inner>
where
    Inner:
        SpacingLookaheadTreeConsumer<'s> + SpacingLookaheadTokenConsumer<'s> + OperatorConsumer<'s>,
{
    fn push_token(&mut self, token: Token<'s>) {
        let rhs = Spacing::of_token(&token);
        let next = self.current.replace(token.into());
        self.emit(next, Some(rhs))
    }
}

impl<'s, Inner> TreeConsumer<'s> for PeekSpacing<'s, Inner>
where
    Inner:
        SpacingLookaheadTreeConsumer<'s> + SpacingLookaheadTokenConsumer<'s> + OperatorConsumer<'s>,
{
    fn push_tree(&mut self, tree: Tree<'s>) {
        let rhs = Spacing::of_tree(&tree);
        let next = self.current.replace(tree.into());
        self.emit(next, Some(rhs));
    }
}

impl<'s, Inner> OperatorConsumer<'s> for PeekSpacing<'s, Inner>
where
    Inner:
        SpacingLookaheadTreeConsumer<'s> + SpacingLookaheadTokenConsumer<'s> + OperatorConsumer<'s>,
{
    fn push_operator(&mut self, operator: Operator<'s>) {
        let rhs = operator.spacing();
        let next = self.current.replace(operator.into());
        self.emit(next, Some(rhs))
    }
}

impl<'s, Inner> GroupHierarchyConsumer<'s> for PeekSpacing<'s, Inner>
where
    Inner: GroupHierarchyConsumer<'s>
        + SpacingLookaheadTreeConsumer<'s>
        + SpacingLookaheadTokenConsumer<'s>
        + OperatorConsumer<'s>,
{
    fn start_group(&mut self, open: token::OpenSymbol<'s>) {
        let prev = self.current.take();
        self.emit(prev, Spacing::of_token(&open).into());
        self.inner.start_group(open);
    }

    fn end_group(&mut self, close: Option<token::CloseSymbol<'s>>) {
        self.flush();
        self.inner.end_group(close);
    }
}
