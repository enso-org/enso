use crate::syntax::operator::section::MaybeSection;
use crate::syntax::token;
use crate::syntax::tree;
use crate::syntax::treebuilding::Spacing;
use crate::syntax::treebuilding::SpacingLookaheadTokenConsumer;
use crate::syntax::treebuilding::SpacingLookaheadTreeConsumer;
use crate::syntax::Finish;
use crate::syntax::GroupHierarchyConsumer;
use crate::syntax::Item;
use crate::syntax::ItemConsumer;
use crate::syntax::ScopeHierarchyConsumer;
use crate::syntax::Token;
use crate::syntax::Tree;

use std::fmt::Debug;



// ================
// === Operator ===
// ================

/// An operator, whose arity and precedence have been determined.
#[derive(Debug)]
pub struct Operator<'s> {
    pub left_precedence:  Option<ModifiedPrecedence>,
    pub right_precedence: ModifiedPrecedence,
    pub associativity:    token::Associativity,
    pub arity:            Arity<'s>,
}


// === Arity ===

/// Classifies the role of an operator.
#[derive(Debug)]
pub enum Arity<'s> {
    Unary(token::UnaryOperator<'s>),
    Binary {
        tokens:            Vec<Token<'s>>,
        missing:           Option<BinaryOperand>,
        reify_rhs_section: bool,
    },
    App,
    NamedApp(Box<NamedApp<'s>>),
}

#[derive(Debug)]
pub struct NamedApp<'s> {
    pub parens:     Option<(token::OpenSymbol<'s>, Option<token::CloseSymbol<'s>>)>,
    pub name:       token::Ident<'s>,
    pub equals:     token::AssignmentOperator<'s>,
    pub expression: Tree<'s>,
}

impl<'s> Arity<'s> {
    pub fn expects_rhs(&self) -> bool {
        matches!(
            self,
            Arity::Unary(_) | Arity::Binary { missing: None | Some(BinaryOperand::Left), .. }
        )
    }
}


// === Binary operand ===

#[derive(Debug)]
pub enum BinaryOperand {
    Left,
    Right,
}


// === Modified precedence ===

#[derive(Debug, Copy, Clone)]
pub struct ModifiedPrecedence {
    value: u8,
    mask:  u8,
}

pub struct ModifiedPrecedenceComparisonResult {
    pub is_greater:           bool,
    pub inconsistent_spacing: bool,
}

impl ModifiedPrecedence {
    pub fn new(spacing: Spacing, precedence: token::Precedence, is_value_operation: bool) -> Self {
        let unspaced_bit = match spacing {
            Spacing::Spaced => 0,
            Spacing::Unspaced => 0x80,
        };
        let value = precedence.into_u8() | unspaced_bit;
        let mask = if is_value_operation { 0x7f } else { 0xff };
        Self { value, mask }
    }

    pub fn compare(&self, other: &Self, include_eq: bool) -> ModifiedPrecedenceComparisonResult {
        let adjusted_self = self.value + include_eq as u8;
        let mask = self.mask | other.mask;
        let is_greater = adjusted_self & mask > other.value & mask;
        let is_greater_including_space = adjusted_self > other.value;
        let inconsistent_spacing = is_greater != is_greater_including_space;
        ModifiedPrecedenceComparisonResult { is_greater, inconsistent_spacing }
    }

    pub fn min() -> Self {
        Self { value: 0, mask: 0xff }
    }
}


// ================
// === Warnings ===
// ================

/// Warnings applicable to an operator.
#[derive(Debug, Default)]
#[allow(missing_copy_implementations)] // Future warnings may have attached information.
pub struct Warnings {
    inconsistent_spacing: bool,
}

impl Warnings {
    /// Mark the operator as having spacing inconsistent with effective precedence.
    pub fn set_inconsistent_spacing(&mut self) {
        self.inconsistent_spacing = true;
    }

    /// Attach the warnings to the tree.
    pub fn apply(self, tree: &mut Tree) {
        let Self { inconsistent_spacing } = self;
        if inconsistent_spacing {
            tree.warnings.push(tree::Warning::inconsistent_spacing());
        }
    }
}


// ======================================
// === Operator and Operand Consumers ===
// ======================================

pub trait OperandConsumer<'s> {
    fn push_operand(&mut self, operand: MaybeSection<Tree<'s>>);
}

pub trait OperatorConsumer<'s> {
    fn push_operator(&mut self, operator: Operator<'s>);
}

pub trait NamedOperandConsumer<'s> {
    fn push_operand(&mut self, operand: OperandMaybeNamed<'s>);
}


// === Debugging ===

#[derive(Debug, Default)]
pub struct Inspect<Inner>(Inner);

impl<Inner> Inspect<Inner> {
    fn observe(&self, event: &impl Debug) {
        eprintln!("-> {:?}", event);
    }

    fn observe_received(&self, event: &impl Debug) {
        eprintln!("<- {:?}", event);
    }

    fn observe_token<'a: 'b, 'b, T: Into<token::Ref<'a, 'b>>>(&self, token: T) {
        eprintln!("-> Token({})", token.into().code.repr.0);
    }
}

impl<'s, Inner: NamedOperandConsumer<'s>> NamedOperandConsumer<'s> for Inspect<Inner> {
    fn push_operand(&mut self, operand: OperandMaybeNamed<'s>) {
        self.observe(&operand);
        self.0.push_operand(operand);
    }
}

impl<'s, Inner: OperandConsumer<'s>> OperandConsumer<'s> for Inspect<Inner> {
    fn push_operand(&mut self, operand: MaybeSection<Tree<'s>>) {
        self.observe(&operand);
        self.0.push_operand(operand);
    }
}

impl<'s, Inner: OperatorConsumer<'s>> OperatorConsumer<'s> for Inspect<Inner> {
    fn push_operator(&mut self, operator: Operator<'s>) {
        self.observe(&operator);
        self.0.push_operator(operator);
    }
}

impl<T: Debug, Inner: ScopeHierarchyConsumer<Result = T>> ScopeHierarchyConsumer
    for Inspect<Inner>
{
    type Result = Inner::Result;

    fn start_scope(&mut self) {
        self.observe(&"StartScope");
        self.0.start_scope();
    }

    fn end_scope(&mut self) -> Self::Result {
        self.observe(&"EndScope");
        let result = self.0.end_scope();
        self.observe_received(&result);
        result
    }
}

impl<'s, Inner: GroupHierarchyConsumer<'s>> GroupHierarchyConsumer<'s> for Inspect<Inner> {
    fn start_group(&mut self, open: token::OpenSymbol<'s>) {
        self.observe_token(&open);
        self.0.start_group(open);
    }

    fn end_group(&mut self, close: token::CloseSymbol<'s>) {
        self.observe_token(&close);
        self.0.end_group(close);
    }
}

impl<'s, Inner: SpacingLookaheadTokenConsumer<'s>> SpacingLookaheadTokenConsumer<'s>
    for Inspect<Inner>
{
    fn push_token(&mut self, token: Token<'s>, following_spacing: Option<Spacing>) {
        self.observe_token(&token);
        self.0.push_token(token, following_spacing);
    }
}

impl<'s, Inner: SpacingLookaheadTreeConsumer<'s>> SpacingLookaheadTreeConsumer<'s>
    for Inspect<Inner>
{
    fn push_tree(&mut self, tree: Tree<'s>, following_spacing: Option<Spacing>) {
        self.observe(&tree);
        self.0.push_tree(tree, following_spacing);
    }
}

impl<'s, Inner: ItemConsumer<'s>> ItemConsumer<'s> for Inspect<Inner> {
    fn push_item(&mut self, item: Item<'s>) {
        match &item {
            Item::Token(token) => self.observe_token(token),
            _ => self.observe(&item),
        }
        self.0.push_item(item);
    }
}

impl<T: Debug, Inner: Finish<Result = T>> Finish for Inspect<Inner> {
    type Result = Inner::Result;

    fn finish(&mut self) -> Self::Result {
        self.observe(&"Finish");
        let result = self.0.finish();
        self.observe_received(&result);
        result
    }
}


// ===========================
// === Operator or Operand ===
// ===========================

#[derive(Debug)]
pub enum MaybeOperator<'s> {
    Operand,
    Operator(Operator<'s>),
}

impl<'s> From<Operator<'s>> for MaybeOperator<'s> {
    fn from(operator: Operator<'s>) -> Self {
        MaybeOperator::Operator(operator)
    }
}

impl<'s> MaybeOperator<'s> {
    pub fn expects_rhs(&self) -> bool {
        match self {
            MaybeOperator::Operand => false,
            MaybeOperator::Operator(op) => op.arity.expects_rhs(),
        }
    }
}


// ======================
// === Named Operands ===
// ======================

#[derive(Debug, PartialEq, Eq)]
#[allow(clippy::large_enum_variant)] // Clippy considers the `Unnamed` is "at least 0 bytes".
pub enum OperandMaybeNamed<'s> {
    Unnamed(MaybeSection<Tree<'s>>),
    Named {
        parens:     Option<(token::OpenSymbol<'s>, Option<token::CloseSymbol<'s>>)>,
        name:       token::Ident<'s>,
        equals:     token::AssignmentOperator<'s>,
        expression: Tree<'s>,
    },
}


// ==========================
// === SectionTermination ===
// ==========================

/// Operator-section/template-function termination behavior of an operator with regard to an
/// operand.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
pub enum SectionTermination {
    /// If the operand is an operator-section/template-function, indicate it by wrapping it in a
    /// suitable node.
    #[default]
    Reify,
    /// Discard any operator-section/template-function properties associated with the operand.
    Unwrap,
}
