//! SpanTree node kind implementation. Each node can be of a different kind (shape) and can contain
//! different information.

use crate::prelude::*;

use crate::ArgumentInfo;
use crate::TagValue;



// ============
// === Kind ===
// ============

/// An enum describing kind of node.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Kind {
    /// A root of the expression tree.
    Root,
    /// A node chained with parent node, part of prefix method application.
    ChainedPrefix,
    /// A node chained with parent node, part of infix operator application.
    ChainedInfix(ArgumentInfo),
    /// A node representing operation (operator or function) of parent Infix, Section or Prefix.
    Operation,
    /// A part of the access chain that is not the primary target (not the leftmost node).
    Access,
    /// A node being a normal (not target) parameter of parent Infix, Section or Prefix.
    Argument(Argument),
    /// A node representing a named argument. Always contains two tokens and an `Argument` node.
    NamedArgument,
    /// A node being a unmodifiable token in macro.
    Token,
    /// A node being a placeholder for inserting new child to Prefix or Operator chain. It should
    /// not have children, but can be assigned with a span representing the number of spaces
    /// between AST tokens. For example, given expression `foo   bar`, the span assigned to the
    /// `InsertionPoint` between `foo` and `bar` should be set to 3.
    InsertionPoint(InsertionPoint),
    /// A single line within a block.
    BlockLine,
}


// === Kind Constructors ===

#[allow(missing_docs)]
impl Kind {
    pub fn this() -> Argument {
        Self::prefix_argument().with_name(Some(Argument::THIS.into()))
    }
    pub fn argument() -> Argument {
        default()
    }
    pub fn prefix_argument() -> Argument {
        Argument::default().in_prefix_chain()
    }
    pub fn insertion_point() -> InsertionPoint {
        default()
    }
    pub fn chained_infix() -> Self {
        Self::ChainedInfix(default())
    }
}


// === Matchers ===

#[allow(missing_docs)]
impl Kind {
    pub fn is_root(&self) -> bool {
        matches!(self, Self::Root { .. })
    }
    pub fn is_chained(&self) -> bool {
        matches!(self, Self::ChainedPrefix | Self::ChainedInfix(..))
    }
    pub fn is_operation(&self) -> bool {
        matches!(self, Self::Operation { .. })
    }
    pub fn is_this(&self) -> bool {
        matches!(
            self,
            Self::Argument(arg)
            if arg.info.name.as_deref() == Some(Argument::THIS)
        )
    }
    pub fn is_argument(&self) -> bool {
        matches!(self, Self::Argument { .. })
    }
    pub fn is_named_argument(&self) -> bool {
        matches!(self, Self::NamedArgument)
    }
    pub fn is_token(&self) -> bool {
        matches!(self, Self::Token { .. })
    }
    pub fn is_insertion_point(&self) -> bool {
        matches!(self, Self::InsertionPoint { .. })
    }

    /// Match the value with `Kind::InsertionPoint{..}` but not
    /// `Kind::InsertionPoint(ExpectedArgument(_))`.
    pub fn is_positional_insertion_point(&self) -> bool {
        self.is_insertion_point() && !self.is_expected_argument() && !self.is_expected_operand()
    }

    /// Match the value with `Kind::InsertionPoint(ExpectedArgument(_))`.
    pub fn is_expected_argument(&self) -> bool {
        matches!(self, Self::InsertionPoint(t) if t.kind.is_expected_argument())
    }

    /// Check if given kind is an insertino point for expected operand of an unfinished infix.
    pub fn is_expected_operand(&self) -> bool {
        matches!(
            self,
            Self::InsertionPoint(InsertionPoint {
                kind: InsertionPointType::ExpectedOperand | InsertionPointType::ExpectedTarget,
                ..
            })
        )
    }

    /// Match the argument in a prefix method application.
    pub fn is_prefix_argument(&self) -> bool {
        matches!(self, Self::Argument(a) if a.in_prefix_chain)
    }

    /// If this kind is an expected argument, return its argument index.
    pub fn expected_argument_index(&self) -> Option<usize> {
        match self {
            Self::InsertionPoint(InsertionPoint {
                kind: InsertionPointType::ExpectedArgument { index, .. },
                ..
            }) => Some(*index),
            _ => None,
        }
    }
}


// === API ===

impl Kind {
    /// Removable flag getter.
    pub fn removable(&self) -> bool {
        match self {
            Self::Argument(t) => t.removable,
            _ => false,
        }
    }

    /// `ArgumentInfo` getter. Returns `None` if the node could not be attached with the
    /// information.
    pub fn argument_info(&self) -> Option<&ArgumentInfo> {
        match self {
            Self::Argument(t) => Some(&t.info),
            Self::InsertionPoint(t) => Some(&t.info),
            Self::ChainedInfix(info) => Some(info),
            _ => None,
        }
    }

    /// Get a reference to the name of an argument represented by this node, if available. Returns
    /// `None` if the node could not be attached with the argument information.
    pub fn argument_name(&self) -> Option<&str> {
        self.argument_info().and_then(|info| info.name.as_deref())
    }

    /// Type getter.
    pub fn tp(&self) -> Option<&String> {
        self.argument_info().and_then(|info| info.tp.as_ref())
    }

    /// Get the definition index of an argument represented by this node, if available. Returns
    /// `None` if the node could not be attached with the argument information.
    pub fn definition_index(&self) -> Option<usize> {
        #[allow(clippy::single_match)]
        match self {
            Self::Argument(t) => t.definition_index,
            _ => None,
        }
    }

    /// Get a reference to tag values of an argument represented by this node, if available. Returns
    /// `None` if the node could not be attached with the argument information.
    pub fn tag_values(&self) -> Option<&[TagValue]> {
        self.argument_info().map(|info| &info.tag_values[..])
    }

    /// Get the function call AST ID associated with this argument.
    pub fn call_id(&self) -> Option<ast::Id> {
        self.argument_info().and_then(|info| info.call_id)
    }

    /// `ArgumentInfo` setter.
    pub fn set_argument_info(&mut self, argument_info: ArgumentInfo) {
        match self {
            Self::Argument(t) => t.info = argument_info,
            Self::InsertionPoint(t) => t.info = argument_info,
            Self::ChainedInfix(info) => *info = argument_info,
            _ => (),
        }
    }

    /// Argument definition index setter.
    pub fn set_definition_index(&mut self, index: usize) {
        if let Self::Argument(t) = self {
            t.definition_index = Some(index);
        }
    }

    /// Call ID setter..
    pub fn set_call_id(&mut self, call_id: Option<ast::Id>) {
        match self {
            Self::Argument(t) => t.info.call_id = call_id,
            Self::InsertionPoint(t) => t.info.call_id = call_id,
            Self::ChainedInfix(info) => info.call_id = call_id,
            _ => (),
        }
    }

    /// Short string representation. Skips the inner fields and returns only the variant name.
    pub fn variant_name(&self) -> &str {
        match self {
            Self::Root => "Root",
            Self::ChainedPrefix => "ChainedPrefix",
            Self::ChainedInfix(_) => "ChainedInfix",
            Self::Operation => "Operation",
            Self::Access => "Access",
            Self::Argument(_) => "Argument",
            Self::NamedArgument => "NamedArgument",
            Self::Token => "Token",
            Self::InsertionPoint(_) => "InsertionPoint",
            Self::BlockLine => "BlockLine",
        }
    }
}


// === Impls ===

impl Default for Kind {
    fn default() -> Self {
        Self::insertion_point().into()
    }
}



// ================
// === Argument ===
// ================

/// Kind representing "argument" node. For example, in the following expressions, `a`, `b`, and `c`
/// are considered "arguments": `foo a b c`, `foo + a`.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
#[allow(missing_docs)]
pub struct Argument {
    pub removable:        bool,
    pub in_prefix_chain:  bool,
    /// The index of the argument in the function definition.
    pub definition_index: Option<usize>,
    pub info:             ArgumentInfo,
}


// === Setters ===

#[allow(missing_docs)]
impl Argument {
    pub const THIS: &'static str = "self";

    pub fn typed(mut self, tp: String) -> Self {
        self.info.tp = Some(tp);
        self
    }
    pub fn removable(mut self) -> Self {
        self.removable = true;
        self
    }
    pub fn in_prefix_chain(mut self) -> Self {
        self.in_prefix_chain = true;
        self
    }
    pub fn with_removable(mut self, rm: bool) -> Self {
        self.removable = rm;
        self
    }
    pub fn with_name(mut self, name: Option<String>) -> Self {
        self.info.name = name;
        self
    }
    pub fn with_tp(mut self, tp: Option<String>) -> Self {
        self.info.tp = tp;
        self
    }
    pub fn with_call_id(mut self, call_id: Option<ast::Id>) -> Self {
        self.info.call_id = call_id;
        self
    }
    pub fn indexed(mut self, index: usize) -> Self {
        self.definition_index = Some(index);
        self
    }
}

impl From<Argument> for Kind {
    fn from(t: Argument) -> Self {
        Self::Argument(t)
    }
}



// ======================
// === InsertionPoint ===
// ======================

/// Kind representing "insertion point" node. For example, in the following expressions, all places
/// marked with `?` are considered "insertion points" (please note that `?` symbols are NOT part
/// of the source code): `foo ? a ? b ? c ? ? ?`. The final question marks indicate non-provided
/// arguments.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
#[allow(missing_docs)]
pub struct InsertionPoint {
    pub kind: InsertionPointType,
    pub info: ArgumentInfo,
}

// === Constructors ===

#[allow(missing_docs)]
impl InsertionPoint {
    pub fn expected_argument(index: usize) -> Self {
        Self::default().with_kind(InsertionPointType::ExpectedArgument { index, named: false })
    }
    pub fn expected_named_argument_erased(index: usize) -> Self {
        Self::default().with_kind(InsertionPointType::ExpectedArgument { index, named: true })
    }
    pub fn expected_named_argument(index: usize, name: impl Str) -> Self {
        let kind = InsertionPointType::ExpectedArgument { index, named: true };
        let name = Some(name.into());
        Self::default().with_kind(kind).with_name(name)
    }
}


// === Setters ===

#[allow(missing_docs)]
impl InsertionPoint {
    pub fn with_kind(mut self, kind: InsertionPointType) -> Self {
        self.kind = kind;
        self
    }

    pub fn with_name(mut self, name: Option<String>) -> Self {
        self.info.name = name;
        self
    }
}

impl From<InsertionPoint> for Kind {
    fn from(t: InsertionPoint) -> Self {
        Self::InsertionPoint(t)
    }
}



// ==========================
// === InsertionPointType ===
// ==========================

/// A helpful information about how the new AST should be inserted during Set action. See `action`
/// module.
#[allow(missing_docs)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum InsertionPointType {
    BeforeArgument(usize),
    Append,
    /// AST should be inserted as an argument at given index into the chain.
    /// Note that this is just argument index in the application, it may be not the same as the
    /// index of the function parameter, as `this` argument might be passed using the `this.func`
    /// notation.
    ExpectedArgument {
        index: usize,
        named: bool,
    },
    /// Expected target of unfinished infix expression.
    ExpectedTarget,
    /// Expected operand of unfinished infix expression.
    ExpectedOperand,
}

// === Matchers ===
#[allow(missing_docs)]
impl InsertionPointType {
    pub fn is_expected_argument(&self) -> bool {
        matches!(self, Self::ExpectedArgument { .. })
    }
}

impl Default for InsertionPointType {
    fn default() -> Self {
        Self::Append
    }
}

impl From<InsertionPointType> for Kind {
    fn from(t: InsertionPointType) -> Self {
        InsertionPoint::from(t).into()
    }
}

impl From<InsertionPointType> for InsertionPoint {
    fn from(t: InsertionPointType) -> Self {
        InsertionPoint::default().with_kind(t)
    }
}
